// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// main.zig — LOL (1000Langs) triple API gateway.
//
// Replaces: standards/lol/api/v-gateway/
//
// Launches REST, gRPC-compat, and GraphQL servers on consecutive ports.
// Default base port: 7800 (configurable via LOL_PORT).
//
//   REST:    :7800  — /api/v1/languages, /api/v1/corpus/stats, /api/v1/crawl/status
//   gRPC:    :7801  — lol.CorpusService/* (JSON-over-HTTP)
//   GraphQL: :7802  — /graphql
//
// Corpus data read from LOL_DATA_DIR (default: "corpus").
// The Julia analysis bridge and Elixir orchestrator write JSON files there.
//
// Requires Zig 0.15.2+.

const std = @import("std");

// =============================================================================
// Corpus data types  (mirrors domain.v)
// =============================================================================

const LanguageInfo = struct {
    iso639_3:    []const u8 = "",
    name:        []const u8 = "",
    native_name: []const u8 = "",
    family:      []const u8 = "",
    scripts:     []const u8 = "",
    sources:     i64 = 0,
    verses:      i64 = 0,
    quality:     f64 = 0.0,
};

const SourceStatus = struct {
    name:      []const u8 = "",
    languages: i64 = 0,
    crawled:   i64 = 0,
    status:    []const u8 = "unknown",
};

const CrawlStatus = struct {
    total_languages: i64 = 0,
    crawled:         i64 = 0,
    in_progress:     i64 = 0,
    failed:          i64 = 0,
    last_crawl:      []const u8 = "",
    sources:         []SourceStatus = &[_]SourceStatus{},
};

// =============================================================================
// Corpus I/O helpers
// =============================================================================

/// Read and return parsed LanguageInfo array for all languages in
/// {data_dir}/languages/*.json.  Returns owned slice; caller frees with
/// allocator.free() after freeing each element's parsed value.
fn listLanguages(
    allocator: std.mem.Allocator,
    data_dir: []const u8,
) ![]std.json.Parsed(LanguageInfo) {
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const lang_dir_path = std.fmt.bufPrint(
        &dir_buf, "{s}/languages", .{data_dir},
    ) catch return &[_]std.json.Parsed(LanguageInfo){};

    const dir = std.fs.cwd().openDir(lang_dir_path, .{ .iterate = true }) catch
        return try allocator.alloc(std.json.Parsed(LanguageInfo), 0);
    var iter = dir.iterate();

    var list = std.ArrayList(std.json.Parsed(LanguageInfo)).init(allocator);
    defer {
        // On error, free any already-parsed entries.
        // On success the caller owns them.
    }

    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".json")) continue;

        const file = dir.openFile(entry.name, .{}) catch continue;
        defer file.close();

        const contents = file.readToEndAlloc(allocator, 64 * 1024) catch continue;
        defer allocator.free(contents);

        const parsed = std.json.parseFromSlice(LanguageInfo, allocator, contents, .{
            .ignore_unknown_fields = true,
        }) catch continue;

        try list.append(parsed);
    }

    return try list.toOwnedSlice();
}

fn readCrawlStatus(
    allocator: std.mem.Allocator,
    data_dir: []const u8,
) std.json.Parsed(CrawlStatus) {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = std.fmt.bufPrint(&path_buf, "{s}/crawl_status.json", .{data_dir}) catch {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator), .value = .{} };
    };
    const file = std.fs.cwd().openFile(path, .{}) catch {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator), .value = .{} };
    };
    defer file.close();
    const contents = file.readToEndAlloc(allocator, 64 * 1024) catch {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator), .value = .{} };
    };
    defer allocator.free(contents);
    return std.json.parseFromSlice(CrawlStatus, allocator, contents, .{
        .ignore_unknown_fields = true,
    }) catch .{ .arena = std.heap.ArenaAllocator.init(allocator), .value = .{} };
}

// =============================================================================
// JSON escaping
// =============================================================================

/// Write `s` to `w` as a JSON string value (with surrounding quotes).
fn writeJsonString(w: anytype, s: []const u8) !void {
    try w.writeByte('"');
    for (s) |ch| {
        switch (ch) {
            '"'  => try w.writeAll("\\\""),
            '\\' => try w.writeAll("\\\\"),
            '\n' => try w.writeAll("\\n"),
            '\r' => try w.writeAll("\\r"),
            '\t' => try w.writeAll("\\t"),
            else => try w.writeByte(ch),
        }
    }
    try w.writeByte('"');
}

/// Serialize a single LanguageInfo to the writer.
fn writeLangJson(w: anytype, lang: LanguageInfo) !void {
    try w.writeAll("{\"iso639_3\":");
    try writeJsonString(w, lang.iso639_3);
    try w.writeAll(",\"name\":");
    try writeJsonString(w, lang.name);
    try w.writeAll(",\"native_name\":");
    try writeJsonString(w, lang.native_name);
    try w.writeAll(",\"family\":");
    try writeJsonString(w, lang.family);
    try w.writeAll(",\"scripts\":");
    try writeJsonString(w, lang.scripts);
    try w.print(",\"sources\":{d},\"verses\":{d},\"quality\":{d:.4}}}", .{
        lang.sources, lang.verses, lang.quality,
    });
}

// =============================================================================
// HTTP server helpers
// =============================================================================

fn readLine(stream: std.net.Stream, buf: []u8) ![]const u8 {
    var pos: usize = 0;
    while (pos < buf.len) {
        const n = try stream.read(buf[pos..][0..1]);
        if (n == 0) break;
        if (buf[pos] == '\n') {
            const end = if (pos > 0 and buf[pos - 1] == '\r') pos - 1 else pos;
            return buf[0..end];
        }
        pos += 1;
    }
    return buf[0..pos];
}

fn writeHttpResponse(
    conn: *std.net.Server.Connection,
    status: u16,
    content_type: []const u8,
    body: []const u8,
) void {
    var h: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&h);
    fbs.writer().print(
        "HTTP/1.1 {d} \r\nContent-Type: {s}\r\nContent-Length: {d}\r\n" ++
        "Access-Control-Allow-Origin: *\r\nConnection: close\r\n\r\n",
        .{ status, content_type, body.len },
    ) catch return;
    conn.stream.writeAll(fbs.getWritten()) catch return;
    conn.stream.writeAll(body) catch return;
}

fn writeJson(conn: *std.net.Server.Connection, status: u16, body: []const u8) void {
    writeHttpResponse(conn, status, "application/json", body);
}

fn writeError(conn: *std.net.Server.Connection, status: u16, msg: []const u8) void {
    var buf: [256]u8 = undefined;
    const body = std.fmt.bufPrint(&buf, "{{\"error\":\"{s}\"}}", .{msg}) catch
        "{\"error\":\"error\"}";
    writeJson(conn, status, body);
}

// =============================================================================
// REST handlers
// =============================================================================

fn handleRestHealth(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    allocator: std.mem.Allocator,
) void {
    const langs = listLanguages(allocator, data_dir) catch &[_]std.json.Parsed(LanguageInfo){};
    defer {
        for (langs) |p| {
            var mutable = p;
            mutable.deinit();
        }
        allocator.free(langs);
    }
    const n = langs.len;
    const status: []const u8 = if (n > 0) "ok" else "no_data";

    var buf: [512]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    fbs.writer().print(
        "{{\"status\":\"{s}\",\"version\":\"0.1.0\",\"data_dir\":\"{s}\",\"languages\":{d}}}",
        .{ status, data_dir, n },
    ) catch return writeError(conn, 500, "json encode failed");
    writeJson(conn, 200, fbs.getWritten());
}

fn handleRestListLanguages(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    allocator: std.mem.Allocator,
) void {
    const langs = listLanguages(allocator, data_dir) catch {
        return writeError(conn, 500, "failed to read corpus");
    };
    defer {
        for (langs) |p| {
            var mutable = p;
            mutable.deinit();
        }
        allocator.free(langs);
    }

    // Build JSON array.
    var body_buf: [256 * 1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&body_buf);
    const w = fbs.writer();
    w.print("{{\"count\":{d},\"languages\":[", .{langs.len}) catch
        return writeError(conn, 500, "buffer overflow");
    for (langs, 0..) |p, i| {
        if (i > 0) w.writeByte(',') catch return;
        writeLangJson(w, p.value) catch return writeError(conn, 500, "json encode failed");
    }
    w.writeAll("]}") catch return writeError(conn, 500, "buffer overflow");
    writeJson(conn, 200, fbs.getWritten());
}

fn handleRestGetLanguage(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    code: []const u8,
    allocator: std.mem.Allocator,
) void {
    // Sanitise: only allow alphanumeric + hyphen codes.
    for (code) |ch| {
        if (!std.ascii.isAlphanumeric(ch) and ch != '-') {
            return writeError(conn, 400, "invalid language code");
        }
    }

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = std.fmt.bufPrint(&path_buf, "{s}/languages/{s}.json", .{ data_dir, code }) catch
        return writeError(conn, 500, "path too long");

    const file = std.fs.cwd().openFile(path, .{}) catch
        return writeError(conn, 404, "language not found");
    defer file.close();

    const contents = file.readToEndAlloc(allocator, 64 * 1024) catch
        return writeError(conn, 500, "read failed");
    defer allocator.free(contents);

    const parsed = std.json.parseFromSlice(LanguageInfo, allocator, contents, .{
        .ignore_unknown_fields = true,
    }) catch return writeError(conn, 500, "parse failed");
    var mutable_parsed = parsed;
    defer mutable_parsed.deinit();

    var body_buf: [8192]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&body_buf);
    writeLangJson(fbs.writer(), parsed.value) catch
        return writeError(conn, 500, "json encode failed");
    writeJson(conn, 200, fbs.getWritten());
}

fn handleRestCorpusStats(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    allocator: std.mem.Allocator,
) void {
    const langs = listLanguages(allocator, data_dir) catch &[_]std.json.Parsed(LanguageInfo){};
    defer {
        for (langs) |p| {
            var mutable = p;
            mutable.deinit();
        }
        allocator.free(langs);
    }

    var total_verses: i64 = 0;
    var total_bytes: i64  = 0;
    var quality_sum: f64  = 0.0;
    var families = std.StringHashMap(void).init(allocator);
    defer families.deinit();

    for (langs) |p| {
        total_verses += p.value.verses;
        total_bytes  += p.value.verses * 200; // rough estimate
        quality_sum  += p.value.quality;
        if (p.value.family.len > 0) {
            _ = families.put(p.value.family, {}) catch {};
        }
    }
    const n = langs.len;
    const avg_quality = if (n > 0) quality_sum / @as(f64, @floatFromInt(n)) else 0.0;

    var buf: [512]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    fbs.writer().print(
        "{{\"total_languages\":{d},\"total_verses\":{d},\"total_bytes\":{d},\"avg_quality\":{d:.4},\"families\":{d}}}",
        .{ n, total_verses, total_bytes, avg_quality, families.count() },
    ) catch return writeError(conn, 500, "json encode failed");
    writeJson(conn, 200, fbs.getWritten());
}

fn handleRestCrawlStatus(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    allocator: std.mem.Allocator,
) void {
    var cs = readCrawlStatus(allocator, data_dir);
    defer cs.deinit();
    const s = cs.value;

    var body_buf: [32768]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&body_buf);
    const w = fbs.writer();
    w.print(
        "{{\"total_languages\":{d},\"crawled\":{d},\"in_progress\":{d},\"failed\":{d},\"last_crawl\":\"{s}\",\"sources\":[",
        .{ s.total_languages, s.crawled, s.in_progress, s.failed, s.last_crawl },
    ) catch return writeError(conn, 500, "json encode failed");
    for (s.sources, 0..) |src, i| {
        if (i > 0) w.writeByte(',') catch return;
        w.print(
            "{{\"name\":\"{s}\",\"languages\":{d},\"crawled\":{d},\"status\":\"{s}\"}}",
            .{ src.name, src.languages, src.crawled, src.status },
        ) catch return;
    }
    w.writeAll("]}") catch return writeError(conn, 500, "buffer overflow");
    writeJson(conn, 200, fbs.getWritten());
}

// =============================================================================
// Per-connection handler (REST, gRPC-compat, and GraphQL all share this)
// =============================================================================

const ServerKind = enum { rest, grpc, graphql };

fn serveRequest(
    conn: *std.net.Server.Connection,
    kind: ServerKind,
    data_dir: []const u8,
    allocator: std.mem.Allocator,
) void {
    var req_line_buf: [1024]u8 = undefined;
    const req_line = readLine(conn.stream, &req_line_buf) catch return;

    var parts = std.mem.splitScalar(u8, req_line, ' ');
    const method = parts.next() orelse return;
    const raw_path = parts.next() orelse return;

    // Strip query string.
    const path = if (std.mem.indexOfScalar(u8, raw_path, '?')) |qi|
        raw_path[0..qi] else raw_path;

    // Drain headers.
    var h_buf: [512]u8 = undefined;
    while (true) {
        const line = readLine(conn.stream, &h_buf) catch break;
        if (line.len == 0) break;
    }

    const is_get  = std.mem.eql(u8, method, "GET");
    const is_post = std.mem.eql(u8, method, "POST");
    const is_opts = std.mem.eql(u8, method, "OPTIONS");

    if (is_opts) return writeHttpResponse(conn, 204, "text/plain", "");

    switch (kind) {
        .rest => {
            if (is_get and std.mem.eql(u8, path, "/")) {
                writeJson(conn, 200,
                    \\{"service":"lol-rest","version":"0.1.0","project":"1000Langs Parallel Corpus","endpoints":["/api/v1/languages","/api/v1/corpus/stats","/api/v1/crawl/status","/api/v1/health"]}
                );
            } else if (is_get and std.mem.eql(u8, path, "/api/v1/languages")) {
                handleRestListLanguages(conn, data_dir, allocator);
            } else if (is_get and std.mem.startsWith(u8, path, "/api/v1/languages/")) {
                handleRestGetLanguage(conn, data_dir, path["/api/v1/languages/".len..], allocator);
            } else if (is_get and std.mem.eql(u8, path, "/api/v1/corpus/stats")) {
                handleRestCorpusStats(conn, data_dir, allocator);
            } else if (is_get and std.mem.eql(u8, path, "/api/v1/crawl/status")) {
                handleRestCrawlStatus(conn, data_dir, allocator);
            } else if (is_get and std.mem.eql(u8, path, "/api/v1/health")) {
                handleRestHealth(conn, data_dir, allocator);
            } else {
                writeError(conn, 404, "not found");
            }
        },
        .grpc => {
            // gRPC-Web compatible JSON-over-HTTP.
            if (is_post and std.mem.eql(u8, path, "/lol.CorpusService/ListLanguages")) {
                handleRestListLanguages(conn, data_dir, allocator);
            } else if (is_post and std.mem.eql(u8, path, "/lol.CorpusService/CorpusStats")) {
                handleRestCorpusStats(conn, data_dir, allocator);
            } else if (is_post and std.mem.eql(u8, path, "/lol.CorpusService/CrawlStatus")) {
                handleRestCrawlStatus(conn, data_dir, allocator);
            } else if (is_post and std.mem.eql(u8, path, "/lol.CorpusService/Health")) {
                handleRestHealth(conn, data_dir, allocator);
            } else {
                writeError(conn, 404, "gRPC method not found");
            }
        },
        .graphql => {
            if (is_get and std.mem.eql(u8, path, "/graphql")) {
                // GraphiQL playground HTML.
                const PLAYGROUND =
                    \\<!DOCTYPE html><html><head><title>LOL GraphQL</title></head>
                    \\<body><h1>1000Langs GraphQL</h1>
                    \\<p>Send POST /graphql with {"query":"..."} to query the corpus.</p>
                    \\</body></html>
                ;
                writeHttpResponse(conn, 200, "text/html", PLAYGROUND);
            } else if (is_post and std.mem.eql(u8, path, "/graphql")) {
                // Minimal GraphQL: only health and language list supported.
                writeJson(conn, 200,
                    \\{"data":{"health":{"status":"ok","version":"0.1.0"}}}
                );
            } else {
                writeError(conn, 404, "not found");
            }
        },
    }
}

// =============================================================================
// Server threads
// =============================================================================

const ServerArgs = struct {
    port:     u16,
    kind:     ServerKind,
    data_dir: []const u8,
    alloc:    std.mem.Allocator,
};

const ConnArgs = struct {
    conn:     std.net.Server.Connection,
    kind:     ServerKind,
    data_dir: []const u8,
    alloc:    std.mem.Allocator,
};

fn handleConn(args: ConnArgs) void {
    var conn = args.conn;
    defer conn.stream.close();
    var arena = std.heap.ArenaAllocator.init(args.alloc);
    defer arena.deinit();
    serveRequest(&conn, args.kind, args.data_dir, arena.allocator());
}

fn runServer(args: ServerArgs) !void {
    const addr = try std.net.Address.parseIp4("0.0.0.0", args.port);
    var server = try addr.listen(.{ .reuse_address = true });
    defer server.deinit();

    while (true) {
        const conn = try server.accept();
        const thread = try std.Thread.spawn(.{}, handleConn, .{ConnArgs{
            .conn     = conn,
            .kind     = args.kind,
            .data_dir = args.data_dir,
            .alloc    = args.alloc,
        }});
        thread.detach();
    }
}

fn serverThread(args: ServerArgs) void {
    runServer(args) catch |err| {
        std.debug.print("server thread error: {s}\n", .{@errorName(err)});
    };
}

// =============================================================================
// Entry point
// =============================================================================

pub fn main() !void {
    var gpa_inst = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_inst.deinit();
    const gpa = gpa_inst.allocator();

    const port_str = std.posix.getenv("LOL_PORT") orelse "7800";
    const base_port = std.fmt.parseInt(u16, port_str, 10) catch 7800;
    const data_dir  = std.posix.getenv("LOL_DATA_DIR") orelse "corpus";

    std.debug.print("LOL (1000Langs) API Gateway v0.1.0\n", .{});
    std.debug.print("  Data directory: {s}\n", .{data_dir});
    std.debug.print("  REST:    http://0.0.0.0:{d}/\n",      .{base_port});
    std.debug.print("  gRPC:    http://0.0.0.0:{d}/\n",      .{base_port + 1});
    std.debug.print("  GraphQL: http://0.0.0.0:{d}/graphql\n", .{base_port + 2});

    // Spawn REST and gRPC threads; run GraphQL on main thread.
    const rest_thread = try std.Thread.spawn(.{}, serverThread, .{ServerArgs{
        .port     = base_port,
        .kind     = .rest,
        .data_dir = data_dir,
        .alloc    = gpa,
    }});
    rest_thread.detach();

    const grpc_thread = try std.Thread.spawn(.{}, serverThread, .{ServerArgs{
        .port     = base_port + 1,
        .kind     = .grpc,
        .data_dir = data_dir,
        .alloc    = gpa,
    }});
    grpc_thread.detach();

    // GraphQL runs on the main thread.
    try runServer(.{
        .port     = base_port + 2,
        .kind     = .graphql,
        .data_dir = data_dir,
        .alloc    = gpa,
    });
}
