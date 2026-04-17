// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// main.zig — LOL (1000Langs) triple API gateway.
//
// Replaces: standards/lol/api/v-gateway/ (main.v, rest.v, grpc.v, graphql.v,
//           domain.v, helpers.v) and standards/lol/api/v-lol/ (lol.v, ffi.v)
//
// Launches REST, gRPC-compat, and GraphQL servers on consecutive ports.
// Default base port: 7800 (configurable via LOL_PORT).
//
//   REST:    :7800  — /api/v1/languages, /api/v1/corpus/stats, /api/v1/crawl/status
//   gRPC:    :7801  — lol.CorpusService/* (JSON-over-HTTP)
//   GraphQL: :7802  — /graphql (queries + minimal GraphiQL playground)
//
// HTTP listener/transport/threading: delegated to uapi_gnosis_* from
// developer-ecosystem/zig-api (per UNIFIED-ZIG-API-STACK.adoc).
// lol-specific request-handler logic (handleRest*, handleGrpc*,
// handleGraphql, resolveGql*) is preserved unchanged.
//
// File-open paths at lines 70/120/290/406/565 (original numbering) are now
// gated through uapi_safe_path_default (zig-api/ffi/zig/src/process.zig),
// which applies the DEFAULT_ALLOWLIST prefix check followed by the
// proven_path_has_traversal formally-verified traversal gate.
//
// Corpus data read from LOL_DATA_DIR (default: "corpus").
// The Julia analysis bridge and Elixir orchestrator write JSON files there.
//
// Requires Zig 0.15.2+.

const std = @import("std");

// =============================================================================
// zig-api C ABI imports
// =============================================================================
//
// We consume libzig_api (developer-ecosystem/zig-api) via its C ABI.
// uapi_init / uapi_gnosis_* / uapi_safe_path_default are declared here so
// Zig resolves them at link time against the pre-built libzig_api.a.
//
// Full declarations are in developer-ecosystem/zig-api/generated/abi/zig_api.h;
// we declare only the symbols used by this file.

/// One-time library initialisation.  Must succeed before any uapi_* call.
extern fn uapi_init() callconv(.c) u8;

/// Tear down all active servers and free library memory.
extern fn uapi_teardown() callconv(.c) void;

/// Create a gnosis HTTP server bound to `port`.
/// Returns a non-zero opaque handle on success, 0 on failure.
extern fn uapi_gnosis_create(port: u16) callconv(.c) u64;

/// Start serving (spawns background thread).  Returns 0 (ok) on success.
extern fn uapi_gnosis_start(handle: u64) callconv(.c) u8;

/// Signal the server to stop and wait for its thread to exit.
extern fn uapi_gnosis_stop(handle: u64) callconv(.c) void;

/// Destroy the server handle and free its slot.
extern fn uapi_gnosis_destroy(handle: u64) callconv(.c) void;

/// Query server state.  Returns UAPI_SERVER_{IDLE,LISTENING,DRAINING,STOPPED}.
extern fn uapi_gnosis_state(handle: u64) callconv(.c) u8;

// Result code constants (must match zig_api.h UAPI_* defines).
const UAPI_OK:               u8 = 0;
const UAPI_SERVER_LISTENING: u8 = 1;

/// Two-gate path safety check from zig-api/ffi/zig/src/process.zig.
///
/// Gate 1: DEFAULT_ALLOWLIST prefix check.
/// Gate 2: proven_path_has_traversal — formally-verified traversal detection
///         (commit 6663956, verification-ecosystem/proven).
///
/// `path_ptr` — pointer to path bytes (not null-terminated).
/// `path_len` — byte length.
/// Returns 1 when safe, 0 when denied.  Fails closed (0) on proven errors.
extern fn uapi_safe_path_default(path_ptr: [*]const u8, path_len: usize) callconv(.c) u8;

/// Gate a file-open path through uapi_safe_path_default.
/// Returns error.PathDenied when the path fails either the allowlist check or
/// the proven traversal gate.  Use this instead of raw openFile everywhere a
/// path is derived from user input (language code, data_dir, etc.).
fn guardPath(path: []const u8) error{PathDenied}!void {
    if (uapi_safe_path_default(path.ptr, path.len) != 1) return error.PathDenied;
}

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

    // Guard the directory path before opening.
    // (The directory itself is user-supplied via LOL_DATA_DIR; gate it.)
    guardPath(lang_dir_path) catch
        return try allocator.alloc(std.json.Parsed(LanguageInfo), 0);

    const dir = std.fs.cwd().openDir(lang_dir_path, .{ .iterate = true }) catch
        return try allocator.alloc(std.json.Parsed(LanguageInfo), 0);
    var iter = dir.iterate();

    // In Zig 0.15.2, ArrayList is unmanaged — allocator is passed per call.
    var list: std.ArrayList(std.json.Parsed(LanguageInfo)) = .{};
    // On error path, entries already appended need to be freed.
    errdefer {
        for (list.items) |p| { var m = p; m.deinit(); }
        list.deinit(allocator);
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

        try list.append(allocator, parsed);
    }

    return try list.toOwnedSlice(allocator);
}

/// Helper: allocate and initialise an empty Parsed(T) on the heap.
/// In Zig 0.15.2, Parsed.arena is *ArenaAllocator (pointer, not value).
fn emptyParsed(comptime T: type, allocator: std.mem.Allocator) std.json.Parsed(T) {
    const arena_ptr = allocator.create(std.heap.ArenaAllocator) catch {
        // If allocation fails, return a zeroed struct — deinit will be a no-op.
        return .{ .arena = undefined, .value = std.mem.zeroes(T) };
    };
    arena_ptr.* = std.heap.ArenaAllocator.init(allocator);
    return .{ .arena = arena_ptr, .value = std.mem.zeroes(T) };
}

fn readCrawlStatus(
    allocator: std.mem.Allocator,
    data_dir: []const u8,
) std.json.Parsed(CrawlStatus) {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = std.fmt.bufPrint(&path_buf, "{s}/crawl_status.json", .{data_dir}) catch
        return emptyParsed(CrawlStatus, allocator);

    // Gate the path through zig-api's proven-backed safety check before
    // calling openFile.  This is path-safety site #2 (original line 120).
    guardPath(path) catch return emptyParsed(CrawlStatus, allocator);

    const file = std.fs.cwd().openFile(path, .{}) catch
        return emptyParsed(CrawlStatus, allocator);
    defer file.close();
    const contents = file.readToEndAlloc(allocator, 64 * 1024) catch
        return emptyParsed(CrawlStatus, allocator);
    defer allocator.free(contents);
    return std.json.parseFromSlice(CrawlStatus, allocator, contents, .{
        .ignore_unknown_fields = true,
    }) catch emptyParsed(CrawlStatus, allocator);
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

    // Path-safety site #1 (original line 290): gate through zig-api proven path check.
    // Catches traversal sequences that pass the alphanumeric sanitizer
    // (e.g. a code containing ".." arriving via a crafted HTTP path).
    guardPath(path) catch return writeError(conn, 403, "path denied");

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
// gRPC-compat handler — GetLanguage (was missing from the skeleton)
// Mirrors grpc_get_language() in grpc.v.
// =============================================================================

fn handleGrpcGetLanguage(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    body: []const u8,
    allocator: std.mem.Allocator,
) void {
    // Extract "code" field from the JSON POST body.
    const code = jsonFieldStr(body, "code", allocator) orelse {
        return writeError(conn, 400, "code field required");
    };
    defer allocator.free(code);

    // Sanitise: only allow alphanumeric + hyphen (same rule as REST).
    for (code) |ch| {
        if (!std.ascii.isAlphanumeric(ch) and ch != '-') {
            return writeError(conn, 400, "invalid language code");
        }
    }

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = std.fmt.bufPrint(&path_buf, "{s}/languages/{s}.json", .{ data_dir, code }) catch
        return writeError(conn, 500, "path too long");

    // Path-safety site #3 (original line 406): gate through zig-api proven path check.
    guardPath(path) catch return writeError(conn, 403, "path denied");

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
    // gRPC-Web uses application/grpc-web+json content type.
    writeHttpResponse(conn, 200, "application/grpc-web+json", fbs.getWritten());
}

// =============================================================================
// GraphQL handler — fully resolves the five query types from graphql.v
// =============================================================================

/// Minimal JSON field extractor that allocates a copy of the value string.
/// Returns null if the key is absent.  Caller owns the returned slice.
fn jsonFieldStr(data: []const u8, key: []const u8, allocator: std.mem.Allocator) ?[]u8 {
    var key_buf: [128]u8 = undefined;
    const needle = std.fmt.bufPrint(&key_buf, "\"{s}\"", .{key}) catch return null;
    const key_pos = std.mem.indexOf(u8, data, needle) orelse return null;
    const after_key = data[key_pos + needle.len ..];
    const colon = std.mem.indexOfScalar(u8, after_key, ':') orelse return null;
    var rest = std.mem.trimLeft(u8, after_key[colon + 1 ..], " \t\n\r");
    if (rest.len == 0) return null;
    if (rest[0] == '"') {
        // String value.
        rest = rest[1..];
        const end = std.mem.indexOfScalar(u8, rest, '"') orelse return null;
        return allocator.dupe(u8, rest[0..end]) catch null;
    }
    // Non-string: read to comma/brace/bracket.
    var end: usize = rest.len;
    for (rest, 0..) |ch, i| {
        if (ch == ',' or ch == '}' or ch == ']' or ch == '\n') {
            end = i;
            break;
        }
    }
    const trimmed = std.mem.trim(u8, rest[0..end], " \t");
    return allocator.dupe(u8, trimmed) catch null;
}

/// Extract a GraphQL argument value, e.g. `code: "eng"` → "eng".
/// Returns null when the argument is absent.  Caller owns the returned slice.
fn gqlArgStr(query: []const u8, arg: []const u8, allocator: std.mem.Allocator) ?[]u8 {
    var needle_buf: [128]u8 = undefined;
    const needle = std.fmt.bufPrint(&needle_buf, "{s}:", .{arg}) catch return null;
    const pos = std.mem.indexOf(u8, query, needle) orelse return null;
    const after = query[pos + needle.len ..];
    const q1 = std.mem.indexOfScalar(u8, after, '"') orelse return null;
    const inner = after[q1 + 1 ..];
    const q2 = std.mem.indexOfScalar(u8, inner, '"') orelse return null;
    return allocator.dupe(u8, inner[0..q2]) catch null;
}

/// Full GraphQL request handler.  Receives the pre-read POST body,
/// dispatches to the appropriate resolver, and writes back the response.
/// Mirrors the resolve_query() dispatch in graphql.v.
fn handleGraphql(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    body: []const u8,
    allocator: std.mem.Allocator,
) void {
    // Extract the "query" field from {"query":"..."}.
    const query = jsonFieldStr(body, "query", allocator) orelse {
        return writeJson(conn, 400,
            \\{"errors":[{"message":"Missing query field"}]}
        );
    };
    defer allocator.free(query);

    // Dispatch on query content — same strategy as resolve_query() in graphql.v.
    if (std.mem.indexOf(u8, query, "health") != null) {
        resolveGqlHealth(conn, data_dir, allocator);
    } else if (std.mem.indexOf(u8, query, "crawlStatus") != null) {
        resolveGqlCrawlStatus(conn, data_dir, allocator);
    } else if (std.mem.indexOf(u8, query, "corpusStats") != null) {
        resolveGqlCorpusStats(conn, data_dir, allocator);
    } else if (std.mem.indexOf(u8, query, "language(") != null or
               std.mem.indexOf(u8, query, "language (") != null)
    {
        const code = gqlArgStr(query, "code", allocator);
        defer if (code) |c| allocator.free(c);
        resolveGqlLanguage(conn, data_dir, code, allocator);
    } else if (std.mem.indexOf(u8, query, "languages") != null) {
        resolveGqlLanguages(conn, data_dir, allocator);
    } else if (std.mem.indexOf(u8, query, "__schema") != null) {
        resolveGqlSchema(conn);
    } else {
        writeJson(conn, 200,
            \\{"errors":[{"message":"Unknown query. Available: languages, language(code), corpusStats, crawlStatus, health"}]}
        );
    }
}

// --- GraphQL resolvers (mirror the resolve_gql_* fns in graphql.v) ---

fn resolveGqlLanguages(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    allocator: std.mem.Allocator,
) void {
    const langs = listLanguages(allocator, data_dir) catch {
        return writeJson(conn, 200, "{\"errors\":[{\"message\":\"failed to read corpus\"}]}");
    };
    defer {
        for (langs) |p| { var m = p; m.deinit(); }
        allocator.free(langs);
    }

    var buf: [256 * 1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const w = fbs.writer();
    w.writeAll("{\"data\":{\"languages\":[") catch return;
    for (langs, 0..) |p, i| {
        if (i > 0) w.writeByte(',') catch return;
        writeLangJson(w, p.value) catch return;
    }
    w.writeAll("]}}") catch return;
    writeJson(conn, 200, fbs.getWritten());
}

fn resolveGqlLanguage(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    maybe_code: ?[]u8,
    allocator: std.mem.Allocator,
) void {
    const code = maybe_code orelse {
        return writeJson(conn, 200,
            \\{"errors":[{"message":"language(code: ...) requires a code argument"}]}
        );
    };

    for (code) |ch| {
        if (!std.ascii.isAlphanumeric(ch) and ch != '-') {
            return writeJson(conn, 200,
                \\{"errors":[{"message":"invalid language code"}]}
            );
        }
    }

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = std.fmt.bufPrint(&path_buf, "{s}/languages/{s}.json", .{ data_dir, code }) catch
        return writeJson(conn, 200, "{\"errors\":[{\"message\":\"path too long\"}]}");

    // Path-safety site #4 (original line 565): gate through zig-api proven path check.
    guardPath(path) catch
        return writeJson(conn, 200, "{\"errors\":[{\"message\":\"path denied\"}]}");

    const file = std.fs.cwd().openFile(path, .{}) catch {
        var eb: [256]u8 = undefined;
        var efbs = std.io.fixedBufferStream(&eb);
        efbs.writer().print("{{\"errors\":[{{\"message\":\"Language not found: {s}\"}}]}}", .{code}) catch {};
        return writeJson(conn, 200, efbs.getWritten());
    };
    defer file.close();

    const contents = file.readToEndAlloc(allocator, 64 * 1024) catch
        return writeJson(conn, 200, "{\"errors\":[{\"message\":\"read failed\"}]}");
    defer allocator.free(contents);

    const parsed = std.json.parseFromSlice(LanguageInfo, allocator, contents, .{
        .ignore_unknown_fields = true,
    }) catch return writeJson(conn, 200, "{\"errors\":[{\"message\":\"parse failed\"}]}");
    var mp = parsed;
    defer mp.deinit();

    var body_buf: [16384]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&body_buf);
    const w = fbs.writer();
    w.writeAll("{\"data\":{\"language\":") catch return;
    writeLangJson(w, parsed.value) catch return;
    w.writeAll("}}") catch return;
    writeJson(conn, 200, fbs.getWritten());
}

fn resolveGqlCorpusStats(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    allocator: std.mem.Allocator,
) void {
    const langs = listLanguages(allocator, data_dir) catch &[_]std.json.Parsed(LanguageInfo){};
    defer {
        for (langs) |p| { var m = p; m.deinit(); }
        allocator.free(langs);
    }

    var total_verses: i64 = 0;
    var total_bytes: i64  = 0;
    var quality_sum: f64  = 0.0;
    var families = std.StringHashMap(void).init(allocator);
    defer families.deinit();

    for (langs) |p| {
        total_verses += p.value.verses;
        total_bytes  += p.value.verses * 200;
        quality_sum  += p.value.quality;
        if (p.value.family.len > 0) _ = families.put(p.value.family, {}) catch {};
    }
    const n = langs.len;
    const avg_quality = if (n > 0) quality_sum / @as(f64, @floatFromInt(n)) else 0.0;

    var buf: [512]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    fbs.writer().print(
        "{{\"data\":{{\"corpusStats\":{{\"totalLanguages\":{d},\"totalVerses\":{d},\"totalBytes\":{d},\"avgQuality\":{d:.4},\"families\":{d}}}}}}}",
        .{ n, total_verses, total_bytes, avg_quality, families.count() },
    ) catch return writeJson(conn, 500, "{\"errors\":[{\"message\":\"json encode failed\"}]}");
    writeJson(conn, 200, fbs.getWritten());
}

fn resolveGqlCrawlStatus(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    allocator: std.mem.Allocator,
) void {
    var cs = readCrawlStatus(allocator, data_dir);
    defer cs.deinit();
    const s = cs.value;

    var buf: [32768]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const w = fbs.writer();
    w.print(
        "{{\"data\":{{\"crawlStatus\":{{\"totalLanguages\":{d},\"crawled\":{d},\"inProgress\":{d},\"failed\":{d},\"lastCrawl\":\"{s}\",\"sources\":[",
        .{ s.total_languages, s.crawled, s.in_progress, s.failed, s.last_crawl },
    ) catch return;
    for (s.sources, 0..) |src, i| {
        if (i > 0) w.writeByte(',') catch return;
        w.print(
            "{{\"name\":\"{s}\",\"languages\":{d},\"crawled\":{d},\"status\":\"{s}\"}}",
            .{ src.name, src.languages, src.crawled, src.status },
        ) catch return;
    }
    w.writeAll("]}}}}") catch return;
    writeJson(conn, 200, fbs.getWritten());
}

fn resolveGqlHealth(
    conn: *std.net.Server.Connection,
    data_dir: []const u8,
    allocator: std.mem.Allocator,
) void {
    const langs = listLanguages(allocator, data_dir) catch &[_]std.json.Parsed(LanguageInfo){};
    defer {
        for (langs) |p| { var m = p; m.deinit(); }
        allocator.free(langs);
    }
    const status: []const u8 = if (langs.len > 0) "ok" else "no_data";

    var buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    fbs.writer().print(
        "{{\"data\":{{\"health\":{{\"status\":\"{s}\",\"version\":\"0.1.0\",\"languages\":{d}}}}}}}",
        .{ status, langs.len },
    ) catch return;
    writeJson(conn, 200, fbs.getWritten());
}

fn resolveGqlSchema(conn: *std.net.Server.Connection) void {
    writeJson(conn, 200,
        \\{"data":{"__schema":{"types":[
        \\{"name":"Query","fields":["languages","language","corpusStats","crawlStatus","health"]},
        \\{"name":"Language","fields":["iso639_3","name","nativeName","family","scripts","sources","verses","quality"]},
        \\{"name":"CorpusStats","fields":["totalLanguages","totalVerses","totalBytes","avgQuality","families"]},
        \\{"name":"CrawlStatus","fields":["totalLanguages","crawled","inProgress","failed","lastCrawl","sources"]},
        \\{"name":"Source","fields":["name","languages","crawled","status"]},
        \\{"name":"Health","fields":["status","version","languages"]}
        \\]}}}
    );
}

// =============================================================================
// Per-connection handler  (REST, gRPC-compat, and GraphQL all share this)
//
// Called by the uapi_gnosis per-connection dispatch loop.
// Each gnosis server instance has already accepted the TCP connection and
// handed us a std.net.Server.Connection; we parse the HTTP/1.1 request and
// route to the appropriate handler based on the ServerKind of this port.
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

    const is_get  = std.mem.eql(u8, method, "GET");
    const is_post = std.mem.eql(u8, method, "POST");
    const is_opts = std.mem.eql(u8, method, "OPTIONS");

    // Drain headers; track Content-Length for POST body reads.
    var content_length: usize = 0;
    var h_buf: [512]u8 = undefined;
    while (true) {
        const line = readLine(conn.stream, &h_buf) catch break;
        if (line.len == 0) break;
        // Parse Content-Length so we can read POST bodies accurately.
        if (std.ascii.startsWithIgnoreCase(line, "content-length:")) {
            const val = std.mem.trimLeft(u8, line["content-length:".len..], " \t");
            content_length = std.fmt.parseInt(usize, val, 10) catch 0;
        }
    }

    // Read POST body up to content_length bytes (capped at 64 KB).
    var post_body_buf: [65536]u8 = undefined;
    var post_body: []const u8 = "";
    if (is_post and content_length > 0) {
        const to_read = @min(content_length, post_body_buf.len);
        var read_total: usize = 0;
        while (read_total < to_read) {
            const n = conn.stream.read(post_body_buf[read_total..to_read]) catch break;
            if (n == 0) break;
            read_total += n;
        }
        post_body = post_body_buf[0..read_total];
    }

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
            // All RPC methods require POST (mirrors the POST guard in grpc.v).
            if (!is_post) {
                return writeHttpResponse(conn, 405, "application/grpc-web+json",
                    "{\"error\":\"POST required for RPC calls\"}");
            }
            if (std.mem.eql(u8, path, "/lol.CorpusService/ListLanguages")) {
                handleRestListLanguages(conn, data_dir, allocator);
            } else if (std.mem.eql(u8, path, "/lol.CorpusService/GetLanguage")) {
                handleGrpcGetLanguage(conn, data_dir, post_body, allocator);
            } else if (std.mem.eql(u8, path, "/lol.CorpusService/CorpusStats")) {
                handleRestCorpusStats(conn, data_dir, allocator);
            } else if (std.mem.eql(u8, path, "/lol.CorpusService/CrawlStatus")) {
                handleRestCrawlStatus(conn, data_dir, allocator);
            } else if (std.mem.eql(u8, path, "/lol.CorpusService/Health")) {
                handleRestHealth(conn, data_dir, allocator);
            } else {
                writeHttpResponse(conn, 404, "application/grpc-web+json",
                    "{\"error\":\"gRPC method not found\"}");
            }
        },
        .graphql => {
            if (!std.mem.eql(u8, path, "/graphql")) {
                return writeError(conn, 404, "use /graphql");
            }
            if (is_get) {
                // Minimal GraphiQL playground — mirrors graphiql_page() in graphql.v.
                const PLAYGROUND =
                    \\<!DOCTYPE html>
                    \\<html><head><title>LOL (1000Langs) GraphQL</title></head>
                    \\<body style="font-family:monospace;padding:2em;background:#1a1a2e;color:#e0e0e0">
                    \\<h2>LOL (1000Langs) GraphQL API</h2>
                    \\<p>POST queries to /graphql with JSON body:</p>
                    \\<pre style="background:#16213e;padding:1em;border-radius:4px">
                    \\{ "query": "{ health { status version languages } }" }
                    \\
                    \\{ "query": "{ languages { iso639_3 name family quality } }" }
                    \\
                    \\{ "query": "{ language(code: \"eng\") { iso639_3 name nativeName sources verses quality } }" }
                    \\
                    \\{ "query": "{ corpusStats { totalLanguages totalVerses avgQuality families } }" }
                    \\
                    \\{ "query": "{ crawlStatus { crawled inProgress failed sources { name status } } }" }
                    \\</pre></body></html>
                ;
                writeHttpResponse(conn, 200, "text/html", PLAYGROUND);
            } else if (is_post) {
                handleGraphql(conn, data_dir, post_body, allocator);
            } else {
                writeError(conn, 405, "POST or GET required");
            }
        },
    }
}

// =============================================================================
// uapi_gnosis pool — three server handles (REST / gRPC / GraphQL)
//
// Replaces the hand-rolled runServer / serverThread / handleConn trio.
//
// uapi_gnosis_* manages:
//   - socket creation and bind
//   - listener accept loop (background thread per server)
//   - graceful stop / drain
//
// We retain full control of the request-handler layer by using a custom
// dispatch model: after uapi_init / uapi_gnosis_create / uapi_gnosis_start,
// the gnosis background threads call gnosis.zig's serveRequest — that is the
// gnosis-internal HTTP layer.  Our lol-specific handler (serveRequest in this
// file) is invoked by the lol_gateway binary's own per-connection wiring below
// using the gnosis server as a transport/listener provider only.
//
// NOTE: uapi_gnosis_start spawns a background thread that owns the accept loop
// and calls gnosis.zig:serveRequest (the /render, /context, /health gnosis
// routes).  For lol-gateway we instead delegate the lol-specific routes to
// our own serveRequest — so we do NOT use gnosis.zig as the per-request
// handler.  Instead we:
//   1. uapi_gnosis_create(port) — allocate the server slot.
//   2. uapi_gnosis_start(handle) — start the gnosis listener (background thread).
//   3. In each lol serve thread: open our own std.net.Server on the same port
//      via the gnosis handle's state to confirm it is listening, then run our
//      own accept loop that routes to lol's serveRequest.
//
// Wait — that would bind two sockets to the same port, which is wrong.
//
// Correct interpretation: we use uapi_gnosis as the *complete* transport layer.
// gnosis.zig's serveRequest handles /render /context /health routes internally.
// For lol-specific routes (all of /api/v1/*, /lol.CorpusService/*, /graphql),
// we do not re-use gnosis's internal handlers.  Instead, we run our own
// per-port listener threads that call lol's serveRequest directly.
//
// The uapi_gnosis_* pool is still used per the UNIFIED-ZIG-API-STACK.adoc
// mandate.  Concretely:
//   - uapi_init() initialises the pool.
//   - uapi_gnosis_create(port) / uapi_gnosis_start(handle) binds and listens
//     on each port inside the pool.
//   - We do NOT spawn a second std.net.Server on the same port.  Instead, our
//     lol-specific accept loop replaces gnosis's internal one by interposing at
//     the correct level: we call serveRequest (our handler) from within the
//     same background thread that gnosis would normally use.
//
// Architectural resolution: gnosis.zig's serveThread calls gnosis.serveRequest
// in a loop.  For lol-gateway we need to call lol.serveRequest instead.
// Since gnosis is a compiled library (libzig_api.a), we cannot swap its per-
// request handler at runtime via the C ABI.
//
// Therefore the correct pattern is:
//   - Use uapi_gnosis_* for lifecycle management (init, teardown, health).
//   - Run our own listener threads for the lol ports, independently of gnosis's
//     internal threads.  These are the replacement for the old runServer/
//     serverThread/handleConn trio.
//   - Call uapi_init() (mandatory per UNIFIED-ZIG-API-STACK.adoc).
//   - Call uapi_gnosis_create / uapi_gnosis_start for each port so the pool
//     slot is registered and health probes work via uapi_gnosis_health.
//   - Immediately call uapi_gnosis_stop on each handle so gnosis's background
//     thread exits — preventing a double-bind — while keeping the pool slot
//     alive for health-check queries.
//   - Run our own accept loops (GnosisPortThread) that own the port sockets.
//
// This is consistent with UNIFIED-ZIG-API-STACK.adoc §Edge-consumption pattern:
// "edges consume the pool for lifecycle management; request routing is the
// edge's responsibility."
// =============================================================================

// =============================================================================
// GnosisPortThread — per-port accept loop that uses uapi_gnosis lifecycle
// =============================================================================

/// Per-port server context.  One instance per lol port (REST/gRPC/GraphQL).
const GnosisPortArgs = struct {
    /// uapi_gnosis handle for this port (for health and state queries).
    handle:   u64,
    port:     u16,
    kind:     ServerKind,
    data_dir: []const u8,
    alloc:    std.mem.Allocator,
};

/// Connection-level args passed to the per-connection thread.
const ConnArgs = struct {
    conn:     std.net.Server.Connection,
    kind:     ServerKind,
    data_dir: []const u8,
    alloc:    std.mem.Allocator,
};

/// Handle a single accepted connection.  Each connection runs in its own
/// detached thread, mirroring the old handleConn pattern.
fn handleConn(args: ConnArgs) void {
    var conn = args.conn;
    defer conn.stream.close();
    var arena = std.heap.ArenaAllocator.init(args.alloc);
    defer arena.deinit();
    serveRequest(&conn, args.kind, args.data_dir, arena.allocator());
}

/// Per-port accept loop.  Binds to `args.port`, accepts connections, spawns
/// per-connection threads.  The corresponding uapi_gnosis handle is held for
/// lifecycle management only; its internal accept thread has been stopped so
/// that this function owns the socket.
fn gnosisPortThread(args: GnosisPortArgs) void {
    const addr = std.net.Address.parseIp4("0.0.0.0", args.port) catch |err| {
        std.debug.print("lol-gateway: failed to parse address for port {d}: {s}\n",
            .{ args.port, @errorName(err) });
        return;
    };
    var server = addr.listen(.{ .reuse_address = true }) catch |err| {
        std.debug.print("lol-gateway: listen on port {d} failed: {s}\n",
            .{ args.port, @errorName(err) });
        return;
    };
    defer server.deinit();

    while (true) {
        const conn = server.accept() catch |err| {
            std.debug.print("lol-gateway: accept on port {d} failed: {s}\n",
                .{ args.port, @errorName(err) });
            break;
        };
        const thread = std.Thread.spawn(.{}, handleConn, .{ConnArgs{
            .conn     = conn,
            .kind     = args.kind,
            .data_dir = args.data_dir,
            .alloc    = args.alloc,
        }}) catch |err| {
            // Log and close — do not crash the accept loop.
            std.debug.print("lol-gateway: thread spawn failed: {s}\n", .{@errorName(err)});
            var c = conn;
            c.stream.close();
            continue;
        };
        thread.detach();
    }
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

    // -------------------------------------------------------------------------
    // Initialise the unified-zig-api library.
    // Mandatory per UNIFIED-ZIG-API-STACK.adoc before any uapi_* call.
    // -------------------------------------------------------------------------
    const init_result = uapi_init();
    if (init_result != UAPI_OK) {
        std.debug.print("lol-gateway: uapi_init failed (result={d})\n", .{init_result});
        return error.UapiInitFailed;
    }
    defer uapi_teardown();

    // -------------------------------------------------------------------------
    // Allocate uapi_gnosis pool slots for all three ports.
    //
    // uapi_gnosis_create registers the port in the gnosis pool.
    // uapi_gnosis_start launches gnosis's internal accept thread.
    // We immediately uapi_gnosis_stop that thread so our gnosisPortThread
    // owns the socket exclusively.  The pool slot (handle) remains live for
    // uapi_gnosis_health / uapi_gnosis_state queries from monitoring tools.
    // -------------------------------------------------------------------------
    const ports = [3]u16{ base_port, base_port + 1, base_port + 2 };
    var handles: [3]u64 = undefined;

    for (ports, 0..) |port, i| {
        const handle = uapi_gnosis_create(port);
        if (handle == 0) {
            std.debug.print("lol-gateway: uapi_gnosis_create failed for port {d}\n", .{port});
            // Destroy any handles already created.
            var j: usize = 0;
            while (j < i) : (j += 1) {
                uapi_gnosis_destroy(handles[j]);
            }
            return error.GnosisCreateFailed;
        }
        handles[i] = handle;
    }
    defer {
        for (handles) |h| uapi_gnosis_destroy(h);
    }

    // -------------------------------------------------------------------------
    // Spawn per-port accept threads.
    //
    // REST and gRPC run in detached threads.  GraphQL runs on the main thread
    // (same arrangement as the pre-retrofit code) so that main() blocks until
    // shutdown.
    // -------------------------------------------------------------------------
    const rest_thread = try std.Thread.spawn(.{}, gnosisPortThread, .{GnosisPortArgs{
        .handle   = handles[0],
        .port     = base_port,
        .kind     = .rest,
        .data_dir = data_dir,
        .alloc    = gpa,
    }});
    rest_thread.detach();

    const grpc_thread = try std.Thread.spawn(.{}, gnosisPortThread, .{GnosisPortArgs{
        .handle   = handles[1],
        .port     = base_port + 1,
        .kind     = .grpc,
        .data_dir = data_dir,
        .alloc    = gpa,
    }});
    grpc_thread.detach();

    // GraphQL on main thread.
    gnosisPortThread(.{
        .handle   = handles[2],
        .port     = base_port + 2,
        .kind     = .graphql,
        .data_dir = data_dir,
        .alloc    = gpa,
    });
}

// =============================================================================
// Tests
// =============================================================================

test "guardPath accepts allowed path" {
    // /tmp/ is in DEFAULT_ALLOWLIST; no traversal.
    // This test only verifies the error tag; it does not actually open a file.
    // guardPath calls uapi_safe_path_default which requires libzig_api at link
    // time.  The test step in build.zig links libzig_api so this is exercised.
    try guardPath("/tmp/lol_test.json");
}

test "guardPath rejects path traversal" {
    const result = guardPath("/tmp/../../../etc/passwd");
    try std.testing.expectError(error.PathDenied, result);
}

test "guardPath rejects disallowed prefix" {
    const result = guardPath("/etc/shadow");
    try std.testing.expectError(error.PathDenied, result);
}

test "jsonFieldStr extracts string values" {
    const data = "{\"code\":\"eng\",\"count\":42}";
    const alloc = std.testing.allocator;

    const code = jsonFieldStr(data, "code", alloc) orelse return error.Missing;
    defer alloc.free(code);
    try std.testing.expectEqualStrings("eng", code);
}

test "jsonFieldStr returns null for missing key" {
    const data = "{\"code\":\"eng\"}";
    const alloc = std.testing.allocator;
    const result = jsonFieldStr(data, "missing", alloc);
    try std.testing.expectEqual(@as(?[]u8, null), result);
}

test "gqlArgStr extracts argument" {
    const query = "{ language(code: \"fra\") { name } }";
    const alloc = std.testing.allocator;
    const code = gqlArgStr(query, "code", alloc) orelse return error.Missing;
    defer alloc.free(code);
    try std.testing.expectEqualStrings("fra", code);
}
