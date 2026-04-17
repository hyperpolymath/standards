// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// main.zig — LOL (1000Langs) single-port path-routed API gateway.
//
// Replaces: standards/lol/api/v-gateway/ (main.v, rest.v, grpc.v, graphql.v,
//           domain.v, helpers.v) and standards/lol/api/v-lol/ (lol.v, ffi.v)
//
// All three protocols are served on ONE port via path routing (default: 7800).
// The HTTP listener is owned by uapi_gnosis_start via uapi_gnosis_set_handler,
// which calls lolHandler for every incoming request.
//
//   /api/v1/*                       — REST handlers
//   /lol.CorpusService/*            — gRPC-compat JSON-over-HTTP handlers
//   /graphql                        — GraphQL handler (+ minimal playground on GET)
//   everything else                 — 404
//
// Previously the gateway used three separate ports (7800/7801/7802) with its
// own per-port accept loops.  The new shape collapses all of these into a single
// uapi_gnosis_create(base_port) + uapi_gnosis_set_handler(lolHandler) +
// uapi_gnosis_start call.  No current consumer fires more than one protocol path
// simultaneously, so consolidating to one port loses nothing.
//
// HTTP listener/transport/threading: fully delegated to uapi_gnosis_* from
// developer-ecosystem/zig-api (per UNIFIED-ZIG-API-STACK.adoc).
// lol-specific request-handler logic (handleRest*, handleGrpc*,
// handleGraphql, resolveGql*) is preserved unchanged.
//
// File-open paths are gated through uapi_safe_path_default
// (zig-api/ffi/zig/src/process.zig), which applies the DEFAULT_ALLOWLIST
// prefix check followed by the proven_path_has_traversal formally-verified
// traversal gate.
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

/// Register an edge handler hook.  Must be called before uapi_gnosis_start.
/// Returns 0 (ok) on success.
extern fn uapi_gnosis_set_handler(
    handle:     u64,
    handler_fn: ?*const fn (req: *const GnosisRequest, resp: *GnosisResponse) callconv(.c) void,
) callconv(.c) u8;

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

// =============================================================================
// C-ABI structs for the gnosis handler hook (mirrors zig_api.h)
// =============================================================================

/// Request context passed to the lolHandler by gnosis's accept loop.
/// All pointer fields are valid only for the duration of the handler call.
const GnosisRequest = extern struct {
    method:   [*:0]const u8,
    path:     [*:0]const u8,
    body_ptr: ?[*]const u8,
    body_len: u32,
};

/// Response the lolHandler must fill before returning.
const GnosisResponse = extern struct {
    status:       u16,
    _pad:         u16,
    content_type: [*:0]const u8,
    body_ptr:     ?[*]const u8,
    body_len:     u32,
};

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
// Single-port path-routing handler (replaces multi-port + GnosisPortThread)
//
// lolHandler is the C-ABI edge hook registered via uapi_gnosis_set_handler.
// It is called by gnosis's accept loop for every incoming request on port 7800.
// All three protocols are path-routed here:
//
//   /api/v1/*              → REST handlers
//   /lol.CorpusService/*   → gRPC-compat JSON-over-HTTP handlers
//   /graphql               → GraphQL handler
//   /                      → discovery JSON
//   else                   → 404
//
// Module-level context (data_dir, allocator) is set by main() before
// uapi_gnosis_start is called; it is read-only after that point.
// =============================================================================

/// Corpus data directory path.  Set once in main() before uapi_gnosis_start.
var g_data_dir: []const u8 = "corpus";

/// General-purpose allocator for handler-level allocations.
/// Set once in main() before uapi_gnosis_start; stable for process lifetime.
var g_allocator: std.mem.Allocator = undefined;

/// Flag: true once main() has initialised g_data_dir and g_allocator.
var g_context_ready: bool = false;

/// Response body buffer used by lolHandler.
///
/// gnosis calls lolHandler once per connection on the gnosis serve thread.
/// Each invocation overwrites this buffer — thread-safe because gnosis.zig
/// spawns one thread per server slot and the slot's serve loop is single-
/// threaded per connection (connections are handled sequentially by one
/// background thread per slot).
///
/// Size: 256 KiB — matches MAX_BODY_BYTES in gnosis.zig.
var g_resp_buf: [256 * 1024]u8 = undefined;

/// Content-type sentinel constants (null-terminated).
const CT_JSON:    [*:0]const u8 = "application/json";
const CT_GRPC:    [*:0]const u8 = "application/grpc-web+json";
const CT_HTML:    [*:0]const u8 = "text/html";
const CT_TEXT:    [*:0]const u8 = "text/plain";

/// GraphiQL playground HTML (served on GET /graphql).
const GRAPHQL_PLAYGROUND =
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

/// Fill `resp` with a JSON error.  `msg` must not contain double-quotes.
fn respError(resp: *GnosisResponse, status: u16, msg: []const u8) void {
    var fbs = std.io.fixedBufferStream(&g_resp_buf);
    fbs.writer().print("{{\"error\":\"{s}\"}}", .{msg}) catch {};
    const written = fbs.getWritten();
    resp.status       = status;
    resp._pad         = 0;
    resp.content_type = CT_JSON;
    resp.body_ptr     = written.ptr;
    resp.body_len     = @intCast(written.len);
}

/// Fill `resp` with a static JSON string.
fn respJsonStatic(resp: *GnosisResponse, status: u16, body: []const u8) void {
    resp.status       = status;
    resp._pad         = 0;
    resp.content_type = CT_JSON;
    resp.body_ptr     = body.ptr;
    resp.body_len     = @intCast(body.len);
}

/// Adapter: route a lolHandler request/response pair through the legacy
/// `handleRestListLanguages` / similar functions that expect a
/// `*std.net.Server.Connection`.
///
/// Because the legacy handlers write directly to a `std.net.Server.Connection`
/// stream, we can't call them from a GnosisRequest/Response context without
/// a real connection.  Instead we replicate their logic inline, writing into
/// `g_resp_buf` and filling `resp` directly.  The business logic (corpus
/// reading, JSON encoding) is unchanged; only the I/O path changes.
///
/// This is the cleanest option that avoids circular dependencies or forking
/// the legacy handler functions.
///
/// `allocator` must be an arena reset between calls.

/// Temporary connection shim — wraps g_resp_buf in a minimal fake connection
/// so the legacy handlers (which call writeJson/writeHttpResponse) can write
/// into our buffer instead of a TCP stream.
///
/// Implementation: we can't construct a real std.net.Server.Connection without
/// a live socket.  The legacy handlers call writeJson → writeHttpResponse →
/// conn.stream.writeAll.  We redirect these by keeping a separate
/// FixedBufferStream and patching the legacy write helpers to work with it.
///
/// The cleaner approach (matching the design intent): inline all routing in
/// lolHandler.  The existing handler functions accept `*std.net.Server.Connection`
/// as their first argument only to write their response.  We reuse the bodies
/// of these functions by calling them with a `*FakeConn`.
///
/// Since Zig doesn't support vtables on arbitrary types for std.net.Stream,
/// the simplest correct approach is: define a lightweight `FakeConn` that
/// satisfies the functions' actual usage (stream.writeAll).  But
/// std.net.Server.Connection.stream is a std.net.Stream (a plain integer fd),
/// not an interface — we cannot substitute it cleanly.
///
/// Resolution: provide thin wrapper functions `lolRestDispatch` /
/// `lolGrpcDispatch` / `lolGraphqlDispatch` that replicate the handler logic
/// using a `fixedBufferStream` writer, then fill `resp`.

fn lolRestDispatch(
    path:      []const u8,
    method:    []const u8,
    body:      []const u8,
    resp:      *GnosisResponse,
    allocator: std.mem.Allocator,
) void {
    const is_get  = std.mem.eql(u8, method, "GET");

    if (is_get and std.mem.eql(u8, path, "/")) {
        respJsonStatic(resp, 200,
            "{\"service\":\"lol-rest\",\"version\":\"0.1.0\"," ++
            "\"project\":\"1000Langs Parallel Corpus\"," ++
            "\"endpoints\":[\"/api/v1/languages\",\"/api/v1/corpus/stats\"," ++
            "\"/api/v1/crawl/status\",\"/api/v1/health\"]}");
        return;
    }

    if (is_get and std.mem.eql(u8, path, "/api/v1/languages")) {
        // Build language list into g_resp_buf.
        const langs = listLanguages(allocator, g_data_dir) catch {
            respError(resp, 500, "failed to read corpus");
            return;
        };
        defer {
            for (langs) |p| { var m = p; m.deinit(); }
            allocator.free(langs);
        }
        var fbs = std.io.fixedBufferStream(&g_resp_buf);
        const w = fbs.writer();
        w.print("{{\"count\":{d},\"languages\":[", .{langs.len}) catch {
            respError(resp, 500, "buffer overflow");
            return;
        };
        for (langs, 0..) |p, i| {
            if (i > 0) w.writeByte(',') catch return;
            writeLangJson(w, p.value) catch { respError(resp, 500, "json encode failed"); return; };
        }
        w.writeAll("]}") catch { respError(resp, 500, "buffer overflow"); return; };
        const written = fbs.getWritten();
        resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
        resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
        return;
    }

    if (is_get and std.mem.startsWith(u8, path, "/api/v1/languages/")) {
        const code = path["/api/v1/languages/".len..];
        for (code) |ch| {
            if (!std.ascii.isAlphanumeric(ch) and ch != '-') {
                respError(resp, 400, "invalid language code");
                return;
            }
        }
        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const fpath = std.fmt.bufPrint(&path_buf, "{s}/languages/{s}.json",
            .{ g_data_dir, code }) catch { respError(resp, 500, "path too long"); return; };
        guardPath(fpath) catch { respError(resp, 403, "path denied"); return; };
        const file = std.fs.cwd().openFile(fpath, .{}) catch {
            respError(resp, 404, "language not found");
            return;
        };
        defer file.close();
        const contents = file.readToEndAlloc(allocator, 64 * 1024) catch {
            respError(resp, 500, "read failed");
            return;
        };
        defer allocator.free(contents);
        const parsed = std.json.parseFromSlice(LanguageInfo, allocator, contents, .{
            .ignore_unknown_fields = true,
        }) catch { respError(resp, 500, "parse failed"); return; };
        var mp = parsed; defer mp.deinit();
        var fbs = std.io.fixedBufferStream(&g_resp_buf);
        writeLangJson(fbs.writer(), parsed.value) catch {
            respError(resp, 500, "json encode failed");
            return;
        };
        const written = fbs.getWritten();
        resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
        resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
        return;
    }

    if (is_get and std.mem.eql(u8, path, "/api/v1/corpus/stats")) {
        lolCorpusStatsInto(resp, allocator);
        return;
    }

    if (is_get and std.mem.eql(u8, path, "/api/v1/crawl/status")) {
        lolCrawlStatusInto(resp, allocator);
        return;
    }

    if (is_get and std.mem.eql(u8, path, "/api/v1/health")) {
        lolHealthInto(resp, allocator);
        return;
    }

    // Unmatched REST path.
    _ = body; // suppress unused warning
    respError(resp, 404, "not found");
}

fn lolGrpcDispatch(
    path:      []const u8,
    method:    []const u8,
    body:      []const u8,
    resp:      *GnosisResponse,
    allocator: std.mem.Allocator,
) void {
    const is_post = std.mem.eql(u8, method, "POST");
    if (!is_post) {
        resp.status = 405; resp._pad = 0; resp.content_type = CT_GRPC;
        const e = "{\"error\":\"POST required for RPC calls\"}";
        resp.body_ptr = e; resp.body_len = e.len;
        return;
    }

    if (std.mem.eql(u8, path, "/lol.CorpusService/ListLanguages")) {
        // Reuse REST list handler (same response shape).
        lolRestDispatch("/api/v1/languages", "GET", "", resp, allocator);
        resp.content_type = CT_GRPC;
        return;
    }
    if (std.mem.eql(u8, path, "/lol.CorpusService/GetLanguage")) {
        const code = jsonFieldStrInto(body, "code", allocator) orelse {
            resp.status = 400; resp._pad = 0; resp.content_type = CT_GRPC;
            const e = "{\"error\":\"code field required\"}";
            resp.body_ptr = e; resp.body_len = e.len;
            return;
        };
        defer allocator.free(code);
        for (code) |ch| {
            if (!std.ascii.isAlphanumeric(ch) and ch != '-') {
                resp.status = 400; resp._pad = 0; resp.content_type = CT_GRPC;
                const e = "{\"error\":\"invalid language code\"}";
                resp.body_ptr = e; resp.body_len = e.len;
                return;
            }
        }
        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const fpath = std.fmt.bufPrint(&path_buf, "{s}/languages/{s}.json",
            .{ g_data_dir, code }) catch {
                resp.status = 500; resp._pad = 0; resp.content_type = CT_GRPC;
                const e = "{\"error\":\"path too long\"}"; resp.body_ptr = e; resp.body_len = e.len;
                return;
            };
        guardPath(fpath) catch {
            resp.status = 403; resp._pad = 0; resp.content_type = CT_GRPC;
            const e = "{\"error\":\"path denied\"}"; resp.body_ptr = e; resp.body_len = e.len;
            return;
        };
        const file = std.fs.cwd().openFile(fpath, .{}) catch {
            resp.status = 404; resp._pad = 0; resp.content_type = CT_GRPC;
            const e = "{\"error\":\"language not found\"}"; resp.body_ptr = e; resp.body_len = e.len;
            return;
        };
        defer file.close();
        const contents = file.readToEndAlloc(allocator, 64 * 1024) catch {
            resp.status = 500; resp._pad = 0; resp.content_type = CT_GRPC;
            const e = "{\"error\":\"read failed\"}"; resp.body_ptr = e; resp.body_len = e.len;
            return;
        };
        defer allocator.free(contents);
        const parsed = std.json.parseFromSlice(LanguageInfo, allocator, contents, .{
            .ignore_unknown_fields = true,
        }) catch {
            resp.status = 500; resp._pad = 0; resp.content_type = CT_GRPC;
            const e = "{\"error\":\"parse failed\"}"; resp.body_ptr = e; resp.body_len = e.len;
            return;
        };
        var mp = parsed; defer mp.deinit();
        var fbs = std.io.fixedBufferStream(&g_resp_buf);
        writeLangJson(fbs.writer(), parsed.value) catch {
            resp.status = 500; resp._pad = 0; resp.content_type = CT_GRPC;
            const e = "{\"error\":\"json encode failed\"}"; resp.body_ptr = e; resp.body_len = e.len;
            return;
        };
        const written = fbs.getWritten();
        resp.status = 200; resp._pad = 0; resp.content_type = CT_GRPC;
        resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
        return;
    }
    if (std.mem.eql(u8, path, "/lol.CorpusService/CorpusStats")) {
        lolCorpusStatsInto(resp, allocator);
        resp.content_type = CT_GRPC;
        return;
    }
    if (std.mem.eql(u8, path, "/lol.CorpusService/CrawlStatus")) {
        lolCrawlStatusInto(resp, allocator);
        resp.content_type = CT_GRPC;
        return;
    }
    if (std.mem.eql(u8, path, "/lol.CorpusService/Health")) {
        lolHealthInto(resp, allocator);
        resp.content_type = CT_GRPC;
        return;
    }
    resp.status = 404; resp._pad = 0; resp.content_type = CT_GRPC;
    const e = "{\"error\":\"gRPC method not found\"}";
    resp.body_ptr = e; resp.body_len = e.len;
}

fn lolGraphqlDispatch(
    method: []const u8,
    body:   []const u8,
    resp:   *GnosisResponse,
    allocator: std.mem.Allocator,
) void {
    const is_get  = std.mem.eql(u8, method, "GET");
    const is_post = std.mem.eql(u8, method, "POST");
    if (is_get) {
        resp.status = 200; resp._pad = 0; resp.content_type = CT_HTML;
        resp.body_ptr = GRAPHQL_PLAYGROUND; resp.body_len = GRAPHQL_PLAYGROUND.len;
        return;
    }
    if (is_post) {
        // Extract "query" field from {"query":"..."}.
        const query = jsonFieldStrInto(body, "query", allocator) orelse {
            const e = "{\"errors\":[{\"message\":\"Missing query field\"}]}";
            resp.status = 400; resp._pad = 0; resp.content_type = CT_JSON;
            resp.body_ptr = e; resp.body_len = e.len;
            return;
        };
        defer allocator.free(query);
        lolGqlResolve(query, resp, allocator);
        return;
    }
    const e = "{\"error\":\"POST or GET required\"}";
    resp.status = 405; resp._pad = 0; resp.content_type = CT_JSON;
    resp.body_ptr = e; resp.body_len = e.len;
}

/// Write corpus stats into g_resp_buf and fill resp.
fn lolCorpusStatsInto(resp: *GnosisResponse, allocator: std.mem.Allocator) void {
    const langs = listLanguages(allocator, g_data_dir) catch &[_]std.json.Parsed(LanguageInfo){};
    defer { for (langs) |p| { var m = p; m.deinit(); } allocator.free(langs); }
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
    var fbs = std.io.fixedBufferStream(&g_resp_buf);
    fbs.writer().print(
        "{{\"total_languages\":{d},\"total_verses\":{d},\"total_bytes\":{d}," ++
        "\"avg_quality\":{d:.4},\"families\":{d}}}",
        .{ n, total_verses, total_bytes, avg_quality, families.count() },
    ) catch { respError(resp, 500, "json encode failed"); return; };
    const written = fbs.getWritten();
    resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
    resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
}

/// Write crawl status into g_resp_buf and fill resp.
fn lolCrawlStatusInto(resp: *GnosisResponse, allocator: std.mem.Allocator) void {
    var cs = readCrawlStatus(allocator, g_data_dir);
    defer cs.deinit();
    const s = cs.value;
    var fbs = std.io.fixedBufferStream(&g_resp_buf);
    const w = fbs.writer();
    w.print(
        "{{\"total_languages\":{d},\"crawled\":{d},\"in_progress\":{d}," ++
        "\"failed\":{d},\"last_crawl\":\"{s}\",\"sources\":[",
        .{ s.total_languages, s.crawled, s.in_progress, s.failed, s.last_crawl },
    ) catch { respError(resp, 500, "json encode failed"); return; };
    for (s.sources, 0..) |src, i| {
        if (i > 0) w.writeByte(',') catch return;
        w.print(
            "{{\"name\":\"{s}\",\"languages\":{d},\"crawled\":{d},\"status\":\"{s}\"}}",
            .{ src.name, src.languages, src.crawled, src.status },
        ) catch return;
    }
    w.writeAll("]}") catch { respError(resp, 500, "buffer overflow"); return; };
    const written = fbs.getWritten();
    resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
    resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
}

/// Write health JSON into g_resp_buf and fill resp.
fn lolHealthInto(resp: *GnosisResponse, allocator: std.mem.Allocator) void {
    const langs = listLanguages(allocator, g_data_dir) catch &[_]std.json.Parsed(LanguageInfo){};
    defer { for (langs) |p| { var m = p; m.deinit(); } allocator.free(langs); }
    const status: []const u8 = if (langs.len > 0) "ok" else "no_data";
    var fbs = std.io.fixedBufferStream(&g_resp_buf);
    fbs.writer().print(
        "{{\"status\":\"{s}\",\"version\":\"0.1.0\"," ++
        "\"data_dir\":\"{s}\",\"languages\":{d}}}",
        .{ status, g_data_dir, langs.len },
    ) catch { respError(resp, 500, "json encode failed"); return; };
    const written = fbs.getWritten();
    resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
    resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
}

/// Resolve a GraphQL query string and fill resp.
fn lolGqlResolve(query: []const u8, resp: *GnosisResponse, allocator: std.mem.Allocator) void {
    var fbs = std.io.fixedBufferStream(&g_resp_buf);
    const w = fbs.writer();

    if (std.mem.indexOf(u8, query, "health") != null) {
        const langs = listLanguages(allocator, g_data_dir) catch &[_]std.json.Parsed(LanguageInfo){};
        defer { for (langs) |p| { var m = p; m.deinit(); } allocator.free(langs); }
        const status: []const u8 = if (langs.len > 0) "ok" else "no_data";
        w.print(
            "{{\"data\":{{\"health\":{{\"status\":\"{s}\",\"version\":\"0.1.0\",\"languages\":{d}}}}}}}",
            .{ status, langs.len },
        ) catch {};
    } else if (std.mem.indexOf(u8, query, "crawlStatus") != null) {
        var cs = readCrawlStatus(allocator, g_data_dir);
        defer cs.deinit();
        const s = cs.value;
        w.print(
            "{{\"data\":{{\"crawlStatus\":{{\"totalLanguages\":{d},\"crawled\":{d}," ++
            "\"inProgress\":{d},\"failed\":{d},\"lastCrawl\":\"{s}\",\"sources\":[",
            .{ s.total_languages, s.crawled, s.in_progress, s.failed, s.last_crawl },
        ) catch {};
        for (s.sources, 0..) |src, i| {
            if (i > 0) w.writeByte(',') catch return;
            w.print(
                "{{\"name\":\"{s}\",\"languages\":{d},\"crawled\":{d},\"status\":\"{s}\"}}",
                .{ src.name, src.languages, src.crawled, src.status },
            ) catch {};
        }
        w.writeAll("]}}}}") catch {};
    } else if (std.mem.indexOf(u8, query, "corpusStats") != null) {
        const langs = listLanguages(allocator, g_data_dir) catch &[_]std.json.Parsed(LanguageInfo){};
        defer { for (langs) |p| { var m = p; m.deinit(); } allocator.free(langs); }
        var total_verses: i64 = 0; var total_bytes: i64 = 0; var quality_sum: f64 = 0.0;
        var families = std.StringHashMap(void).init(allocator); defer families.deinit();
        for (langs) |p| {
            total_verses += p.value.verses; total_bytes += p.value.verses * 200;
            quality_sum += p.value.quality;
            if (p.value.family.len > 0) _ = families.put(p.value.family, {}) catch {};
        }
        const n = langs.len;
        const avg_quality = if (n > 0) quality_sum / @as(f64, @floatFromInt(n)) else 0.0;
        w.print(
            "{{\"data\":{{\"corpusStats\":{{\"totalLanguages\":{d},\"totalVerses\":{d}," ++
            "\"totalBytes\":{d},\"avgQuality\":{d:.4},\"families\":{d}}}}}}}",
            .{ n, total_verses, total_bytes, avg_quality, families.count() },
        ) catch {};
    } else if (std.mem.indexOf(u8, query, "language(") != null or
               std.mem.indexOf(u8, query, "language (") != null)
    {
        const code = gqlArgStr(query, "code", allocator);
        defer if (code) |c| allocator.free(c);
        if (code == null) {
            w.writeAll("{\"errors\":[{\"message\":\"language(code: ...) requires a code argument\"}]}") catch {};
        } else {
            const c = code.?;
            var valid = true;
            for (c) |ch| { if (!std.ascii.isAlphanumeric(ch) and ch != '-') { valid = false; break; } }
            if (!valid) {
                w.writeAll("{\"errors\":[{\"message\":\"invalid language code\"}]}") catch {};
            } else {
                var path_buf: [std.fs.max_path_bytes]u8 = undefined;
                const fpath = std.fmt.bufPrint(&path_buf, "{s}/languages/{s}.json",
                    .{ g_data_dir, c }) catch {
                        w.writeAll("{\"errors\":[{\"message\":\"path too long\"}]}") catch {};
                        const written = fbs.getWritten();
                        resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
                        resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
                        return;
                    };
                guardPath(fpath) catch {
                    w.writeAll("{\"errors\":[{\"message\":\"path denied\"}]}") catch {};
                    const written = fbs.getWritten();
                    resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
                    resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
                    return;
                };
                const file = std.fs.cwd().openFile(fpath, .{}) catch {
                    var eb: [256]u8 = undefined;
                    var efbs = std.io.fixedBufferStream(&eb);
                    efbs.writer().print(
                        "{{\"errors\":[{{\"message\":\"Language not found: {s}\"}}]}}", .{c},
                    ) catch {};
                    const ee = efbs.getWritten();
                    w.writeAll(ee) catch {};
                    const written = fbs.getWritten();
                    resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
                    resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
                    return;
                };
                defer file.close();
                const contents = file.readToEndAlloc(allocator, 64 * 1024) catch {
                    w.writeAll("{\"errors\":[{\"message\":\"read failed\"}]}") catch {};
                    const written = fbs.getWritten();
                    resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
                    resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
                    return;
                };
                defer allocator.free(contents);
                const parsed = std.json.parseFromSlice(LanguageInfo, allocator, contents, .{
                    .ignore_unknown_fields = true,
                }) catch {
                    w.writeAll("{\"errors\":[{\"message\":\"parse failed\"}]}") catch {};
                    const written = fbs.getWritten();
                    resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
                    resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
                    return;
                };
                var mp = parsed; defer mp.deinit();
                w.writeAll("{\"data\":{\"language\":") catch {};
                writeLangJson(w, parsed.value) catch {};
                w.writeAll("}}") catch {};
            }
        }
    } else if (std.mem.indexOf(u8, query, "languages") != null) {
        const langs = listLanguages(allocator, g_data_dir) catch {
            w.writeAll("{\"errors\":[{\"message\":\"failed to read corpus\"}]}") catch {};
            const written = fbs.getWritten();
            resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
            resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
            return;
        };
        defer { for (langs) |p| { var m = p; m.deinit(); } allocator.free(langs); }
        w.writeAll("{\"data\":{\"languages\":[") catch {};
        for (langs, 0..) |p, i| {
            if (i > 0) w.writeByte(',') catch return;
            writeLangJson(w, p.value) catch {};
        }
        w.writeAll("]}}") catch {};
    } else if (std.mem.indexOf(u8, query, "__schema") != null) {
        w.writeAll(
            \\{"data":{"__schema":{"types":[
            \\{"name":"Query","fields":["languages","language","corpusStats","crawlStatus","health"]},
            \\{"name":"Language","fields":["iso639_3","name","nativeName","family","scripts","sources","verses","quality"]},
            \\{"name":"CorpusStats","fields":["totalLanguages","totalVerses","totalBytes","avgQuality","families"]},
            \\{"name":"CrawlStatus","fields":["totalLanguages","crawled","inProgress","failed","lastCrawl","sources"]},
            \\{"name":"Source","fields":["name","languages","crawled","status"]},
            \\{"name":"Health","fields":["status","version","languages"]}
            \\]}}}
        ) catch {};
    } else {
        w.writeAll(
            "{\"errors\":[{\"message\":\"Unknown query. Available: " ++
            "languages, language(code), corpusStats, crawlStatus, health\"}]}",
        ) catch {};
    }

    const written = fbs.getWritten();
    resp.status = 200; resp._pad = 0; resp.content_type = CT_JSON;
    resp.body_ptr = written.ptr; resp.body_len = @intCast(written.len);
}

/// Allocating JSON field extractor.  Returns an owned copy; caller frees.
fn jsonFieldStrInto(data: []const u8, key: []const u8, allocator: std.mem.Allocator) ?[]u8 {
    var key_buf: [128]u8 = undefined;
    const needle = std.fmt.bufPrint(&key_buf, "\"{s}\"", .{key}) catch return null;
    const key_pos = std.mem.indexOf(u8, data, needle) orelse return null;
    const after_key = data[key_pos + needle.len ..];
    const colon = std.mem.indexOfScalar(u8, after_key, ':') orelse return null;
    var rest = std.mem.trimLeft(u8, after_key[colon + 1 ..], " \t\n\r");
    if (rest.len == 0) return null;
    if (rest[0] == '"') {
        rest = rest[1..];
        const end = std.mem.indexOfScalar(u8, rest, '"') orelse return null;
        return allocator.dupe(u8, rest[0..end]) catch null;
    }
    var end: usize = rest.len;
    for (rest, 0..) |ch, i| {
        if (ch == ',' or ch == '}' or ch == ']' or ch == '\n') { end = i; break; }
    }
    const trimmed = std.mem.trim(u8, rest[0..end], " \t");
    return allocator.dupe(u8, trimmed) catch null;
}

/// The edge handler registered with uapi_gnosis_set_handler.
///
/// gnosis calls this for every HTTP request accepted on the pool port.
/// The method, path, and body are already parsed by gnosis; lolHandler
/// path-dispatches to the appropriate protocol family.
///
/// g_allocator and g_data_dir must have been set before uapi_gnosis_start.
export fn lolHandler(req: *const GnosisRequest, resp: *GnosisResponse) callconv(.c) void {
    // Sanity guard: if context isn't ready (shouldn't happen in production),
    // return a 503 rather than reading uninitialised memory.
    if (!g_context_ready) {
        respError(resp, 503, "gateway not ready");
        return;
    }

    var arena_inst = std.heap.ArenaAllocator.init(g_allocator);
    defer arena_inst.deinit();
    const arena = arena_inst.allocator();

    const method = std.mem.span(req.method);
    const path   = std.mem.span(req.path);
    const body: []const u8 = if (req.body_ptr) |p| p[0..req.body_len] else "";

    // CORS preflight — respond immediately.
    if (std.mem.eql(u8, method, "OPTIONS")) {
        resp.status = 204; resp._pad = 0; resp.content_type = CT_TEXT;
        resp.body_ptr = null; resp.body_len = 0;
        return;
    }

    // Path-based protocol routing.
    if (std.mem.startsWith(u8, path, "/api/v1/") or std.mem.eql(u8, path, "/")) {
        lolRestDispatch(path, method, body, resp, arena);
    } else if (std.mem.startsWith(u8, path, "/lol.CorpusService/")) {
        lolGrpcDispatch(path, method, body, resp, arena);
    } else if (std.mem.startsWith(u8, path, "/graphql")) {
        lolGraphqlDispatch(method, body, resp, arena);
    } else {
        respError(resp, 404, "not found");
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

    std.debug.print("LOL (1000Langs) API Gateway v0.2.0 (single-port)\n", .{});
    std.debug.print("  Data directory : {s}\n", .{data_dir});
    std.debug.print("  All protocols  : http://0.0.0.0:{d}/\n", .{base_port});
    std.debug.print("    REST         : /api/v1/*\n", .{});
    std.debug.print("    gRPC-compat  : /lol.CorpusService/*\n", .{});
    std.debug.print("    GraphQL      : /graphql\n", .{});

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
    // Set up module-level handler context before uapi_gnosis_start.
    // g_data_dir and g_allocator are read by lolHandler; they are set here and
    // never mutated after uapi_gnosis_start returns.
    // -------------------------------------------------------------------------
    g_data_dir     = data_dir;
    g_allocator    = gpa;
    g_context_ready = true;

    // -------------------------------------------------------------------------
    // Single-port setup:
    //   1. Create one gnosis pool slot on base_port.
    //   2. Register lolHandler as the edge dispatch hook.
    //   3. Start the gnosis accept loop (blocks until stop or signal).
    // -------------------------------------------------------------------------
    const handle = uapi_gnosis_create(base_port);
    if (handle == 0) {
        std.debug.print("lol-gateway: uapi_gnosis_create failed for port {d}\n", .{base_port});
        return error.GnosisCreateFailed;
    }
    defer uapi_gnosis_destroy(handle);

    const set_rc = uapi_gnosis_set_handler(handle, &lolHandler);
    if (set_rc != UAPI_OK) {
        std.debug.print("lol-gateway: uapi_gnosis_set_handler failed (result={d})\n", .{set_rc});
        return error.GnosisSetHandlerFailed;
    }

    // uapi_gnosis_start spawns a background thread that owns the accept loop.
    // It returns once the thread is running (listening state).
    const start_rc = uapi_gnosis_start(handle);
    if (start_rc != UAPI_OK) {
        std.debug.print("lol-gateway: uapi_gnosis_start failed (result={d})\n", .{start_rc});
        return error.GnosisStartFailed;
    }

    std.debug.print("lol-gateway: listening on :{d} (gnosis handle {d})\n",
        .{ base_port, handle });

    // Block main thread until the server stops (e.g. SIGINT / uapi_gnosis_stop).
    // gnosis's background thread runs independently; we sleep here.
    while (uapi_gnosis_state(handle) == UAPI_SERVER_LISTENING) {
        std.Thread.sleep(1 * std.time.ns_per_s);
    }
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
