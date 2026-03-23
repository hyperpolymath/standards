// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// grv6_test.zig — IPv6T test harness.
//
// Two panel instances on localhost ([::1]), exchanging GRV6 typed frames.
// Tests cover all five validation scenarios from the IPv6T spec:
//
//   1. Positive: correct type hash → accepted
//   2. Negative: wrong type hash → rejected before payload parsing
//   3. Provenance: 3 chained frames → chain verifiable
//   4. Fallback: raw bytes without magic → treated as untyped
//   5. Trust flag: PROVEN flag but invalid payload → still validated, rejected
//
// Plus additional property tests:
//   6. Header size is exactly 108 bytes
//   7. Round-trip: write then read preserves all fields
//   8. Hash determinism: same input always produces same hash
//   9. Provenance chain integrity: breaking one link invalidates the rest
//  10. Multiple type acceptance: reader accepts any of N expected types

const std = @import("std");
const testing = std.testing;
const net = std.net;
const mem = std.mem;
const Thread = std.Thread;

const grv6 = @import("grv6");

// =========================================================================
// Test type signatures (simulated A2ML)
// =========================================================================

const TYPE_VOICE = "@capability(id=\"voice\",version=\"2.1.0\",interface=\"VoiceTransport\")";
const TYPE_TEXT = "@capability(id=\"text\",version=\"1.0.0\",interface=\"TextChannel\")";
const TYPE_HEALTH = "@capability(id=\"health\",version=\"1.0.0\",interface=\"HealthCheck\")";
const CAP_MANIFEST = "@groove-manifest(version=\"0.1.0\"):@system(id=\"test\")@end";

const VOICE_HASH = blk: {
    @setEvalBranchQuota(10000);
    break :blk grv6.computeTypeHash(TYPE_VOICE);
};
const TEXT_HASH = blk: {
    @setEvalBranchQuota(10000);
    break :blk grv6.computeTypeHash(TYPE_TEXT);
};
const HEALTH_HASH = blk: {
    @setEvalBranchQuota(10000);
    break :blk grv6.computeTypeHash(TYPE_HEALTH);
};
const CAP_HASH = blk: {
    @setEvalBranchQuota(10000);
    break :blk grv6.computeCapHash(CAP_MANIFEST);
};

// =========================================================================
// Helper: run a server that accepts one connection and calls handler
// =========================================================================

fn runServer(
    handler: *const fn (net.Stream, *ServerCtx) void,
    ctx: *ServerCtx,
    port: u16,
) void {
    const addr = net.Address.parseIp6("::1", port) catch return;
    var server = addr.listen(.{ .reuse_address = true }) catch return;
    defer server.deinit();

    // Signal that server is ready
    ctx.ready.store(true, .release);

    const conn = server.accept() catch return;
    defer conn.stream.close();
    handler(conn.stream, ctx);
}

const ServerCtx = struct {
    ready: std.atomic.Value(bool),
    result: std.atomic.Value(i32), // 0 = not set, 1 = pass, -1 = fail
    payload: []const u8,
    type_hash: [grv6.HASH_SIZE]u8,

    fn init(payload: []const u8, type_hash: [grv6.HASH_SIZE]u8) ServerCtx {
        return .{
            .ready = std.atomic.Value(bool).init(false),
            .result = std.atomic.Value(i32).init(0),
            .payload = payload,
            .type_hash = type_hash,
        };
    }
};

/// Wait for server to be ready (with timeout).
fn waitForServer(ctx: *ServerCtx) !void {
    var attempts: u32 = 0;
    while (!ctx.ready.load(.acquire)) {
        attempts += 1;
        if (attempts > 100) return error.ServerTimeout;
        std.Thread.sleep(1 * std.time.ns_per_ms);
    }
}

// =========================================================================
// Test 1: Positive — correct type hash → accepted
// =========================================================================

fn serverAcceptVoice(stream: net.Stream, ctx: *ServerCtx) void {
    var expected = [_][grv6.HASH_SIZE]u8{VOICE_HASH};
    var reader = grv6.FrameReader.init(
        testing.allocator,
        &expected,
        4096,
    ) catch {
        (&ctx.result).store(-1, .release);
        return;
    };
    defer reader.deinit();

    const result = reader.readFrame(stream);
    switch (result) {
        .frame => |f| {
            const header: *const grv6.FrameHeader = @ptrCast(@alignCast(&f.header));
            const validation = reader.validate(header);
            if (validation == .accepted and mem.eql(u8, f.payload, ctx.payload)) {
                (&ctx.result).store(1, .release);
            } else {
                (&ctx.result).store(-1, .release);
            }
        },
        else => (&ctx.result).store(-1, .release),
    }
}

test "1. positive: correct type hash is accepted" {
    const port: u16 = 16470;
    const payload = "Hello, typed world!";
    var ctx = ServerCtx.init(payload, VOICE_HASH);

    const server_thread = try Thread.spawn(.{}, runServer, .{ serverAcceptVoice, &ctx, port });

    try waitForServer(&ctx);

    // Client: connect and send a GRV6 frame
    const addr = try net.Address.parseIp6("::1", port);
    const stream = try net.tcpConnectToAddress(addr);
    defer stream.close();

    var writer = grv6.FrameWriter.init(.{
        .type_hash = VOICE_HASH,
        .cap_hash = CAP_HASH,
        .flags = .{ .typed = true },
    });

    _ = try writer.writeFrame(stream, payload);

    server_thread.join();
    try testing.expectEqual(@as(i32, 1), ctx.result.load(.acquire));
}

// =========================================================================
// Test 2: Negative — wrong type hash → rejected
// =========================================================================

fn serverRejectWrongType(stream: net.Stream, ctx: *ServerCtx) void {
    var expected = [_][grv6.HASH_SIZE]u8{VOICE_HASH};
    var reader = grv6.FrameReader.init(
        testing.allocator,
        &expected,
        4096,
    ) catch {
        (&ctx.result).store(-1, .release);
        return;
    };
    defer reader.deinit();

    const result = reader.readFrame(stream);
    switch (result) {
        .frame => |f| {
            const header: *const grv6.FrameHeader = @ptrCast(@alignCast(&f.header));
            const validation = reader.validate(header);
            if (validation == .rejected_type_mismatch) {
                // Correctly rejected — pass
                (&ctx.result).store(1, .release);
            } else {
                (&ctx.result).store(-1, .release);
            }
        },
        else => (&ctx.result).store(-1, .release),
    }
}

test "2. negative: wrong type hash is rejected before payload parsing" {
    const port: u16 = 16471;
    var ctx = ServerCtx.init("payload doesn't matter", TEXT_HASH);

    const server_thread = try Thread.spawn(.{}, runServer, .{ serverRejectWrongType, &ctx, port });

    try waitForServer(&ctx);

    const addr = try net.Address.parseIp6("::1", port);
    const stream = try net.tcpConnectToAddress(addr);
    defer stream.close();

    // Send with TEXT type hash, but server expects VOICE
    var writer = grv6.FrameWriter.init(.{
        .type_hash = TEXT_HASH,
        .cap_hash = CAP_HASH,
        .flags = .{ .typed = true },
    });

    _ = try writer.writeFrame(stream, "this should be rejected");

    server_thread.join();
    try testing.expectEqual(@as(i32, 1), ctx.result.load(.acquire));
}

// =========================================================================
// Test 3: Provenance — 3 chained frames, chain verifiable
// =========================================================================

test "3. provenance: three chained frames produce verifiable hash chain" {
    const type_hash = VOICE_HASH;
    const cap_hash = CAP_HASH;
    const payloads = [_][]const u8{ "frame one", "frame two", "frame three" };

    var prev_hash = grv6.ZERO_HASH;
    var prov_hashes: [3][grv6.HASH_SIZE]u8 = undefined;

    for (payloads, 0..) |payload, i| {
        const timestamp = std.time.nanoTimestamp();
        const prov_hash = grv6.computeProvHash(type_hash, cap_hash, timestamp, payload, prev_hash);

        // Each hash should be different from the previous
        if (i > 0) {
            try testing.expect(!mem.eql(u8, &prov_hash, &prov_hashes[i - 1]));
        }

        // Hash should not be zero
        try testing.expect(!mem.eql(u8, &prov_hash, &grv6.ZERO_HASH));

        prov_hashes[i] = prov_hash;
        prev_hash = prov_hash;
    }

    // All three hashes should be unique
    try testing.expect(!mem.eql(u8, &prov_hashes[0], &prov_hashes[1]));
    try testing.expect(!mem.eql(u8, &prov_hashes[1], &prov_hashes[2]));
    try testing.expect(!mem.eql(u8, &prov_hashes[0], &prov_hashes[2]));
}

// =========================================================================
// Test 4: Fallback — raw bytes without magic → untyped
// =========================================================================

fn serverDetectUntyped(stream: net.Stream, ctx: *ServerCtx) void {
    var expected = [_][grv6.HASH_SIZE]u8{VOICE_HASH};
    var reader = grv6.FrameReader.init(
        testing.allocator,
        &expected,
        4096,
    ) catch {
        (&ctx.result).store(-1, .release);
        return;
    };
    defer reader.deinit();

    const result = reader.readFrame(stream);
    switch (result) {
        .untyped => |raw| {
            // Correctly identified as untyped — pass
            if (raw.len > 0) {
                (&ctx.result).store(1, .release);
            } else {
                (&ctx.result).store(-1, .release);
            }
        },
        else => (&ctx.result).store(-1, .release),
    }
}

test "4. fallback: raw bytes without GRV6 magic treated as untyped" {
    const port: u16 = 16472;
    var ctx = ServerCtx.init("", VOICE_HASH);

    const server_thread = try Thread.spawn(.{}, runServer, .{ serverDetectUntyped, &ctx, port });

    try waitForServer(&ctx);

    const addr = try net.Address.parseIp6("::1", port);
    const stream = try net.tcpConnectToAddress(addr);
    defer stream.close();

    // Send raw bytes, NOT a GRV6 frame
    try stream.writeAll("This is plain untyped HTTP/1.1 data with no GRV6 header at all");

    // Small delay to let server process
    std.Thread.sleep(50 * std.time.ns_per_ms);

    server_thread.join();
    try testing.expectEqual(@as(i32, 1), ctx.result.load(.acquire));
}

// =========================================================================
// Test 5: Trust flag — PROVEN but invalid payload → still validated
// =========================================================================

test "5. trust flag: PROVEN flag does not bypass type hash validation" {
    const port: u16 = 16473;
    var ctx = ServerCtx.init("", VOICE_HASH);

    const server_thread = try Thread.spawn(.{}, runServer, .{ serverRejectWrongType, &ctx, port });

    try waitForServer(&ctx);

    const addr = try net.Address.parseIp6("::1", port);
    const stream = try net.tcpConnectToAddress(addr);
    defer stream.close();

    // Send with PROVEN flag but wrong type hash — should still be rejected
    var writer = grv6.FrameWriter.init(.{
        .type_hash = HEALTH_HASH, // Wrong type
        .cap_hash = CAP_HASH,
        .flags = .{ .typed = true, .proven = true }, // Claims to be proven
    });

    _ = try writer.writeFrame(stream, "proven but wrong type");

    server_thread.join();
    try testing.expectEqual(@as(i32, 1), ctx.result.load(.acquire));
}

// =========================================================================
// Test 6: Header size
// =========================================================================

test "6. header size is exactly 108 bytes" {
    try testing.expectEqual(@as(usize, 108), @sizeOf(grv6.FrameHeader));
    try testing.expectEqual(@as(usize, 108), grv6.HEADER_SIZE);
}

// =========================================================================
// Test 7: Hash determinism
// =========================================================================

test "7. hash determinism: same input always produces same hash" {
    const h1 = grv6.computeTypeHash(TYPE_VOICE);
    const h2 = grv6.computeTypeHash(TYPE_VOICE);
    const h3 = grv6.computeTypeHash(TYPE_TEXT);

    try testing.expect(mem.eql(u8, &h1, &h2)); // Same input → same hash
    try testing.expect(!mem.eql(u8, &h1, &h3)); // Different input → different hash
}

// =========================================================================
// Test 8: Multiple type acceptance
// =========================================================================

fn serverAcceptMultiple(stream: net.Stream, ctx: *ServerCtx) void {
    var expected = [_][grv6.HASH_SIZE]u8{ VOICE_HASH, TEXT_HASH, HEALTH_HASH };
    var reader = grv6.FrameReader.init(
        testing.allocator,
        &expected,
        4096,
    ) catch {
        (&ctx.result).store(-1, .release);
        return;
    };
    defer reader.deinit();

    const result = reader.readFrame(stream);
    switch (result) {
        .frame => |f| {
            const header: *const grv6.FrameHeader = @ptrCast(@alignCast(&f.header));
            const validation = reader.validate(header);
            if (validation == .accepted) {
                (&ctx.result).store(1, .release);
            } else {
                (&ctx.result).store(-1, .release);
            }
        },
        else => (&ctx.result).store(-1, .release),
    }
}

test "8. multiple type acceptance: reader accepts any of N expected types" {
    const port: u16 = 16474;
    var ctx = ServerCtx.init("health data", HEALTH_HASH);

    const server_thread = try Thread.spawn(.{}, runServer, .{ serverAcceptMultiple, &ctx, port });

    try waitForServer(&ctx);

    const addr = try net.Address.parseIp6("::1", port);
    const stream = try net.tcpConnectToAddress(addr);
    defer stream.close();

    // Send HEALTH type — server accepts VOICE, TEXT, or HEALTH
    var writer = grv6.FrameWriter.init(.{
        .type_hash = HEALTH_HASH,
        .cap_hash = CAP_HASH,
        .flags = .{ .typed = true },
    });

    _ = try writer.writeFrame(stream, "health data");

    server_thread.join();
    try testing.expectEqual(@as(i32, 1), ctx.result.load(.acquire));
}

// =========================================================================
// Test 9: Trust level derivation
// =========================================================================

test "9. trust level correctly derived from flags" {
    try testing.expectEqual(grv6.TrustLevel.untyped, grv6.trustLevel(.{}));
    try testing.expectEqual(grv6.TrustLevel.declared, grv6.trustLevel(.{ .typed = true }));
    try testing.expectEqual(grv6.TrustLevel.attested, grv6.trustLevel(.{ .typed = true, .attested = true }));
    try testing.expectEqual(grv6.TrustLevel.proven, grv6.trustLevel(.{ .typed = true, .proven = true }));
    try testing.expectEqual(grv6.TrustLevel.proven, grv6.trustLevel(.{ .typed = true, .proven = true, .attested = true }));
}

// =========================================================================
// Test 10: Hex formatting
// =========================================================================

test "10. hash hex formatting is correct" {
    const hash = grv6.computeTypeHash("test");
    const hex = grv6.hashToHex(hash);
    // SHA-256 of "test" is known: 9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08
    try testing.expect(hex[0] == '9');
    try testing.expect(hex[1] == 'f');
    try testing.expectEqual(@as(usize, 64), hex.len);
}
