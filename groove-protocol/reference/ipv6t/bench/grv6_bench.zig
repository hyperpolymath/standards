// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// grv6_bench.zig — GRV6 protocol benchmarks.
//
// Measures throughput for the core cryptographic operations used in
// the Groove protocol's typed-frame system. All benchmarks use
// std.time.Timer for wall-clock measurement and print results to stdout.
//
// Operations benchmarked:
//   1. Type hash computation (SHA-256 of A2ML type signature, 100K iters)
//   2. Capability hash computation (SHA-256 of capability manifest, 100K iters)
//   3. Provenance hash chain computation (100K chain steps)
//   4. Frame header serialization (layout only, 100K iters)
//   5. Hash hex formatting (100K iters)
//
// Run with: zig build bench  (or see build.zig bench step)
//
// ENVIRONMENT NOTES:
// - Network I/O benchmarks (frame write/read over loopback TCP) are NOT
//   included here because they depend on OS scheduling and cannot run
//   reliably in CI without introducing flakiness.
// - All benchmarks here are pure computation (no I/O, no allocations in hot path).

const std = @import("std");
const grv6 = @import("grv6");
const time = std.time;

// =========================================================================
// Benchmark configuration
// =========================================================================

/// Number of iterations for each benchmark.
const ITERS: u64 = 100_000;

/// Type signature used in type hash benchmarks.
const TYPE_SIG = "@capability(id=\"voice\",version=\"2.1.0\",interface=\"VoiceTransport\")";

/// Capability manifest used in cap hash benchmarks.
const CAP_MANIFEST = "@groove-manifest(version=\"0.1.0\"):@system(id=\"bench\")@end";

/// Payload used in provenance chain benchmarks.
const PAYLOAD = "benchmark frame payload data for provenance chain measurement";

// =========================================================================
// Benchmark runner helper
// =========================================================================

const BenchResult = struct {
    name: []const u8,
    iters: u64,
    total_ns: u64,
    ns_per_iter: f64,
    iters_per_sec: f64,
};

fn runBench(name: []const u8, iters: u64, comptime bench_fn: fn () void) BenchResult {
    // Warmup: 1000 iterations to prime caches and branch predictors
    for (0..1000) |_| {
        bench_fn();
    }

    var timer = time.Timer.start() catch @panic("timer unavailable");
    for (0..iters) |_| {
        bench_fn();
    }
    const total_ns = timer.read();

    const ns_per_iter: f64 = @as(f64, @floatFromInt(total_ns)) / @as(f64, @floatFromInt(iters));
    const iters_per_sec: f64 = 1_000_000_000.0 / ns_per_iter;

    return .{
        .name = name,
        .iters = iters,
        .total_ns = total_ns,
        .ns_per_iter = ns_per_iter,
        .iters_per_sec = iters_per_sec,
    };
}

fn printResult(r: BenchResult) void {
    std.debug.print(
        "  {s:<50} {:>10.1} ns/iter  ({d:.2} M/s)\n",
        .{ r.name, r.ns_per_iter, r.iters_per_sec / 1_000_000.0 },
    );
}

// =========================================================================
// Benchmark functions (comptime, no state)
// =========================================================================

var _bench_sink: [grv6.HASH_SIZE]u8 = undefined;
var _hex_sink: [grv6.HASH_SIZE * 2]u8 = undefined;
var _header_sink: grv6.FrameHeader = undefined;

fn benchTypeHash() void {
    _bench_sink = grv6.computeTypeHash(TYPE_SIG);
    std.mem.doNotOptimizeAway(&_bench_sink);
}

fn benchCapHash() void {
    _bench_sink = grv6.computeCapHash(CAP_MANIFEST);
    std.mem.doNotOptimizeAway(&_bench_sink);
}

fn benchProvHashChainStep() void {
    // One step in the provenance chain (includes two SHA-256 operations: payload hash + chain hash)
    _bench_sink = grv6.computeProvHash(
        _bench_sink, // use previous result as type_hash to simulate chain
        _bench_sink, // cap_hash
        std.time.nanoTimestamp(),
        PAYLOAD,
        _bench_sink, // prev_hash
    );
    std.mem.doNotOptimizeAway(&_bench_sink);
}

fn benchHashHexFormat() void {
    _hex_sink = grv6.hashToHex(_bench_sink);
    std.mem.doNotOptimizeAway(&_hex_sink);
}

fn benchHeaderSizeOf() void {
    // Just to document that header is 108 bytes — this is a compile-time constant.
    // We benchmark the @sizeOf call to confirm no overhead.
    const size = @sizeOf(grv6.FrameHeader);
    std.mem.doNotOptimizeAway(&size);
}

fn benchTrustLevelDerivation() void {
    const flags = grv6.Flags{ .typed = true, .proven = false, .attested = true };
    const level = grv6.trustLevel(flags);
    std.mem.doNotOptimizeAway(&level);
}

// =========================================================================
// Main benchmark entry point
// =========================================================================

pub fn main() void {
    std.debug.print("\n", .{});
    std.debug.print("GRV6 Groove Protocol Benchmarks\n", .{});
    std.debug.print("================================\n", .{});
    std.debug.print("Iterations: {d}\n", .{ITERS});
    std.debug.print("\n", .{});
    std.debug.print("  {s:<50} {s:>14}  {s}\n", .{ "Benchmark", "Time", "Throughput" });
    std.debug.print("  " ++ ("-" ** 78) ++ "\n", .{});

    const results = [_]BenchResult{
        runBench("type_hash (SHA-256 of A2ML type sig ~70 bytes)", ITERS, benchTypeHash),
        runBench("cap_hash (SHA-256 of capability manifest ~50 bytes)", ITERS, benchCapHash),
        runBench("prov_hash chain step (2x SHA-256 + state)", ITERS, benchProvHashChainStep),
        runBench("hash_to_hex (32 bytes -> 64 hex chars)", ITERS, benchHashHexFormat),
        runBench("trust_level_derivation (flags -> enum)", ITERS, benchTrustLevelDerivation),
        runBench("header_sizeof (compile-time constant access)", ITERS, benchHeaderSizeOf),
    };

    for (results) |r| {
        printResult(r);
    }

    std.debug.print("\n", .{});
    std.debug.print("Baseline notes:\n", .{});
    std.debug.print("  - type_hash and cap_hash: one SHA-256 operation each\n", .{});
    std.debug.print("  - prov_hash: two SHA-256 operations (payload hash + chain hash)\n", .{});
    std.debug.print("  - All benchmarks: pure computation, no I/O, no alloc in hot path\n", .{});
    std.debug.print("  - Network benchmarks (frame roundtrip over loopback TCP) require\n", .{});
    std.debug.print("    live environment - see docs/LIVE-BENCHMARKS.adoc\n", .{});
    std.debug.print("\n", .{});
}
