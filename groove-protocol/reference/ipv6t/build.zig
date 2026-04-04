// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Create root module for the library
    const lib_mod = b.createModule(.{
        .root_source_file = b.path("src/grv6.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Library
    const lib = b.addLibrary(.{
        .name = "grv6",
        .root_module = lib_mod,
    });
    b.installArtifact(lib);

    // Create test module with grv6 as import
    const test_mod = b.createModule(.{
        .root_source_file = b.path("test/grv6_test.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "grv6", .module = lib_mod },
        },
    });

    // Tests
    const tests = b.addTest(.{
        .root_module = test_mod,
    });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run GRV6 frame tests");
    test_step.dependOn(&run_tests.step);

    // Benchmarks
    const bench_mod = b.createModule(.{
        .root_source_file = b.path("bench/grv6_bench.zig"),
        .target = target,
        .optimize = .ReleaseFast, // benchmarks always use ReleaseFast
        .imports = &.{
            .{ .name = "grv6", .module = lib_mod },
        },
    });

    const bench_exe = b.addExecutable(.{
        .name = "grv6-bench",
        .root_module = bench_mod,
    });
    b.installArtifact(bench_exe);

    const run_bench = b.addRunArtifact(bench_exe);
    const bench_step = b.step("bench", "Run GRV6 performance benchmarks");
    bench_step.dependOn(&run_bench.step);
}
