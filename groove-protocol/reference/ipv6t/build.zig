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
}
