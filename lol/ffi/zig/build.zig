// LOL i18n Service — FFI Build Configuration
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Shared library (liblol.so / liblol.dylib / lol.dll)
    const lib = b.addSharedLibrary(.{
        .name = "lol",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    lib.version = .{ .major = 0, .minor = 1, .patch = 0 };

    // Static library (liblol.a)
    const lib_static = b.addStaticLibrary(.{
        .name = "lol",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Install artifacts
    b.installArtifact(lib);
    b.installArtifact(lib_static);

    // Install the generated C header
    const header = b.addInstallHeader(
        b.path("../../generated/abi/lol.h"),
        "lol.h",
    );
    b.getInstallStep().dependOn(&header.step);

    // Unit tests
    const lib_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_lib_tests = b.addRunArtifact(lib_tests);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_lib_tests.step);

    // Integration tests
    const integration_tests = b.addTest(.{
        .root_source_file = b.path("test/integration_test.zig"),
        .target = target,
        .optimize = optimize,
    });

    integration_tests.linkLibrary(lib);

    const run_integration_tests = b.addRunArtifact(integration_tests);

    const integration_test_step = b.step("test-integration", "Run integration tests");
    integration_test_step.dependOn(&run_integration_tests.step);
}
