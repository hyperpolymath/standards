// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // For Zig 0.15, we compile to object files and link separately
    // This creates libstamp.a (static) which can be used by other programs

    const stamp_mod = b.addModule("stamp", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Build as static library for now (shared libs need different approach in 0.15)
    const lib = b.addStaticLibrary(.{
        .name = "stamp",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib.linkLibC();
    b.installArtifact(lib);

    // Install C header file
    const install_header = b.addInstallFile(
        b.path("src/stamp.h"),
        "include/stamp.h",
    );
    b.getInstallStep().dependOn(&install_header.step);

    // Unit tests
    const tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);

    // Example program
    const example = b.addExecutable(.{
        .name = "example",
        .root_source_file = b.path("src/example.zig"),
        .target = target,
        .optimize = optimize,
    });

    example.root_module.addImport("main", stamp_mod);
    example.linkLibrary(lib);
    example.linkLibC();
    b.installArtifact(example);

    const run_example = b.addRunArtifact(example);
    const example_step = b.step("example", "Run example program");
    example_step.dependOn(&run_example.step);
}
