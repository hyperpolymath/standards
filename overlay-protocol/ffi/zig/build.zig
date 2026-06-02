// SPDX-License-Identifier: AGPL-3.0-or-later
// Overlay Protocol FFI Build Configuration

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const root_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Shared library (.so, .dylib, .dll)
    const lib = b.addLibrary(.{
        .linkage = .dynamic,
        .name = "overlay_protocol",
        .root_module = root_mod,
        .version = .{ .major = 1, .minor = 0, .patch = 0 },
    });

    // Static library (.a)
    const lib_static = b.addLibrary(.{
        .linkage = .static,
        .name = "overlay_protocol",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    b.installArtifact(lib);
    b.installArtifact(lib_static);

    // Unit tests
    const lib_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_lib_tests = b.addRunArtifact(lib_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_tests.step);

    // Integration tests
    const integration_mod = b.createModule(.{
        .root_source_file = b.path("test/integration_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    integration_mod.addImport("overlay", root_mod);

    const integration_tests = b.addTest(.{
        .root_module = integration_mod,
    });

    const run_integration_tests = b.addRunArtifact(integration_tests);

    const integration_test_step = b.step("test-integration", "Run integration tests");
    integration_test_step.dependOn(&run_integration_tests.step);
}
