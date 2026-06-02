// LOL i18n Service — FFI Build Configuration
// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Builds liblol as both shared and static libraries.
// The library implements the C ABI declared in src/abi/Foreign.idr
// and specified in generated/abi/lol.h.
//
// Modules:
//   src/main.zig    — C ABI exports and service state
//   src/locale.zig  — BCP 47 locale parsing, validation, normalisation
//   src/store.zig   — Thread-safe translation store with fallback
//   src/plural.zig  — CLDR plural rule selection per language
//   src/ffi.zig     — Module re-exports and FFI surface documentation

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const root_source = b.path("src/main.zig");

    // Shared library (liblol.so / liblol.dylib / lol.dll)
    const lib = b.addLibrary(.{
        .name = "lol",
        .root_module = b.createModule(.{
            .root_source_file = root_source,
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
        .linkage = .dynamic,
    });
    // Static library (liblol.a)
    const lib_static = b.addLibrary(.{
        .name = "lol",
        .root_module = b.createModule(.{
            .root_source_file = root_source,
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
        .linkage = .static,
    });

    // Install artifacts
    b.installArtifact(lib);
    b.installArtifact(lib_static);

    // Install the generated C header alongside the library.
    b.installFile("../../generated/abi/lol.h", "include/lol.h");

    // Unit tests (main module — needs libc for c_allocator)
    const lib_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = root_source,
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    const run_lib_tests = b.addRunArtifact(lib_tests);

    // Sub-module tests: locale
    const locale_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/locale.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_locale_tests = b.addRunArtifact(locale_tests);

    // Sub-module tests: plural
    const plural_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/plural.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_plural_tests = b.addRunArtifact(plural_tests);

    // Sub-module tests: store
    const store_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/store.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_store_tests = b.addRunArtifact(store_tests);

    const test_step = b.step("test", "Run all library tests");
    test_step.dependOn(&run_lib_tests.step);
    test_step.dependOn(&run_locale_tests.step);
    test_step.dependOn(&run_plural_tests.step);
    test_step.dependOn(&run_store_tests.step);

    // Integration tests
    const integration_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/integration_test.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    integration_tests.root_module.linkLibrary(lib);

    const run_integration_tests = b.addRunArtifact(integration_tests);

    const integration_test_step = b.step("test-integration", "Run integration tests");
    integration_test_step.dependOn(&run_integration_tests.step);
}
