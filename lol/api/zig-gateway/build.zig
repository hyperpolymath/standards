// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Build configuration for LOL (1000Langs) Zig API gateway.
// Replaces the V vweb gateway in api/v-gateway/.
// Requires Zig 0.15.2+.
//
// Modules compiled:
//   src/main.zig     — HTTP gateway, REST / gRPC-compat / GraphQL handlers
//   src/types.zig    — Public domain types (replaces v-lol/src/types.v)
//   src/lol_ffi.zig  — Zig wrapper around liblol (replaces v-lol/src/lol.v + ffi.v)
//
// External dependencies:
//
//   liblol     — LOL i18n Zig FFI from ffi/zig/ (in-repo).
//                Build with `zig build` from standards/lol/ffi/zig/ if absent.
//                Linked from:  ../../ffi/zig/zig-out/lib
//                Headers from: ../../generated/abi
//
//   libzig_api — Unified Zig API stack from developer-ecosystem/zig-api.
//                Provides:
//                  uapi_init / uapi_teardown            — library lifecycle
//                  uapi_gnosis_create / _start / _stop  — HTTP server pool
//                  uapi_gnosis_destroy / _state          — pool management
//                  uapi_safe_path_default               — proven-backed path gate
//                Build with `zig build` in developer-ecosystem/zig-api/ffi/zig/.
//                Default paths override-able via -Dzig-api-lib-path and
//                -Dzig-api-include-path for CI environments.
//
//   libproven_ffi — formally-verified safety primitives from
//                   verification-ecosystem/proven.
//                   Needed at link time because libzig_api.a references
//                   proven_path_has_traversal.
//                   Default path override-able via -Dproven-lib-path.

const std = @import("std");

// -----------------------------------------------------------------------------
// Default paths — canonical Eclipse-drive locations
// -----------------------------------------------------------------------------

/// Pre-built libzig_api (developer-ecosystem/zig-api/ffi/zig/zig-out/lib).
const DEFAULT_ZIG_API_LIB_PATH =
    "/var/mnt/eclipse/repos/developer-ecosystem/zig-api/ffi/zig/zig-out/lib";

/// C header for libzig_api (developer-ecosystem/zig-api/generated/abi).
const DEFAULT_ZIG_API_INCLUDE_PATH =
    "/var/mnt/eclipse/repos/developer-ecosystem/zig-api/generated/abi";

/// Pre-built libproven_ffi (verification-ecosystem/proven/ffi/zig/zig-out/lib).
/// Points to proven's standard zig-out/lib output (zig-out-standalone symlink removed 2026-04-17).
const DEFAULT_PROVEN_LIB_PATH =
    "/var/mnt/eclipse/repos/verification-ecosystem/proven/ffi/zig/zig-out/lib";

pub fn build(b: *std.Build) void {
    const target   = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // -------------------------------------------------------------------------
    // Build options — allow CI to override library paths
    // -------------------------------------------------------------------------
    const zig_api_lib_path = b.option(
        []const u8,
        "zig-api-lib-path",
        "Directory containing libzig_api.a (default: " ++ DEFAULT_ZIG_API_LIB_PATH ++ ")",
    ) orelse DEFAULT_ZIG_API_LIB_PATH;

    const zig_api_include_path = b.option(
        []const u8,
        "zig-api-include-path",
        "Directory containing zig_api.h (default: " ++ DEFAULT_ZIG_API_INCLUDE_PATH ++ ")",
    ) orelse DEFAULT_ZIG_API_INCLUDE_PATH;

    const proven_lib_path = b.option(
        []const u8,
        "proven-lib-path",
        "Directory containing libproven_ffi.a (default: " ++ DEFAULT_PROVEN_LIB_PATH ++ ")",
    ) orelse DEFAULT_PROVEN_LIB_PATH;

    // -------------------------------------------------------------------------
    // Main module
    // -------------------------------------------------------------------------

    const main_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target           = target,
        .optimize         = optimize,
        // liblol and libzig_api both use the C standard library.
        .link_libc        = true,
    });

    // ── In-repo liblol (LOL i18n FFI) ────────────────────────────────────────
    // `zig build` in ffi/zig/ produces zig-out/lib/liblol.a and liblol.so.
    // We prefer the static library to keep the gateway binary self-contained.
    main_mod.addLibraryPath(b.path("../../ffi/zig/zig-out/lib"));
    main_mod.addIncludePath(b.path("../../generated/abi"));
    main_mod.linkSystemLibrary("lol", .{});

    // ── libzig_api (unified-zig-api HTTP server pool + path safety) ───────────
    main_mod.addLibraryPath(.{ .cwd_relative = zig_api_lib_path });
    main_mod.addIncludePath(.{ .cwd_relative = zig_api_include_path });
    main_mod.linkSystemLibrary("zig_api", .{});

    // ── libproven_ffi (transitive dep of libzig_api) ──────────────────────────
    // libzig_api.a references proven_path_has_traversal from libproven_ffi.
    // Link it here so the final executable resolves all symbols.
    main_mod.addLibraryPath(.{ .cwd_relative = proven_lib_path });
    main_mod.linkSystemLibrary("proven_ffi", .{});

    // -------------------------------------------------------------------------
    // Gateway executable
    // -------------------------------------------------------------------------

    const exe = b.addExecutable(.{
        .name        = "lol_gateway",
        .root_module = main_mod,
    });
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);
    const run_step = b.step("run", "Run the LOL API gateway");
    run_step.dependOn(&run_cmd.step);

    // -------------------------------------------------------------------------
    // Unit tests (covers main.zig, types.zig, lol_ffi.zig)
    // -------------------------------------------------------------------------

    const unit_tests = b.addTest(.{ .root_module = main_mod });
    const run_tests  = b.addRunArtifact(unit_tests);
    const test_step  = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);

    // -------------------------------------------------------------------------
    // Separate type / FFI module smoke tests (can run without liblol present)
    // -------------------------------------------------------------------------

    const types_mod = b.createModule(.{
        .root_source_file = b.path("src/types.zig"),
        .target           = target,
        .optimize         = optimize,
    });
    const types_tests = b.addTest(.{ .root_module = types_mod });
    const run_types_tests = b.addRunArtifact(types_tests);
    const types_test_step = b.step("test-types", "Run types.zig tests");
    types_test_step.dependOn(&run_types_tests.step);
}
