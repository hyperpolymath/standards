// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Build configuration for LOL (1000Langs) Zig API gateway.
// Replaces the V vweb gateway in api/v-gateway/.
// Requires Zig 0.15.2+.

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target   = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const main_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target           = target,
        .optimize         = optimize,
    });

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

    const unit_tests = b.addTest(.{ .root_module = main_mod });
    const run_tests  = b.addRunArtifact(unit_tests);
    const test_step  = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);
}
