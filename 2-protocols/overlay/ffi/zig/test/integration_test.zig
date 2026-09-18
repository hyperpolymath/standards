// SPDX-License-Identifier: MPL-2.0
// Overlay Protocol FFI Integration Tests
//
// Tests the FFI from an external consumer's perspective, verifying
// that the C ABI surface works correctly for all five invariants.

const std = @import("std");
const overlay = @import("overlay");

// ============================================================================
// Scenario: HOL-o-extension style overlay (o-extension peer type)
// ============================================================================

test "scenario: conformant o-extension overlay" {
    // Set up declaration matching HOL-o-extension
    var decl = overlay.overlay_declaration_new(.o_extension);
    setField(&decl, 0, "../HOL");
    setField(&decl, 1, "https://github.com/HOL-Theorem-Prover/HOL");
    setField(&decl, 2, "source activate.sh");
    setField(&decl, 3, "unset HOL_OEXT_ACTIVE; unset HOL_OEXT_DIR");
    setField(&decl, 4, "Optional overlay extending HOL4 with custom theories");

    // Base state before and after overlay presence (unchanged)
    const base = overlay.BaseState{ .state_id = 100, .file_count = 2500 };
    const overlay_files: u64 = 12; // theories + tactics + activate.sh

    // Identity activation (overlay adds to load path, doesn't change base)
    const flags = overlay.overlay_check_conformance(
        &decl,
        &base,
        &base,
        overlay_files,
        &identityActivate,
        &identityDeactivate,
    );

    try std.testing.expect(flags.isConformant());
    try std.testing.expectEqual(@as(u8, 5), flags.passCount());
    try std.testing.expectEqual(@as(u32, 1), overlay.overlay_is_conformant(flags));
}

// ============================================================================
// Scenario: aggregate-library style overlay
// ============================================================================

test "scenario: conformant aggregate-library overlay" {
    var decl = overlay.overlay_declaration_new(.aggregate_library);
    setField(&decl, 0, "multiple ecosystem standard libraries");
    setField(&decl, 1, "https://github.com/hyperpolymath/developer-ecosystem");
    setField(&decl, 2, "import aggregate-library");
    setField(&decl, 3, "remove dependency");
    setField(&decl, 4, "Curated subset with conformance tests");

    const base = overlay.BaseState{ .state_id = 200, .file_count = 50000 };
    const overlay_specs: u64 = 29;

    const flags = overlay.overlay_check_conformance(
        &decl,
        &base,
        &base,
        overlay_specs,
        &identityActivate,
        &identityDeactivate,
    );

    try std.testing.expect(flags.isConformant());
}

// ============================================================================
// Scenario: non-conformant overlay (modifies base)
// ============================================================================

test "scenario: overlay that modifies base fails non-modification" {
    var decl = overlay.overlay_declaration_new(.o_extension);
    setField(&decl, 0, "../base");
    setField(&decl, 1, "https://example.com");
    setField(&decl, 2, "source activate.sh");
    setField(&decl, 3, "unset X");
    setField(&decl, 4, "Bad overlay");

    const before = overlay.BaseState{ .state_id = 1, .file_count = 100 };
    const after = overlay.BaseState{ .state_id = 1, .file_count = 101 }; // modified!

    const flags = overlay.overlay_check_conformance(
        &decl,
        &before,
        &after,
        5,
        &identityActivate,
        &identityDeactivate,
    );

    try std.testing.expect(!flags.non_modification);
    try std.testing.expect(!flags.additive_only);
    try std.testing.expect(!flags.isConformant());
}

// ============================================================================
// Scenario: non-reversible activation fails switchable
// ============================================================================

test "scenario: non-reversible activation fails switchable" {
    var decl = overlay.overlay_declaration_new(.o_extension);
    setField(&decl, 0, "../base");
    setField(&decl, 1, "https://example.com");
    setField(&decl, 2, "source activate.sh");
    setField(&decl, 3, "unset X");
    setField(&decl, 4, "Irreversible overlay");

    const base = overlay.BaseState{ .state_id = 1, .file_count = 100 };

    const flags = overlay.overlay_check_conformance(
        &decl,
        &base,
        &base,
        5,
        &incrementActivate,
        &identityDeactivate,
    );

    try std.testing.expect(flags.non_modification);
    try std.testing.expect(flags.additive_only);
    try std.testing.expect(!flags.switchable); // deactivate(activate(base)) != base
    try std.testing.expect(!flags.idempotent); // activate(activate(base)) != activate(base)
    try std.testing.expect(!flags.isConformant());
}

// ============================================================================
// Scenario: composability of mixed peer types
// ============================================================================

test "scenario: mixed peer types composable on same base" {
    const base = overlay.BaseState{ .state_id = 42, .file_count = 1000 };

    try std.testing.expectEqual(
        overlay.Result.ok,
        overlay.overlay_check_mixed_composable(.o_extension, .aggregate_library, &base, &base),
    );
}

test "scenario: same peer types rejected by mixed composability" {
    const base = overlay.BaseState{ .state_id = 42, .file_count = 1000 };

    try std.testing.expectEqual(
        overlay.Result.invalid_param,
        overlay.overlay_check_mixed_composable(.o_extension, .o_extension, &base, &base),
    );
}

// ============================================================================
// Scenario: incomplete declaration
// ============================================================================

test "scenario: declaration missing upstream_url fails" {
    var decl = overlay.overlay_declaration_new(.o_extension);
    setField(&decl, 0, "../base");
    // upstream_url deliberately omitted
    setField(&decl, 2, "source activate.sh");
    setField(&decl, 3, "unset X");
    setField(&decl, 4, "Missing upstream");

    try std.testing.expectEqual(
        overlay.Result.not_conformant,
        overlay.overlay_check_declaration(&decl),
    );
}

// ============================================================================
// Helpers
// ============================================================================

fn setField(decl: *overlay.OverlayDeclaration, field: u32, value: []const u8) void {
    _ = overlay.overlay_declaration_set_field(decl, field, value.ptr, @intCast(value.len));
}

fn identityActivate(base: *const overlay.BaseState, out: *overlay.BaseState) callconv(.c) void {
    out.* = base.*;
}

fn identityDeactivate(base: *const overlay.BaseState, out: *overlay.BaseState) callconv(.c) void {
    out.* = base.*;
}

fn incrementActivate(base: *const overlay.BaseState, out: *overlay.BaseState) callconv(.c) void {
    out.* = overlay.BaseState{
        .state_id = base.state_id + 1,
        .file_count = base.file_count,
    };
}
