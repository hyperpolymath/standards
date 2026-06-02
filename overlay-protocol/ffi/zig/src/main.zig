// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Overlay Protocol FFI Implementation
//
// C-compatible implementation of the Overlay Protocol ABI defined in
// src/abi/OverlayProtocol.idr. Provides runtime validation of all five
// core invariants: non-modification, additive-only, switchable,
// idempotent activation, and declared relationship.

const std = @import("std");

const VERSION = "1.0.0-draft";

// ============================================================================
// Core Types (match src/abi/OverlayProtocol.idr)
// ============================================================================

/// Peer type enum — maps to Idris2 PeerType.
pub const PeerType = enum(c_int) {
    o_extension = 0,
    aggregate_library = 1,
};

/// Result codes for FFI calls.
pub const Result = enum(c_int) {
    ok = 0,
    err = 1,
    invalid_param = 2,
    null_pointer = 3,
    not_conformant = 4,
};

/// Conformance check result — one bit per invariant.
pub const ConformanceFlags = packed struct {
    non_modification: bool = false,
    additive_only: bool = false,
    switchable: bool = false,
    idempotent: bool = false,
    declared_relationship: bool = false,
    _padding: u3 = 0,

    pub fn isConformant(self: ConformanceFlags) bool {
        return self.non_modification and
            self.additive_only and
            self.switchable and
            self.idempotent and
            self.declared_relationship;
    }

    pub fn passCount(self: ConformanceFlags) u8 {
        var count: u8 = 0;
        if (self.non_modification) count += 1;
        if (self.additive_only) count += 1;
        if (self.switchable) count += 1;
        if (self.idempotent) count += 1;
        if (self.declared_relationship) count += 1;
        return count;
    }
};

/// Maximum length for string fields in the declaration.
const MAX_STRING_LEN = 4096;

/// Overlay declaration — C-compatible struct matching Idris2 OverlayDeclaration.
pub const OverlayDeclaration = extern struct {
    peer_type: PeerType,
    base_path: [MAX_STRING_LEN]u8,
    base_path_len: u32,
    upstream_url: [MAX_STRING_LEN]u8,
    upstream_url_len: u32,
    activation: [MAX_STRING_LEN]u8,
    activation_len: u32,
    deactivation: [MAX_STRING_LEN]u8,
    deactivation_len: u32,
    description: [MAX_STRING_LEN]u8,
    description_len: u32,
};

/// Base state snapshot — corresponds to Idris2 BaseState.
pub const BaseState = extern struct {
    state_id: u64,
    file_count: u64,
};

// ============================================================================
// Thread-local error storage
// ============================================================================

threadlocal var last_error_buf: [1024]u8 = undefined;
threadlocal var last_error_len: usize = 0;

fn setError(msg: []const u8) void {
    const len = @min(msg.len, last_error_buf.len);
    @memcpy(last_error_buf[0..len], msg[0..len]);
    last_error_len = len;
}

fn clearError() void {
    last_error_len = 0;
}

// ============================================================================
// Declaration Construction
// ============================================================================

/// Create a new empty overlay declaration.
pub export fn overlay_declaration_new(peer_type: PeerType) OverlayDeclaration {
    return OverlayDeclaration{
        .peer_type = peer_type,
        .base_path = std.mem.zeroes([MAX_STRING_LEN]u8),
        .base_path_len = 0,
        .upstream_url = std.mem.zeroes([MAX_STRING_LEN]u8),
        .upstream_url_len = 0,
        .activation = std.mem.zeroes([MAX_STRING_LEN]u8),
        .activation_len = 0,
        .deactivation = std.mem.zeroes([MAX_STRING_LEN]u8),
        .deactivation_len = 0,
        .description = std.mem.zeroes([MAX_STRING_LEN]u8),
        .description_len = 0,
    };
}

/// Set a string field on the declaration. Field index:
///   0 = base_path, 1 = upstream_url, 2 = activation,
///   3 = deactivation, 4 = description
pub export fn overlay_declaration_set_field(
    decl: *OverlayDeclaration,
    field: u32,
    value: [*]const u8,
    value_len: u32,
) Result {
    if (value_len > MAX_STRING_LEN) {
        setError("Field value exceeds maximum length");
        return .invalid_param;
    }

    const len: usize = @intCast(value_len);

    switch (field) {
        0 => {
            @memcpy(decl.base_path[0..len], value[0..len]);
            decl.base_path_len = value_len;
        },
        1 => {
            @memcpy(decl.upstream_url[0..len], value[0..len]);
            decl.upstream_url_len = value_len;
        },
        2 => {
            @memcpy(decl.activation[0..len], value[0..len]);
            decl.activation_len = value_len;
        },
        3 => {
            @memcpy(decl.deactivation[0..len], value[0..len]);
            decl.deactivation_len = value_len;
        },
        4 => {
            @memcpy(decl.description[0..len], value[0..len]);
            decl.description_len = value_len;
        },
        else => {
            setError("Invalid field index");
            return .invalid_param;
        },
    }

    clearError();
    return .ok;
}

// ============================================================================
// Invariant 1: Non-Modification
// ============================================================================

/// Check that two base states are identical (non-modification invariant).
/// Returns ok if before == after, not_conformant otherwise.
pub export fn overlay_check_non_modification(
    before: *const BaseState,
    after: *const BaseState,
) Result {
    if (before.state_id == after.state_id and before.file_count == after.file_count) {
        clearError();
        return .ok;
    }
    setError("Non-modification violated: base state changed");
    return .not_conformant;
}

// ============================================================================
// Invariant 2: Additive Only
// ============================================================================

/// Check that the overlay is purely additive: base unchanged and overlay
/// contributes at least one item.
pub export fn overlay_check_additive(
    base_before: *const BaseState,
    base_after: *const BaseState,
    overlay_item_count: u64,
) Result {
    if (base_before.state_id != base_after.state_id or
        base_before.file_count != base_after.file_count)
    {
        setError("Additive-only violated: base state changed");
        return .not_conformant;
    }
    if (overlay_item_count == 0) {
        setError("Additive-only violated: overlay contributes zero items");
        return .not_conformant;
    }
    clearError();
    return .ok;
}

// ============================================================================
// Invariant 3: Switchable
// ============================================================================

/// Activation function pointer type (C ABI).
/// Takes a base state, returns a new state.
pub const ActivateFn = *const fn (*const BaseState, *BaseState) callconv(.c) void;
pub const DeactivateFn = *const fn (*const BaseState, *BaseState) callconv(.c) void;

/// Check that deactivate(activate(base)) == base.
pub export fn overlay_check_switchable(
    base: *const BaseState,
    activate_fn: ActivateFn,
    deactivate_fn: DeactivateFn,
) Result {
    // activate(base) -> activated
    var activated: BaseState = undefined;
    activate_fn(base, &activated);

    // deactivate(activated) -> restored
    var restored: BaseState = undefined;
    deactivate_fn(&activated, &restored);

    if (restored.state_id == base.state_id and restored.file_count == base.file_count) {
        clearError();
        return .ok;
    }
    setError("Switchable violated: deactivate(activate(base)) != base");
    return .not_conformant;
}

// ============================================================================
// Invariant 4: Idempotent Activation
// ============================================================================

/// Check that activate(activate(base)) == activate(base) and
/// deactivate(deactivate(base)) == deactivate(base).
pub export fn overlay_check_idempotent(
    base: *const BaseState,
    activate_fn: ActivateFn,
    deactivate_fn: DeactivateFn,
) Result {
    // activate(base) -> a1
    var a1: BaseState = undefined;
    activate_fn(base, &a1);

    // activate(a1) -> a2
    var a2: BaseState = undefined;
    activate_fn(&a1, &a2);

    if (a1.state_id != a2.state_id or a1.file_count != a2.file_count) {
        setError("Idempotent violated: activate(activate(base)) != activate(base)");
        return .not_conformant;
    }

    // deactivate(base) -> d1
    var d1: BaseState = undefined;
    deactivate_fn(base, &d1);

    // deactivate(d1) -> d2
    var d2: BaseState = undefined;
    deactivate_fn(&d1, &d2);

    if (d1.state_id != d2.state_id or d1.file_count != d2.file_count) {
        setError("Idempotent violated: deactivate(deactivate(base)) != deactivate(base)");
        return .not_conformant;
    }

    clearError();
    return .ok;
}

// ============================================================================
// Invariant 5: Declared Relationship
// ============================================================================

/// Check that the declaration has all required non-empty fields.
pub export fn overlay_check_declaration(decl: *const OverlayDeclaration) Result {
    if (decl.base_path_len == 0) {
        setError("Declaration invalid: base_path is empty");
        return .not_conformant;
    }
    if (decl.upstream_url_len == 0) {
        setError("Declaration invalid: upstream_url is empty");
        return .not_conformant;
    }
    if (decl.activation_len == 0) {
        setError("Declaration invalid: activation is empty");
        return .not_conformant;
    }
    if (decl.deactivation_len == 0) {
        setError("Declaration invalid: deactivation is empty");
        return .not_conformant;
    }
    if (decl.description_len == 0) {
        setError("Declaration invalid: description is empty");
        return .not_conformant;
    }
    clearError();
    return .ok;
}

// ============================================================================
// Full Conformance Check
// ============================================================================

/// Run all five invariant checks and return a ConformanceFlags bitfield.
pub export fn overlay_check_conformance(
    decl: *const OverlayDeclaration,
    base_before: *const BaseState,
    base_after: *const BaseState,
    overlay_item_count: u64,
    activate_fn: ?ActivateFn,
    deactivate_fn: ?DeactivateFn,
) ConformanceFlags {
    var flags = ConformanceFlags{};

    // 1. Non-modification
    flags.non_modification = (overlay_check_non_modification(base_before, base_after) == .ok);

    // 2. Additive only
    flags.additive_only = (overlay_check_additive(base_before, base_after, overlay_item_count) == .ok);

    // 3. Switchable
    if (activate_fn != null and deactivate_fn != null) {
        flags.switchable = (overlay_check_switchable(base_before, activate_fn.?, deactivate_fn.?) == .ok);
    }

    // 4. Idempotent
    if (activate_fn != null and deactivate_fn != null) {
        flags.idempotent = (overlay_check_idempotent(base_before, activate_fn.?, deactivate_fn.?) == .ok);
    }

    // 5. Declared relationship
    flags.declared_relationship = (overlay_check_declaration(decl) == .ok);

    return flags;
}

/// Check if a ConformanceFlags indicates full conformance.
pub export fn overlay_is_conformant(flags: ConformanceFlags) u32 {
    return if (flags.isConformant()) 1 else 0;
}

/// Get the number of passed invariants from a ConformanceFlags.
pub export fn overlay_pass_count(flags: ConformanceFlags) u32 {
    return flags.passCount();
}

// ============================================================================
// Composition
// ============================================================================

/// Check that two overlays target the same base (composability prerequisite).
pub export fn overlay_check_composable(
    base_a: *const BaseState,
    base_b: *const BaseState,
) Result {
    if (base_a.state_id == base_b.state_id and base_a.file_count == base_b.file_count) {
        clearError();
        return .ok;
    }
    setError("Not composable: overlays target different base states");
    return .not_conformant;
}

/// Check mixed peer-type composability (o-extension + aggregate-library).
pub export fn overlay_check_mixed_composable(
    peer_a: PeerType,
    peer_b: PeerType,
    base_a: *const BaseState,
    base_b: *const BaseState,
) Result {
    if (peer_a == peer_b) {
        setError("Mixed composability requires different peer types");
        return .invalid_param;
    }
    return overlay_check_composable(base_a, base_b);
}

// ============================================================================
// Peer Type Utilities
// ============================================================================

/// Get the string name of a peer type.
pub export fn overlay_peer_type_name(peer_type: PeerType) [*:0]const u8 {
    return switch (peer_type) {
        .o_extension => "o-extension",
        .aggregate_library => "aggregate-library",
    };
}

/// Check that two peer types are distinct.
pub export fn overlay_peer_types_distinct(a: PeerType, b: PeerType) u32 {
    return if (a != b) 1 else 0;
}

// ============================================================================
// Error Handling
// ============================================================================

/// Get the last error message. Returns null if no error.
pub export fn overlay_last_error() ?[*:0]const u8 {
    if (last_error_len == 0) return null;
    // Null-terminate for C compatibility
    if (last_error_len < last_error_buf.len) {
        last_error_buf[last_error_len] = 0;
    }
    return @ptrCast(&last_error_buf);
}

// ============================================================================
// Version
// ============================================================================

/// Get the Overlay Protocol FFI version string.
pub export fn overlay_version() [*:0]const u8 {
    return VERSION;
}

// ============================================================================
// Tests
// ============================================================================

// Test helpers
fn identityActivate(base: *const BaseState, out: *BaseState) callconv(.c) void {
    out.* = base.*;
}

fn identityDeactivate(base: *const BaseState, out: *BaseState) callconv(.c) void {
    out.* = base.*;
}

fn badActivate(base: *const BaseState, out: *BaseState) callconv(.c) void {
    out.* = BaseState{ .state_id = base.state_id + 1, .file_count = base.file_count };
}

test "non-modification: identical states pass" {
    const s = BaseState{ .state_id = 42, .file_count = 10 };
    try std.testing.expectEqual(Result.ok, overlay_check_non_modification(&s, &s));
}

test "non-modification: different states fail" {
    const before = BaseState{ .state_id = 42, .file_count = 10 };
    const after = BaseState{ .state_id = 42, .file_count = 11 };
    try std.testing.expectEqual(Result.not_conformant, overlay_check_non_modification(&before, &after));
}

test "additive: base unchanged with items passes" {
    const s = BaseState{ .state_id = 1, .file_count = 5 };
    try std.testing.expectEqual(Result.ok, overlay_check_additive(&s, &s, 3));
}

test "additive: zero items fails" {
    const s = BaseState{ .state_id = 1, .file_count = 5 };
    try std.testing.expectEqual(Result.not_conformant, overlay_check_additive(&s, &s, 0));
}

test "additive: base changed fails" {
    const before = BaseState{ .state_id = 1, .file_count = 5 };
    const after = BaseState{ .state_id = 1, .file_count = 6 };
    try std.testing.expectEqual(Result.not_conformant, overlay_check_additive(&before, &after, 3));
}

test "switchable: identity functions pass" {
    const s = BaseState{ .state_id = 7, .file_count = 20 };
    try std.testing.expectEqual(Result.ok, overlay_check_switchable(&s, &identityActivate, &identityDeactivate));
}

test "switchable: non-reversible fails" {
    const s = BaseState{ .state_id = 7, .file_count = 20 };
    try std.testing.expectEqual(Result.not_conformant, overlay_check_switchable(&s, &badActivate, &identityDeactivate));
}

test "idempotent: identity functions pass" {
    const s = BaseState{ .state_id = 3, .file_count = 8 };
    try std.testing.expectEqual(Result.ok, overlay_check_idempotent(&s, &identityActivate, &identityDeactivate));
}

test "declaration: all fields present passes" {
    var decl = overlay_declaration_new(.o_extension);
    const base = "../ HOL";
    const url = "https://github.com/example/hol";
    const act = "source activate.sh";
    const deact = "unset ACTIVE";
    const desc = "Test overlay";
    _ = overlay_declaration_set_field(&decl, 0, base, base.len);
    _ = overlay_declaration_set_field(&decl, 1, url, url.len);
    _ = overlay_declaration_set_field(&decl, 2, act, act.len);
    _ = overlay_declaration_set_field(&decl, 3, deact, deact.len);
    _ = overlay_declaration_set_field(&decl, 4, desc, desc.len);
    try std.testing.expectEqual(Result.ok, overlay_check_declaration(&decl));
}

test "declaration: empty field fails" {
    const decl = overlay_declaration_new(.o_extension);
    try std.testing.expectEqual(Result.not_conformant, overlay_check_declaration(&decl));
}

test "full conformance: identity overlay passes all" {
    var decl = overlay_declaration_new(.o_extension);
    const base = "../base";
    const url = "https://example.com";
    const act = "source activate.sh";
    const deact = "unset X";
    const desc = "Test";
    _ = overlay_declaration_set_field(&decl, 0, base, base.len);
    _ = overlay_declaration_set_field(&decl, 1, url, url.len);
    _ = overlay_declaration_set_field(&decl, 2, act, act.len);
    _ = overlay_declaration_set_field(&decl, 3, deact, deact.len);
    _ = overlay_declaration_set_field(&decl, 4, desc, desc.len);

    const s = BaseState{ .state_id = 1, .file_count = 10 };
    const flags = overlay_check_conformance(&decl, &s, &s, 5, &identityActivate, &identityDeactivate);

    try std.testing.expect(flags.isConformant());
    try std.testing.expectEqual(@as(u8, 5), flags.passCount());
}

test "composable: same base passes" {
    const s = BaseState{ .state_id = 99, .file_count = 42 };
    try std.testing.expectEqual(Result.ok, overlay_check_composable(&s, &s));
}

test "peer type names" {
    const oext = std.mem.span(overlay_peer_type_name(.o_extension));
    try std.testing.expectEqualStrings("o-extension", oext);

    const alib = std.mem.span(overlay_peer_type_name(.aggregate_library));
    try std.testing.expectEqualStrings("aggregate-library", alib);
}

test "peer types distinct" {
    try std.testing.expectEqual(@as(u32, 1), overlay_peer_types_distinct(.o_extension, .aggregate_library));
    try std.testing.expectEqual(@as(u32, 0), overlay_peer_types_distinct(.o_extension, .o_extension));
}

test "version" {
    const ver = std.mem.span(overlay_version());
    try std.testing.expectEqualStrings(VERSION, ver);
}
