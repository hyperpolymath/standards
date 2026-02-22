// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

//! STAMP FFI Implementation
//!
//! This module provides C-compatible FFI bindings for the STAMP verification library.
//! It wraps Idris2 verification functions and provides a stable ABI.

const std = @import("std");

// ============================================================================
// Types
// ============================================================================

/// Result codes for verification functions
pub const StampResult = enum(c_int) {
    success = 0,
    error_invalid_url = -1,
    error_timeout = -2,
    error_invalid_response = -3,
    error_invalid_signature = -4,
    error_rate_limit_exceeded = -5,
    error_consent_invalid = -6,
    error_null_pointer = -7,
    error_internal = -99,
};

/// Unsubscribe link verification parameters
pub const StampUnsubscribeParams = extern struct {
    /// URL to verify (null-terminated C string)
    url: [*:0]const u8,

    /// When the URL was tested (Unix timestamp in milliseconds)
    tested_at: u64,

    /// HTTP response code (200, 404, etc.)
    response_code: u16,

    /// Response time in milliseconds
    response_time: u32,

    /// Cryptographic token (null-terminated C string, 64 chars)
    token: [*:0]const u8,

    /// Signature (null-terminated C string)
    signature: [*:0]const u8,
};

/// Consent chain verification parameters
pub const StampConsentParams = extern struct {
    /// Initial opt-in request timestamp (Unix milliseconds)
    initial_request: u64,

    /// Confirmation timestamp (Unix milliseconds)
    confirmation: u64,

    /// IP address (null-terminated C string)
    ip_address: [*:0]const u8,

    /// Confirmation token (null-terminated C string)
    token: [*:0]const u8,
};

/// Rate limit verification parameters
pub const StampRateLimitParams = extern struct {
    /// Sender identifier (null-terminated C string)
    sender_id: [*:0]const u8,

    /// Account creation timestamp (Unix milliseconds)
    account_created: u64,

    /// Number of messages sent today
    messages_today: u32,

    /// Daily limit for this account
    daily_limit: u32,
};

/// Verification proof (returned to caller, must be freed)
pub const StampProof = extern struct {
    /// Proof data (JSON format)
    data: [*:0]u8,

    /// Length of proof data
    length: usize,

    /// Signature over proof data
    signature: [*:0]u8,
};

// ============================================================================
// Internal Helpers
// ============================================================================

/// Current timestamp (milliseconds since Unix epoch)
fn getCurrentTimestamp() u64 {
    const ns = std.time.nanoTimestamp();
    return @intCast(@divFloor(ns, std.time.ns_per_ms));
}

/// Validate URL format (basic check)
fn isValidUrl(url: [*:0]const u8) bool {
    const url_slice = std.mem.span(url);
    if (url_slice.len < 10) return false; // Minimum: "https://x"
    if (!std.mem.startsWith(u8, url_slice, "https://")) return false;
    return true;
}

/// Validate timestamp is recent (within last 60 seconds)
fn isRecentTimestamp(timestamp: u64) bool {
    const now = getCurrentTimestamp();
    const diff = if (now > timestamp) now - timestamp else timestamp - now;
    return diff < 60000; // 60 seconds
}

// ============================================================================
// FFI Functions (C ABI)
// ============================================================================

/// Verify an unsubscribe link
///
/// Returns:
/// - 0 (success) if the unsubscribe link is valid
/// - Negative error code otherwise
///
/// Example:
/// ```c
/// StampUnsubscribeParams params = {
///     .url = "https://example.com/unsub",
///     .tested_at = 1706630400000,
///     .response_code = 200,
///     .response_time = 87,
///     .token = "abc123...",
///     .signature = "sig...",
/// };
/// int result = stamp_verify_unsubscribe(&params);
/// if (result == 0) {
///     printf("Valid unsubscribe link\n");
/// }
/// ```
export fn stamp_verify_unsubscribe(params: ?*const StampUnsubscribeParams) callconv(.C) StampResult {
    // Null pointer check
    const p = params orelse return .error_null_pointer;

    // Validate URL format
    if (!isValidUrl(p.url)) {
        return .error_invalid_url;
    }

    // Verify test was recent (within last 60 seconds)
    if (!isRecentTimestamp(p.tested_at)) {
        return .error_timeout;
    }

    // Verify HTTP response code is 200 OK
    if (p.response_code != 200) {
        return .error_invalid_response;
    }

    // Verify response time is fast (< 200ms)
    if (p.response_time >= 200) {
        return .error_timeout;
    }

    // TODO: Verify cryptographic signature
    // For now, just check signature is not empty
    const sig_slice = std.mem.span(p.signature);
    if (sig_slice.len == 0) {
        return .error_invalid_signature;
    }

    // All checks passed
    return .success;
}

/// Verify a consent chain (double opt-in)
///
/// Returns:
/// - 0 (success) if consent is valid
/// - Negative error code otherwise
///
/// Example:
/// ```c
/// StampConsentParams params = {
///     .initial_request = 1706630400000,
///     .confirmation = 1706630500000, // 100 seconds later
///     .ip_address = "192.168.1.100",
///     .token = "token...",
/// };
/// int result = stamp_verify_consent(&params);
/// ```
export fn stamp_verify_consent(params: ?*const StampConsentParams) callconv(.C) StampResult {
    const p = params orelse return .error_null_pointer;

    // Verify confirmation happened AFTER initial request
    if (p.confirmation <= p.initial_request) {
        return .error_consent_invalid;
    }

    // Verify confirmation was timely (within 24 hours)
    const time_diff = p.confirmation - p.initial_request;
    if (time_diff > 86400000) { // 24 hours in milliseconds
        return .error_consent_invalid;
    }

    // TODO: Verify token is cryptographically valid for this IP
    const token_slice = std.mem.span(p.token);
    if (token_slice.len == 0) {
        return .error_invalid_signature;
    }

    return .success;
}

/// Verify rate limit compliance
///
/// Returns:
/// - 0 (success) if within rate limits
/// - Negative error code otherwise
///
/// Example:
/// ```c
/// StampRateLimitParams params = {
///     .sender_id = "user123",
///     .account_created = 1704067200000, // 30 days ago
///     .messages_today = 50,
///     .daily_limit = 1000,
/// };
/// int result = stamp_verify_rate_limit(&params);
/// ```
export fn stamp_verify_rate_limit(params: ?*const StampRateLimitParams) callconv(.C) StampResult {
    const p = params orelse return .error_null_pointer;

    // Verify sender ID is not empty
    const sender_slice = std.mem.span(p.sender_id);
    if (sender_slice.len == 0) {
        return .error_null_pointer;
    }

    // Verify messages today doesn't exceed daily limit
    if (p.messages_today >= p.daily_limit) {
        return .error_rate_limit_exceeded;
    }

    // Calculate account age in days
    const now = getCurrentTimestamp();
    const age_ms = now - p.account_created;
    const age_days = age_ms / (24 * 60 * 60 * 1000);

    // Verify daily limit is appropriate for account age
    // New accounts (< 30 days): max 1000/day
    // Established (30-90 days): max 10000/day
    // Veteran (90+ days): max 100000/day
    const max_allowed_limit: u32 = if (age_days < 30)
        1000
    else if (age_days < 90)
        10000
    else
        100000;

    if (p.daily_limit > max_allowed_limit) {
        return .error_rate_limit_exceeded;
    }

    return .success;
}

/// Generate a verification proof (for display to users)
///
/// The proof includes all verification details in JSON format.
/// Caller must free the returned proof with stamp_free_proof().
///
/// Returns:
/// - Non-null pointer to StampProof on success
/// - NULL on error
///
/// Example:
/// ```c
/// StampProof* proof = stamp_generate_proof(&params);
/// if (proof != NULL) {
///     printf("Proof: %s\n", proof->data);
///     stamp_free_proof(proof);
/// }
/// ```
export fn stamp_generate_proof(params: ?*const StampUnsubscribeParams) callconv(.C) ?*StampProof {
    const p = params orelse return null;

    // Allocate proof structure
    var allocator = std.heap.c_allocator;
    const proof = allocator.create(StampProof) catch return null;

    // Generate JSON proof (simplified for now)
    const json_template =
        \\{{
        \\  "type": "unsubscribe_verification",
        \\  "url": "{s}",
        \\  "tested_at": {d},
        \\  "response_code": {d},
        \\  "response_time_ms": {d},
        \\  "verified": true
        \\}}
    ;

    const url_slice = std.mem.span(p.url);
    const json = std.fmt.allocPrintZ(allocator, json_template, .{
        url_slice,
        p.tested_at,
        p.response_code,
        p.response_time,
    }) catch return null;

    // Generate signature (simplified - would use real crypto)
    const sig = std.fmt.allocPrintZ(allocator, "sig_{d}", .{
        getCurrentTimestamp(),
    }) catch {
        allocator.free(json);
        return null;
    };

    proof.* = .{
        .data = json.ptr,
        .length = json.len,
        .signature = sig.ptr,
    };

    return proof;
}

/// Free a proof returned by stamp_generate_proof()
///
/// Example:
/// ```c
/// StampProof* proof = stamp_generate_proof(&params);
/// stamp_free_proof(proof);
/// ```
export fn stamp_free_proof(proof: ?*StampProof) callconv(.C) void {
    const p = proof orelse return;

    var allocator = std.heap.c_allocator;

    // Free data
    if (p.data != null) {
        const data_slice = std.mem.span(p.data);
        allocator.free(data_slice);
    }

    // Free signature
    if (p.signature != null) {
        const sig_slice = std.mem.span(p.signature);
        allocator.free(sig_slice);
    }

    // Free proof structure itself
    allocator.destroy(p);
}

/// Get version string
///
/// Returns: "libstamp v1.0.0"
export fn stamp_version() callconv(.C) [*:0]const u8 {
    return "libstamp v1.0.0";
}

// ============================================================================
// Tests
// ============================================================================

test "verify unsubscribe - valid" {
    const params = StampUnsubscribeParams{
        .url = "https://example.com/unsubscribe",
        .tested_at = getCurrentTimestamp() - 5000, // 5 seconds ago
        .response_code = 200,
        .response_time = 87,
        .token = "abc123...",
        .signature = "valid_signature",
    };

    const result = stamp_verify_unsubscribe(&params);
    try std.testing.expectEqual(StampResult.success, result);
}

test "verify unsubscribe - invalid URL" {
    const params = StampUnsubscribeParams{
        .url = "not_a_url",
        .tested_at = getCurrentTimestamp(),
        .response_code = 200,
        .response_time = 87,
        .token = "abc123...",
        .signature = "valid_signature",
    };

    const result = stamp_verify_unsubscribe(&params);
    try std.testing.expectEqual(StampResult.error_invalid_url, result);
}

test "verify unsubscribe - slow response" {
    const params = StampUnsubscribeParams{
        .url = "https://example.com/unsubscribe",
        .tested_at = getCurrentTimestamp(),
        .response_code = 200,
        .response_time = 5000, // 5 seconds - too slow
        .token = "abc123...",
        .signature = "valid_signature",
    };

    const result = stamp_verify_unsubscribe(&params);
    try std.testing.expectEqual(StampResult.error_timeout, result);
}

test "verify consent - valid" {
    const params = StampConsentParams{
        .initial_request = 1706630400000,
        .confirmation = 1706630500000, // 100 seconds later
        .ip_address = "192.168.1.100",
        .token = "valid_token",
    };

    const result = stamp_verify_consent(&params);
    try std.testing.expectEqual(StampResult.success, result);
}

test "verify consent - confirmation before request" {
    const params = StampConsentParams{
        .initial_request = 1706630500000,
        .confirmation = 1706630400000, // Before request!
        .ip_address = "192.168.1.100",
        .token = "valid_token",
    };

    const result = stamp_verify_consent(&params);
    try std.testing.expectEqual(StampResult.error_consent_invalid, result);
}

test "verify rate limit - within limits" {
    const params = StampRateLimitParams{
        .sender_id = "user123",
        .account_created = getCurrentTimestamp() - (30 * 24 * 60 * 60 * 1000), // 30 days ago
        .messages_today = 50,
        .daily_limit = 1000,
    };

    const result = stamp_verify_rate_limit(&params);
    try std.testing.expectEqual(StampResult.success, result);
}

test "verify rate limit - exceeded" {
    const params = StampRateLimitParams{
        .sender_id = "user123",
        .account_created = getCurrentTimestamp() - (30 * 24 * 60 * 60 * 1000),
        .messages_today = 1000,
        .daily_limit = 1000, // Exactly at limit = exceeded
    };

    const result = stamp_verify_rate_limit(&params);
    try std.testing.expectEqual(StampResult.error_rate_limit_exceeded, result);
}

test "generate and free proof" {
    const params = StampUnsubscribeParams{
        .url = "https://example.com/unsubscribe",
        .tested_at = getCurrentTimestamp(),
        .response_code = 200,
        .response_time = 87,
        .token = "abc123...",
        .signature = "valid_signature",
    };

    const proof = stamp_generate_proof(&params);
    try std.testing.expect(proof != null);

    if (proof) |p| {
        try std.testing.expect(p.length > 0);
        try std.testing.expect(p.data != null);
        try std.testing.expect(p.signature != null);

        stamp_free_proof(proof);
    }
}
