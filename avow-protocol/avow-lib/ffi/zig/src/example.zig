// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

//! Example program demonstrating libstamp usage

const std = @import("std");
const stamp = @import("main.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    try stdout.print("=== STAMP Verification Library Example ===\n\n", .{});
    try stdout.print("Version: {s}\n\n", .{stamp.stamp_version()});

    // Example 1: Verify unsubscribe link
    try stdout.print("Example 1: Unsubscribe Verification\n", .{});
    try stdout.print("====================================\n", .{});

    const unsub_params = stamp.StampUnsubscribeParams{
        .url = "https://example.com/unsubscribe",
        .tested_at = getCurrentTimestamp() - 5000, // 5 seconds ago
        .response_code = 200,
        .response_time = 87,
        .token = "abc123def456...", // Would be real token
        .signature = "valid_signature_here", // Would be real signature
    };

    const unsub_result = stamp.stamp_verify_unsubscribe(&unsub_params);
    try stdout.print("URL: {s}\n", .{unsub_params.url});
    try stdout.print("Response: {d} (in {d}ms)\n", .{
        unsub_params.response_code,
        unsub_params.response_time,
    });
    try stdout.print("Result: {s}\n\n", .{resultToString(unsub_result)});

    // Generate proof
    if (unsub_result == .success) {
        const proof = stamp.stamp_generate_proof(&unsub_params);
        if (proof) |p| {
            try stdout.print("Proof generated:\n{s}\n\n", .{p.data});
            stamp.stamp_free_proof(proof);
        }
    }

    // Example 2: Verify consent chain
    try stdout.print("Example 2: Consent Chain Verification\n", .{});
    try stdout.print("======================================\n", .{});

    const consent_params = stamp.StampConsentParams{
        .initial_request = 1706630400000,
        .confirmation = 1706630500000, // 100 seconds later
        .ip_address = "192.168.1.100",
        .token = "consent_token_123",
    };

    const consent_result = stamp.stamp_verify_consent(&consent_params);
    try stdout.print("Initial request: {d}\n", .{consent_params.initial_request});
    try stdout.print("Confirmation: {d}\n", .{consent_params.confirmation});
    try stdout.print("Time diff: {d} seconds\n", .{
        (consent_params.confirmation - consent_params.initial_request) / 1000,
    });
    try stdout.print("Result: {s}\n\n", .{resultToString(consent_result)});

    // Example 3: Verify rate limits
    try stdout.print("Example 3: Rate Limit Verification\n", .{});
    try stdout.print("===================================\n", .{});

    const now = getCurrentTimestamp();
    const rate_params = stamp.StampRateLimitParams{
        .sender_id = "user_12345",
        .account_created = now - (45 * 24 * 60 * 60 * 1000), // 45 days ago
        .messages_today = 150,
        .daily_limit = 10000,
    };

    const rate_result = stamp.stamp_verify_rate_limit(&rate_params);
    const account_age_days = (now - rate_params.account_created) / (24 * 60 * 60 * 1000);
    try stdout.print("Sender: {s}\n", .{rate_params.sender_id});
    try stdout.print("Account age: {d} days\n", .{account_age_days});
    try stdout.print("Messages today: {d}/{d}\n", .{
        rate_params.messages_today,
        rate_params.daily_limit,
    });
    try stdout.print("Result: {s}\n\n", .{resultToString(rate_result)});

    // Example 4: Error cases
    try stdout.print("Example 4: Error Cases\n", .{});
    try stdout.print("======================\n", .{});

    // Invalid URL
    const bad_url_params = stamp.StampUnsubscribeParams{
        .url = "not_a_valid_url",
        .tested_at = now,
        .response_code = 200,
        .response_time = 87,
        .token = "token",
        .signature = "sig",
    };
    const bad_url_result = stamp.stamp_verify_unsubscribe(&bad_url_params);
    try stdout.print("Invalid URL: {s}\n", .{resultToString(bad_url_result)});

    // Slow response
    const slow_params = stamp.StampUnsubscribeParams{
        .url = "https://example.com/unsub",
        .tested_at = now,
        .response_code = 200,
        .response_time = 5000, // 5 seconds!
        .token = "token",
        .signature = "sig",
    };
    const slow_result = stamp.stamp_verify_unsubscribe(&slow_params);
    try stdout.print("Slow response: {s}\n", .{resultToString(slow_result)});

    // Rate limit exceeded
    const exceeded_params = stamp.StampRateLimitParams{
        .sender_id = "spammer",
        .account_created = now - (10 * 24 * 60 * 60 * 1000), // 10 days old
        .messages_today = 1000, // At limit
        .daily_limit = 1000,
    };
    const exceeded_result = stamp.stamp_verify_rate_limit(&exceeded_params);
    try stdout.print("Rate limit exceeded: {s}\n", .{resultToString(exceeded_result)});

    try stdout.print("\nAll examples completed!\n", .{});
}

fn getCurrentTimestamp() u64 {
    const ns = std.time.nanoTimestamp();
    return @intCast(@divFloor(ns, std.time.ns_per_ms));
}

fn resultToString(result: stamp.StampResult) []const u8 {
    return switch (result) {
        .success => "✓ SUCCESS",
        .error_invalid_url => "✗ INVALID_URL",
        .error_timeout => "✗ TIMEOUT",
        .error_invalid_response => "✗ INVALID_RESPONSE",
        .error_invalid_signature => "✗ INVALID_SIGNATURE",
        .error_rate_limit_exceeded => "✗ RATE_LIMIT_EXCEEDED",
        .error_consent_invalid => "✗ CONSENT_INVALID",
        .error_null_pointer => "✗ NULL_POINTER",
        .error_internal => "✗ INTERNAL_ERROR",
    };
}
