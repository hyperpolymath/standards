// LOL i18n Service — Integration Tests
// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// These tests verify that the Zig FFI correctly implements the Idris2 ABI
// for the LOL i18n service. They exercise the full public C API surface.

const std = @import("std");
const testing = std.testing;

// Import FFI functions via the C ABI (linked against liblol)
extern fn lol_init(?[*:0]const u8) ?*anyopaque;
extern fn lol_free(?*anyopaque) void;
extern fn lol_resolve_locale(?*anyopaque, ?[*:0]const u8) ?*anyopaque;
extern fn lol_free_locale(?*anyopaque) void;
extern fn lol_translate(?*anyopaque, ?[*:0]const u8, ?[*:0]const u8) ?*anyopaque;
extern fn lol_free_translation(?*anyopaque) void;
extern fn lol_translation_text(?*anyopaque) ?[*:0]const u8;
extern fn lol_select_plural(?*anyopaque, ?[*:0]const u8, u64) u32;
extern fn lol_translate_plural(?*anyopaque, ?[*:0]const u8, ?[*:0]const u8, u64) ?*anyopaque;
extern fn lol_language_count(?*anyopaque) u32;
extern fn lol_get_language(?*anyopaque, ?[*:0]const u8) ?*anyopaque;
extern fn lol_free_language(?*anyopaque) void;
extern fn lol_get_plural_rule(?*anyopaque, ?[*:0]const u8) ?*anyopaque;
extern fn lol_free_plural_rule(?*anyopaque) void;
extern fn lol_fallback_chain_len(?*anyopaque, ?[*:0]const u8) u32;
extern fn lol_last_error() ?[*:0]const u8;
extern fn lol_version() [*:0]const u8;
extern fn lol_build_info() [*:0]const u8;
extern fn lol_is_initialized(?*anyopaque) u32;

// ===========================================================================
// Lifecycle Tests
// ===========================================================================

test "create and destroy handle" {
    const handle = lol_init(null) orelse return error.InitFailed;
    defer lol_free(handle);

    try testing.expect(handle != @as(?*anyopaque, null));
}

test "handle is initialized after init" {
    const handle = lol_init(null) orelse return error.InitFailed;
    defer lol_free(handle);

    try testing.expectEqual(@as(u32, 1), lol_is_initialized(handle));
}

test "null handle is not initialized" {
    try testing.expectEqual(@as(u32, 0), lol_is_initialized(null));
}

test "init with explicit data dir" {
    const handle = lol_init("/tmp/lol-test-corpus") orelse return error.InitFailed;
    defer lol_free(handle);

    try testing.expectEqual(@as(u32, 1), lol_is_initialized(handle));
}

// ===========================================================================
// Locale Resolution Tests
// ===========================================================================

test "resolve simple locale" {
    const handle = lol_init(null) orelse return error.InitFailed;
    defer lol_free(handle);

    const locale = lol_resolve_locale(handle, "en");
    if (locale) |ptr| {
        defer lol_free_locale(ptr);
    }
}

test "resolve locale with region" {
    const handle = lol_init(null) orelse return error.InitFailed;
    defer lol_free(handle);

    const locale = lol_resolve_locale(handle, "en-US");
    if (locale) |ptr| {
        defer lol_free_locale(ptr);
    }
}

test "resolve locale with script and region" {
    const handle = lol_init(null) orelse return error.InitFailed;
    defer lol_free(handle);

    const locale = lol_resolve_locale(handle, "zh-Hans-CN");
    if (locale) |ptr| {
        defer lol_free_locale(ptr);
    }
}

test "resolve locale with null handle returns null" {
    try testing.expect(lol_resolve_locale(null, "en") == null);
}

test "resolve locale with null tag returns null" {
    const handle = lol_init(null) orelse return error.InitFailed;
    defer lol_free(handle);

    try testing.expect(lol_resolve_locale(handle, null) == null);
}

// ===========================================================================
// Translation Tests
// ===========================================================================

test "translate with null handle returns null" {
    try testing.expect(lol_translate(null, "en", "greeting") == null);
}

test "translate with null locale returns null" {
    const handle = lol_init(null) orelse return error.InitFailed;
    defer lol_free(handle);

    try testing.expect(lol_translate(handle, null, "greeting") == null);
}

test "translate with null key returns null" {
    const handle = lol_init(null) orelse return error.InitFailed;
    defer lol_free(handle);

    try testing.expect(lol_translate(handle, "en", null) == null);
}

// ===========================================================================
// Plural Selection Tests
// ===========================================================================

test "English plural: 1 is one, everything else is other" {
    try testing.expectEqual(@as(u32, 1), lol_select_plural(null, "en", 1)); // one
    try testing.expectEqual(@as(u32, 5), lol_select_plural(null, "en", 0)); // other
    try testing.expectEqual(@as(u32, 5), lol_select_plural(null, "en", 2)); // other
    try testing.expectEqual(@as(u32, 5), lol_select_plural(null, "en", 100)); // other
}

test "Arabic plural: 6 forms" {
    try testing.expectEqual(@as(u32, 0), lol_select_plural(null, "ar", 0)); // zero
    try testing.expectEqual(@as(u32, 1), lol_select_plural(null, "ar", 1)); // one
    try testing.expectEqual(@as(u32, 2), lol_select_plural(null, "ar", 2)); // two
    try testing.expectEqual(@as(u32, 3), lol_select_plural(null, "ar", 7)); // few
    try testing.expectEqual(@as(u32, 4), lol_select_plural(null, "ar", 50)); // many
    try testing.expectEqual(@as(u32, 5), lol_select_plural(null, "ar", 100)); // other
}

test "Japanese plural: always other" {
    try testing.expectEqual(@as(u32, 5), lol_select_plural(null, "ja", 0));
    try testing.expectEqual(@as(u32, 5), lol_select_plural(null, "ja", 1));
    try testing.expectEqual(@as(u32, 5), lol_select_plural(null, "ja", 42));
}

test "Russian plural: one, few, many" {
    try testing.expectEqual(@as(u32, 1), lol_select_plural(null, "ru", 1)); // one
    try testing.expectEqual(@as(u32, 3), lol_select_plural(null, "ru", 2)); // few
    try testing.expectEqual(@as(u32, 4), lol_select_plural(null, "ru", 5)); // many
    try testing.expectEqual(@as(u32, 4), lol_select_plural(null, "ru", 11)); // many (not one)
    try testing.expectEqual(@as(u32, 1), lol_select_plural(null, "ru", 21)); // one
}

// ===========================================================================
// Plural Rule Tests
// ===========================================================================

test "get plural rule for English" {
    const rule = lol_get_plural_rule(null, "en") orelse return error.RuleFailed;
    defer lol_free_plural_rule(rule);
}

test "get plural rule for Arabic" {
    const rule = lol_get_plural_rule(null, "ar") orelse return error.RuleFailed;
    defer lol_free_plural_rule(rule);
}

test "get plural rule with null code returns null" {
    try testing.expect(lol_get_plural_rule(null, null) == null);
}

// ===========================================================================
// Fallback Chain Tests
// ===========================================================================

test "fallback chain length for simple locale" {
    try testing.expectEqual(@as(u32, 2), lol_fallback_chain_len(null, "en")); // en, default
}

test "fallback chain length for locale with region" {
    try testing.expectEqual(@as(u32, 3), lol_fallback_chain_len(null, "en-US")); // en-US, en, default
}

test "fallback chain length for complex locale" {
    try testing.expectEqual(@as(u32, 4), lol_fallback_chain_len(null, "zh-Hans-CN")); // zh-Hans-CN, zh-Hans, zh, default
}

// ===========================================================================
// Version Tests
// ===========================================================================

test "version string is not empty" {
    const ver = lol_version();
    const ver_str = std.mem.span(ver);
    try testing.expect(ver_str.len > 0);
}

test "version is semantic format" {
    const ver = lol_version();
    const ver_str = std.mem.span(ver);
    try testing.expect(std.mem.count(u8, ver_str, ".") >= 1);
}

test "build info is not empty" {
    const info = lol_build_info();
    const info_str = std.mem.span(info);
    try testing.expect(info_str.len > 0);
}

// ===========================================================================
// Error Handling Tests
// ===========================================================================

test "last error after null handle translation" {
    _ = lol_translate(null, "en", "key");
    const err = lol_last_error();
    try testing.expect(err != null);
    if (err) |e| {
        const err_str = std.mem.span(e);
        try testing.expect(err_str.len > 0);
    }
}

// ===========================================================================
// Memory Safety Tests
// ===========================================================================

test "multiple handles are independent" {
    const h1 = lol_init(null) orelse return error.InitFailed;
    defer lol_free(h1);

    const h2 = lol_init(null) orelse return error.InitFailed;
    defer lol_free(h2);

    try testing.expect(h1 != h2);
}

test "free null is safe for all types" {
    lol_free(null);
    lol_free_locale(null);
    lol_free_translation(null);
    lol_free_language(null);
    lol_free_plural_rule(null);
}
