// LOL i18n Service — Locale Operations
//
// Implements BCP 47 locale parsing, validation, normalisation, and
// fallback chain construction. Matches the Idris2 ABI definitions
// in src/abi/Locale.idr.
//
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

const std = @import("std");

/// Parsed BCP 47 locale components.
/// Matches the Locale extern struct in main.zig and the Idris2 Locale record.
pub const LocaleComponents = struct {
    language: []const u8,
    script: []const u8,
    region: []const u8,
};

/// Parse a BCP 47 locale tag into its component parts.
///
/// Handles the common patterns:
///   "en"         -> language="en", script="", region=""
///   "en-US"      -> language="en", script="", region="US"
///   "zh-Hans"    -> language="zh", script="Hans", region=""
///   "zh-Hans-CN" -> language="zh", script="Hans", region="CN"
///   "sr-Latn-RS" -> language="sr", script="Latn", region="RS"
pub fn parse(tag: []const u8) LocaleComponents {
    var result = LocaleComponents{
        .language = tag,
        .script = "",
        .region = "",
    };

    var it = std.mem.splitScalar(u8, tag, '-');
    if (it.next()) |lang| {
        result.language = lang;
        if (it.next()) |part2| {
            if (part2.len == 4 and isAllAlpha(part2)) {
                // ISO 15924 script code (4 letters, e.g. "Hans", "Latn")
                result.script = part2;
                if (it.next()) |part3| {
                    result.region = part3;
                }
            } else {
                // ISO 3166-1 region code (2-3 chars)
                result.region = part2;
            }
        }
    }

    return result;
}

/// Validate that a locale tag has a structurally valid language subtag.
/// Returns true if the language part is 2-3 ASCII alpha characters.
pub fn isValid(tag: []const u8) bool {
    if (tag.len == 0) return false;

    var it = std.mem.splitScalar(u8, tag, '-');
    const lang = it.next() orelse return false;

    return (lang.len == 2 or lang.len == 3) and isAllAlpha(lang);
}

/// Normalise a locale tag to BCP 47 canonical form:
///   language = lowercase, script = titlecase, region = uppercase.
/// Writes the result into the provided buffer and returns the written slice.
///
/// Example: "EN-latn-us" -> "en-Latn-US"
pub fn normalise(tag: []const u8, buf: []u8) []const u8 {
    const components = parse(tag);
    var pos: usize = 0;

    // Language: lowercase
    for (components.language) |c| {
        if (pos >= buf.len) break;
        buf[pos] = std.ascii.toLower(c);
        pos += 1;
    }

    // Script: titlecase (first upper, rest lower)
    if (components.script.len > 0) {
        if (pos < buf.len) {
            buf[pos] = '-';
            pos += 1;
        }
        for (components.script, 0..) |c, i| {
            if (pos >= buf.len) break;
            buf[pos] = if (i == 0) std.ascii.toUpper(c) else std.ascii.toLower(c);
            pos += 1;
        }
    }

    // Region: uppercase
    if (components.region.len > 0) {
        if (pos < buf.len) {
            buf[pos] = '-';
            pos += 1;
        }
        for (components.region) |c| {
            if (pos >= buf.len) break;
            buf[pos] = std.ascii.toUpper(c);
            pos += 1;
        }
    }

    return buf[0..pos];
}

/// Compute the fallback chain length for a locale tag.
/// For "en-GB" returns 3: en-GB, en, default.
/// For "zh-Hans-CN" returns 4: zh-Hans-CN, zh-Hans, zh, default.
pub fn fallbackChainLength(tag: []const u8) u32 {
    var count: u32 = 1; // The tag itself
    for (tag) |c| {
        if (c == '-') count += 1;
    }
    return count + 1; // +1 for "default" at the end
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Check if all characters in a slice are ASCII alphabetic.
fn isAllAlpha(s: []const u8) bool {
    for (s) |c| {
        if (!std.ascii.isAlphabetic(c)) return false;
    }
    return true;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "parse simple locale" {
    const r = parse("en");
    try std.testing.expectEqualStrings("en", r.language);
    try std.testing.expectEqualStrings("", r.script);
    try std.testing.expectEqualStrings("", r.region);
}

test "parse locale with region" {
    const r = parse("en-US");
    try std.testing.expectEqualStrings("en", r.language);
    try std.testing.expectEqualStrings("", r.script);
    try std.testing.expectEqualStrings("US", r.region);
}

test "parse locale with script and region" {
    const r = parse("zh-Hans-CN");
    try std.testing.expectEqualStrings("zh", r.language);
    try std.testing.expectEqualStrings("Hans", r.script);
    try std.testing.expectEqualStrings("CN", r.region);
}

test "parse locale with script only" {
    const r = parse("sr-Latn");
    try std.testing.expectEqualStrings("sr", r.language);
    try std.testing.expectEqualStrings("Latn", r.script);
    try std.testing.expectEqualStrings("", r.region);
}

test "validate good tags" {
    try std.testing.expect(isValid("en"));
    try std.testing.expect(isValid("en-US"));
    try std.testing.expect(isValid("zh-Hans-CN"));
    try std.testing.expect(isValid("eng"));
}

test "validate bad tags" {
    try std.testing.expect(!isValid(""));
    try std.testing.expect(!isValid("1234"));
    try std.testing.expect(!isValid("toolongname"));
}

test "normalise mixed case" {
    var buf: [64]u8 = undefined;
    const result = normalise("EN-latn-us", &buf);
    try std.testing.expectEqualStrings("en-Latn-US", result);
}

test "normalise simple" {
    var buf: [64]u8 = undefined;
    const result = normalise("EN", &buf);
    try std.testing.expectEqualStrings("en", result);
}

test "fallback chain length" {
    try std.testing.expectEqual(@as(u32, 2), fallbackChainLength("en"));
    try std.testing.expectEqual(@as(u32, 3), fallbackChainLength("en-US"));
    try std.testing.expectEqual(@as(u32, 4), fallbackChainLength("zh-Hans-CN"));
}
