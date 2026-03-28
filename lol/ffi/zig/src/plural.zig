// LOL i18n Service — Plural Form Selection
//
// Implements CLDR plural rules for common language families.
// Matches the Idris2 ABI definitions in src/abi/PluralForm.idr.
//
// The CLDR specification defines six plural categories:
//   zero, one, two, few, many, other
// Different languages use different subsets of these categories.
//
// @see https://www.unicode.org/cldr/charts/latest/supplemental/language_plural_rules.html
//
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

const std = @import("std");

/// CLDR plural categories (must match src/abi/Types.idr PluralCategory).
/// Integer values match the Idris2 pluralToInt mapping.
pub const Category = enum(u32) {
    zero = 0,
    one = 1,
    two = 2,
    few = 3,
    many = 4,
    other = 5,
};

/// Plural rule info: how many forms and which categories a language uses.
pub const RuleInfo = struct {
    form_count: u32,
    /// Categories used, packed as u32 values matching Category enum.
    /// Unused slots are 0.
    categories: [6]u32,
};

/// Select the CLDR plural category for a (language, quantity) pair.
///
/// Language codes are matched as both ISO 639-1 (2-letter) and
/// ISO 639-3 (3-letter) forms. Unknown languages default to the
/// English rule (one/other), which is the most common pattern worldwide.
pub fn selectCategory(lang: []const u8, quantity: u64) Category {
    // East Asian languages: always "other" (no plural distinction)
    if (isEastAsian(lang)) return .other;

    // Arabic: all six forms
    if (isArabic(lang)) return selectArabic(quantity);

    // Polish: one/few/many/other
    if (isPolish(lang)) return selectPolish(quantity);

    // Russian/Ukrainian: one/few/many/other
    if (isRussianLike(lang)) return selectRussian(quantity);

    // Czech/Slovak: one/few/other
    if (isCzechLike(lang)) return selectCzech(quantity);

    // French/Portuguese: one includes 0
    if (isFrenchLike(lang)) return if (quantity == 0 or quantity == 1) .one else .other;

    // Default (English, German, Dutch, Swedish, etc.): one vs other
    return if (quantity == 1) .one else .other;
}

/// Get the plural rule info for a language.
/// Returns the number of forms and which categories the language uses.
pub fn getRuleInfo(lang: []const u8) RuleInfo {
    if (isEastAsian(lang)) {
        return .{ .form_count = 1, .categories = .{ 5, 0, 0, 0, 0, 0 } };
    }
    if (isArabic(lang)) {
        return .{ .form_count = 6, .categories = .{ 0, 1, 2, 3, 4, 5 } };
    }
    if (isPolish(lang) or isRussianLike(lang)) {
        return .{ .form_count = 4, .categories = .{ 1, 3, 4, 5, 0, 0 } };
    }
    if (isCzechLike(lang)) {
        return .{ .form_count = 3, .categories = .{ 1, 3, 5, 0, 0, 0 } };
    }
    if (isFrenchLike(lang)) {
        return .{ .form_count = 2, .categories = .{ 1, 5, 0, 0, 0, 0 } };
    }
    // Default: one/other
    return .{ .form_count = 2, .categories = .{ 1, 5, 0, 0, 0, 0 } };
}

/// Get the string suffix for a plural category.
/// Used to build pluralised translation keys (e.g. "items.one").
pub fn suffix(cat: Category) []const u8 {
    return switch (cat) {
        .zero => "zero",
        .one => "one",
        .two => "two",
        .few => "few",
        .many => "many",
        .other => "other",
    };
}

// ---------------------------------------------------------------------------
// Language family checks
// ---------------------------------------------------------------------------

fn isEastAsian(lang: []const u8) bool {
    return eqlAny(lang, &.{ "zh", "ja", "ko", "vi", "zho", "jpn", "kor", "vie" });
}

fn isArabic(lang: []const u8) bool {
    return eqlAny(lang, &.{ "ar", "ara" });
}

fn isPolish(lang: []const u8) bool {
    return eqlAny(lang, &.{ "pl", "pol" });
}

fn isRussianLike(lang: []const u8) bool {
    return eqlAny(lang, &.{ "ru", "uk", "rus", "ukr" });
}

fn isCzechLike(lang: []const u8) bool {
    return eqlAny(lang, &.{ "cs", "sk", "ces", "slk" });
}

fn isFrenchLike(lang: []const u8) bool {
    return eqlAny(lang, &.{ "fr", "pt", "fra", "por" });
}

fn eqlAny(s: []const u8, options: []const []const u8) bool {
    for (options) |opt| {
        if (std.mem.eql(u8, s, opt)) return true;
    }
    return false;
}

// ---------------------------------------------------------------------------
// Language-specific selection rules
// ---------------------------------------------------------------------------

/// Arabic: zero(0), one(1), two(2), few(3-10), many(11-99), other(100+)
fn selectArabic(quantity: u64) Category {
    return switch (quantity) {
        0 => .zero,
        1 => .one,
        2 => .two,
        3...10 => .few,
        11...99 => .many,
        else => .other,
    };
}

/// Polish: one(1), few(mod10 2-4 excl. mod100 12-14), many(rest), other
fn selectPolish(quantity: u64) Category {
    if (quantity == 1) return .one;
    const mod10 = quantity % 10;
    const mod100 = quantity % 100;
    if (mod10 >= 2 and mod10 <= 4 and (mod100 < 12 or mod100 > 14)) return .few;
    if ((mod10 == 0 or mod10 == 1) or (mod10 >= 5 and mod10 <= 9) or (mod100 >= 12 and mod100 <= 14)) return .many;
    return .other;
}

/// Russian/Ukrainian: one(mod10=1, mod100!=11), few(mod10 2-4, excl. 12-14), many(rest)
fn selectRussian(quantity: u64) Category {
    const mod10 = quantity % 10;
    const mod100 = quantity % 100;
    if (mod10 == 1 and mod100 != 11) return .one;
    if (mod10 >= 2 and mod10 <= 4 and (mod100 < 12 or mod100 > 14)) return .few;
    return .many;
}

/// Czech/Slovak: one(1), few(2-4), other
fn selectCzech(quantity: u64) Category {
    return switch (quantity) {
        1 => .one,
        2...4 => .few,
        else => .other,
    };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "English: one vs other" {
    try std.testing.expectEqual(Category.one, selectCategory("en", 1));
    try std.testing.expectEqual(Category.other, selectCategory("en", 0));
    try std.testing.expectEqual(Category.other, selectCategory("en", 2));
    try std.testing.expectEqual(Category.other, selectCategory("en", 100));
}

test "English ISO 639-3" {
    try std.testing.expectEqual(Category.one, selectCategory("eng", 1));
    try std.testing.expectEqual(Category.other, selectCategory("eng", 5));
}

test "Arabic: all six forms" {
    try std.testing.expectEqual(Category.zero, selectCategory("ar", 0));
    try std.testing.expectEqual(Category.one, selectCategory("ar", 1));
    try std.testing.expectEqual(Category.two, selectCategory("ar", 2));
    try std.testing.expectEqual(Category.few, selectCategory("ar", 7));
    try std.testing.expectEqual(Category.many, selectCategory("ar", 50));
    try std.testing.expectEqual(Category.other, selectCategory("ar", 100));
}

test "Japanese: always other" {
    try std.testing.expectEqual(Category.other, selectCategory("ja", 0));
    try std.testing.expectEqual(Category.other, selectCategory("ja", 1));
    try std.testing.expectEqual(Category.other, selectCategory("ja", 42));
}

test "Russian: one/few/many" {
    try std.testing.expectEqual(Category.one, selectCategory("ru", 1));
    try std.testing.expectEqual(Category.few, selectCategory("ru", 2));
    try std.testing.expectEqual(Category.many, selectCategory("ru", 5));
    try std.testing.expectEqual(Category.many, selectCategory("ru", 11));
    try std.testing.expectEqual(Category.one, selectCategory("ru", 21));
}

test "Polish: one/few/many/other" {
    try std.testing.expectEqual(Category.one, selectCategory("pl", 1));
    try std.testing.expectEqual(Category.few, selectCategory("pl", 2));
    try std.testing.expectEqual(Category.many, selectCategory("pl", 5));
}

test "French: 0 and 1 are one" {
    try std.testing.expectEqual(Category.one, selectCategory("fr", 0));
    try std.testing.expectEqual(Category.one, selectCategory("fr", 1));
    try std.testing.expectEqual(Category.other, selectCategory("fr", 2));
}

test "Czech: one/few/other" {
    try std.testing.expectEqual(Category.one, selectCategory("cs", 1));
    try std.testing.expectEqual(Category.few, selectCategory("cs", 3));
    try std.testing.expectEqual(Category.other, selectCategory("cs", 5));
}

test "rule info English" {
    const info = getRuleInfo("en");
    try std.testing.expectEqual(@as(u32, 2), info.form_count);
}

test "rule info Arabic" {
    const info = getRuleInfo("ar");
    try std.testing.expectEqual(@as(u32, 6), info.form_count);
}

test "suffix mapping" {
    try std.testing.expectEqualStrings("one", suffix(.one));
    try std.testing.expectEqualStrings("other", suffix(.other));
    try std.testing.expectEqualStrings("zero", suffix(.zero));
}
