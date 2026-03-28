// LOL i18n Service — Zig FFI Implementation
//
// Implements the C-compatible FFI declared in src/abi/Foreign.idr.
// All types and layouts must match the Idris2 ABI definitions.
//
// LOL is called as a service: consumers call lol_init() with a corpus
// data directory, perform translation lookups, then call lol_free().
//
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

const std = @import("std");

// ---------------------------------------------------------------------------
// Version
// ---------------------------------------------------------------------------

const VERSION = "0.1.0";
const BUILD_INFO = "LOL i18n Service built with Zig " ++ @import("builtin").zig_version_string;

// ---------------------------------------------------------------------------
// Result codes (must match src/abi/Types.idr Result type)
// ---------------------------------------------------------------------------

pub const Result = enum(c_int) {
    ok = 0,
    err = 1,
    invalid_param = 2,
    out_of_memory = 3,
    null_pointer = 4,
    locale_not_found = 5,
    key_not_found = 6,
    plural_out_of_range = 7,
};

// ---------------------------------------------------------------------------
// CLDR Plural Categories (must match src/abi/Types.idr PluralCategory)
// ---------------------------------------------------------------------------

pub const PluralCategory = enum(u32) {
    zero = 0,
    one = 1,
    two = 2,
    few = 3,
    many = 4,
    other = 5,
};

// ---------------------------------------------------------------------------
// C-compatible structs (must match src/abi/Layout.idr)
// ---------------------------------------------------------------------------

/// Locale struct — 32 bytes on 64-bit, 16 bytes on 32-bit.
/// All string pointers are owned by the service and freed by lol_free_locale.
pub const Locale = extern struct {
    tag: ?[*:0]const u8,
    language: ?[*:0]const u8,
    script: ?[*:0]const u8,
    region: ?[*:0]const u8,
};

/// Translation result struct — 24 bytes.
pub const TranslationResult = extern struct {
    text: ?[*:0]const u8,
    resolved_locale: ?[*:0]const u8,
    is_fallback: u32,
    _padding: u32 = 0,
};

/// Language info struct — 56 bytes.
pub const LanguageInfo = extern struct {
    iso639_3: ?[*:0]const u8,
    name: ?[*:0]const u8,
    native_name: ?[*:0]const u8,
    family: ?[*:0]const u8,
    scripts: ?[*:0]const u8,
    source_count: u32,
    verse_count: u32,
    quality: f64,
};

/// Plural rule struct — 40 bytes.
pub const PluralRule = extern struct {
    language: ?[*:0]const u8,
    form_count: u32,
    categories: [6]u32,
    _padding: u32 = 0,
};

// ---------------------------------------------------------------------------
// Thread-local error storage
// ---------------------------------------------------------------------------

threadlocal var last_error: ?[]const u8 = null;

fn setError(msg: []const u8) void {
    last_error = msg;
}

fn clearError() void {
    last_error = null;
}

// ---------------------------------------------------------------------------
// Internal service state
// ---------------------------------------------------------------------------

/// Internal handle state. Opaque to callers via the C API.
const ServiceState = struct {
    allocator: std.mem.Allocator,
    initialized: bool,
    data_dir: []const u8,
    default_locale: []const u8,
    enable_fallback: bool,

    /// Duplicate a Zig string slice as a null-terminated C string.
    /// Caller must free via freeStr.
    fn dupeStr(self: *ServiceState, s: []const u8) ?[*:0]u8 {
        const result = self.allocator.dupeZ(u8, s) catch return null;
        return result.ptr;
    }

    /// Free a null-terminated C string previously created by dupeStr.
    fn freeStr(self: *ServiceState, s: ?[*:0]const u8) void {
        if (s) |ptr| {
            const slice = std.mem.span(ptr);
            self.allocator.free(slice);
        }
    }
};

// ---------------------------------------------------------------------------
// Library Lifecycle
// ---------------------------------------------------------------------------

/// Initialise the LOL service.
/// `data_dir` is the path to the corpus data directory (null-terminated).
/// Returns a handle (opaque pointer), or null on failure.
export fn lol_init(data_dir: ?[*:0]const u8) ?*anyopaque {
    const allocator = std.heap.c_allocator;
    const dir_str = if (data_dir) |d| std.mem.span(d) else "corpus";

    const state = allocator.create(ServiceState) catch {
        setError("Failed to allocate service state");
        return null;
    };

    const dir_copy = allocator.dupe(u8, dir_str) catch {
        allocator.destroy(state);
        setError("Failed to copy data_dir");
        return null;
    };

    state.* = .{
        .allocator = allocator,
        .initialized = true,
        .data_dir = dir_copy,
        .default_locale = "en",
        .enable_fallback = true,
    };

    clearError();
    return @ptrCast(state);
}

/// Free the LOL service handle and all associated resources.
export fn lol_free(handle: ?*anyopaque) void {
    const state = stateFromHandle(handle) orelse return;
    const allocator = state.allocator;

    allocator.free(state.data_dir);
    state.initialized = false;
    allocator.destroy(state);

    clearError();
}

// ---------------------------------------------------------------------------
// Locale Resolution
// ---------------------------------------------------------------------------

/// Parse and resolve a BCP 47 locale tag against the corpus.
/// Returns a pointer to a Locale struct, or null if resolution fails.
/// Caller must free with lol_free_locale.
export fn lol_resolve_locale(handle: ?*anyopaque, tag: ?[*:0]const u8) ?*anyopaque {
    const state = stateFromHandle(handle) orelse {
        setError("Null handle");
        return null;
    };

    const tag_str = if (tag) |t| std.mem.span(t) else {
        setError("Null locale tag");
        return null;
    };

    if (tag_str.len == 0) {
        setError("Empty locale tag");
        return null;
    }

    const locale = state.allocator.create(Locale) catch {
        setError("Failed to allocate locale");
        return null;
    };

    // Parse the BCP 47 tag into language, script, region components.
    // A full BCP 47 parser would be more complex; this handles the
    // common patterns: "en", "en-US", "zh-Hans", "zh-Hans-CN".
    var language: []const u8 = tag_str;
    var script: []const u8 = "";
    var region: []const u8 = "";

    var it = std.mem.splitScalar(u8, tag_str, '-');
    if (it.next()) |lang| {
        language = lang;
        if (it.next()) |part2| {
            if (part2.len == 4) {
                // ISO 15924 script code (4 letters)
                script = part2;
                if (it.next()) |part3| {
                    region = part3;
                }
            } else {
                // ISO 3166-1 region code (2-3 chars)
                region = part2;
            }
        }
    }

    locale.* = .{
        .tag = state.dupeStr(tag_str),
        .language = state.dupeStr(language),
        .script = state.dupeStr(script),
        .region = state.dupeStr(region),
    };

    clearError();
    return @ptrCast(locale);
}

/// Free a Locale struct returned by lol_resolve_locale.
export fn lol_free_locale(locale_ptr: ?*anyopaque) void {
    if (locale_ptr == null) return;
    const allocator = std.heap.c_allocator;
    const locale: *Locale = @ptrCast(@alignCast(locale_ptr.?));

    // Free string fields.
    freeSpan(allocator, locale.tag);
    freeSpan(allocator, locale.language);
    freeSpan(allocator, locale.script);
    freeSpan(allocator, locale.region);

    allocator.destroy(locale);
}

// ---------------------------------------------------------------------------
// Translation Lookup
// ---------------------------------------------------------------------------

/// Look up a translation key for the given locale.
/// Returns a pointer to a TranslationResult, or null if not found.
/// Caller must free with lol_free_translation.
export fn lol_translate(
    handle: ?*anyopaque,
    locale_tag: ?[*:0]const u8,
    key: ?[*:0]const u8,
) ?*anyopaque {
    const state = stateFromHandle(handle) orelse {
        setError("Null handle");
        return null;
    };

    const tag_str = if (locale_tag) |t| std.mem.span(t) else {
        setError("Null locale tag");
        return null;
    };
    const key_str = if (key) |k| std.mem.span(k) else {
        setError("Null translation key");
        return null;
    };

    // Look up the translation in the corpus data directory.
    // Path: <data_dir>/translations/<lang>/<key>.txt
    // Falls back through the locale chain if enableFallback is set.
    const result = lookupTranslation(state, tag_str, key_str) orelse {
        setError("Translation key not found");
        return null;
    };

    clearError();
    return @ptrCast(result);
}

/// Free a TranslationResult returned by lol_translate or lol_translate_plural.
export fn lol_free_translation(result_ptr: ?*anyopaque) void {
    if (result_ptr == null) return;
    const allocator = std.heap.c_allocator;
    const result: *TranslationResult = @ptrCast(@alignCast(result_ptr.?));

    freeSpan(allocator, result.text);
    freeSpan(allocator, result.resolved_locale);

    allocator.destroy(result);
}

/// Get the text field from a TranslationResult pointer.
/// Returns a null-terminated C string (not owned by caller).
export fn lol_translation_text(result_ptr: ?*anyopaque) ?[*:0]const u8 {
    if (result_ptr == null) return null;
    const result: *TranslationResult = @ptrCast(@alignCast(result_ptr.?));
    return result.text;
}

// ---------------------------------------------------------------------------
// Plural Form Selection
// ---------------------------------------------------------------------------

/// Select the CLDR plural category for a quantity in a given locale.
/// Returns the plural category as a u32 (see PluralCategory enum).
export fn lol_select_plural(
    handle: ?*anyopaque,
    locale_tag: ?[*:0]const u8,
    quantity: u64,
) u32 {
    _ = handle;

    const tag_str = if (locale_tag) |t| std.mem.span(t) else return @intFromEnum(PluralCategory.other);

    // Extract the language subtag from the locale tag.
    var it = std.mem.splitScalar(u8, tag_str, '-');
    const lang = it.next() orelse return @intFromEnum(PluralCategory.other);

    return @intFromEnum(selectPluralForLanguage(lang, quantity));
}

/// Translate with plural form selection combined.
/// Appends the plural category suffix to the key (e.g. "items" -> "items.one").
export fn lol_translate_plural(
    handle: ?*anyopaque,
    locale_tag: ?[*:0]const u8,
    key: ?[*:0]const u8,
    quantity: u64,
) ?*anyopaque {
    const state = stateFromHandle(handle) orelse {
        setError("Null handle");
        return null;
    };

    const tag_str = if (locale_tag) |t| std.mem.span(t) else {
        setError("Null locale tag");
        return null;
    };
    const key_str = if (key) |k| std.mem.span(k) else {
        setError("Null translation key");
        return null;
    };

    // Determine the plural category for this quantity.
    var lang_it = std.mem.splitScalar(u8, tag_str, '-');
    const lang = lang_it.next() orelse tag_str;
    const cat = selectPluralForLanguage(lang, quantity);
    const suffix = pluralSuffix(cat);

    // Build the suffixed key: "key.suffix"
    const suffixed_key = std.fmt.allocPrint(state.allocator, "{s}.{s}", .{ key_str, suffix }) catch {
        setError("Failed to build plural key");
        return null;
    };
    defer state.allocator.free(suffixed_key);

    const result = lookupTranslation(state, tag_str, suffixed_key) orelse {
        // Fall back to unsuffixed key
        const fallback = lookupTranslation(state, tag_str, key_str) orelse {
            setError("Plural translation key not found");
            return null;
        };
        return @ptrCast(fallback);
    };

    clearError();
    return @ptrCast(result);
}

// ---------------------------------------------------------------------------
// Language Metadata
// ---------------------------------------------------------------------------

/// Get the number of languages in the corpus.
export fn lol_language_count(handle: ?*anyopaque) u32 {
    const state = stateFromHandle(handle) orelse return 0;

    // Count JSON files in <data_dir>/metadata/
    const meta_path = std.fmt.allocPrint(state.allocator, "{s}/metadata", .{state.data_dir}) catch return 0;
    defer state.allocator.free(meta_path);

    var dir = std.fs.cwd().openDir(meta_path, .{ .iterate = true }) catch return 0;
    defer dir.close();

    var count: u32 = 0;
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (std.mem.endsWith(u8, entry.name, ".json")) {
            count += 1;
        }
    }
    return count;
}

/// Look up metadata for a language by ISO 639-3 code.
/// Returns a pointer to LanguageInfo, or null if not found.
/// Caller must free with lol_free_language.
export fn lol_get_language(handle: ?*anyopaque, code: ?[*:0]const u8) ?*anyopaque {
    const state = stateFromHandle(handle) orelse {
        setError("Null handle");
        return null;
    };

    const code_str = if (code) |c| std.mem.span(c) else {
        setError("Null language code");
        return null;
    };

    const info = state.allocator.create(LanguageInfo) catch {
        setError("Failed to allocate language info");
        return null;
    };

    // Read metadata from <data_dir>/metadata/<code>.json
    const path = std.fmt.allocPrint(state.allocator, "{s}/metadata/{s}.json", .{ state.data_dir, code_str }) catch {
        state.allocator.destroy(info);
        setError("Failed to build path");
        return null;
    };
    defer state.allocator.free(path);

    const file = std.fs.cwd().openFile(path, .{}) catch {
        state.allocator.destroy(info);
        setError("Language not found");
        return null;
    };
    defer file.close();

    // For now, return a stub with the code filled in.
    // A full implementation would parse the JSON metadata file.
    info.* = .{
        .iso639_3 = state.dupeStr(code_str),
        .name = state.dupeStr(""),
        .native_name = state.dupeStr(""),
        .family = state.dupeStr(""),
        .scripts = state.dupeStr(""),
        .source_count = 0,
        .verse_count = 0,
        .quality = 0.0,
    };

    clearError();
    return @ptrCast(info);
}

/// Free a LanguageInfo struct.
export fn lol_free_language(info_ptr: ?*anyopaque) void {
    if (info_ptr == null) return;
    const allocator = std.heap.c_allocator;
    const info: *LanguageInfo = @ptrCast(@alignCast(info_ptr.?));

    freeSpan(allocator, info.iso639_3);
    freeSpan(allocator, info.name);
    freeSpan(allocator, info.native_name);
    freeSpan(allocator, info.family);
    freeSpan(allocator, info.scripts);

    allocator.destroy(info);
}

// ---------------------------------------------------------------------------
// Plural Rules
// ---------------------------------------------------------------------------

/// Get the plural rule for a language by ISO 639-3 code.
/// Returns a pointer to PluralRule, or null if not available.
/// Caller must free with lol_free_plural_rule.
export fn lol_get_plural_rule(handle: ?*anyopaque, lang_code: ?[*:0]const u8) ?*anyopaque {
    _ = handle;
    const allocator = std.heap.c_allocator;

    const code_str = if (lang_code) |c| std.mem.span(c) else {
        setError("Null language code");
        return null;
    };

    const rule = allocator.create(PluralRule) catch {
        setError("Failed to allocate plural rule");
        return null;
    };

    const duped = allocator.dupeZ(u8, code_str) catch {
        allocator.destroy(rule);
        setError("Failed to copy language code");
        return null;
    };

    // Determine plural rule based on language.
    // This is a simplified subset of CLDR rules for common languages.
    const info = getPluralInfo(code_str);

    rule.* = .{
        .language = duped.ptr,
        .form_count = info.form_count,
        .categories = info.categories,
    };

    clearError();
    return @ptrCast(rule);
}

/// Free a PluralRule struct.
export fn lol_free_plural_rule(rule_ptr: ?*anyopaque) void {
    if (rule_ptr == null) return;
    const allocator = std.heap.c_allocator;
    const rule: *PluralRule = @ptrCast(@alignCast(rule_ptr.?));

    freeSpan(allocator, rule.language);
    allocator.destroy(rule);
}

// ---------------------------------------------------------------------------
// Locale Fallback Chain
// ---------------------------------------------------------------------------

/// Get the fallback chain length for a locale tag.
/// For "en-GB" this returns 3 (en-GB, en, default).
export fn lol_fallback_chain_len(_: ?*anyopaque, locale_tag: ?[*:0]const u8) u32 {
    const tag_str = if (locale_tag) |t| std.mem.span(t) else return 1;
    var count: u32 = 1; // The tag itself
    for (tag_str) |c| {
        if (c == '-') count += 1;
    }
    return count + 1; // +1 for "default" at the end
}

/// Get the fallback chain as a null-terminated array of C strings.
/// Caller must free with lol_free_fallback_chain.
export fn lol_fallback_chain(handle: ?*anyopaque, locale_tag: ?[*:0]const u8) ?*anyopaque {
    _ = handle;
    _ = locale_tag;
    // Stub: full implementation would build the chain
    return null;
}

/// Free a fallback chain array.
export fn lol_free_fallback_chain(chain_ptr: ?*anyopaque) void {
    _ = chain_ptr;
    // Stub: would free the array and all strings
}

// ---------------------------------------------------------------------------
// List languages (array result)
// ---------------------------------------------------------------------------

/// List all languages. Writes array pointer; returns count.
export fn lol_list_languages(handle: ?*anyopaque, out_array: ?*anyopaque) u32 {
    _ = handle;
    _ = out_array;
    // Stub: full implementation would populate the array
    return 0;
}

// ---------------------------------------------------------------------------
// Error Handling
// ---------------------------------------------------------------------------

/// Get the last error message as a null-terminated C string.
/// Returns null if there is no error.
export fn lol_last_error() ?[*:0]const u8 {
    const err = last_error orelse return null;
    const allocator = std.heap.c_allocator;
    const c_str = allocator.dupeZ(u8, err) catch return null;
    return c_str.ptr;
}

// ---------------------------------------------------------------------------
// Version Information
// ---------------------------------------------------------------------------

/// Get the library version string.
export fn lol_version() [*:0]const u8 {
    return VERSION.ptr;
}

/// Get build information string.
export fn lol_build_info() [*:0]const u8 {
    return BUILD_INFO.ptr;
}

// ---------------------------------------------------------------------------
// Utility
// ---------------------------------------------------------------------------

/// Check if a service handle is initialised and ready.
/// Returns 1 if initialised, 0 otherwise.
export fn lol_is_initialized(handle: ?*anyopaque) u32 {
    const state = stateFromHandle(handle) orelse return 0;
    return if (state.initialized) 1 else 0;
}

// ===========================================================================
// Internal helpers (not exported)
// ===========================================================================

/// Safely cast an opaque handle to a ServiceState pointer.
fn stateFromHandle(handle: ?*anyopaque) ?*ServiceState {
    const ptr = handle orelse return null;
    return @ptrCast(@alignCast(ptr));
}

/// Free a null-terminated C string span via the given allocator.
fn freeSpan(allocator: std.mem.Allocator, s: ?[*:0]const u8) void {
    if (s) |ptr| {
        const slice = std.mem.span(ptr);
        allocator.free(slice);
    }
}

/// Plural rule info used internally to populate PluralRule structs.
const PluralInfo = struct {
    form_count: u32,
    categories: [6]u32,
};

/// Look up CLDR plural rule info for a language.
/// Returns a simplified subset covering the most common patterns.
fn getPluralInfo(lang: []const u8) PluralInfo {
    // Languages with only "other" (1 form): Chinese, Japanese, Korean, Vietnamese, etc.
    if (std.mem.eql(u8, lang, "zh") or
        std.mem.eql(u8, lang, "ja") or
        std.mem.eql(u8, lang, "ko") or
        std.mem.eql(u8, lang, "vi") or
        std.mem.eql(u8, lang, "zho") or
        std.mem.eql(u8, lang, "jpn") or
        std.mem.eql(u8, lang, "kor") or
        std.mem.eql(u8, lang, "vie"))
    {
        return .{ .form_count = 1, .categories = .{ 5, 0, 0, 0, 0, 0 } };
    }

    // Languages with "one" and "other" (2 forms): English, German, etc.
    if (std.mem.eql(u8, lang, "en") or
        std.mem.eql(u8, lang, "de") or
        std.mem.eql(u8, lang, "nl") or
        std.mem.eql(u8, lang, "sv") or
        std.mem.eql(u8, lang, "eng") or
        std.mem.eql(u8, lang, "deu") or
        std.mem.eql(u8, lang, "nld") or
        std.mem.eql(u8, lang, "swe"))
    {
        return .{ .form_count = 2, .categories = .{ 1, 5, 0, 0, 0, 0 } };
    }

    // Languages with "one", "few", "other" (3 forms): Czech, Slovak, etc.
    if (std.mem.eql(u8, lang, "cs") or
        std.mem.eql(u8, lang, "sk") or
        std.mem.eql(u8, lang, "ces") or
        std.mem.eql(u8, lang, "slk"))
    {
        return .{ .form_count = 3, .categories = .{ 1, 3, 5, 0, 0, 0 } };
    }

    // Arabic: zero, one, two, few, many, other (6 forms)
    if (std.mem.eql(u8, lang, "ar") or std.mem.eql(u8, lang, "ara")) {
        return .{ .form_count = 6, .categories = .{ 0, 1, 2, 3, 4, 5 } };
    }

    // French, Portuguese: "one" and "other" (but "one" includes 0 in some)
    if (std.mem.eql(u8, lang, "fr") or
        std.mem.eql(u8, lang, "pt") or
        std.mem.eql(u8, lang, "fra") or
        std.mem.eql(u8, lang, "por"))
    {
        return .{ .form_count = 2, .categories = .{ 1, 5, 0, 0, 0, 0 } };
    }

    // Polish: one, few, many, other (4 forms)
    if (std.mem.eql(u8, lang, "pl") or std.mem.eql(u8, lang, "pol")) {
        return .{ .form_count = 4, .categories = .{ 1, 3, 4, 5, 0, 0 } };
    }

    // Russian, Ukrainian: one, few, many, other (4 forms)
    if (std.mem.eql(u8, lang, "ru") or
        std.mem.eql(u8, lang, "uk") or
        std.mem.eql(u8, lang, "rus") or
        std.mem.eql(u8, lang, "ukr"))
    {
        return .{ .form_count = 4, .categories = .{ 1, 3, 4, 5, 0, 0 } };
    }

    // Default: "one" and "other" (the most common pattern)
    return .{ .form_count = 2, .categories = .{ 1, 5, 0, 0, 0, 0 } };
}

/// Select the CLDR plural category for a (language, quantity) pair.
/// Simplified CLDR rules for the most common language families.
fn selectPluralForLanguage(lang: []const u8, quantity: u64) PluralCategory {
    // East Asian languages: always "other"
    if (std.mem.eql(u8, lang, "zh") or
        std.mem.eql(u8, lang, "ja") or
        std.mem.eql(u8, lang, "ko") or
        std.mem.eql(u8, lang, "vi") or
        std.mem.eql(u8, lang, "zho") or
        std.mem.eql(u8, lang, "jpn") or
        std.mem.eql(u8, lang, "kor") or
        std.mem.eql(u8, lang, "vie"))
    {
        return .other;
    }

    // Arabic (6 forms)
    if (std.mem.eql(u8, lang, "ar") or std.mem.eql(u8, lang, "ara")) {
        return switch (quantity) {
            0 => .zero,
            1 => .one,
            2 => .two,
            3...10 => .few,
            11...99 => .many,
            else => .other,
        };
    }

    // Polish (4 forms: one, few, many, other)
    if (std.mem.eql(u8, lang, "pl") or std.mem.eql(u8, lang, "pol")) {
        if (quantity == 1) return .one;
        const mod10 = quantity % 10;
        const mod100 = quantity % 100;
        if (mod10 >= 2 and mod10 <= 4 and (mod100 < 12 or mod100 > 14)) return .few;
        if ((mod10 == 0 or mod10 == 1) or (mod10 >= 5 and mod10 <= 9) or (mod100 >= 12 and mod100 <= 14)) return .many;
        return .other;
    }

    // Russian/Ukrainian (4 forms: one, few, many, other)
    if (std.mem.eql(u8, lang, "ru") or
        std.mem.eql(u8, lang, "uk") or
        std.mem.eql(u8, lang, "rus") or
        std.mem.eql(u8, lang, "ukr"))
    {
        const mod10 = quantity % 10;
        const mod100 = quantity % 100;
        if (mod10 == 1 and mod100 != 11) return .one;
        if (mod10 >= 2 and mod10 <= 4 and (mod100 < 12 or mod100 > 14)) return .few;
        return .many;
    }

    // Czech/Slovak (3 forms: one, few, other)
    if (std.mem.eql(u8, lang, "cs") or
        std.mem.eql(u8, lang, "sk") or
        std.mem.eql(u8, lang, "ces") or
        std.mem.eql(u8, lang, "slk"))
    {
        return switch (quantity) {
            1 => .one,
            2...4 => .few,
            else => .other,
        };
    }

    // Default (most Germanic, Romance, etc.): "one" vs "other"
    return if (quantity == 1) .one else .other;
}

/// Get the string suffix for a plural category.
fn pluralSuffix(cat: PluralCategory) []const u8 {
    return switch (cat) {
        .zero => "zero",
        .one => "one",
        .two => "two",
        .few => "few",
        .many => "many",
        .other => "other",
    };
}

/// Look up a translation in the corpus data directory.
/// Path: <data_dir>/translations/<lang>/<key>.txt
/// Returns a heap-allocated TranslationResult, or null.
fn lookupTranslation(state: *ServiceState, locale_tag: []const u8, key: []const u8) ?*TranslationResult {
    // Extract language subtag from the locale tag.
    var it = std.mem.splitScalar(u8, locale_tag, '-');
    const lang = it.next() orelse locale_tag;

    // Build the file path.
    const path = std.fmt.allocPrint(state.allocator, "{s}/translations/{s}/{s}.txt", .{ state.data_dir, lang, key }) catch return null;
    defer state.allocator.free(path);

    // Try to read the translation file.
    const text = blk: {
        const file = std.fs.cwd().openFile(path, .{}) catch {
            if (!state.enable_fallback) return null;
            // Try default locale fallback.
            const fallback_path = std.fmt.allocPrint(state.allocator, "{s}/translations/{s}/{s}.txt", .{ state.data_dir, state.default_locale, key }) catch return null;
            defer state.allocator.free(fallback_path);

            const fb_file = std.fs.cwd().openFile(fallback_path, .{}) catch return null;
            defer fb_file.close();

            const content = fb_file.readToEndAlloc(state.allocator, 1024 * 1024) catch return null;
            break :blk content;
        };
        defer file.close();

        const content = file.readToEndAlloc(state.allocator, 1024 * 1024) catch return null;
        break :blk content;
    };
    defer state.allocator.free(text);

    // Determine if we used the original locale or fell back.
    const used_fallback = !std.mem.eql(u8, lang, locale_tag[0..@min(lang.len, locale_tag.len)]);

    const result = state.allocator.create(TranslationResult) catch return null;
    result.* = .{
        .text = state.dupeStr(text),
        .resolved_locale = state.dupeStr(lang),
        .is_fallback = if (used_fallback) 1 else 0,
    };

    return result;
}

// ===========================================================================
// Tests
// ===========================================================================

test "lifecycle" {
    const handle = lol_init(null) orelse return error.InitFailed;
    defer lol_free(handle);

    try std.testing.expect(lol_is_initialized(handle) == 1);
}

test "null handle returns 0" {
    try std.testing.expect(lol_is_initialized(null) == 0);
}

test "error handling on null" {
    const result = lol_translate(null, null, null);
    try std.testing.expect(result == null);

    const err = lol_last_error();
    try std.testing.expect(err != null);
}

test "version" {
    const ver = lol_version();
    const ver_str = std.mem.span(ver);
    try std.testing.expectEqualStrings(VERSION, ver_str);
}

test "locale resolution" {
    const handle = lol_init(null) orelse return error.InitFailed;
    defer lol_free(handle);

    const locale_ptr = lol_resolve_locale(handle, "en-US");
    if (locale_ptr) |ptr| {
        defer lol_free_locale(ptr);
        const locale: *Locale = @ptrCast(@alignCast(ptr));
        const tag = std.mem.span(locale.tag.?);
        try std.testing.expectEqualStrings("en-US", tag);
        const lang = std.mem.span(locale.language.?);
        try std.testing.expectEqualStrings("en", lang);
        const region = std.mem.span(locale.region.?);
        try std.testing.expectEqualStrings("US", region);
    }
}

test "plural selection — English" {
    try std.testing.expectEqual(@intFromEnum(PluralCategory.one), lol_select_plural(null, "en", 1));
    try std.testing.expectEqual(@intFromEnum(PluralCategory.other), lol_select_plural(null, "en", 0));
    try std.testing.expectEqual(@intFromEnum(PluralCategory.other), lol_select_plural(null, "en", 5));
}

test "plural selection — Arabic" {
    try std.testing.expectEqual(@intFromEnum(PluralCategory.zero), lol_select_plural(null, "ar", 0));
    try std.testing.expectEqual(@intFromEnum(PluralCategory.one), lol_select_plural(null, "ar", 1));
    try std.testing.expectEqual(@intFromEnum(PluralCategory.two), lol_select_plural(null, "ar", 2));
    try std.testing.expectEqual(@intFromEnum(PluralCategory.few), lol_select_plural(null, "ar", 5));
    try std.testing.expectEqual(@intFromEnum(PluralCategory.many), lol_select_plural(null, "ar", 50));
}

test "plural selection — Japanese (no plural)" {
    try std.testing.expectEqual(@intFromEnum(PluralCategory.other), lol_select_plural(null, "ja", 1));
    try std.testing.expectEqual(@intFromEnum(PluralCategory.other), lol_select_plural(null, "ja", 100));
}

test "plural rule — English" {
    const rule_ptr = lol_get_plural_rule(null, "en") orelse return error.RuleFailed;
    defer lol_free_plural_rule(rule_ptr);
    const rule: *PluralRule = @ptrCast(@alignCast(rule_ptr));
    try std.testing.expectEqual(@as(u32, 2), rule.form_count);
}

test "free null is safe" {
    lol_free(null);
    lol_free_locale(null);
    lol_free_translation(null);
    lol_free_language(null);
    lol_free_plural_rule(null);
}
