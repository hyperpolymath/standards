// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// lol_ffi.zig — Zig wrapper around liblol (the LOL i18n C ABI).
//
// Replaces:
//   api/v-lol/src/lol.v   (Service struct + public methods)
//   api/v-lol/src/ffi.v   (raw C declarations — inlined here)
//
// This module calls the C functions declared in generated/abi/lol.h and
// converts their output into idiomatic Zig types defined in types.zig.
// All memory from liblol is freed here before returning; callers receive
// only owned Zig values (backed by their own allocators).
//
// The Service struct is the primary API surface — create one with
// Service.open(), use it, then call deinit() to release the handle.

const std = @import("std");
const types = @import("types.zig");

// =============================================================================
// C ABI declarations — mirrors ffi.v (inlined, no separate file needed)
// =============================================================================
// These extern declarations match generated/abi/lol.h exactly.
// The build system passes -llol so the linker resolves them to liblol.

// Opaque C types for the structs.  We read fields via @field after a
// @ptrCast to the typed C struct pointer — but we never expose the C
// pointers outside this module.

const CLolLocale = extern struct {
    tag:      ?[*:0]const u8,
    language: ?[*:0]const u8,
    script:   ?[*:0]const u8,
    region:   ?[*:0]const u8,
};

const CLolTranslationResult = extern struct {
    text:            ?[*:0]const u8,
    resolved_locale: ?[*:0]const u8,
    is_fallback:     u32,
    _padding:        u32,
};

const CLolLanguageInfo = extern struct {
    iso639_3:    ?[*:0]const u8,
    name:        ?[*:0]const u8,
    native_name: ?[*:0]const u8,
    family:      ?[*:0]const u8,
    scripts:     ?[*:0]const u8,
    source_count: u32,
    verse_count:  u32,
    quality:      f64,
};

const CLolPluralRule = extern struct {
    language:   ?[*:0]const u8,
    form_count: u32,
    categories: [6]u32,
    _padding:   u32,
};

// Library lifecycle
extern fn lol_init(data_dir: [*:0]const u8) ?*anyopaque;
extern fn lol_free(handle: *anyopaque) void;
extern fn lol_is_initialized(handle: *anyopaque) u32;

// Locale resolution
extern fn lol_resolve_locale(handle: *anyopaque, tag: [*:0]const u8) ?*CLolLocale;
extern fn lol_free_locale(locale: *CLolLocale) void;

// Translation
extern fn lol_translate(handle: *anyopaque, locale_tag: [*:0]const u8, key: [*:0]const u8) ?*CLolTranslationResult;
extern fn lol_free_translation(result: *CLolTranslationResult) void;
extern fn lol_translation_text(result: *CLolTranslationResult) ?[*:0]const u8;

// Plural
extern fn lol_select_plural(handle: *anyopaque, locale_tag: [*:0]const u8, quantity: u64) u32;
extern fn lol_translate_plural(handle: *anyopaque, locale_tag: [*:0]const u8, key: [*:0]const u8, quantity: u64) ?*CLolTranslationResult;

// Language metadata
extern fn lol_language_count(handle: *anyopaque) u32;
extern fn lol_get_language(handle: *anyopaque, code: [*:0]const u8) ?*CLolLanguageInfo;
extern fn lol_free_language(info: *CLolLanguageInfo) void;

// Plural rules
extern fn lol_get_plural_rule(handle: *anyopaque, lang_code: [*:0]const u8) ?*CLolPluralRule;
extern fn lol_free_plural_rule(rule: *CLolPluralRule) void;

// Fallback chain
extern fn lol_fallback_chain_len(handle: *anyopaque, locale_tag: [*:0]const u8) u32;

// Error / version
extern fn lol_last_error() ?[*:0]const u8;
extern fn lol_version() ?[*:0]const u8;
extern fn lol_build_info() ?[*:0]const u8;

// =============================================================================
// Internal helpers
// =============================================================================

/// Convert an optional C null-terminated string pointer to a Zig slice.
/// Returns an empty slice when the pointer is null.
/// The returned slice points into memory owned by liblol; callers must copy
/// before the owning C struct is freed.
fn cStrToSlice(ptr: ?[*:0]const u8) []const u8 {
    const p = ptr orelse return "";
    return std.mem.span(p);
}

/// Duplicate a slice into allocator-owned memory.  Used to copy strings out
/// of C structs before we call the corresponding lol_free_* function.
fn dupeStr(allocator: std.mem.Allocator, s: []const u8) ![]const u8 {
    return allocator.dupe(u8, s);
}

// =============================================================================
// Service — public API, mirrors lol.v
// =============================================================================

/// Primary interface to the LOL i18n service.  Wraps an opaque liblol handle.
///
/// Usage:
///   var svc = try Service.open(allocator, "corpus");
///   defer svc.deinit();
///   const result = try svc.translate(allocator, "en-US", "greeting");
///   // use result.text ...
pub const Service = struct {
    handle: *anyopaque,

    // -------------------------------------------------------------------------
    // Lifecycle
    // -------------------------------------------------------------------------

    /// Open a LOL service instance, loading corpus data from `data_dir`.
    /// Returns `error.GenericError` (with last_error() populated) on failure.
    pub fn open(data_dir: [:0]const u8) types.LolError!Service {
        const h = lol_init(data_dir.ptr) orelse return error.GenericError;
        return Service{ .handle = h };
    }

    /// Open with the default corpus directory ("corpus").
    pub fn openDefault() types.LolError!Service {
        return open("corpus");
    }

    /// Release all resources.  Safe to call exactly once; the Service must
    /// not be used after deinit().
    pub fn deinit(self: *Service) void {
        lol_free(self.handle);
        // Poison the pointer so use-after-free is detectable in debug builds.
        self.handle = @ptrFromInt(0xDEAD_BEEF_DEAD_BEEF);
    }

    /// Return true if the handle is valid and ready for calls.
    pub fn isInitialized(self: *const Service) bool {
        return lol_is_initialized(self.handle) == 1;
    }

    // -------------------------------------------------------------------------
    // Locale resolution
    // -------------------------------------------------------------------------

    /// Parse and resolve a BCP 47 locale tag against the corpus.
    /// The returned Locale's string fields are allocated from `allocator`.
    pub fn resolveLocale(
        self: *const Service,
        allocator: std.mem.Allocator,
        tag: [:0]const u8,
    ) (types.LolError || std.mem.Allocator.Error)!types.Locale {
        const raw = lol_resolve_locale(self.handle, tag.ptr) orelse
            return error.LocaleNotFound;
        defer lol_free_locale(raw);

        return types.Locale{
            .tag      = try dupeStr(allocator, cStrToSlice(raw.tag)),
            .language = try dupeStr(allocator, cStrToSlice(raw.language)),
            .script   = try dupeStr(allocator, cStrToSlice(raw.script)),
            .region   = try dupeStr(allocator, cStrToSlice(raw.region)),
        };
    }

    // -------------------------------------------------------------------------
    // Translation
    // -------------------------------------------------------------------------

    /// Look up a translation key for the given locale.  Walks the fallback
    /// chain (e.g. en-US → en → default) when the exact locale is missing.
    /// The returned TranslationResult's string fields are allocated from
    /// `allocator`.
    pub fn translate(
        self: *const Service,
        allocator: std.mem.Allocator,
        locale_tag: [:0]const u8,
        key: [:0]const u8,
    ) (types.LolError || std.mem.Allocator.Error)!types.TranslationResult {
        const raw = lol_translate(self.handle, locale_tag.ptr, key.ptr) orelse
            return error.KeyNotFound;
        defer lol_free_translation(raw);

        return types.TranslationResult{
            .text            = try dupeStr(allocator, cStrToSlice(raw.text)),
            .resolved_locale = try dupeStr(allocator, cStrToSlice(raw.resolved_locale)),
            .is_fallback     = raw.is_fallback != 0,
        };
    }

    /// Combine plural form selection with a translation lookup.
    /// The `quantity` determines the CLDR plural category, which is appended
    /// as a suffix to the key (e.g. "items" → "items.one").
    pub fn translatePlural(
        self: *const Service,
        allocator: std.mem.Allocator,
        locale_tag: [:0]const u8,
        key: [:0]const u8,
        quantity: u64,
    ) (types.LolError || std.mem.Allocator.Error)!types.TranslationResult {
        const raw = lol_translate_plural(self.handle, locale_tag.ptr, key.ptr, quantity) orelse
            return error.KeyNotFound;
        defer lol_free_translation(raw);

        return types.TranslationResult{
            .text            = try dupeStr(allocator, cStrToSlice(raw.text)),
            .resolved_locale = try dupeStr(allocator, cStrToSlice(raw.resolved_locale)),
            .is_fallback     = raw.is_fallback != 0,
        };
    }

    // -------------------------------------------------------------------------
    // Plural selection
    // -------------------------------------------------------------------------

    /// Return the CLDR plural category for `quantity` in the given locale.
    /// Never fails — values outside the 0–5 ABI range map to `.other`.
    pub fn selectPlural(
        self: *const Service,
        locale_tag: [:0]const u8,
        quantity: u64,
    ) types.PluralCategory {
        const raw = lol_select_plural(self.handle, locale_tag.ptr, quantity);
        return types.PluralCategory.fromInt(raw);
    }

    // -------------------------------------------------------------------------
    // Language metadata
    // -------------------------------------------------------------------------

    /// Return the number of languages available in the corpus.
    pub fn languageCount(self: *const Service) u32 {
        return lol_language_count(self.handle);
    }

    /// Look up metadata for an ISO 639-3 language code (e.g. "eng").
    /// String fields in the returned LanguageInfo are allocated from
    /// `allocator`.
    pub fn getLanguage(
        self: *const Service,
        allocator: std.mem.Allocator,
        code: [:0]const u8,
    ) (types.LolError || std.mem.Allocator.Error)!types.LanguageInfo {
        const raw = lol_get_language(self.handle, code.ptr) orelse
            return error.LocaleNotFound;
        defer lol_free_language(raw);

        return types.LanguageInfo{
            .iso639_3    = try dupeStr(allocator, cStrToSlice(raw.iso639_3)),
            .name        = try dupeStr(allocator, cStrToSlice(raw.name)),
            .native_name = try dupeStr(allocator, cStrToSlice(raw.native_name)),
            .family      = try dupeStr(allocator, cStrToSlice(raw.family)),
            .scripts     = try dupeStr(allocator, cStrToSlice(raw.scripts)),
            .source_count = raw.source_count,
            .verse_count  = raw.verse_count,
            .quality      = raw.quality,
        };
    }

    // -------------------------------------------------------------------------
    // Plural rules
    // -------------------------------------------------------------------------

    /// Retrieve the CLDR plural rule for a language.
    /// `categories` slice in the returned PluralRule is allocated from
    /// `allocator`; string fields are also allocator-owned.
    pub fn getPluralRule(
        self: *const Service,
        allocator: std.mem.Allocator,
        lang_code: [:0]const u8,
    ) (types.LolError || std.mem.Allocator.Error)!types.PluralRule {
        const raw = lol_get_plural_rule(self.handle, lang_code.ptr) orelse
            return error.LocaleNotFound;
        defer lol_free_plural_rule(raw);

        // form_count is proven to be 1–6 in the Idris2 ABI; cap defensively.
        const count: u32 = @min(raw.form_count, 6);
        const cats = try allocator.alloc(types.PluralCategory, count);
        for (cats, 0..) |*slot, i| {
            slot.* = types.PluralCategory.fromInt(raw.categories[i]);
        }

        return types.PluralRule{
            .language   = try dupeStr(allocator, cStrToSlice(raw.language)),
            .form_count = count,
            .categories = cats,
        };
    }

    // -------------------------------------------------------------------------
    // Fallback chain
    // -------------------------------------------------------------------------

    /// Return the number of locales in the fallback chain for `locale_tag`.
    /// For "en-GB" this is typically 3: en-GB → en → default.
    pub fn fallbackChainLength(
        self: *const Service,
        locale_tag: [:0]const u8,
    ) u32 {
        return lol_fallback_chain_len(self.handle, locale_tag.ptr);
    }
};

// =============================================================================
// Module-level helpers (no Service required) — mirrors module-level fns in lol.v
// =============================================================================

/// Return the most recent liblol error message, or an empty slice.
pub fn lastError() []const u8 {
    return cStrToSlice(lol_last_error());
}

/// Return the liblol version string.
pub fn version() []const u8 {
    return cStrToSlice(lol_version());
}

/// Return the liblol build information string.
pub fn buildInfo() []const u8 {
    return cStrToSlice(lol_build_info());
}
