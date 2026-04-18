// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// types.zig — LOL (1000Langs) public domain types for the Zig API gateway.
//
// Mirrors types.v from the deprecated v-lol module.  These are idiomatic Zig
// value types (no raw pointers exposed); the lol_ffi.zig module performs
// all conversions from C-ABI structs into these.

// =============================================================================
// Error codes — mirrors lol_result_t from generated/abi/lol.h
// =============================================================================

/// All errors that the LOL service can surface to callers.
pub const LolError = error{
    /// Generic service-side error (check lol_last_error()).
    GenericError,
    /// A parameter value was null or malformed.
    InvalidParam,
    /// The service ran out of heap memory.
    OutOfMemory,
    /// An internal null pointer was encountered.
    NullPointer,
    /// The requested locale is not present in the corpus.
    LocaleNotFound,
    /// The requested translation key does not exist.
    KeyNotFound,
    /// A plural form index was out of the valid 0-5 range.
    PluralOutOfRange,
};

// =============================================================================
// CLDR plural categories — mirrors PluralCategory in types.v
// =============================================================================

/// Unicode CLDR plural category.  Different languages use different subsets
/// of these six values to classify quantities.
pub const PluralCategory = enum(u32) {
    /// Typically 0 in some languages (Arabic, Welsh, etc.).
    zero = 0,
    /// Typically 1 (English "1 item").
    one = 1,
    /// Typically 2 (Arabic dual, Welsh, etc.).
    two = 2,
    /// Small numbers (3–10 in Arabic, 2–4 in Czech, etc.).
    few = 3,
    /// Large numbers (11–99 in Arabic, etc.).
    many = 4,
    /// Catch-all (English "2 items", "5 items", etc.).
    other = 5,

    /// Convert a raw u32 from the C ABI into a PluralCategory.
    /// Values outside 0–5 map to `.other` rather than causing undefined
    /// behaviour — the corpus guarantees 0–5, but we defend at the boundary.
    pub fn fromInt(raw: u32) PluralCategory {
        return switch (raw) {
            0 => .zero,
            1 => .one,
            2 => .two,
            3 => .few,
            4 => .many,
            else => .other,
        };
    }

    /// Canonical lowercase string name (matches CLDR spec).
    pub fn name(self: PluralCategory) []const u8 {
        return switch (self) {
            .zero  => "zero",
            .one   => "one",
            .two   => "two",
            .few   => "few",
            .many  => "many",
            .other => "other",
        };
    }
};

// =============================================================================
// Locale — mirrors Locale struct in types.v
// =============================================================================

/// A parsed and corpus-resolved BCP 47 locale tag.
/// All string fields are caller-owned slices backed by the arena allocator
/// passed to the lol_ffi conversion functions.
pub const Locale = struct {
    /// Full BCP 47 tag, e.g. "en-US".
    tag: []const u8,
    /// ISO 639 language subtag, e.g. "en".
    language: []const u8,
    /// ISO 15924 script subtag (empty if unspecified), e.g. "Latn".
    script: []const u8,
    /// ISO 3166-1 region subtag (empty if unspecified), e.g. "US".
    region: []const u8,
};

// =============================================================================
// TranslationResult — mirrors TranslationResult in types.v
// =============================================================================

/// The output of a translation lookup, including fallback metadata.
pub const TranslationResult = struct {
    /// The resolved translation text (UTF-8).
    text: []const u8,
    /// The locale that was actually used (may differ from the requested one
    /// when the fallback chain was walked).
    resolved_locale: []const u8,
    /// True when a fallback locale was used rather than the exact one requested.
    is_fallback: bool,
};

// =============================================================================
// LanguageInfo — mirrors LanguageInfo in types.v
// =============================================================================

/// Metadata about a single language in the LOL parallel corpus.
pub const LanguageInfo = struct {
    /// ISO 639-3 three-letter code (e.g. "eng", "deu").
    iso639_3: []const u8,
    /// English name of the language.
    name: []const u8,
    /// Autonym — the name in the language itself.
    native_name: []const u8,
    /// Language family (e.g. "Indo-European").
    family: []const u8,
    /// Writing systems used (e.g. "Latin, Cyrillic").
    scripts: []const u8,
    /// Number of corpus sources available for this language.
    source_count: u32,
    /// Total verse count crawled for this language.
    verse_count: u32,
    /// Quality score produced by the VeriSimDB pipeline (0.0–1.0).
    quality: f64,
};

// =============================================================================
// PluralRule — mirrors PluralRule in types.v
// =============================================================================

/// Describes which CLDR plural categories a language uses and how many
/// distinct forms it requires.  `categories` has exactly `form_count`
/// valid entries; the remaining slots (up to 6) are unspecified.
pub const PluralRule = struct {
    /// ISO 639 language code.
    language: []const u8,
    /// Number of distinct plural forms (1–6, proven in the Idris2 ABI).
    form_count: u32,
    /// Which CLDR categories this language uses, in order.
    /// Slice is backed by the arena allocator passed at conversion time.
    categories: []PluralCategory,
};

// =============================================================================
// Config — mirrors Config in types.v
// =============================================================================

/// Initialisation parameters for the LOL service handle.
pub const Config = struct {
    /// Path to the corpus data directory.  Defaults to "corpus".
    data_dir: []const u8 = "corpus",
    /// Default fallback locale tag.  Defaults to "en".
    default_locale: []const u8 = "en",
    /// Whether to walk the fallback chain when the exact locale is missing.
    enable_fallback: bool = true,
};
