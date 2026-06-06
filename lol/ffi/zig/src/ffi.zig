// LOL i18n Service — C ABI Exports
//
// Re-exports all public C ABI functions from the main module.
// This file serves as the documentation index for the FFI surface area.
// The actual implementations are in main.zig, which imports locale.zig,
// store.zig, and plural.zig for modular implementation.
//
// See generated/abi/lol.h for the C header that consumers include.
// See src/abi/Foreign.idr for the Idris2 FFI declarations.
//
// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

/// Re-export the sub-modules for use by main.zig and tests.
pub const locale = @import("locale.zig");
pub const store = @import("store.zig");
pub const plural = @import("plural.zig");

// ---------------------------------------------------------------------------
// FFI Function Index
// ---------------------------------------------------------------------------
//
// The following C ABI functions are exported by main.zig:
//
// Lifecycle:
//   lol_init(data_dir)           -> handle
//   lol_free(handle)
//   lol_is_initialized(handle)   -> u32
//
// Locale Resolution:
//   lol_resolve_locale(handle, tag) -> *Locale
//   lol_free_locale(*Locale)
//
// Translation Lookup:
//   lol_translate(handle, locale, key) -> *TranslationResult
//   lol_free_translation(*TranslationResult)
//   lol_translation_text(*TranslationResult) -> *c_char
//
// Plural Selection:
//   lol_select_plural(handle, locale, quantity) -> u32
//   lol_translate_plural(handle, locale, key, quantity) -> *TranslationResult
//
// Language Metadata:
//   lol_language_count(handle) -> u32
//   lol_get_language(handle, code) -> *LanguageInfo
//   lol_free_language(*LanguageInfo)
//   lol_list_languages(handle, out_array) -> u32
//
// Plural Rules:
//   lol_get_plural_rule(handle, lang) -> *PluralRule
//   lol_free_plural_rule(*PluralRule)
//
// Fallback Chain:
//   lol_fallback_chain_len(handle, locale) -> u32
//   lol_fallback_chain(handle, locale) -> **c_char
//   lol_free_fallback_chain(**c_char)
//
// Error Handling:
//   lol_last_error() -> *c_char
//
// Version:
//   lol_version() -> *c_char
//   lol_build_info() -> *c_char
