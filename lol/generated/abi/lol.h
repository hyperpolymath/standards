/*
 * LOL i18n Service — Generated C Header
 *
 * Auto-generated from src/abi/ Idris2 definitions.
 * Do NOT edit manually; regenerate from the ABI definitions.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 * Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
 */

#ifndef LOL_H
#define LOL_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* -------------------------------------------------------------------------
 * Result Codes (matches Lol.ABI.Types.Result)
 * ------------------------------------------------------------------------- */

typedef enum {
    LOL_OK               = 0,
    LOL_ERROR            = 1,
    LOL_INVALID_PARAM    = 2,
    LOL_OUT_OF_MEMORY    = 3,
    LOL_NULL_POINTER     = 4,
    LOL_LOCALE_NOT_FOUND = 5,
    LOL_KEY_NOT_FOUND    = 6,
    LOL_PLURAL_OUT_OF_RANGE = 7,
} lol_result_t;

/* -------------------------------------------------------------------------
 * CLDR Plural Categories (matches Lol.ABI.Types.PluralCategory)
 * ------------------------------------------------------------------------- */

typedef enum {
    LOL_PLURAL_ZERO  = 0,
    LOL_PLURAL_ONE   = 1,
    LOL_PLURAL_TWO   = 2,
    LOL_PLURAL_FEW   = 3,
    LOL_PLURAL_MANY  = 4,
    LOL_PLURAL_OTHER = 5,
} lol_plural_category_t;

/* -------------------------------------------------------------------------
 * Opaque Handle
 * ------------------------------------------------------------------------- */

typedef void* lol_handle_t;

/* -------------------------------------------------------------------------
 * Structs (matches src/abi/Layout.idr)
 * ------------------------------------------------------------------------- */

/** Parsed BCP 47 locale — 32 bytes on 64-bit. */
typedef struct {
    const char *tag;       /**< Full BCP 47 tag (e.g. "en-US") */
    const char *language;  /**< ISO 639 language subtag */
    const char *script;    /**< ISO 15924 script (empty if unspecified) */
    const char *region;    /**< ISO 3166-1 region (empty if unspecified) */
} lol_locale_t;

/** Translation result — 24 bytes. */
typedef struct {
    const char *text;            /**< The resolved translation text (UTF-8) */
    const char *resolved_locale; /**< Locale actually used */
    uint32_t    is_fallback;     /**< 1 if a fallback locale was used */
    uint32_t    _padding;        /**< Alignment padding */
} lol_translation_result_t;

/** Language metadata — 56 bytes. */
typedef struct {
    const char *iso639_3;    /**< ISO 639-3 code */
    const char *name;        /**< English name */
    const char *native_name; /**< Autonym */
    const char *family;      /**< Language family */
    const char *scripts;     /**< Writing systems */
    uint32_t    source_count;/**< Number of corpus sources */
    uint32_t    verse_count; /**< Total verses crawled */
    double      quality;     /**< Quality score (0.0-1.0) */
} lol_language_info_t;

/** CLDR plural rule — 40 bytes. */
typedef struct {
    const char *language;      /**< ISO 639 code */
    uint32_t    form_count;    /**< Number of plural forms (1-6) */
    uint32_t    categories[6]; /**< Which CLDR categories are used */
    uint32_t    _padding;      /**< Alignment padding */
} lol_plural_rule_t;

/* -------------------------------------------------------------------------
 * Library Lifecycle
 * ------------------------------------------------------------------------- */

/** Initialise the LOL service with a corpus data directory. */
lol_handle_t lol_init(const char *data_dir);

/** Free the LOL service handle and all resources. */
void lol_free(lol_handle_t handle);

/** Check if a handle is initialised (returns 1 or 0). */
uint32_t lol_is_initialized(lol_handle_t handle);

/* -------------------------------------------------------------------------
 * Locale Resolution
 * ------------------------------------------------------------------------- */

/** Parse and resolve a BCP 47 locale tag. Caller must free with lol_free_locale. */
lol_locale_t* lol_resolve_locale(lol_handle_t handle, const char *tag);

/** Free a locale returned by lol_resolve_locale. */
void lol_free_locale(lol_locale_t *locale);

/* -------------------------------------------------------------------------
 * Translation Lookup
 * ------------------------------------------------------------------------- */

/** Look up a translation. Caller must free with lol_free_translation. */
lol_translation_result_t* lol_translate(lol_handle_t handle,
                                         const char *locale_tag,
                                         const char *key);

/** Free a translation result. */
void lol_free_translation(lol_translation_result_t *result);

/** Get the text from a translation result (not owned by caller). */
const char* lol_translation_text(lol_translation_result_t *result);

/* -------------------------------------------------------------------------
 * Plural Form Selection
 * ------------------------------------------------------------------------- */

/** Select the CLDR plural category for a quantity in a locale. */
uint32_t lol_select_plural(lol_handle_t handle,
                            const char *locale_tag,
                            uint64_t quantity);

/** Translate with automatic plural form selection. */
lol_translation_result_t* lol_translate_plural(lol_handle_t handle,
                                                const char *locale_tag,
                                                const char *key,
                                                uint64_t quantity);

/* -------------------------------------------------------------------------
 * Language Metadata
 * ------------------------------------------------------------------------- */

/** Get the number of languages in the corpus. */
uint32_t lol_language_count(lol_handle_t handle);

/** Look up language metadata by ISO 639-3 code. Caller must free. */
lol_language_info_t* lol_get_language(lol_handle_t handle, const char *code);

/** Free a language info struct. */
void lol_free_language(lol_language_info_t *info);

/** List all languages. Returns count, writes array to out_array. */
uint32_t lol_list_languages(lol_handle_t handle, lol_language_info_t **out_array);

/* -------------------------------------------------------------------------
 * Plural Rules
 * ------------------------------------------------------------------------- */

/** Get the plural rule for a language. Caller must free. */
lol_plural_rule_t* lol_get_plural_rule(lol_handle_t handle, const char *lang_code);

/** Free a plural rule struct. */
void lol_free_plural_rule(lol_plural_rule_t *rule);

/* -------------------------------------------------------------------------
 * Locale Fallback Chain
 * ------------------------------------------------------------------------- */

/** Get the fallback chain length for a locale tag. */
uint32_t lol_fallback_chain_len(lol_handle_t handle, const char *locale_tag);

/** Get the fallback chain. Caller must free with lol_free_fallback_chain. */
const char** lol_fallback_chain(lol_handle_t handle, const char *locale_tag);

/** Free a fallback chain array. */
void lol_free_fallback_chain(const char **chain);

/* -------------------------------------------------------------------------
 * Error Handling
 * ------------------------------------------------------------------------- */

/** Get the last error message (may be NULL). */
const char* lol_last_error(void);

/* -------------------------------------------------------------------------
 * Version Information
 * ------------------------------------------------------------------------- */

/** Get the library version string. */
const char* lol_version(void);

/** Get build information string. */
const char* lol_build_info(void);

#ifdef __cplusplus
}
#endif

#endif /* LOL_H */
