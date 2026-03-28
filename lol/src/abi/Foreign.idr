-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
||| LOL i18n Service — Foreign Function Interface Declarations
|||
||| Declares all C-compatible functions that will be implemented in
||| the Zig FFI layer (ffi/zig/src/main.zig). Each function is
||| declared with its primitive FFI signature and wrapped in a
||| safe Idris2 API that handles null pointers and error codes.
|||
||| LOL is called as a service, never embedded. Consumers initialise
||| with lol_init, perform lookups, then clean up with lol_free.

module Lol.ABI.Foreign

import Lol.ABI.Types
import Lol.ABI.Layout

%default total

--------------------------------------------------------------------------------
-- Library Lifecycle
--------------------------------------------------------------------------------

||| Initialise the LOL service with a corpus data directory.
||| Returns a handle to the service instance, or null on failure.
export
%foreign "C:lol_init, liblol"
prim__init : String -> PrimIO Bits64

||| Safe wrapper for service initialisation.
||| Takes the path to the corpus data directory and returns a Handle
||| if initialisation succeeds.
export
init : String -> IO (Maybe Handle)
init dataDir = do
  ptr <- primIO (prim__init dataDir)
  pure (createHandle ptr)

||| Release all resources held by the LOL service.
export
%foreign "C:lol_free, liblol"
prim__free : Bits64 -> PrimIO ()

||| Safe wrapper for cleanup.
export
free : Handle -> IO ()
free h = primIO (prim__free (handlePtr h))

--------------------------------------------------------------------------------
-- Locale Resolution
--------------------------------------------------------------------------------

||| Parse a BCP 47 locale tag and resolve it against the corpus.
||| Returns a pointer to a lol_locale_t struct, or null if the locale
||| cannot be resolved.
export
%foreign "C:lol_resolve_locale, liblol"
prim__resolveLocale : Bits64 -> String -> PrimIO Bits64

||| Safe locale resolution.
||| Parses the tag and looks up the best matching corpus entry.
export
resolveLocale : Handle -> String -> IO (Maybe Bits64)
resolveLocale h tag = do
  ptr <- primIO (prim__resolveLocale (handlePtr h) tag)
  pure (if ptr == 0 then Nothing else Just ptr)

||| Free a locale struct returned by resolveLocale.
export
%foreign "C:lol_free_locale, liblol"
prim__freeLocale : Bits64 -> PrimIO ()

||| Safe locale cleanup.
export
freeLocale : Bits64 -> IO ()
freeLocale ptr = primIO (prim__freeLocale ptr)

--------------------------------------------------------------------------------
-- Translation Lookup
--------------------------------------------------------------------------------

||| Look up a translation key for a given locale.
||| Returns a pointer to a lol_translation_result_t, or null if not found.
||| The result includes the resolved locale (which may differ from the
||| requested one if fallback was used).
export
%foreign "C:lol_translate, liblol"
prim__translate : Bits64 -> String -> String -> PrimIO Bits64

||| Safe translation lookup.
||| Takes a locale tag and translation key, returns the translation result.
export
translate : Handle -> (localeTag : String) -> (key : TranslationKey) -> IO (Maybe Bits64)
translate h localeTag key = do
  ptr <- primIO (prim__translate (handlePtr h) localeTag key)
  pure (if ptr == 0 then Nothing else Just ptr)

||| Free a translation result struct.
export
%foreign "C:lol_free_translation, liblol"
prim__freeTranslation : Bits64 -> PrimIO ()

||| Safe translation result cleanup.
export
freeTranslation : Bits64 -> IO ()
freeTranslation ptr = primIO (prim__freeTranslation ptr)

||| Get the text field from a translation result pointer.
export
%foreign "C:lol_translation_text, liblol"
prim__translationText : Bits64 -> PrimIO Bits64

||| Get translation text as a string.
export
%foreign "support:idris2_getString, libidris2_support"
prim__getString : Bits64 -> String

||| Safe translation text getter.
export
translationText : Bits64 -> IO (Maybe String)
translationText ptr = do
  strPtr <- primIO (prim__translationText ptr)
  if strPtr == 0
    then pure Nothing
    else pure (Just (prim__getString strPtr))

--------------------------------------------------------------------------------
-- Plural Form Selection
--------------------------------------------------------------------------------

||| Select the appropriate CLDR plural category for a quantity in a
||| given locale. Returns the plural category as a Bits32 (see pluralToInt).
export
%foreign "C:lol_select_plural, liblol"
prim__selectPlural : Bits64 -> String -> Bits64 -> PrimIO Bits32

||| Safe plural category selection.
||| Given a locale tag and a quantity, returns the CLDR plural category
||| that should be used for that quantity in that language.
export
selectPlural : Handle -> (localeTag : String) -> (quantity : Bits64) -> IO (Maybe PluralCategory)
selectPlural h localeTag quantity = do
  raw <- primIO (prim__selectPlural (handlePtr h) localeTag quantity)
  pure (pluralFromInt raw)

||| Look up a pluralised translation.
||| Combines plural selection and translation lookup in a single call.
||| The key is suffixed with the plural category (e.g. "items.one", "items.other").
export
%foreign "C:lol_translate_plural, liblol"
prim__translatePlural : Bits64 -> String -> String -> Bits64 -> PrimIO Bits64

||| Safe pluralised translation.
export
translatePlural : Handle -> (localeTag : String) -> (key : TranslationKey) -> (quantity : Bits64) -> IO (Maybe Bits64)
translatePlural h localeTag key quantity = do
  ptr <- primIO (prim__translatePlural (handlePtr h) localeTag key quantity)
  pure (if ptr == 0 then Nothing else Just ptr)

--------------------------------------------------------------------------------
-- Language Metadata
--------------------------------------------------------------------------------

||| List all languages in the corpus.
||| Returns a pointer to an array of lol_language_info_t structs and
||| writes the count to the output parameter.
export
%foreign "C:lol_list_languages, liblol"
prim__listLanguages : Bits64 -> Bits64 -> PrimIO Bits32

||| Get language count in the corpus.
export
%foreign "C:lol_language_count, liblol"
prim__languageCount : Bits64 -> PrimIO Bits32

||| Safe language count getter.
export
languageCount : Handle -> IO Bits32
languageCount h = primIO (prim__languageCount (handlePtr h))

||| Look up metadata for a specific language by ISO 639-3 code.
export
%foreign "C:lol_get_language, liblol"
prim__getLanguage : Bits64 -> String -> PrimIO Bits64

||| Safe language lookup.
export
getLanguage : Handle -> String -> IO (Maybe Bits64)
getLanguage h code = do
  ptr <- primIO (prim__getLanguage (handlePtr h) code)
  pure (if ptr == 0 then Nothing else Just ptr)

||| Free a language info struct.
export
%foreign "C:lol_free_language, liblol"
prim__freeLanguage : Bits64 -> PrimIO ()

||| Safe language info cleanup.
export
freeLanguage : Bits64 -> IO ()
freeLanguage ptr = primIO (prim__freeLanguage ptr)

--------------------------------------------------------------------------------
-- Plural Rules
--------------------------------------------------------------------------------

||| Get the plural rule for a specific language.
||| Returns a pointer to lol_plural_rule_t, or null if not available.
export
%foreign "C:lol_get_plural_rule, liblol"
prim__getPluralRule : Bits64 -> String -> PrimIO Bits64

||| Safe plural rule lookup.
export
getPluralRule : Handle -> String -> IO (Maybe Bits64)
getPluralRule h langCode = do
  ptr <- primIO (prim__getPluralRule (handlePtr h) langCode)
  pure (if ptr == 0 then Nothing else Just ptr)

||| Free a plural rule struct.
export
%foreign "C:lol_free_plural_rule, liblol"
prim__freePluralRule : Bits64 -> PrimIO ()

||| Safe plural rule cleanup.
export
freePluralRule : Bits64 -> IO ()
freePluralRule ptr = primIO (prim__freePluralRule ptr)

--------------------------------------------------------------------------------
-- Locale Fallback Chain
--------------------------------------------------------------------------------

||| Get the fallback chain for a locale.
||| Returns a null-terminated array of locale tag strings.
||| For example, "en-GB" might produce ["en-GB", "en", "default"].
export
%foreign "C:lol_fallback_chain, liblol"
prim__fallbackChain : Bits64 -> String -> PrimIO Bits64

||| Get the length of a fallback chain.
export
%foreign "C:lol_fallback_chain_len, liblol"
prim__fallbackChainLen : Bits64 -> String -> PrimIO Bits32

||| Free a fallback chain array.
export
%foreign "C:lol_free_fallback_chain, liblol"
prim__freeFallbackChain : Bits64 -> PrimIO ()

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

||| Get the last error message from the service.
export
%foreign "C:lol_last_error, liblol"
prim__lastError : PrimIO Bits64

||| Retrieve the last error as a string.
export
lastError : IO (Maybe String)
lastError = do
  ptr <- primIO prim__lastError
  if ptr == 0
    then pure Nothing
    else pure (Just (prim__getString ptr))

||| Get a human-readable description for a result code.
export
errorDescription : Result -> String
errorDescription Ok               = "Success"
errorDescription Error             = "Generic error"
errorDescription InvalidParam      = "Invalid parameter"
errorDescription OutOfMemory       = "Out of memory"
errorDescription NullPointer       = "Null pointer"
errorDescription LocaleNotFound    = "Locale not found in corpus"
errorDescription KeyNotFound       = "Translation key not found"
errorDescription PluralOutOfRange  = "Plural form index out of range"

--------------------------------------------------------------------------------
-- Version Information
--------------------------------------------------------------------------------

||| Get the LOL library version string.
export
%foreign "C:lol_version, liblol"
prim__version : PrimIO Bits64

||| Get version as a string.
export
version : IO String
version = do
  ptr <- primIO prim__version
  pure (prim__getString ptr)

||| Get build information.
export
%foreign "C:lol_build_info, liblol"
prim__buildInfo : PrimIO Bits64

||| Get build information as a string.
export
buildInfo : IO String
buildInfo = do
  ptr <- primIO prim__buildInfo
  pure (prim__getString ptr)

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

||| Check if the service handle is initialised and ready.
export
%foreign "C:lol_is_initialized, liblol"
prim__isInitialized : Bits64 -> PrimIO Bits32

||| Safe initialisation check.
export
isInitialized : Handle -> IO Bool
isInitialized h = do
  result <- primIO (prim__isInitialized (handlePtr h))
  pure (result /= 0)
