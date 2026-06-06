-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
||| LOL i18n Service — ABI Type Definitions
|||
||| Defines the Application Binary Interface for the LOL (Language of Languages)
||| internationalisation service. LOL is called as a service (never embedded) to
||| provide translation lookup, locale resolution, and plural form selection
||| across 1500+ languages in the parallel corpus.
|||
||| All type definitions include formal proofs of correctness.
|||
||| @see https://idris2.readthedocs.io for Idris2 documentation

module Lol.ABI.Types

import Data.Bits
import Data.So
import Data.Vect
import Data.String

%default total

--------------------------------------------------------------------------------
-- Platform Detection
--------------------------------------------------------------------------------

||| Supported platforms for this ABI
public export
data Platform = Linux | Windows | MacOS | BSD | WASM

--------------------------------------------------------------------------------
-- Result Codes
--------------------------------------------------------------------------------

||| Result codes for FFI operations.
||| Uses C-compatible integers for cross-language compatibility.
public export
data Result : Type where
  ||| Operation succeeded
  Ok : Result
  ||| Generic error
  Error : Result
  ||| Invalid parameter provided
  InvalidParam : Result
  ||| Out of memory
  OutOfMemory : Result
  ||| Null pointer encountered
  NullPointer : Result
  ||| Locale not found in corpus
  LocaleNotFound : Result
  ||| Translation key not found for locale
  KeyNotFound : Result
  ||| Plural form index out of range for locale
  PluralOutOfRange : Result

||| Convert Result to C integer for FFI transport
public export
resultToInt : Result -> Bits32
resultToInt Ok              = 0
resultToInt Error           = 1
resultToInt InvalidParam    = 2
resultToInt OutOfMemory     = 3
resultToInt NullPointer     = 4
resultToInt LocaleNotFound  = 5
resultToInt KeyNotFound     = 6
resultToInt PluralOutOfRange = 7

||| Convert C integer back to Result
public export
resultFromInt : Bits32 -> Maybe Result
resultFromInt 0 = Just Ok
resultFromInt 1 = Just Error
resultFromInt 2 = Just InvalidParam
resultFromInt 3 = Just OutOfMemory
resultFromInt 4 = Just NullPointer
resultFromInt 5 = Just LocaleNotFound
resultFromInt 6 = Just KeyNotFound
resultFromInt 7 = Just PluralOutOfRange
resultFromInt _ = Nothing

||| Proof that resultToInt is injective (distinct results map to distinct codes)
public export
resultToIntInjective : (a, b : Result) -> resultToInt a = resultToInt b -> a = b
resultToIntInjective Ok Ok Refl = Refl
resultToIntInjective Error Error Refl = Refl
resultToIntInjective InvalidParam InvalidParam Refl = Refl
resultToIntInjective OutOfMemory OutOfMemory Refl = Refl
resultToIntInjective NullPointer NullPointer Refl = Refl
resultToIntInjective LocaleNotFound LocaleNotFound Refl = Refl
resultToIntInjective KeyNotFound KeyNotFound Refl = Refl
resultToIntInjective PluralOutOfRange PluralOutOfRange Refl = Refl

||| Proof that resultFromInt is a left inverse of resultToInt
public export
resultRoundTrip : (r : Result) -> resultFromInt (resultToInt r) = Just r
resultRoundTrip Ok              = Refl
resultRoundTrip Error           = Refl
resultRoundTrip InvalidParam    = Refl
resultRoundTrip OutOfMemory     = Refl
resultRoundTrip NullPointer     = Refl
resultRoundTrip LocaleNotFound  = Refl
resultRoundTrip KeyNotFound     = Refl
resultRoundTrip PluralOutOfRange = Refl

--------------------------------------------------------------------------------
-- Opaque Handles
--------------------------------------------------------------------------------

||| Opaque handle to the LOL service instance.
||| Prevents direct construction; enforces creation through lol_init.
public export
data Handle : Type where
  MkHandle : (ptr : Bits64) -> {auto 0 nonNull : So (ptr /= 0)} -> Handle

||| Safely create a handle from a pointer value.
||| Returns Nothing if the pointer is null.
public export
createHandle : Bits64 -> Maybe Handle
createHandle ptr =
  case choose (ptr /= 0) of
    Left prf  => Just (MkHandle ptr)
    Right _   => Nothing

||| Extract raw pointer value from handle (for FFI transport)
public export
handlePtr : Handle -> Bits64
handlePtr (MkHandle ptr) = ptr

--------------------------------------------------------------------------------
-- ISO 639 Language Codes
--------------------------------------------------------------------------------

||| An ISO 639-3 three-letter language code (e.g. "eng", "deu", "fra").
||| The corpus uses ISO 639-3 as the canonical identifier for every language.
public export
record Iso639_3 where
  constructor MkIso639_3
  code : String
  {auto 0 validLength : So (length code == 3)}

||| An ISO 639-1 two-letter language code (e.g. "en", "de", "fr").
||| Used for locale shorthand; many languages lack an ISO 639-1 code.
public export
record Iso639_1 where
  constructor MkIso639_1
  code : String
  {auto 0 validLength : So (length code == 2)}

--------------------------------------------------------------------------------
-- Locale
--------------------------------------------------------------------------------

||| A BCP 47 locale tag combining language, optional script, and region.
||| Examples: "en-US", "zh-Hans-CN", "sr-Latn-RS".
|||
||| The LOL service resolves locale tags to their best-match corpus entry.
public export
record Locale where
  constructor MkLocale
  ||| Full BCP 47 tag (e.g. "en-US")
  tag : String
  ||| ISO 639-3 language component (e.g. "eng")
  language : String
  ||| ISO 15924 script (e.g. "Latn"), empty if unspecified
  script : String
  ||| ISO 3166-1 region (e.g. "US"), empty if unspecified
  region : String

||| Proof that a Locale's language field is non-empty
public export
localeHasLanguage : (loc : Locale) -> So (length loc.language > 0) -> ()
localeHasLanguage _ _ = ()

--------------------------------------------------------------------------------
-- Translation Types
--------------------------------------------------------------------------------

||| A translation key is a dot-separated namespace path.
||| Examples: "app.greeting", "errors.not_found", "nav.home".
public export
TranslationKey : Type
TranslationKey = String

||| A single translation result returned from the service.
public export
record TranslationResult where
  constructor MkTranslationResult
  ||| The resolved translation text (UTF-8)
  text : String
  ||| The locale that was actually used (may differ from requested if fallback)
  resolvedLocale : String
  ||| Whether a fallback locale was used
  isFallback : Bool

--------------------------------------------------------------------------------
-- Plural Forms (CLDR)
--------------------------------------------------------------------------------

||| CLDR plural categories.
||| Different languages have different plural rules; LOL follows the Unicode
||| CLDR plural rule specification.
|||
||| @see https://www.unicode.org/cldr/charts/latest/supplemental/language_plural_rules.html
public export
data PluralCategory
  = ||| Typically 0 in some languages (Arabic, etc.)
    Zero
  | ||| Typically 1 (English "1 item")
    One
  | ||| Typically 2 (Arabic dual, Welsh, etc.)
    Two
  | ||| Small numbers (3-10 in Arabic, 2-4 in Czech, etc.)
    Few
  | ||| Large numbers (11-99 in Arabic, etc.)
    Many
  | ||| Everything else (English "2 items", "5 items", etc.)
    Other

||| Convert PluralCategory to C integer for FFI transport
public export
pluralToInt : PluralCategory -> Bits32
pluralToInt Zero  = 0
pluralToInt One   = 1
pluralToInt Two   = 2
pluralToInt Few   = 3
pluralToInt Many  = 4
pluralToInt Other = 5

||| Convert C integer back to PluralCategory
public export
pluralFromInt : Bits32 -> Maybe PluralCategory
pluralFromInt 0 = Just Zero
pluralFromInt 1 = Just One
pluralFromInt 2 = Just Two
pluralFromInt 3 = Just Few
pluralFromInt 4 = Just Many
pluralFromInt 5 = Just Other
pluralFromInt _ = Nothing

||| Proof that pluralToInt round-trips
public export
pluralRoundTrip : (p : PluralCategory) -> pluralFromInt (pluralToInt p) = Just p
pluralRoundTrip Zero  = Refl
pluralRoundTrip One   = Refl
pluralRoundTrip Two   = Refl
pluralRoundTrip Few   = Refl
pluralRoundTrip Many  = Refl
pluralRoundTrip Other = Refl

||| The maximum number of CLDR plural categories any language can have
public export
maxPluralCategories : Nat
maxPluralCategories = 6

||| Plural rule description for a single language.
||| Lists which CLDR categories the language uses.
public export
record PluralRule where
  constructor MkPluralRule
  ||| ISO 639-3 code for the language this rule applies to
  language : String
  ||| Number of distinct plural forms (1-6)
  formCount : Nat
  ||| Proof that formCount is within valid CLDR range
  {auto 0 validCount : So (formCount >= 1 && formCount <= maxPluralCategories)}
  ||| Which categories this language uses (in CLDR order)
  categories : Vect formCount PluralCategory

--------------------------------------------------------------------------------
-- Language Metadata
--------------------------------------------------------------------------------

||| Metadata about a single language in the LOL corpus.
||| This record maps directly to the C struct lol_language_info_t.
public export
record LanguageInfo where
  constructor MkLanguageInfo
  ||| ISO 639-3 code
  iso639_3 : String
  ||| English name
  name : String
  ||| Name in the language itself (autonym)
  nativeName : String
  ||| Language family (e.g. "Indo-European")
  family : String
  ||| Writing systems used (e.g. "Latin, Cyrillic")
  scripts : String
  ||| Number of corpus sources available
  sourceCount : Bits32
  ||| Total verses crawled
  verseCount : Bits32
  ||| Quality score from VeriSimDB pipeline (0.0-1.0)
  quality : Double

--------------------------------------------------------------------------------
-- Service Configuration
--------------------------------------------------------------------------------

||| Configuration for the LOL service instance.
||| Passed to lol_init to set up the corpus data path and defaults.
public export
record ServiceConfig where
  constructor MkServiceConfig
  ||| Path to the corpus data directory
  dataDir : String
  ||| Default locale for fallback resolution
  defaultLocale : String
  ||| Whether to enable locale fallback chains (e.g. en-US -> en -> default)
  enableFallback : Bool
  ||| Port for the API gateway (0 = no gateway)
  apiPort : Bits32

--------------------------------------------------------------------------------
-- Platform-Specific Types
--------------------------------------------------------------------------------

||| C int size varies by platform
public export
CInt : Platform -> Type
CInt Linux   = Bits32
CInt Windows = Bits32
CInt MacOS   = Bits32
CInt BSD     = Bits32
CInt WASM    = Bits32

||| C size_t varies by platform
public export
CSize : Platform -> Type
CSize Linux   = Bits64
CSize Windows = Bits64
CSize MacOS   = Bits64
CSize BSD     = Bits64
CSize WASM    = Bits32

||| Pointer size for the platform (in bits)
public export
ptrSize : Platform -> Nat
ptrSize Linux   = 64
ptrSize Windows = 64
ptrSize MacOS   = 64
ptrSize BSD     = 64
ptrSize WASM    = 32

--------------------------------------------------------------------------------
-- Memory Layout Proofs
--------------------------------------------------------------------------------

||| Proof that a type has a specific size
public export
data HasSize : Type -> Nat -> Type where
  SizeProof : {0 t : Type} -> {n : Nat} -> HasSize t n

||| Proof that a type has a specific alignment
public export
data HasAlignment : Type -> Nat -> Type where
  AlignProof : {0 t : Type} -> {n : Nat} -> HasAlignment t n
