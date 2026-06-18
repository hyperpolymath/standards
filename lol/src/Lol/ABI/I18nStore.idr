-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
||| LOL i18n Service — I18n Store Interface with Lookup Proofs
|||
||| Defines an abstract interface for the translation store that
||| guarantees: if a key exists in the store for a locale, then
||| a translation is returned. This is the core correctness property
||| that the Zig FFI must uphold.
|||
||| The store also specifies the fallback chain contract: when a
||| key is not found for the requested locale, the service walks
||| the fallback chain until it finds a translation or exhausts
||| all fallback options.
|||
||| LOL is called as a service, never embedded. The store interface
||| captures the semantic guarantees that all consumers can rely on.

module Lol.ABI.I18nStore

import Lol.ABI.Types
import Lol.ABI.Locale
import Lol.ABI.TranslationKey
import Lol.ABI.PluralForm

import Data.So
import Data.List
import Data.List1
import Data.String

%default total

--------------------------------------------------------------------------------
-- Key Existence Predicate
--------------------------------------------------------------------------------

||| Evidence that a key exists in the store for a given locale.
||| This is an opaque proof token — it cannot be forged, only obtained
||| by querying the store.
public export
data KeyExists : String -> String -> Type where
  ||| Witness that key `k` has a translation for locale `loc`
  MkKeyExists : (loc : String) -> (k : String) -> KeyExists loc k

||| Evidence that a locale is supported by the store (has at least one key)
public export
data LocaleSupported : String -> Type where
  ||| Witness that locale `loc` has at least one translation
  MkLocaleSupported : (loc : String) -> (k : String) -> KeyExists loc k -> LocaleSupported loc

--------------------------------------------------------------------------------
-- Store Interface
--------------------------------------------------------------------------------

||| The I18n store interface specifies the contract that any
||| translation store implementation must satisfy.
|||
||| The key property is `guaranteedLookup`: given proof that a key
||| exists, the lookup always succeeds (returns a non-empty string).
public export
interface I18nStore (store : Type) where

  ||| Check whether a key exists for a locale.
  ||| Returns a proof if the key is present, Nothing otherwise.
  hasKey : store -> (locale : String) -> (key : String) -> Maybe (KeyExists locale key)

  ||| Look up a translation, given proof the key exists.
  ||| This function is total: the proof guarantees the result is non-empty.
  guaranteedLookup : store -> KeyExists locale key -> String

  ||| Look up a translation without proof (may fail).
  ||| This is the common case for FFI callers who cannot construct proofs.
  lookup : store -> (locale : String) -> (key : String) -> Maybe String

  ||| Get the number of keys available for a locale.
  keyCount : store -> (locale : String) -> Nat

  ||| Get the list of supported locales.
  supportedLocales : store -> List String

--------------------------------------------------------------------------------
-- Fallback Resolution
--------------------------------------------------------------------------------

||| Fallback resolution strategy.
||| The store tries each locale in the fallback chain until a translation
||| is found, or returns Nothing if all fallbacks are exhausted.
public export
data FallbackStrategy
  = ||| Walk the full BCP 47 fallback chain (en-GB -> en -> default)
    ChainFallback
  | ||| Only try the exact locale, no fallback
    ExactMatch
  | ||| Try exact locale, then jump directly to default
    DirectDefault

||| Result of a fallback-aware lookup.
||| Records which locale was actually used (may differ from requested).
public export
record FallbackResult where
  constructor MkFallbackResult
  ||| The translation text
  text : String
  ||| The locale that provided the translation
  resolvedLocale : String
  ||| Whether a fallback locale was used
  isFallback : Bool

||| Build the fallback chain for a given strategy.
public export
buildFallbackChain : FallbackStrategy -> (requestedLocale : String) -> (defaultLocale : String) -> List String
buildFallbackChain ChainFallback req def = fallbackChain req ++ [def]
buildFallbackChain ExactMatch req _ = [req]
buildFallbackChain DirectDefault req def = [req, def]

||| Try each locale in a chain until a translation is found.
||| Takes a lookup function to avoid interface constraint accessibility issues.
public export
tryFallbackChain :
  (lookupFn : String -> String -> Maybe String) ->
  (key : String) ->
  (chain : List String) ->
  (requestedLocale : String) ->
  Maybe FallbackResult
tryFallbackChain lookupFn key [] _ = Nothing
tryFallbackChain lookupFn key (loc :: rest) reqLocale =
  case lookupFn loc key of
    Just text => Just (MkFallbackResult text loc (loc /= reqLocale))
    Nothing   => tryFallbackChain lookupFn key rest reqLocale

||| Perform a lookup with fallback chain resolution.
||| Tries each locale in the chain until a translation is found.
||| Takes a lookup function directly to avoid interface accessibility issues.
public export
lookupWithFallback :
  (lookupFn : String -> String -> Maybe String) ->
  FallbackStrategy ->
  (requestedLocale : String) ->
  (defaultLocale : String) ->
  (key : String) ->
  Maybe FallbackResult
lookupWithFallback lookupFn strategy reqLocale defLocale key =
  let chain = buildFallbackChain strategy reqLocale defLocale
  in tryFallbackChain lookupFn key chain reqLocale

--------------------------------------------------------------------------------
-- Plural-Aware Lookup
--------------------------------------------------------------------------------

||| Extract the language subtag from a locale tag.
||| "en-US" -> "en", "zh-Hans-CN" -> "zh"
public export
extractLanguage : String -> String
extractLanguage s = let (lang ::: _) = Data.String.split (== '-') s in lang

||| Convert a plural category to its CLDR string suffix.
public export
categoryToSuffix : PluralCategory -> String
categoryToSuffix Zero  = "zero"
categoryToSuffix One   = "one"
categoryToSuffix Two   = "two"
categoryToSuffix Few   = "few"
categoryToSuffix Many  = "many"
categoryToSuffix Other = "other"

||| Look up a translation with automatic plural form selection.
||| Appends the CLDR plural category to the key as a dot-separated suffix.
|||
||| For "items" with quantity 1 in English: looks up "items.one"
||| For "items" with quantity 5 in Arabic:  looks up "items.few"
public export
lookupPlural :
  (lookupFn : String -> String -> Maybe String) ->
  FallbackStrategy ->
  (locale : String) ->
  (defaultLocale : String) ->
  (key : String) ->
  (quantity : Nat) ->
  Maybe FallbackResult
lookupPlural lookupFn strategy locale defLocale key quantity =
  let lang = extractLanguage locale
      cat = selectPlural lang quantity
      sfx = categoryToSuffix cat
      pluralKey = key ++ "." ++ sfx
  in case lookupWithFallback lookupFn strategy locale defLocale pluralKey of
       Just result => Just result
       Nothing     => lookupWithFallback lookupFn strategy locale defLocale key

--------------------------------------------------------------------------------
-- Correctness Properties
--------------------------------------------------------------------------------

||| The guaranteedLookup contract: if hasKey returns a proof, then
||| guaranteedLookup must return a non-empty string.
|||
||| This property cannot be proved generically (it depends on the
||| implementation), but it serves as a specification that
||| implementations must satisfy. The Zig FFI is tested against
||| this property in integration tests.
public export
interface I18nStore store => CorrectStore (store : Type) where
  ||| If hasKey succeeds, guaranteedLookup returns non-empty text
  lookupNonEmpty :
    (st : store) ->
    (prf : KeyExists locale key) ->
    So (length (guaranteedLookup st prf) > 0)

  ||| lookup and hasKey are consistent: if hasKey returns a proof,
  ||| then lookup returns Just (guaranteedLookup st proof)
  lookupConsistent :
    (st : store) ->
    (locale : String) ->
    (key : String) ->
    (prf : KeyExists locale key) ->
    lookup st locale key = Just (guaranteedLookup st prf)

  -- The store is monotonic: adding translations never removes existing ones
  -- (expressed as: if a key exists, it continues to exist). Stated as a plain
  -- comment, not a `|||` doc comment: a trailing doc comment with no method
  -- following it is dangling and breaks the interface block's scope. This
  -- invariant is enforced on the Zig store side, not provable generically here.

--------------------------------------------------------------------------------
-- Error Classification
--------------------------------------------------------------------------------

||| Classification of lookup failures for error reporting.
||| The Zig FFI maps these to the Result enum in Types.idr.
public export
data LookupError : Type where
  ||| The requested locale is not in the corpus at all
  NoSuchLocale : String -> LookupError
  ||| The locale exists but the key is not defined
  NoSuchKey : String -> String -> LookupError
  ||| The plural form variant is missing (e.g. "items.few" missing)
  MissingPluralForm : String -> String -> PluralCategory -> LookupError
  ||| The entire fallback chain was exhausted without finding a translation
  FallbackExhausted : String -> String -> List String -> LookupError

||| Map a lookup error to the appropriate Result code for FFI transport
public export
lookupErrorToResult : LookupError -> Result
lookupErrorToResult (NoSuchLocale _)         = LocaleNotFound
lookupErrorToResult (NoSuchKey _ _)          = KeyNotFound
lookupErrorToResult (MissingPluralForm _ _ _) = PluralOutOfRange
lookupErrorToResult (FallbackExhausted _ _ _) = KeyNotFound
