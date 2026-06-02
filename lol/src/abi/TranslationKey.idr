-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
||| LOL i18n Service — Type-Safe Translation Keys
|||
||| Translation keys in LOL are dot-separated namespace paths
||| (e.g. "app.greeting", "errors.not_found", "nav.home").
||| This module provides a refined type that proves keys are
||| non-empty and structurally valid at compile time.
|||
||| Key format: segment(.segment)*
|||   segment: [a-z][a-z0-9_]* (lowercase start, alphanumeric + underscore)
|||
||| Examples:
|||   "greeting"          — single segment
|||   "app.greeting"      — two segments
|||   "errors.not_found"  — nested with underscore
|||   "ui.nav.home"       — deeply nested

module Lol.ABI.TranslationKey

import Data.So
import Data.String
import Data.List
import Data.List1

%default total

--------------------------------------------------------------------------------
-- Key Segment Validation
--------------------------------------------------------------------------------

||| Check if a character is valid in a key segment (lowercase alpha, digit, underscore)
public export
isSegmentChar : Char -> Bool
isSegmentChar c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_'

||| Check if a character is a valid segment start (lowercase alpha only)
public export
isSegmentStart : Char -> Bool
isSegmentStart c = c >= 'a' && c <= 'z'

||| Validate a single key segment.
||| A segment must start with a lowercase letter, followed by zero or more
||| lowercase letters, digits, or underscores.
public export
isValidSegment : String -> Bool
isValidSegment s =
  case unpack s of
    []        => False
    (c :: cs) => isSegmentStart c && all isSegmentChar cs

||| Validate a complete translation key.
||| Must be non-empty, consist of dot-separated segments, and each
||| segment must be a valid identifier.
public export
isValidKey : String -> Bool
isValidKey s =
  let segments = split (== '.') s
  in all isValidSegment (Data.List1.forget segments)

--------------------------------------------------------------------------------
-- Validated Translation Key Type
--------------------------------------------------------------------------------

||| A translation key with a proof that it is non-empty and structurally valid.
||| The proof is checked at construction time (runtime validation) or can be
||| provided statically for known-good literals.
public export
record ValidKey where
  constructor MkValidKey
  ||| The raw key string (e.g. "app.greeting")
  key : String
  ||| Proof that the key is structurally valid
  {auto 0 valid : So (isValidKey key)}

||| Attempt to create a validated key from a raw string.
||| Returns Nothing if the key is empty or structurally invalid.
public export
mkKey : (s : String) -> Maybe ValidKey
mkKey s = case choose (isValidKey s) of
            Left prf  => Just (MkValidKey s)
            Right _   => Nothing

||| Get the raw string from a validated key.
public export
keyStr : ValidKey -> String
keyStr k = k.key

||| Count the number of segments (namespace depth) in a key.
||| "greeting" = 1, "app.greeting" = 2, "ui.nav.home" = 3
public export
keyDepth : ValidKey -> Nat
keyDepth k = length (Data.List1.forget (split (== '.') k.key))

||| Get the top-level namespace of a key.
||| "app.greeting" -> "app", "greeting" -> "greeting"
public export
keyNamespace : ValidKey -> String
keyNamespace k =
  head (split (== '.') k.key)

||| Get the final segment (leaf) of a key.
||| "app.greeting" -> "greeting", "greeting" -> "greeting"
public export
keyLeaf : ValidKey -> String
keyLeaf k =
  last (split (== '.') k.key)

--------------------------------------------------------------------------------
-- Key Operations
--------------------------------------------------------------------------------

||| Append a suffix to a key (used for plural form keys).
||| "items" + "one" -> "items.one"
||| "app.items" + "other" -> "app.items.other"
public export
appendSuffix : ValidKey -> String -> Maybe ValidKey
appendSuffix k suffix =
  let combined = k.key ++ "." ++ suffix
  in mkKey combined

||| Prepend a namespace to a key.
||| "errors" + "not_found" -> "errors.not_found"
public export
prependNamespace : String -> ValidKey -> Maybe ValidKey
prependNamespace ns k =
  let combined = ns ++ "." ++ k.key
  in mkKey combined

||| Strip the top-level namespace from a key.
||| "app.greeting" -> Just "greeting"
||| "greeting" -> Nothing (no namespace to strip)
public export
stripNamespace : ValidKey -> Maybe ValidKey
stripNamespace k =
  let (_ ::: rest) = split (== '.') k.key
  in case rest of
       []   => Nothing
       _    => mkKey (joinBy "." rest)

--------------------------------------------------------------------------------
-- Proofs
--------------------------------------------------------------------------------

-- Note: A proof that a valid key has at least one segment (keyDepth >= 1)
-- would require reasoning about split on runtime strings, which is not
-- reducible at compile time. The property holds by construction:
-- isValidKey requires at least one valid segment, and split on a
-- non-empty string always produces at least one part (List1).
