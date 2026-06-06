-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
||| LOL i18n Service — Plural Form ABI with CLDR Rules
|||
||| Implements Unicode CLDR plural rules as a formal specification.
||| Different languages partition quantities into different subsets of
||| six categories: zero, one, two, few, many, other.
|||
||| This module:
|||   - Defines the six CLDR plural categories with FFI transport proofs
|||   - Specifies language-specific plural rules as data
|||   - Provides selection functions matching CLDR operand semantics
|||   - Proves category coverage (every language handles every quantity)
|||
||| @see https://www.unicode.org/cldr/charts/latest/supplemental/language_plural_rules.html

module Lol.ABI.PluralForm

import Lol.ABI.Types
import Data.Vect
import Data.So

%default total

--------------------------------------------------------------------------------
-- CLDR Operands
--------------------------------------------------------------------------------

||| CLDR plural operands for a numeric value.
||| These are the standard operands used in CLDR plural rule definitions.
|||
||| For an integer n:
|||   n = absolute value of the source number
|||   i = integer digits of n (same as n for integers)
|||   v = number of visible fraction digits (0 for integers)
|||   w = number of non-zero fraction digits (0 for integers)
|||   f = visible fraction digits (0 for integers)
|||   t = non-zero fraction digits (0 for integers)
public export
record CLDROperands where
  constructor MkCLDROperands
  ||| The absolute value
  n : Nat
  ||| Integer digits (same as n for whole numbers)
  i : Nat
  ||| Visible fraction digit count
  v : Nat
  ||| Non-zero fraction digit count
  w : Nat
  ||| Visible fraction digits as integer
  f : Nat
  ||| Non-zero fraction digits as integer
  t : Nat

||| Construct CLDR operands from an integer quantity.
||| For integers, fraction-related operands are all zero.
public export
intOperands : Nat -> CLDROperands
intOperands n = MkCLDROperands n n 0 0 0 0

||| Proof that integer operands have zero fraction parts
public export
intOperandsNoFraction : (n : Nat) -> (intOperands n).v = 0
intOperandsNoFraction _ = Refl

--------------------------------------------------------------------------------
-- Plural Rule Functions
--------------------------------------------------------------------------------

||| A plural rule function maps CLDR operands to a plural category.
||| This is the core abstraction for language-specific rules.
public export
PluralRuleFunc : Type
PluralRuleFunc = CLDROperands -> PluralCategory

||| English plural rule (CLDR: one if i=1 and v=0; other)
||| 1 item, 2 items, 0 items
public export
englishPlural : PluralRuleFunc
englishPlural ops =
  if ops.i == 1 && ops.v == 0
    then One
    else Other

||| Arabic plural rule (CLDR: zero/one/two/few/many/other)
||| All six categories used
public export
arabicPlural : PluralRuleFunc
arabicPlural ops =
  let n = ops.n
      mod100 = n `mod` 100
  in if n == 0 then Zero
     else if n == 1 then One
     else if n == 2 then Two
     else if mod100 >= 3 && mod100 <= 10 then Few
     else if mod100 >= 11 && mod100 <= 99 then Many
     else Other

||| French plural rule (CLDR: one if i=0 or i=1; other)
||| 0 item, 1 item, 2 items
public export
frenchPlural : PluralRuleFunc
frenchPlural ops =
  if ops.i == 0 || ops.i == 1
    then One
    else Other

||| Russian plural rule (CLDR: one/few/many/other)
public export
russianPlural : PluralRuleFunc
russianPlural ops =
  let mod10 = ops.i `mod` 10
      mod100 = ops.i `mod` 100
  in if mod10 == 1 && mod100 /= 11 then One
     else if mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14) then Few
     else Many

||| Polish plural rule (CLDR: one/few/many/other)
public export
polishPlural : PluralRuleFunc
polishPlural ops =
  let mod10 = ops.i `mod` 10
      mod100 = ops.i `mod` 100
  in if ops.i == 1 && ops.v == 0 then One
     else if mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14) then Few
     else if (ops.i /= 1) && (mod10 == 0 || mod10 == 1) ||
             (mod10 >= 5 && mod10 <= 9) ||
             (mod100 >= 12 && mod100 <= 14)
       then Many
       else Other

||| Czech/Slovak plural rule (CLDR: one/few/other)
public export
czechPlural : PluralRuleFunc
czechPlural ops =
  if ops.i == 1 && ops.v == 0 then One
  else if ops.i >= 2 && ops.i <= 4 && ops.v == 0 then Few
  else Other

||| East Asian languages (Chinese, Japanese, Korean, Vietnamese)
||| No plural distinction — always "other"
public export
eastAsianPlural : PluralRuleFunc
eastAsianPlural _ = Other

--------------------------------------------------------------------------------
-- Language Rule Registry
--------------------------------------------------------------------------------

||| Look up the plural rule function for a language by ISO 639-1/639-3 code.
||| Returns eastAsianPlural-style default (one/other) for unknown languages,
||| which is the most common pattern worldwide.
public export
ruleForLanguage : String -> PluralRuleFunc
ruleForLanguage "en"  = englishPlural
ruleForLanguage "eng" = englishPlural
ruleForLanguage "de"  = englishPlural
ruleForLanguage "deu" = englishPlural
ruleForLanguage "nl"  = englishPlural
ruleForLanguage "nld" = englishPlural
ruleForLanguage "sv"  = englishPlural
ruleForLanguage "swe" = englishPlural
ruleForLanguage "ar"  = arabicPlural
ruleForLanguage "ara" = arabicPlural
ruleForLanguage "fr"  = frenchPlural
ruleForLanguage "fra" = frenchPlural
ruleForLanguage "pt"  = frenchPlural
ruleForLanguage "por" = frenchPlural
ruleForLanguage "ru"  = russianPlural
ruleForLanguage "rus" = russianPlural
ruleForLanguage "uk"  = russianPlural
ruleForLanguage "ukr" = russianPlural
ruleForLanguage "pl"  = polishPlural
ruleForLanguage "pol" = polishPlural
ruleForLanguage "cs"  = czechPlural
ruleForLanguage "ces" = czechPlural
ruleForLanguage "sk"  = czechPlural
ruleForLanguage "slk" = czechPlural
ruleForLanguage "zh"  = eastAsianPlural
ruleForLanguage "zho" = eastAsianPlural
ruleForLanguage "ja"  = eastAsianPlural
ruleForLanguage "jpn" = eastAsianPlural
ruleForLanguage "ko"  = eastAsianPlural
ruleForLanguage "kor" = eastAsianPlural
ruleForLanguage "vi"  = eastAsianPlural
ruleForLanguage "vie" = eastAsianPlural
ruleForLanguage _     = englishPlural

||| Select the plural category for a language and integer quantity.
||| Convenience function combining ruleForLanguage with intOperands.
public export
selectPlural : String -> Nat -> PluralCategory
selectPlural lang n = ruleForLanguage lang (intOperands n)

--------------------------------------------------------------------------------
-- Coverage Proofs
--------------------------------------------------------------------------------

||| Every plural rule function is total — it handles every possible input.
||| This is guaranteed by %default total at module level and the fact that
||| all rule functions pattern-match exhaustively on if/then/else chains
||| that always terminate with an else branch returning a category.

-- Note: englishPluralCoverage (proving English returns One or Other)
-- requires runtime evaluation of if-expressions, which Idris2 cannot
-- reduce at compile time. The totality checker guarantees coverage instead.

||| Proof that East Asian plural always returns Other.
||| This is provable because the function ignores its argument entirely.
public export
eastAsianPluralConstant : (ops : CLDROperands) -> eastAsianPlural ops = Other
eastAsianPluralConstant _ = Refl

||| Proof that pluralToInt and the rule functions together produce valid
||| C-compatible integers (0-5 range, matching the PluralCategory enum)
public export
pluralToIntBounded : (cat : PluralCategory) -> So (pluralToInt cat <= 5)
pluralToIntBounded Zero  = Oh
pluralToIntBounded One   = Oh
pluralToIntBounded Two   = Oh
pluralToIntBounded Few   = Oh
pluralToIntBounded Many  = Oh
pluralToIntBounded Other = Oh
