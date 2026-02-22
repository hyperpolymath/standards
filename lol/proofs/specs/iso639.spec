-- SPDX-License-Identifier: MIT AND LicenseRef-Palimpsest-0.8
-- SPDX-FileCopyrightText: 2024-2025 Ehsaneddin Asgari and Contributors
--
-- ISO 639 Language Code Specification
-- ====================================
-- Formal specification of language code validation and conversion

module ISO639.Spec where

-- ============================================================================
-- Types
-- ============================================================================

-- ISO 639-1: 2-letter codes
record ISO639_1 : Set where
  field
    code : String
    length_proof : length code ≡ 2
    format_proof : all isLowerAlpha code

-- ISO 639-3: 3-letter codes
record ISO639_3 : Set where
  field
    code : String
    length_proof : length code ≡ 3
    format_proof : all isLowerAlpha code

-- Language scope classification
data LanguageScope : Set where
  Individual    : LanguageScope  -- Single language
  Macrolanguage : LanguageScope  -- Group of related languages
  Special       : LanguageScope  -- Special codes (und, mul, mis, zxx)

-- Language type classification
data LanguageType : Set where
  Living      : LanguageType  -- Currently spoken
  Historical  : LanguageType  -- Distinct historical stage
  Extinct     : LanguageType  -- No living speakers
  Ancient     : LanguageType  -- Extinct before recorded history
  Constructed : LanguageType  -- Artificial language

-- Full language entry
record LanguageEntry : Set where
  field
    iso639_3 : ISO639_3
    iso639_2b : Maybe ISO639_3      -- Bibliographic code
    iso639_2t : Maybe ISO639_3      -- Terminological code
    iso639_1 : Maybe ISO639_1
    scope : LanguageScope
    type_ : LanguageType
    name : String

-- ============================================================================
-- Validation Properties
-- ============================================================================

-- Property: ISO 639-1 codes are exactly 2 lowercase letters
theorem iso639_1_format :
  ∀ (code : String) →
    isValidISO639_1 code ↔
    (length code ≡ 2 ∧ all isLowerAlpha code)

-- Property: ISO 639-3 codes are exactly 3 lowercase letters
theorem iso639_3_format :
  ∀ (code : String) →
    isValidISO639_3 code ↔
    (length code ≡ 3 ∧ all isLowerAlpha code)

-- Property: Valid codes are non-empty
theorem valid_codes_nonempty :
  ∀ (code : String) →
    isValidISO639_1 code ∨ isValidISO639_3 code →
    length code > 0

-- Property: Normalization is idempotent
theorem normalize_idempotent :
  ∀ (code : String) →
    normalize (normalize code) ≡ normalize code

-- Property: Normalization produces lowercase
theorem normalize_lowercase :
  ∀ (code : String) →
    all isLowerAlpha (normalize code)

-- ============================================================================
-- Special Codes
-- ============================================================================

-- Special ISO 639 codes
undetermined : ISO639_3
undetermined = mkISO639_3 "und"

multiple : ISO639_3
multiple = mkISO639_3 "mul"

miscellaneous : ISO639_3
miscellaneous = mkISO639_3 "mis"

noLinguistic : ISO639_3
noLinguistic = mkISO639_3 "zxx"

-- Property: Special codes are mutually distinct
theorem special_codes_distinct :
  undetermined ≢ multiple ∧
  undetermined ≢ miscellaneous ∧
  undetermined ≢ noLinguistic ∧
  multiple ≢ miscellaneous ∧
  multiple ≢ noLinguistic ∧
  miscellaneous ≢ noLinguistic

-- Property: Special codes are valid ISO 639-3
theorem special_codes_valid :
  isValidISO639_3 "und" ∧
  isValidISO639_3 "mul" ∧
  isValidISO639_3 "mis" ∧
  isValidISO639_3 "zxx"

-- Property: isSpecial correctly identifies special codes
theorem is_special_complete :
  ∀ (code : ISO639_3) →
    isSpecial code ↔
    (code ≡ undetermined ∨ code ≡ multiple ∨
     code ≡ miscellaneous ∨ code ≡ noLinguistic)

-- ============================================================================
-- Conversion Properties
-- ============================================================================

-- Property: ISO 639-1 to 639-3 conversion is injective
-- (Different 639-1 codes map to different 639-3 codes)
theorem iso1_to_iso3_injective :
  ∀ (a b : ISO639_1) →
    toISO639_3 a ≡ toISO639_3 b →
    a ≡ b

-- Property: Conversion preserves validity
theorem conversion_preserves_validity :
  ∀ (code : ISO639_1) →
    isValidISO639_3 (toISO639_3 code)

-- Property: Valid ISO 639-3 codes convert to themselves
theorem iso3_identity :
  ∀ (code : ISO639_3) →
    toISO639_3 code ≡ Some code

-- Property: Code type detection is total
theorem detect_type_total :
  ∀ (code : String) →
    detectCodeType code ∈ {Some ISO639_1, Some ISO639_3, None}

-- ============================================================================
-- Registry Properties
-- ============================================================================

-- Property: Empty registry has zero entries
theorem empty_registry :
  count (empty ()) ≡ 0

-- Property: Adding entry increases count
theorem add_increases_count :
  ∀ (registry : Registry) (entry : LanguageEntry) →
    count (add registry entry) ≡ count registry + 1

-- Property: Added entries can be found
theorem add_then_find :
  ∀ (registry : Registry) (entry : LanguageEntry) →
    let registry' = add registry entry in
    findByCode registry' (code (iso639_3 entry)) ≡ Some entry

-- Property: Finding by ISO 639-1 works when present
theorem find_by_iso1 :
  ∀ (registry : Registry) (entry : LanguageEntry) →
    isSome (iso639_1 entry) →
    let registry' = add registry entry in
    findByCode registry' (fromJust (iso639_1 entry)) ≡ Some entry

-- Property: Finding by name is case-insensitive
theorem find_by_name_case_insensitive :
  ∀ (registry : Registry) (entry : LanguageEntry) →
    let registry' = add registry entry in
    findByName registry' (toLower (name entry)) ≡ Some entry ∧
    findByName registry' (toUpper (name entry)) ≡ Some entry

-- ============================================================================
-- Completeness Properties
-- ============================================================================

-- Property: Major languages have ISO 639-1 codes
-- (Not all 639-3 codes have 639-1 equivalents)
theorem major_languages_have_iso1 :
  ∀ (entry : LanguageEntry) →
    isMajorLanguage entry →
    isSome (iso639_1 entry)

-- Property: All entries have valid ISO 639-3 codes
theorem all_entries_have_iso3 :
  ∀ (entry : LanguageEntry) →
    isValidISO639_3 (code (iso639_3 entry))
