-- SPDX-License-Identifier: MIT AND LicenseRef-Palimpsest-0.8
-- SPDX-FileCopyrightText: 2024-2025 Ehsaneddin Asgari and Contributors
--
-- Parallel Corpus Alignment Specification
-- ========================================
-- Formal specification of alignment properties for multilingual corpora

module Alignment.Spec where

-- ============================================================================
-- Types
-- ============================================================================

-- Verse reference (canonical identifier)
record VerseRef : Set where
  constructor mkVerseRef
  field
    book : BookCode    -- 3-letter book code
    chapter : ℕ⁺       -- Positive natural
    verse : ℕ⁺         -- Positive natural

-- Language code (ISO 639-3)
record LangCode : Set where
  constructor mkLangCode
  field
    code : String
    valid : IsISO639_3 code

-- Translation: text in a specific language
record Translation : Set where
  field
    language : LangCode
    text : String
    ref : VerseRef

-- Aligned segment: same verse in multiple languages
record AlignedSegment : Set where
  field
    ref : VerseRef
    translations : Map LangCode String
    non_empty : size translations > 0

-- Corpus alignment: collection of aligned segments
record CorpusAlignment : Set where
  field
    segments : Map VerseRef AlignedSegment
    languages : Set LangCode

-- ============================================================================
-- Alignment Properties
-- ============================================================================

-- Property: Reference consistency
-- All translations in a segment refer to the same verse
theorem segment_reference_consistency :
  ∀ (seg : AlignedSegment) (lang : LangCode) →
    lang ∈ keys (translations seg) →
    lookup lang (translations seg) references seg.ref

-- Property: Language coverage monotonicity
-- Adding a translation cannot decrease language coverage
theorem coverage_monotonic :
  ∀ (corpus : CorpusAlignment) (trans : Translation) →
    let corpus' = addTranslation corpus trans in
    languages corpus ⊆ languages corpus'

-- Property: Segment uniqueness
-- Each verse reference appears at most once
theorem segment_uniqueness :
  ∀ (corpus : CorpusAlignment) (ref : VerseRef) →
    count (λ seg → seg.ref ≡ ref) (segments corpus) ≤ 1

-- ============================================================================
-- Alignment Correctness
-- ============================================================================

-- Property: Transitivity of alignment
-- If verse V in language A aligns with language B,
-- and B aligns with C for the same verse,
-- then A aligns with C
theorem alignment_transitive :
  ∀ (corpus : CorpusAlignment) (ref : VerseRef) (a b c : LangCode) →
    hasTranslation corpus ref a →
    hasTranslation corpus ref b →
    hasTranslation corpus ref c →
    aligned corpus ref a b ∧ aligned corpus ref b c →
    aligned corpus ref a c

-- Property: Alignment reflexivity
-- Any translation is aligned with itself
theorem alignment_reflexive :
  ∀ (corpus : CorpusAlignment) (ref : VerseRef) (lang : LangCode) →
    hasTranslation corpus ref lang →
    aligned corpus ref lang lang

-- Property: Alignment symmetry
-- Alignment is symmetric across languages
theorem alignment_symmetric :
  ∀ (corpus : CorpusAlignment) (ref : VerseRef) (a b : LangCode) →
    aligned corpus ref a b →
    aligned corpus ref b a

-- ============================================================================
-- Coverage Properties
-- ============================================================================

-- Coverage: proportion of segments with translation
coverage : CorpusAlignment → LangCode → ℚ
coverage corpus lang =
  let total = size (segments corpus)
      present = count (λ seg → lang ∈ keys (translations seg)) (segments corpus)
  in present / total

-- Property: Coverage is bounded [0, 1]
theorem coverage_bounded :
  ∀ (corpus : CorpusAlignment) (lang : LangCode) →
    0 ≤ coverage corpus lang ∧ coverage corpus lang ≤ 1

-- Property: Full coverage means all segments have translation
theorem full_coverage :
  ∀ (corpus : CorpusAlignment) (lang : LangCode) →
    coverage corpus lang ≡ 1 ↔
    (∀ seg ∈ segments corpus → lang ∈ keys (translations seg))

-- Quality classification
data AlignmentQuality : Set where
  Complete : AlignmentQuality                    -- All languages present
  Partial  : (missing : ℕ) → AlignmentQuality   -- Some missing
  Sparse   : AlignmentQuality                    -- < 50% coverage

-- Property: Quality classification is total
theorem quality_total :
  ∀ (corpus : CorpusAlignment) (ref : VerseRef) →
    ∃! (q : AlignmentQuality) → qualityOf corpus ref ≡ q

-- ============================================================================
-- Builder Properties
-- ============================================================================

-- Property: Empty builder has no segments
theorem empty_builder :
  ∀ (name : String) →
    let b = Builder.make name in
    size (segments (Builder.build b)) ≡ 0

-- Property: Adding translation increases segment or translation count
theorem add_translation_increases :
  ∀ (b : Builder) (trans : Translation) →
    let b' = Builder.addTranslation b trans in
    let c = Builder.build b
        c' = Builder.build b' in
    size (segments c') ≥ size (segments c) ∧
    totalTranslations c' > totalTranslations c

-- Property: Build is idempotent (building twice gives same result)
theorem build_idempotent :
  ∀ (b : Builder) →
    Builder.build b ≡ Builder.build b

-- ============================================================================
-- Verse Reference Properties
-- ============================================================================

-- Property: Canonical ID is injective
theorem canonical_id_injective :
  ∀ (r1 r2 : VerseRef) →
    toCanonicalId r1 ≡ toCanonicalId r2 →
    r1 ≡ r2

-- Property: Parsing is inverse of serialization
theorem parse_serialize_inverse :
  ∀ (ref : VerseRef) →
    fromString (toString ref) ≡ Some ref

-- Property: Verse ordering is total
theorem verse_order_total :
  ∀ (r1 r2 : VerseRef) →
    compare r1 r2 ∈ {LT, EQ, GT}

-- Property: Verse ordering is transitive
theorem verse_order_transitive :
  ∀ (r1 r2 r3 : VerseRef) →
    compare r1 r2 ≡ LT →
    compare r2 r3 ≡ LT →
    compare r1 r3 ≡ LT
