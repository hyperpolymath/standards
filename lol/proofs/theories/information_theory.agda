-- SPDX-License-Identifier: MIT AND LicenseRef-Palimpsest-0.8
-- SPDX-FileCopyrightText: 2024-2025 Ehsaneddin Asgari and Contributors
--
-- Information Theory Foundations
-- ==============================
-- Agda formalization of information-theoretic concepts

{-# OPTIONS --safe --without-K #-}

module InformationTheory where

open import Data.Nat using (ℕ; zero; suc)
open import Data.Float using (Float)
open import Data.Vec using (Vec; []; _∷_; map; zipWith; foldr)
open import Data.Product using (_×_; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- ============================================================================
-- Basic Definitions
-- ============================================================================

-- Non-negative real (approximated as Float for computation)
ℝ⁺ : Set
ℝ⁺ = Float

-- Probability value [0, 1]
record Probability : Set where
  constructor mkProb
  field
    value : Float
    -- Invariants would be proven separately

-- Probability distribution over n outcomes
record Distribution (n : ℕ) : Set where
  constructor mkDist
  field
    probabilities : Vec Probability n

-- ============================================================================
-- Entropy
-- ============================================================================

-- Shannon entropy: H(X) = -Σ p(x) log₂ p(x)
-- Uses convention that 0 log 0 = 0

postulate
  log₂ : Float → Float
  _*f_ : Float → Float → Float
  _+f_ : Float → Float → Float
  negateF : Float → Float

plogp : Probability → Float
plogp p = let v = Probability.value p in
  if v == 0.0 then 0.0
  else negateF (v *f log₂ v)

entropy : ∀ {n} → Distribution n → Float
entropy (mkDist ps) = foldr (λ _ → Float) _+f_ 0.0 (map plogp ps)

-- ============================================================================
-- KL-Divergence
-- ============================================================================

-- KL-Divergence: D_KL(P||Q) = Σ P(i) log(P(i)/Q(i))

postulate
  _/f_ : Float → Float → Float

kl-term : Probability → Probability → Float
kl-term p q =
  let pv = Probability.value p
      qv = Probability.value q
  in if pv == 0.0 then 0.0
     else pv *f log₂ (pv /f qv)

kl-divergence : ∀ {n} → Distribution n → Distribution n → Float
kl-divergence (mkDist ps) (mkDist qs) =
  foldr (λ _ → Float) _+f_ 0.0 (zipWith kl-term ps qs)

-- ============================================================================
-- Jensen-Shannon Divergence
-- ============================================================================

-- Midpoint of two distributions
midpoint-prob : Probability → Probability → Probability
midpoint-prob p q = mkProb ((Probability.value p +f Probability.value q) /f 2.0)

midpoint : ∀ {n} → Distribution n → Distribution n → Distribution n
midpoint (mkDist ps) (mkDist qs) = mkDist (zipWith midpoint-prob ps qs)

-- JSD: (D_KL(P||M) + D_KL(Q||M)) / 2
jensen-shannon : ∀ {n} → Distribution n → Distribution n → Float
jensen-shannon p q =
  let m = midpoint p q
  in (kl-divergence p m +f kl-divergence q m) /f 2.0

-- ============================================================================
-- Theorems (Postulated for now, to be proven)
-- ============================================================================

-- Entropy is non-negative
postulate
  entropy-nonnegative : ∀ {n} (d : Distribution n) → entropy d ≥ 0.0
    where
      _≥_ : Float → Float → Set

-- KL-divergence is non-negative (Gibbs' inequality)
postulate
  kl-nonnegative : ∀ {n} (p q : Distribution n) → kl-divergence p q ≥ 0.0
    where
      _≥_ : Float → Float → Set

-- Jensen-Shannon is symmetric
postulate
  js-symmetric : ∀ {n} (p q : Distribution n) →
    jensen-shannon p q ≡ jensen-shannon q p

-- Jensen-Shannon is bounded [0, 1]
postulate
  js-bounded : ∀ {n} (p q : Distribution n) →
    0.0 ≤ jensen-shannon p q × jensen-shannon p q ≤ 1.0
    where
      _≤_ : Float → Float → Set
