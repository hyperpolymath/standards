-- SPDX-License-Identifier: MIT AND LicenseRef-Palimpsest-0.8
-- SPDX-FileCopyrightText: 2024-2025 Ehsaneddin Asgari and Contributors
--
-- Statistical Properties Specification
-- =====================================
-- Formal specification of statistical functions used in 1000Langs

module Statistics.Spec where

-- Probability distribution: non-negative reals that sum to 1
record Distribution (n : ℕ) : Set where
  field
    values : Vec ℝ n
    non_negative : ∀ i → values[i] ≥ 0
    sums_to_one : sum values ≡ 1

-- ============================================================================
-- Shannon Entropy
-- ============================================================================

-- Definition: H(X) = -Σ p(x) log p(x)
entropy : Distribution n → ℝ
entropy d = negate (sum (map (λ p → p * log₂ p) (values d)))

-- Property: Entropy is non-negative
theorem entropy_nonnegative : ∀ (d : Distribution n) → entropy d ≥ 0

-- Property: Entropy is maximized by uniform distribution
theorem entropy_maximum :
  ∀ (d : Distribution n) →
    entropy d ≤ log₂ n

-- Property: Entropy equals log(n) for uniform distribution
theorem entropy_uniform :
  ∀ (n : ℕ) →
    let uniform = Distribution.uniform n in
    entropy uniform ≡ log₂ n

-- ============================================================================
-- KL-Divergence
-- ============================================================================

-- Definition: D_KL(P||Q) = Σ P(i) log(P(i)/Q(i))
kl_divergence : Distribution n → Distribution n → ℝ
kl_divergence p q = sum (zipWith (λ pi qi → pi * log₂ (pi / qi)) (values p) (values q))

-- Property: KL-divergence is non-negative (Gibbs' inequality)
theorem kl_nonnegative :
  ∀ (p q : Distribution n) →
    kl_divergence p q ≥ 0

-- Property: KL-divergence is zero iff distributions are identical
theorem kl_zero_iff_equal :
  ∀ (p q : Distribution n) →
    kl_divergence p q ≡ 0 ↔ p ≡ q

-- Property: KL-divergence is asymmetric (generally)
-- Note: This is a non-property, showing asymmetry
example kl_asymmetric :
  ∃ (p q : Distribution n) →
    kl_divergence p q ≢ kl_divergence q p

-- ============================================================================
-- Jensen-Shannon Divergence
-- ============================================================================

-- Midpoint distribution
midpoint : Distribution n → Distribution n → Distribution n
midpoint p q = Distribution.fromVec ((values p + values q) / 2)

-- Definition: JSD(P||Q) = (D_KL(P||M) + D_KL(Q||M)) / 2
jensen_shannon : Distribution n → Distribution n → ℝ
jensen_shannon p q =
  let m = midpoint p q in
  (kl_divergence p m + kl_divergence q m) / 2

-- Property: Jensen-Shannon is symmetric
theorem js_symmetric :
  ∀ (p q : Distribution n) →
    jensen_shannon p q ≡ jensen_shannon q p

-- Property: Jensen-Shannon is bounded [0, 1] (using log base 2)
theorem js_bounded :
  ∀ (p q : Distribution n) →
    0 ≤ jensen_shannon p q ∧ jensen_shannon p q ≤ 1

-- Property: Jensen-Shannon is zero iff distributions are identical
theorem js_zero_iff_equal :
  ∀ (p q : Distribution n) →
    jensen_shannon p q ≡ 0 ↔ p ≡ q

-- Property: Square root of JSD is a metric
theorem js_sqrt_metric :
  let d = λ p q → sqrt (jensen_shannon p q) in
  IsMetric d

-- ============================================================================
-- Distance Functions
-- ============================================================================

-- Euclidean distance
euclidean : Vec ℝ n → Vec ℝ n → ℝ
euclidean a b = sqrt (sum (zipWith (λ ai bi → (ai - bi)²) a b))

-- Property: Euclidean distance is a metric
theorem euclidean_metric : IsMetric euclidean

-- Cosine similarity (converted to distance)
cosine_distance : Vec ℝ n → Vec ℝ n → ℝ
cosine_distance a b = 1 - (dot a b) / (norm a * norm b)

-- Property: Cosine distance is bounded [0, 2]
theorem cosine_bounded :
  ∀ (a b : Vec ℝ n) →
    0 ≤ cosine_distance a b ∧ cosine_distance a b ≤ 2

-- ============================================================================
-- Normalization Properties
-- ============================================================================

-- Property: Normalization preserves ratios
theorem normalize_preserves_ratios :
  ∀ (v : Vec ℝ n) (i j : Fin n) →
    let nv = normalize v in
    v[i] / v[j] ≡ nv[i] / nv[j]

-- Property: Normalized vectors sum to 1
theorem normalize_sums_to_one :
  ∀ (v : Vec ℝ n) →
    sum (normalize v) ≡ 1

-- Property: Z-score normalization results in mean 0
theorem zscore_mean_zero :
  ∀ (v : Vec ℝ n) →
    mean (zscore_normalize v) ≡ 0

-- Property: Z-score normalization results in std dev 1
theorem zscore_std_one :
  ∀ (v : Vec ℝ n) →
    std_dev (zscore_normalize v) ≡ 1
