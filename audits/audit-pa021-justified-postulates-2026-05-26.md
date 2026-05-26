<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
-->

# Audit: justified real-analysis postulates (PA021)

**Auditor**: Jonathan D.A. Jewell
**Date**: 2026-05-26
**Scope**: 1 PA021 ProofDrift finding covering 4 postulates in `lol/proofs/theories/information_theory.agda`.
**Cross-reference**: campaign tracker [hyperpolymath/panic-attack#32](https://github.com/hyperpolymath/panic-attack/issues/32).
**Registry**: `audits/assail-classifications.a2ml`.

## Context

`lol/proofs/theories/information_theory.agda` contains 4 postulates:

1. `entropy-nonnegative : ∀ {n} (d : Distribution n) → entropy d ≥ 0.0` — follows from `-p·log(p) ≥ 0` on `0 ≤ p ≤ 1`.
2. `kl-nonnegative : ∀ {n} (p q : Distribution n) → kl-divergence p q ≥ 0.0` — Gibbs' inequality / log-sum inequality.
3. `js-symmetric : ∀ {n} (p q : Distribution n) → jensen-shannon p q ≡ jensen-shannon q p` — follows from symmetry of KL terms in the midpoint construction.
4. `js-bounded : ∀ {n} (p q : Distribution n) → 0.0 ≤ jensen-shannon p q × jensen-shannon p q ≤ 1.0` — Lin 1991; upper bound via Jensen's inequality.

The file's source-internal comment at line 111-112 reads:

> proofs would require a real-analysis formalisation (e.g. over ℝ). Classified as justified postulates, not proof debt.

Each statement has a textbook proof, but discharging them in Agda requires:

- A real-analysis library formalising ℝ as a Cauchy/Dedekind-complete ordered field
- Log / exp / Jensen's-inequality lemmas
- Probability-distribution structures (likely via `Vec` of reals summing to 1 with positivity)

This is a substantial library-development effort orthogonal to the standards repo's purpose. Per the file's own classification, these are **justified postulates** — known-true theorems from classical information theory whose Agda formalisation is out of scope.

## Anti-gameability

The registry is `audits/assail-classifications.a2ml`. The classification covers only this specific file; any new postulate in another proof file remains visible. Additional postulates added inside `information_theory.agda` would require updating both this audit doc and the rationale, both visible in the diff.

## Verification

No proof source touched; `agda --check` rebuild is moot (input unchanged from main).

Refs hyperpolymath/panic-attack#32.
