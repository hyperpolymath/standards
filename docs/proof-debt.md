<!--
SPDX-License-Identifier: CC-BY-SA-4.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Proof Debt — standards

**Schema**: [`docs/TRUSTED-BASE-REDUCTION-POLICY.adoc`](./TRUSTED-BASE-REDUCTION-POLICY.adoc) — the canonical policy, originally landed via this repo at PR#203.

This file is the schema-conformant per-repo proof-debt index for
`hyperpolymath/standards` itself. The broader strategic proof goals
across the standards monorepo live in [`PROOF-NEEDS.md`](../PROOF-NEEDS.md);
this file enumerates the *specific* soundness-relevant escape hatches
the [`scripts/check-trusted-base.sh`](../scripts/check-trusted-base.sh)
CI gate (PR#211) detects in source.

## Marker count (2026-05-26)

15 syntactic markers detected by `check-trusted-base.sh`. 4 of those are
already covered by [`PROOF-NEEDS.md`](../PROOF-NEEDS.md) references
(matched by file path). The remaining 11 are enumerated below.

## (a) DISCHARGED in this repo

*(None yet — entries move here when a marker is removed or replaced by
a total/proven counterpart.)*

## (b) BUDGETED — tested with a refutation budget

*(None yet. Candidates for §(b) classification: the `a2ml/src/A2ML/`
test/parser code listed in §(d) below if any of it is covered by
property tests at a documented budget. Audit pending.)*

## (c) NECESSARY AXIOM

*(None — standards does not introduce metatheoretic axioms.)*

## (d) DEBT — actively to be closed

All entries are Idris2 top-level `partial` pragmas. The pragma waives the
Idris2 totality check on a definition. These are *not* in the same
disposition class as `believe_me` (which is a soundness break) — `partial`
is a *correctness-relevant* signal that "this function may not terminate
or may be incomplete" rather than an unconditional logical escape.

The recommended disposition for these is one of:

- Discharge by adding a `%hint` for totality OR rewriting to be
  structurally recursive on a fuel argument. (Most of the entries below
  are mutually-recursive descents over `Doc`/`Block`/`Section`/`Inline`
  types where Idris2's totality checker can't see through the
  mutual-recursion bundle.)
- Re-classify as §(b) by adding a property test that covers terminations
  empirically (e.g. Hedgehog over generated `Doc` ASTs to N=1e5).
- Re-classify as §(c) if the recursion is genuinely on a quotient that
  no Idris2-expressible measure terminates.

### `a2ml/src/A2ML/Converters.idr:339` — `testMarkdownRoundTrip`

- **Function**: `testMarkdownRoundTrip : Doc -> Bool`
- **Purpose**: tests that A2ML → Markdown → A2ML preserves structural
  block count.
- **Why partial**: invokes `parseDocument` which is the parser combinator
  return type and not currently total.
- **Owner**: @hyperpolymath
- **Deadline**: undeclared.

### `a2ml/src/A2ML/Converters.idr:349` — `testJSONRoundTrip` (or similar)

- **Function**: companion roundtrip test in the same file.
- **Same disposition** as the entry above.

### `a2ml/src/A2ML/TypedCore.idr:66` — `collectIds`

- **Function**: `collectIds : Doc -> List Id`
- **Why partial**: recursive descent over `MkDoc blocks` with mutually
  recursive `collectBlock : Block -> List Id`. The mutual bundle isn't
  visible to Idris2's totality check.
- **Owner**: @hyperpolymath
- **Deadline**: undeclared.

### `a2ml/src/A2ML/TypedCore.idr:77`, `:102`, `:107`, `:112`, `:121`, `:130`, `:135`, `:140` — mutual-recursion siblings

- Each of these is part of the same mutual-recursion family (`collectBlock`,
  `collectInline`, `collectSection`, etc.). All have the same root cause as
  `collectIds` above: Idris2's totality checker doesn't see through the
  mutual bundle on the `Doc`/`Block`/`Inline` AST.
- **Disposition**: collectively dischargeable by either (a) refactoring the
  mutual bundle into a single fuel-indexed recursion, or (b) re-classifying
  as §(b) BUDGETED with a property-test covering termination.
- **Owner**: @hyperpolymath
- **Deadline**: undeclared.

## How to update this file

When markers in source change:

1. Re-run `bash scripts/check-trusted-base.sh .` from the repo root.
2. If the count drops, move the resolved entries from §(d) → §(a).
3. If new markers appear, add them to §(d) with an owner + deadline.
4. The `check-trusted-base` CI job (governance-reusable's `trusted-base`
   step, landed in PR#211) will fail on un-annotated AND un-enumerated
   markers — so every change in source MUST be reflected here.

## Self-referential note

`standards` is both the **publisher** of the trusted-base reduction
policy and a **consumer** of it. This file demonstrates the schema
in-action; the eleven `partial` entries above are this repo's own debt
in §(d), exactly as downstream repos will record theirs.

The pattern: every estate repo with proof-bearing files seeds a
`docs/proof-debt.md` of this shape, then the maintainer triages each
§(d) entry into §(a)/(b)/(c) over time.

## Companion documents

- [`docs/TRUSTED-BASE-REDUCTION-POLICY.adoc`](./TRUSTED-BASE-REDUCTION-POLICY.adoc) — the canonical schema (PR#203 introduced this).
- [`scripts/check-trusted-base.sh`](../scripts/check-trusted-base.sh) — the CI gate (PR#211 introduced this).
- [`PROOF-NEEDS.md`](../PROOF-NEEDS.md) — the broader strategic proof-debt narrative for standards as a monorepo.
- [`docs/audits/2026-05-26-estate-proof-debt.md`](./audits/2026-05-26-estate-proof-debt.md) — the audit that motivated this whole policy chain (PR#195).

---

🤖 Initial seed by Claude Code, 2026-05-26. Replaces the implicit "we'll
get to it" debt with explicit §(d) entries that the trusted-base CI gate
can affirm.
