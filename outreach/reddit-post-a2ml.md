# Reddit Post: A2ML

## Title
I made a markup language where broken references are compile errors, not runtime surprises

## Post Body (r/programming)

**TL;DR:** A2ML is Markdown + dependent types. If you reference `Figure 12` but there's no Figure 12, it won't compile. Mathematical proofs at compile time, zero runtime cost.

---

Hey r/programming,

I've been working on A2ML (Attested Markup Language)—a markup format that looks like Markdown but uses dependent types to prove structural invariants at compile time.

**Why?**

Ever written a long document and broken a cross-reference? Your Markdown/AsciiDoc renderer doesn't catch it. Your build tool doesn't catch it. Reviewers catch it. That sucks.

**What A2ML does differently:**

Documents compile to an Idris2 core where the type system proves:
- All `@ref(id)` calls resolve to actual IDs
- All IDs are unique (no duplicate `{#intro}` tags)
- Required sections exist (e.g., `@abstract` is present)

Not at runtime—at *compile time*. With mathematical proofs.

**Example:**

```a2ml
# My Paper

@abstract:
This paper presents A2ML.
@end

## Introduction {#intro}

See @ref(methods) for details.

## Methods {#methods}

We implemented A2ML in Idris2...
```

Delete the Methods section? Compile error. The type system proves all references resolve.

**How it works:**

Surface syntax is Djot-like (easy to write). It compiles to an Idris2 core:

```idris
-- Proof that all IDs are unique
data Unique : List Id -> Type where
  UniqueNil  : Unique []
  UniqueCons : Not (Elem x xs) -> Unique xs -> Unique (x :: xs)

-- Validated document with proofs
record ValidatedDoc where
  constructor MkValidatedDoc
  doc : Doc
  {auto uniqueProof : Unique ids}
  {auto resolvedProof : AllIn refs ids}
```

Proofs computed at compile time, erased at runtime. Zero cost.

**Progressive strictness:**

- **Lax mode**: Warnings only (drafts)
- **Checked mode**: Proofs required (final docs)
- **Attested mode**: Cryptographic signatures (archival)

Choose based on document lifecycle.

**Performance:**

2-3x slower than CommonMark (e.g., 1s vs 500ms for 100KB docs). Overhead entirely from semantic validation. For documents where correctness matters, worth it.

**What's included (v1.0.0 released today):**

- CLI: `a2ml validate paper.a2ml`
- REPL: interactive development
- Converters: A2ML → HTML/Markdown/LaTeX/Djot
- LSP architecture (implementation coming in v1.1)

**Dogfooding:**

The A2ML spec and README are written in A2ML. If it can document itself, it works.

**Use cases:**

- Academic papers (guaranteed reference resolution)
- Technical specs (structural requirements)
- Archival docs (byte-for-byte preservation)
- API docs (validated cross-references)

**Why build this?**

Dependent types can solve practical problems beyond traditional verification. Markup has real invariants—why not prove them?

**Trade-off:**

You write `@ref(methods)` instead of "Section 5". Explicit IDs required (`{#methods}`). Slight verbosity for strong guarantees.

**Links:**

- Repo: https://github.com/hyperpolymath/a2ml
- Release: https://github.com/hyperpolymath/a2ml/releases/tag/v1.0.0
- Spec (in A2ML): https://github.com/hyperpolymath/a2ml/blob/main/SPEC.a2ml

**Questions I expect:**

- *Isn't this overkill?* For blogs, yes. For papers/specs, no.
- *Why not just lint?* Linters check post-hoc. A2ML proves at compile time.
- *Why Idris2?* Need dependent types. Rust/TS don't have them. Agda/Coq too heavy.
- *What about Markdown compatibility?* `a2ml convert markdown a2ml doc.md` works.

Curious what you think. Is compile-time proof for markup useful, or am I solving a problem that doesn't exist?

---

## Alternate Post Body (r/ProgrammingLanguages)

**TL;DR:** Dependent types for markup languages. Prove cross-references resolve at compile time.

---

Hi r/ProgrammingLanguages,

I built A2ML—a markup language with a dependently-typed core. Thought this community might appreciate the type system aspects.

**The Problem:**

Markup languages (Markdown, AsciiDoc) have structural invariants:
- Cross-references should resolve
- IDs should be unique
- Required sections should exist

But these aren't checked until render time (or never). Can we do better?

**The A2ML Approach:**

Surface syntax compiles to an Idris2 core where invariants are *proven* at the type level:

```idris
-- Proof that all IDs in a list are unique
data Unique : List Id -> Type where
  UniqueNil  : Unique []
  UniqueCons : Not (Elem x xs) -> Unique xs -> Unique (x :: xs)

-- Proof that all refs resolve to IDs
data AllIn : List Id -> List Id -> Type where
  AllInNil  : AllIn [] ids
  AllInCons : Elem x ids -> AllIn xs ids -> AllIn (x :: xs) ids

-- Document carrying proofs
record ValidatedDoc where
  constructor MkValidatedDoc
  doc : Doc
  ids : List Id
  refs : List Id
  {auto 0 uniqueProof : Unique ids}
  {auto 0 resolvedProof : AllIn refs ids}
```

The `auto` implicits mean the type checker finds proofs automatically during parsing.

**Key Type System Features Used:**

1. **GADTs** for document structure (`Block`, `Inline`, `Directive`)
2. **Indexed types** tracking ID sets during parsing
3. **Auto-implicit proofs** (`{auto 0 ...}`) for proof search
4. **Proof erasure** (runtime rep is zero-cost)
5. **Totality checking** (`%default total`) prevents infinite loops

**Interesting Type-Level Challenges:**

- How to encode "all refs resolve" at the type level? `AllIn refs ids` works but requires decidable equality on `Id`
- How to make proof search tractable? Keep proofs structurally simple (lists, membership)
- How to handle opaque payloads (LaTeX blocks)? Use `ByteString` with no interpretation

**Example Theorems Proven:**

```idris
-- Theorem: Uniqueness is decidable
uniqueDecidable : (ids : List Id) -> Dec (Unique ids)

-- Theorem: Reference resolution is decidable
allInDecidable : (refs, ids : List Id) -> Dec (AllIn refs ids)

-- Theorem: Validated docs satisfy invariants
validatedImpliesUnique : ValidatedDoc -> Unique ids
validatedImpliesResolved : ValidatedDoc -> AllIn refs ids
```

All proofs discharge at compile time. Runtime representation has no proof overhead.

**Performance:**

Compile time for 100KB document: ~1s (vs ~500ms for CommonMark). Overhead from semantic validation, not proof search.

**Progressive Strictness:**

Three modes:
- **Lax**: Parse, warn about unresolved refs (no proofs)
- **Checked**: Require proofs (won't compile with broken refs)
- **Attested**: Proofs + Ed25519 signatures

User chooses based on document lifecycle.

**Why This Matters:**

Dependent types are often seen as academic. A2ML shows they can solve practical problems (markup correctness) with acceptable performance.

Also demonstrates that **auto-implicit proofs** can make dependent types ergonomic—user writes markup, compiler finds proofs.

**Open Questions:**

1. Can we prove richer properties? (e.g., "figures referenced before they're defined")
2. Can we extend to multi-file projects with cross-doc refs?
3. Can we export proofs to other systems (Coq, Lean)?

**Links:**

- Repo: https://github.com/hyperpolymath/a2ml
- Spec (written in A2ML): https://github.com/hyperpolymath/a2ml/blob/main/SPEC.a2ml
- Core types: https://github.com/hyperpolymath/a2ml/blob/main/src/A2ML/Core.idr

Feedback welcome, especially on the type-level encoding of markup semantics!

---

## Posting Strategy

**Best subreddits:**

| Subreddit | Post Version | Best Day/Time |
|-----------|--------------|---------------|
| r/programming | Main post (TL;DR + use cases) | Tue-Thu, 9-11am EST |
| r/ProgrammingLanguages | Type system focus | Any weekday |
| r/haskell | Cross-post ("dependent types in practice") | Any weekday |
| r/Idris | Community-specific | Any time |
| r/dependent_types | If exists | Any time |

**Engagement tactics:**

- Respond to "overkill" comments with use-case fit (papers, not blogs)
- Link to specific proof functions for "show me the types" requests
- Acknowledge Idris2 ecosystem size (trade-off for dependent types)
- Offer to explain specific type-level encodings (Unique, AllIn)

**What NOT to do:**

- Don't claim it replaces Markdown for everything
- Don't oversell performance (2-3x is honest)
- Don't argue with skeptics (acknowledge valid criticisms)
- Don't ignore "why not language X" questions

**Follow-up content ideas:**

- "A2ML type system deep dive" blog post
- "Proof search performance optimization" technical write-up
- Video: "Dependent types for everyday problems"
