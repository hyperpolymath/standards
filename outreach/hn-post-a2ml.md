# Hacker News Post: A2ML

## Title
A2ML: What if Markdown had formal proofs?

## Post Body

I've been working on A2ML (Attested Markup Language), a markup format that looks like Markdown but compiles to a formally verified core using dependent types in Idris2.

**The Problem:**
You're writing a 50-page paper with 73 cross-references. You reference Figure 12, but there's no Figure 12—you deleted it during editing. Your Markdown renderer doesn't catch this. Your PDF builder doesn't catch this. The error ships to reviewers.

**The A2ML Approach:**
Documents compile to a typed core where the type system proves invariants:
- All references resolve (no broken @ref() calls)
- All IDs are unique (no duplicate section IDs)
- Required sections exist (e.g., @abstract is present)

Not at runtime. At compile time. With mathematical proofs.

**Example:**
```a2ml
# My Research Paper

@abstract:
This paper presents A2ML, a markup language with formal guarantees.
@end

## Introduction {#intro}

See @ref(methods) for details.

## Methods {#methods}

We implemented A2ML in Idris2...
```

If you delete the Methods section, A2ML won't compile. The type system proves all references resolve.

**How it works:**

The surface syntax is Djot-like (easy to write), but it compiles to an Idris2 core with dependent types:

```idris
-- Proof that all IDs are unique
data Unique : List Id -> Type where
  UniqueNil  : Unique []
  UniqueCons : Not (Elem x xs) -> Unique xs -> Unique (x :: xs)

-- Validated document with proofs attached
record ValidatedDoc where
  constructor MkValidatedDoc
  doc : Doc
  {auto uniqueProof : Unique ids}
  {auto resolvedProof : AllIn refs ids}
```

All proof computation happens at compile time and is erased at runtime. Zero runtime cost.

**Progressive Strictness:**
- Lax mode: warnings only (for drafts)
- Checked mode: proofs required (for final docs)
- Attested mode: cryptographic signatures (for archival)

**Performance:**
Benchmarks show A2ML is 2-3x slower than CommonMark, but that's entirely from richer semantic validation—worth it for the guarantees.

**What's included in v1.0.0 (released today):**
- CLI: `a2ml validate paper.a2ml`
- REPL: interactive development
- Converters: A2ML → HTML/Markdown/LaTeX/Djot
- LSP architecture (implementation in v1.1)

**Dogfooding:**
The A2ML specification and README are written in A2ML. If A2ML can document itself with formal proofs, it works.

**Use cases:**
- Academic papers (guaranteed reference resolution)
- Technical specifications (structural requirements)
- Long-term archival (byte-for-byte preservation with opaque payloads)
- API documentation (validated cross-references)

**Why build this?**
Dependent types can bring formal guarantees to everyday programming tasks beyond traditional verification domains. Markup languages seem trivial, but they have real invariants—why not prove them?

**Trade-off:**
You write `@ref(methods)` instead of "Section 5", and you need explicit IDs (`{#methods}`). Slight verbosity for strong guarantees: broken references become compile errors, not runtime surprises.

**Links:**
- Repo: https://github.com/hyperpolymath/a2ml
- v1.0.0 Release: https://github.com/hyperpolymath/a2ml/releases/tag/v1.0.0
- Spec (written in A2ML): https://github.com/hyperpolymath/a2ml/blob/main/SPEC.a2ml
- arXiv paper: pending acceptance (cs.PL)

**License:** PMPL-1.0 (Polymath Public Meta-License) - open source

Curious what HN thinks about applying dependent types to markup languages. Is this overkill, or are there other domains where compile-time proofs would be valuable?

---

## Expected Questions & Answers

**Q: Isn't this just stricter linting?**
A: No. Linters check properties post-hoc and can have false positives/negatives. A2ML's type system mathematically proves properties hold—it's impossible to compile a document where references don't resolve. The proof is part of the artifact.

**Q: Why not use existing schema validators (JSON Schema, XML Schema)?**
A: Those validate structure, not semantic properties like "all references resolve". They also operate at runtime, not compile-time. A2ML proves invariants during compilation, with zero runtime cost.

**Q: Idris2 has a small ecosystem. Why not use a more popular language?**
A: Dependent types are essential for this. Rust/TypeScript don't have them. Agda/Coq are too heavy for practical tooling. Idris2 is the sweet spot: dependent types + practical compilation + decent performance.

**Q: What's the performance overhead for real documents?**
A: 2-3x CommonMark for 100KB documents (~1s vs 500ms). The overhead is entirely from semantic validation (checking uniqueness, resolution). For documents where correctness matters, this is a worthwhile trade.

**Q: Can I use this for my blog?**
A: You could, but probably shouldn't. Markdown is fine for blogs. A2ML is for documents where structural guarantees matter: papers, specs, long-term archives. Use the right tool for the job.

**Q: What's an "opaque payload"?**
A: Content stored as raw bytes without interpretation—critical for LaTeX equations, code blocks, binary data. Your PDF renderer might change, but the original bytes are immutable and attested.

**Q: Why three strictness modes?**
A: Drafts don't need full proofs (lax mode). Final documents do (checked mode). Archival documents need cryptographic attestation (attested mode). Progressive strictness lets you choose based on document lifecycle.

**Q: Is there a migration path from Markdown?**
A: Yes. The CLI includes `a2ml convert markdown a2ml document.md > document.a2ml`. Not all conversions are perfect (Markdown has ambiguities), but it's a starting point.

**Q: Can I embed A2ML in other languages (like Rustdoc)?**
A: Not yet, but it's on the roadmap. The architecture supports it: parse A2ML string → get validated AST → render however you want.

**Q: What's IANA registration?**
A: Media type registration (application/vnd.a2ml) so A2ML files have official MIME types. Submitted, pending approval. Useful for HTTP Content-Type headers, browser handling, etc.

---

## Posting Strategy

**Best subreddits:**
- /r/programming (main audience)
- /r/ProgrammingLanguages (type systems focus)
- /r/haskell (dependent types adjacent)
- /r/Idris (Idris community)
- /r/dependent_types (if it exists)

**Best time:** Tuesday-Thursday, 9-11am EST (peak HN activity)

**Follow-up engagement:**
- Respond to questions within 30 minutes (HN moves fast)
- Provide code examples for skeptics
- Link to specific files in repo for "show me the code" requests
- Acknowledge valid criticisms (e.g., "yes, Idris2 ecosystem is small—that's a trade-off")

**What NOT to do:**
- Don't oversell ("revolutionize documentation!")
- Don't dismiss Markdown ("Markdown is bad")
- Don't argue with "this is overkill" comments (acknowledge use-case fit)
- Don't get defensive about performance (2-3x is honest, own it)
