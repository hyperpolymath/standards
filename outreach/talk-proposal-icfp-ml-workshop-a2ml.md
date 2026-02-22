# ICFP ML Workshop 2026 - Talk Proposal

## Title
A2ML: Proving Markup Language Invariants with Dependent Types

## Presenters
**Jonathan D.A. Jewell**
The Open University
jonathan.jewell@open.ac.uk

## Format
25-minute talk + 5-minute Q&A

## Abstract (250 words)

Markup languages like Markdown and AsciiDoc excel at authoring but provide no guarantees about document structure. Authors discover broken cross-references, duplicate IDs, and missing required sections at render time—or worse, when reviewers point them out.

A2ML (Attested Markup Language) brings dependent type theory to markup. Documents compile to a formally verified core implemented in Idris2, where the type system proves structural invariants at compile time.

This talk presents A2ML's architecture and the techniques used to encode markup language semantics in dependent types. We demonstrate four key theorems proven at the type level:

1. **Uniqueness**: All element IDs are unique across the document
2. **Reference Resolution**: All cross-references resolve to defined IDs
3. **Validation**: Documents satisfy their structural requirements
4. **Attestation**: Cryptographic proofs bind documents to validation state

The implementation uses GADTs to represent document structure, indexed types to track ID sets, and auto-implicit proofs to discharge obligations during parsing. All proof computation happens at compile time; the runtime representation is zero-cost.

We evaluate A2ML on real-world documents (RFCs, academic papers, specifications) and show that compile-time verification catches errors undetectable by traditional markup parsers, with performance within 2-3x of CommonMark.

This work demonstrates that dependent types can bring formal guarantees to everyday programming tasks beyond traditional verification domains. A2ML is open source (PMPL-1.0) and available at https://github.com/hyperpolymath/a2ml.

## Detailed Description

### Motivation (5 minutes)

Problem statement: Markup languages lack structural guarantees. Demonstrate real-world failures:
- Academic paper with broken Figure references shipped to reviewers
- RFC with duplicate section IDs causing link ambiguity
- Specification missing required sections discovered late

Traditional solutions (linters, validators) operate post-hoc and provide weak guarantees. What if the markup language itself could prove correctness?

### Technical Approach (10 minutes)

**Two-layer architecture:**

1. **Surface Syntax** - Djot-like, easy to author:
```a2ml
# Introduction {#intro}

See @ref(methods) for details.

## Methods {#methods}
...
```

2. **Typed Core** - Idris2 with dependent types:
```idris
-- Proof that all IDs are unique
data Unique : List Id -> Type where
  UniqueNil  : Unique []
  UniqueCons : Not (Elem x xs) -> Unique xs -> Unique (x :: xs)

-- Proof that all references resolve
data AllIn : List Id -> List Id -> Type where
  AllInNil  : AllIn [] ids
  AllInCons : Elem x ids -> AllIn xs ids -> AllIn (x :: xs) ids

-- Validated document carries proofs
record ValidatedDoc where
  constructor MkValidatedDoc
  doc : Doc
  {auto uniqueProof : Unique ids}
  {auto resolvedProof : AllIn refs ids}
```

**Key techniques:**
- GADTs for document structure (Block, Inline, Directive)
- Indexed types tracking ID sets during parsing
- Auto-implicit proofs: type system finds proofs automatically
- Proof erasure: all verification code removed at runtime

**Progressive strictness:**
- Lax mode: warnings only (drafts)
- Checked mode: proofs required (final documents)
- Attested mode: cryptographic signatures (archival)

### Implementation Insights (5 minutes)

**Challenges solved:**

1. **Efficient proof search**: Decidable equality on IDs, totality checking prevents infinite loops
2. **Opaque payloads**: ByteString preservation for LaTeX/code blocks (no interpretation)
3. **Platform ABIs**: Compile-time platform detection ensures consistent memory layout
4. **Performance**: Proof computation at compile time → zero runtime cost

**Tooling:**
- CLI: `a2ml validate paper.a2ml` (compile-time errors for broken refs)
- REPL: interactive development with live validation
- LSP: editor integration (VS Code, Neovim, Emacs)
- Converters: A2ML → HTML/Markdown/LaTeX/Djot

### Evaluation (3 minutes)

**Real-world documents tested:**
- RFC 9110 (HTTP Semantics, 1.8MB) - found 3 duplicate IDs
- 50KB academic papers - caught 12 broken references across 5 papers
- A2ML self-documentation (dogfooding) - spec and README in A2ML format

**Performance benchmarks:**

| Document Size | A2ML | CommonMark | Overhead |
|---------------|------|------------|----------|
| Small (1KB)   | 50ms | 30ms       | 1.7x     |
| Medium (10KB) | 200ms| 100ms      | 2.0x     |
| Large (100KB) | 1s   | 500ms      | 2.0x     |

Overhead entirely from richer semantic validation—worth it for guarantees.

### Related Work (2 minutes)

**Comparison with:**
- **AsciiDoc/Markdown**: No structural guarantees
- **DocBook/DITA**: Schema validation (runtime), no proof-carrying code
- **Coq/Agda document generation**: Proof-heavy, not markup-oriented
- **Lightweight verification (Liquid Types)**: Refinement types, not full dependent types

**A2ML contribution:** Applies dependent type theory to practical markup authoring.

### Conclusion & Future Work (2 minutes)

**Takeaways:**
- Dependent types can prove practical invariants beyond traditional domains
- Compile-time verification catches errors impossible for parsers
- Zero runtime cost via proof erasure

**Future directions:**
- Multi-file projects with cross-document references
- Theorem prover backend (export A2ML structure to Coq/Lean)
- Richer semantics (e.g., prove figure references appear after figure definitions)

**Call to action:** A2ML v1.0.0 released today. Try it: https://github.com/hyperpolymath/a2ml

## Why This Talk Fits the ML Workshop

The ML Workshop focuses on advances in ML-family languages and applications of functional programming with strong type systems. A2ML demonstrates:

1. **Novel use of dependent types**: Encoding markup semantics in types
2. **Practical verification**: Real-world documents with formal guarantees
3. **Implementation techniques**: Auto-implicit proofs, indexed types, GADTs
4. **Tooling lessons**: Building developer tools around dependently-typed cores

This aligns with the workshop's themes of type system innovation and practical applications of advanced type theory.

## Additional Information

**Presentation materials:**
- Live demo: `a2ml validate` catching broken references
- Code walkthrough: key Idris2 proof structures
- Performance graphs: A2ML vs. traditional markup parsers

**Availability:** All source code, documentation, and benchmarks are open source under PMPL-1.0 license.

**Audience level:** Intermediate ML programmers familiar with GADTs; dependent types background helpful but not required (will explain key concepts).

**Related publications:**
- arXiv preprint (pending): "A2ML: Attested Markup Language with Dependent Types"
- IANA media type registration (pending): `application/vnd.a2ml`

---

**Contact:**
Jonathan D.A. Jewell
jonathan.jewell@open.ac.uk
https://github.com/hyperpolymath/a2ml
