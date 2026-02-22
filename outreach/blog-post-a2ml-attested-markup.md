# A2ML: What If Markdown Had Formal Proofs?

**Author:** Jonathan D.A. Jewell
**Date:** 2026-01-30
**Tags:** markup-languages, formal-verification, dependent-types, idris2

---

## The Problem with Markup Languages

You're writing a 50-page academic paper in Markdown. You have 73 cross-references to figures, tables, and sections. You reference `Figure 12` in the text, but there's no Figure 12—you deleted it during editing.

Your Markdown renderer doesn't catch this. Your PDF builder doesn't catch this. Your collaborators don't catch this. The error ships to reviewers.

**What if your markup language could prove at compile time that all references resolve?**

## Introducing A2ML

A2ML (Attested Markup Language) is a lightweight markup format that looks like Markdown but compiles to a formally verified core.

### A Simple Example

```a2ml
# My Research Paper

@abstract:
This paper presents A2ML, a markup language with formal guarantees.
@end

## Introduction

Documents need structure. We propose a solution using dependent types.

See @ref(methods) for details.

## Methods

We implemented A2ML in Idris2...

@refs:
[1] Brady, E. (2021). Idris 2: Quantitative Type Theory in Practice.
@end
```

When you run `a2ml validate paper.a2ml`, the type system proves:
- All references resolve (no broken `@ref()` calls)
- All IDs are unique (no duplicate section IDs)
- Required sections exist (e.g., `@abstract` is present)

Not at runtime. At *compile time*. With mathematical proofs.

## How It Works

A2ML has two layers:

**1. Surface Syntax** (what you write)

Looks like Djot or Markdown. Easy to author, easy to read.

**2. Typed Core** (what the compiler sees)

Implemented in Idris2 with dependent types:

```idris
-- Proof that all IDs are unique
data Unique : List Id -> Type where
  UniqueNil  : Unique []
  UniqueCons : Not (Elem x xs) -> Unique xs -> Unique (x :: xs)

-- Proof that all references resolve
data AllIn : List Id -> List Id -> Type where
  AllInNil  : AllIn [] ids
  AllInCons : Elem x ids -> AllIn xs ids -> AllIn (x :: xs) ids

-- Validated document with proofs attached
record ValidatedDoc where
  constructor MkValidatedDoc
  doc : Doc
  {auto uniqueProof : Unique ids}
  {auto resolvedProof : AllIn refs ids}
```

These proofs are computed at compile time and erased at runtime. Zero runtime cost for the guarantee.

## Progressive Strictness

A2ML has three modes:

**Lax**: Parse anything, warnings only
```bash
$ a2ml check draft.a2ml
⚠ Warning: Unresolved reference @ref(fig:missing)
✓ Valid syntax (lax mode)
```

**Checked**: References must resolve
```bash
$ a2ml validate draft.a2ml
✗ Error: Unresolved reference @ref(fig:missing)
Validation failed
```

**Attested**: Full proofs + cryptographic signatures
```bash
$ a2ml validate --attested paper.a2ml
✓ All proofs discharge
✓ Signature valid (Ed25519)
✓ Document attested
```

You choose the level based on your needs. Draft documents? Use lax mode. Final paper? Use attested mode.

## Opaque Payloads

Here's the killer feature: **byte-for-byte preservation** of critical content.

```a2ml
@opaque(lang="latex", id="eq-pythagorean"):
\begin{equation}
a^2 + b^2 = c^2
\end{equation}
@end
```

The parser stores this as a `ByteString` without interpretation. Math equations, code blocks, binary data—all preserved exactly as written. Renderers can transform for display, but the original is always there.

For archival documents, this is crucial. Your PDF renderer might change. Your LaTeX engine might change. But the original bytes are immutable and attested.

## Real-World Usage

A2ML is dogfooding itself:

- `README.a2ml` - Main documentation in A2ML
- `SPEC.a2ml` - Specification in A2ML
- CI validates all `.a2ml` files automatically
- Generates HTML/Markdown/LaTeX from A2ML sources

If A2ML can document its own specification with formal proofs, it works.

## Performance

"Formal verification sounds slow."

Benchmarks show A2ML is competitive with Markdown:

| Document Size | A2ML | CommonMark |
|---------------|------|------------|
| Small (1KB) | <50ms | ~30ms |
| Medium (10KB) | <200ms | ~100ms |
| Large (100KB) | <1s | ~500ms |

Target: 2-3x CommonMark speed while providing guarantees impossible in traditional markup.

The Idris2 compiler optimizes away all the proof machinery. At runtime, you're just parsing text and validating structure—fast operations.

## Tooling

A2ML v1.0.0 (released today!) includes:

**CLI:**
```bash
# Validate document
a2ml validate paper.a2ml

# Convert to HTML
a2ml convert a2ml html paper.a2ml > paper.html

# Quick syntax check
a2ml check draft.a2ml
```

**REPL:**
```
a2ml> :load paper.a2ml
✓ Loaded

a2ml> :validate
✓ Document is valid
  Unique IDs: 12
  Resolved refs: 5

a2ml> :convert html
<!DOCTYPE html>...
```

**LSP** (architecture ready, implementation in v1.1):
- Real-time validation in VS Code, Neovim, Emacs
- Auto-completion for directives
- Go-to-definition for references

## When Should You Use A2ML?

**Good candidates:**
- Academic papers (guaranteed reference resolution)
- Technical specifications (structural requirements)
- Long-term archival documents (byte-for-byte preservation)
- API documentation (validated cross-references)

**Stick with Markdown for:**
- GitHub READMEs (Markdown is the standard)
- Quick notes (A2ML is overkill)
- Blog posts (unless you need formal structure)

## The Trade-Off

A2ML trades some convenience for guarantees:

**Markdown:**
```markdown
## Introduction

See Section 5 for details.
```

If you delete Section 5, Markdown won't notice.

**A2ML:**
```a2ml
## Introduction {#intro}

See @ref(methods) for details.

## Methods {#methods}

...
```

If you delete the Methods section, A2ML *won't compile*. The type system proves all references resolve.

That's a trade-off: slight verbosity (explicit IDs, `@ref()` syntax) for strong guarantees (broken references are compile errors, not runtime surprises).

## Try It

```bash
# Clone repository
git clone https://github.com/hyperpolymath/a2ml.git
cd a2ml

# Build CLI
cd cli && ./build.sh

# Try it
echo "# Hello, A2ML" | ../build/exec/a2ml check -
```

Or read the spec (written in A2ML, of course): https://github.com/hyperpolymath/a2ml/blob/main/SPEC.a2ml

## What's Next

**v1.1.0** (Q2 2026):
- LSP server implementation
- VS Code extension
- Incremental parsing for editor responsiveness

**v2.0.0** (2027):
- Multi-file projects with cross-document references
- Blockchain-based attestation
- GPU-accelerated parsing

## The Bigger Picture

A2ML is part of my research on formally verified software systems at The Open University. If we can prove markup language invariants at compile time, what else can we prove?

Next: Formally verified configuration management (spoiler: it's called K9, and it uses Nickel).

## Conclusion

Markdown revolutionized documentation by making authoring simple. A2ML adds one thing: *guarantees*.

Your references resolve. Your IDs are unique. Your structure is valid. All proven, not assumed.

For documents that matter—papers, specs, standards—that's worth a bit of extra syntax.

---

**Links:**
- A2ML v1.0.0: https://github.com/hyperpolymath/a2ml/releases/tag/v1.0.0
- Specification: https://github.com/hyperpolymath/a2ml/blob/main/SPEC.a2ml
- arXiv Paper: arXiv:XXXX.XXXXX (pending acceptance)
- Try the REPL: https://github.com/hyperpolymath/a2ml/tree/main/repl

**About the Author:**
Jonathan D.A. Jewell is a researcher at The Open University working on formal methods for software preservation. He builds things that prove things.

Email: jonathan.jewell@open.ac.uk
