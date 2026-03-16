---
title: Specification
date: 2026-03-16
order: 2
---

# A2ML Specification

## Overview

A2ML (Attested Markup Language) is a text-based document format that combines human-readable markup with machine-verifiable attestation blocks. This page describes the format as currently implemented in the [pandoc-a2ml](https://github.com/hyperpolymath/pandoc-a2ml) toolchain.

## File Format

- **Extension:** `.a2ml`
- **Media type:** `application/vnd.a2ml` (IANA registration pending)
- **Encoding:** UTF-8
- **Line endings:** LF (Unix-style) recommended; CRLF accepted

## Basic Syntax

A2ML uses a Markdown-like syntax with additional constructs for attestation and structured metadata.

### Headings

```
# Top-level heading
## Second-level heading
### Third-level heading
```

### Inline Formatting

```
**Bold text** for emphasis
*Italic text* for secondary emphasis
[Link text](https://example.com) for hyperlinks
@ref(section-id) for internal cross-references
`inline code` for identifiers
```

### Lists

```
- Unordered list item
- Another item
* Asterisk syntax also works

1. Ordered list item
2. Another item
```

### Code Blocks

````
```language
code goes here
```
````

### Comments

```
;; Scheme-style comments are stripped during parsing.
;; Use these for notes that should not appear in output.
```

## Directive Blocks

Directives are the core extension that distinguishes A2ML from plain Markdown. They carry structured, machine-readable content within annotated blocks.

### Syntax

```
@directive-name:
Content of the directive.
Can span multiple lines.
@end
```

The parser collects everything between `@directive-name:` and `@end` into a named container (rendered as a `<div>` with `class="directive-name"` in HTML output).

### Standard Directives

The following directives have conventional meaning across the A2ML ecosystem. Tooling may validate or enforce these.

#### `@abstract`

A short summary of the document's purpose. Typically one to three sentences.

```
@abstract:
A2ML is a typed, attested markup format for AI agent identity.
@end
```

#### `@attestation`

A trust claim with structured fields. This is the fundamental unit of the A2ML trust model.

```
@attestation:
agent-id: hypatia-scanner-v3
attested-by: rhodibot
trust-level: verified
timestamp: 2026-03-16T14:30:00Z
signature: sha256:9f86d08...
capabilities:
  - static-analysis
  - secret-detection
  - dependency-audit
scope: repository
@end
```

**Fields:**

| Field | Required | Description |
|-------|----------|-------------|
| `agent-id` | Yes | Unique identifier for the agent being attested |
| `attested-by` | Yes | Identifier of the attesting authority |
| `trust-level` | Yes | One of: `self-declared`, `peer-reviewed`, `verified`, `audited` |
| `timestamp` | Yes | ISO 8601 timestamp of the attestation |
| `signature` | Recommended | Cryptographic signature (algorithm:hash) |
| `capabilities` | Yes | List of declared capabilities |
| `scope` | No | Scope of the attestation (e.g., `repository`, `organization`, `global`) |

#### `@refs`

References and citations. Used for linking to external specifications, standards, or related documents.

```
@refs:
[1] Attested Markup Language Specification (draft), 2026
[2] SLSA Supply Chain Framework, https://slsa.dev
@end
```

#### `@policy`

Declares a policy constraint that tooling should enforce.

```
@policy:
require: attestation.trust-level >= verified
enforce: ci-pipeline
action: block-merge
@end
```

#### `@provenance`

Records the origin and lineage of the document.

```
@provenance:
created-by: Jonathan D.A. Jewell
created: 2026-01-15
last-modified: 2026-03-16
source: https://github.com/hyperpolymath/a2ml-spec
version: 0.3.0
@end
```

## Attestation Chains

The real power of A2ML comes from **attestation chains** — sequences of attestations where each one references or builds upon previous ones.

```
;; Agent declares its own capabilities
@attestation:
agent-id: codebot-v2
attested-by: codebot-v2
trust-level: self-declared
capabilities:
  - code-review
  - style-checking
@end

;; Security team verifies the agent
@attestation:
agent-id: codebot-v2
attested-by: security-team
trust-level: verified
verifies: codebot-v2/self-declared/2026-03-10
signature: sha256:a3f2c8...
@end

;; Auditor provides highest-level attestation
@attestation:
agent-id: codebot-v2
attested-by: external-auditor
trust-level: audited
verifies: codebot-v2/verified/2026-03-12
audit-report: https://audits.example.com/codebot-v2
signature: sha256:7b1e4d...
@end
```

Each successive attestation raises the trust level and creates an auditable trail from `self-declared` through `verified` to `audited`.

## Trust Levels

| Level | Meaning | Typical Attester |
|-------|---------|------------------|
| `self-declared` | Agent claims its own capabilities | The agent itself |
| `peer-reviewed` | Another agent or team has reviewed the claims | A peer agent or team lead |
| `verified` | A security or operations team has validated behaviour | Security team, CI system |
| `audited` | An independent audit has confirmed the claims | External auditor |

## Canonical File Locations

In repositories following the [Rhodium Standard](https://github.com/hyperpolymath/rhodium-standard-repositories), A2ML files live in the `.machine_readable/` directory:

```
.machine_readable/
  STATE.a2ml         # Project state and progress
  META.a2ml          # Architecture decisions, governance
  ECOSYSTEM.a2ml     # Position in ecosystem, relationships
  AGENTIC.a2ml       # AI agent interaction patterns
  NEUROSYM.a2ml      # Neurosymbolic integration config
  PLAYBOOK.a2ml      # Operational runbook
  anchors/
    ANCHOR.a2ml      # Canonical authority declaration
  policies/
    MAINTENANCE-AXES.a2ml
    MAINTENANCE-CHECKLIST.a2ml
```

The top-level `0-AI-MANIFEST.a2ml` serves as the entry point that all AI agents must read first.

## Pandoc Integration

A2ML is a first-class Pandoc format. Convert between A2ML and any Pandoc-supported format:

```bash
# A2ML to HTML
pandoc -f a2ml-reader.lua input.a2ml -o output.html

# A2ML to PDF (via LaTeX)
pandoc -f a2ml-reader.lua input.a2ml -o output.pdf

# Markdown to A2ML
pandoc input.md -t a2ml-writer.lua -o output.a2ml

# A2ML to A2ML (normalise)
pandoc -f a2ml-reader.lua input.a2ml -t a2ml-writer.lua -o normalised.a2ml
```
