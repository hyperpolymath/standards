<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
# IANA Media Type Registration Application: application/vnd.a2ml

> Prepared for submission to IANA per RFC 6838 (Vendor Tree)
> Submission URL: https://www.iana.org/form/media-types

---

## Applicant Information

| Field | Value |
|-------|-------|
| **Full Name** | Jonathan D.A. Jewell |
| **Email** | j.d.a.jewell@open.ac.uk |

---

## Media Type Details

| Field | Value |
|-------|-------|
| **Top-Level Type** | application |
| **Subtype** | vnd.a2ml |
| **Tree** | Vendor (vnd.) |

---

## Technical Parameters

### Required Parameters

N/A

### Optional Parameters

- **charset**: If specified, MUST be "utf-8". SHOULD NOT be specified if the
  document contains opaque payloads with arbitrary binary content. A2ML
  documents are UTF-8 by default for textual content (RFC 3629).

### Encoding Considerations

**8-bit text** (with binary note)

A2ML documents are primarily UTF-8 text but MAY include opaque payload blocks
(via the `@opaque` directive) containing arbitrary binary data.
Implementations MUST preserve byte-for-byte fidelity of opaque blocks. Line
endings are LF (U+000A) by convention; parsers MUST accept CR+LF
(U+000D U+000A) as well.

---

## Security Considerations

A2ML is a document markup format similar to Markdown and AsciiDoc. It is not
executable by itself and does not contain active content.

**Executable content:** A2ML MAY embed opaque payload blocks (using the
`@opaque` directive) and code blocks (using fenced code blocks) that can
contain code, scripts, or other executable content. Processors MUST treat
opaque payloads and code blocks as untrusted data and MUST NOT execute
embedded content by default. If an implementation offers execution or
evaluation features (e.g., running code blocks in a REPL environment), it
MUST:

- Operate in a sandboxed context with restricted privileges
- Require explicit user consent before execution
- Clearly indicate which content is being executed
- Provide mechanisms to disable execution entirely

**Privacy and integrity:** A2ML documents may contain personally identifiable
information (PII) in author metadata, abstracts, or content blocks.
Implementations SHOULD provide mechanisms to redact or strip metadata when
sharing documents. Opaque payloads may contain sensitive data and SHOULD be
inspected before transmission.

**Cryptographic attestation:** A2ML documents support cryptographic
attestation via Ed25519 signatures for opaque payloads and document structure.
Implementations that verify signatures MUST check signature validity,
timestamp authenticity, and certificate chain integrity.

**Compression:** A2ML does not define a compression layer. If documents are
compressed for transport, standard HTTP content-encoding or transfer-encoding
mechanisms should be used.

**External references:** A2ML link syntax (`[label](url)`) and `@ref()`
directives may reference external resources. Implementations MUST NOT
automatically fetch external resources without user consent.

---

## Interoperability Considerations

A2ML is designed for cross-platform interoperability with progressive
strictness modes:

- **Lax mode**: Permissive parsing, warnings only
- **Checked mode**: Structural validation required (unique IDs, valid
  references)
- **Attested mode**: Cryptographic attestation required, enforced by
  dependent-type proofs in the Idris2 core

Character encoding is UTF-8 (RFC 3629). Opaque payloads are preserved
byte-for-byte across parsing and serialisation. A2ML is renderer-agnostic
and can be converted to HTML5, LaTeX/PDF, Markdown (CommonMark), Djot, or
plain text.

Implementations SHOULD support all three strictness modes to ensure
interoperability across different use cases (authoring vs. publication).

---

## Published Specification

- **Primary specification (v1.0.0, Stable):**
  https://github.com/hyperpolymath/standards/blob/main/a2ml/SPEC-v1.0.adoc

- **Surface grammar specification (v0, Draft):**
  https://github.com/hyperpolymath/standards/blob/main/a2ml/SPEC.adoc

- **Formal verification (Idris2 core):**
  https://github.com/hyperpolymath/a2ml/tree/main/src/A2ML

---

## Application Usage

A2ML is used by:

- A2ML compilers and validators (the `a2ml` CLI tool)
- Static site generators that consume A2ML documents
- Document management systems requiring formal structure guarantees
- Academic publishing workflows for papers and specifications
- Technical documentation with verifiable cross-references
- Standards bodies requiring attested document integrity
- AI agent manifest files (0-AI-MANIFEST.a2ml, AI.a2ml)

Reference implementation: https://github.com/hyperpolymath/standards/tree/main/a2ml

---

## Fragment Identifier Considerations

Fragment identifiers for A2ML documents refer to element IDs.

**Syntax:** `#<id>` where `<id>` is a valid A2ML identifier matching
`[A-Za-z][A-Za-z0-9:_-]*`

**Examples:**
- `#intro` -- references a section with id="intro"
- `#fig:results` -- references a figure with id="fig:results"
- `#tab:data` -- references a table with id="tab:data"

**Resolution:** Fragment MUST match an element with the specified ID. If no
match, the user agent SHOULD ignore the fragment (no error). In attested
mode, A2ML enforces unique IDs via dependent-type proofs. In checked mode,
duplicate IDs are validation errors. In lax mode, duplicate IDs generate
warnings.

---

## Restrictions on Usage

None.

---

## Provisional Registration

**No.** (Vendor-tree registration; provisional applies only to standards-tree.)

---

## Additional Information

| Field | Value |
|-------|-------|
| **Deprecated alias names** | None |
| **Magic number(s)** | None (text-based format; identified by file extension or content detection of A2ML-specific directives such as `@abstract:`, `@refs:`, `@opaque:`) |
| **File extension(s)** | `.a2ml` |
| **Macintosh file type code(s)** | None |
| **Object Identifier(s) / OID(s)** | None |
| **Intended usage** | COMMON |

### Other Comments

A2ML (Attested Markup Language) is a lightweight markup language that compiles
to a typed, verifiable core with formal proof obligations. It enables
progressive strictness: from permissive authoring to formally verified
structural invariants enforced by dependent types in Idris2.

The format is designed for long-term document preservation with cryptographic
attestation and byte-for-byte opaque payload fidelity.

---

## Contact Information

| Field | Value |
|-------|-------|
| **Contact Name** | Jonathan D.A. Jewell |
| **Contact Email** | j.d.a.jewell@open.ac.uk |
| **Affiliation** | The Open University |
| **Author/Change Controller** | Jonathan D.A. Jewell, The Open University |

---

## References

1. RFC 6838 -- Media Type Specifications and Registration Procedures
   https://www.rfc-editor.org/rfc/rfc6838.html

2. A2ML Specification (v1.0.0, Stable)
   https://github.com/hyperpolymath/standards/blob/main/a2ml/SPEC-v1.0.adoc

3. A2ML Idris2 Core Implementation
   https://github.com/hyperpolymath/a2ml/tree/main/src/A2ML

4. IANA Media Type Registration Form
   https://www.iana.org/form/media-types

---

## Submission Checklist

- [ ] Review all fields for accuracy
- [ ] Verify published specification links are accessible
- [ ] Submit via IANA web form at https://www.iana.org/form/media-types
- [ ] Monitor IANA email for review feedback
- [ ] Update specification with assigned media type upon approval

---

*Prepared: 2026-03-16*
*Status: Draft -- ready for submission review*
