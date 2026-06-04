<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# IANA Media Type Registration Application: application/vnd.a2ml

> Prepared for submission to IANA per RFC 6838 (Vendor Tree)
> Submission URL: https://www.iana.org/form/media-types
> Revision 2 -- 2026-04-03

---

## Applicant Information

| Field | Value |
|-------|-------|
| **Full Name** | Jonathan D.A. Jewell |
| **Email** | j.d.a.jewell@open.ac.uk |
| **Affiliation** | The Open University |

---

## Media Type Details

| Field | Value |
|-------|-------|
| **Top-Level Type** | application |
| **Subtype** | vnd.a2ml |
| **Tree** | Vendor (vnd.) |

**Note:** A2ML is distinct from ASAM A2L (`application/A2L`), which is a
measurement and calibration data format for automotive ECUs. Despite similar
names, the two formats are unrelated in purpose, syntax, and application domain.

---

## Technical Parameters

### Required Parameters

N/A

### Optional Parameters

- **charset**: If specified, the value MUST be "utf-8" (case-insensitive).
  A2ML documents are UTF-8 by default (RFC 3629). The charset parameter
  SHOULD NOT be specified if the document contains opaque payload blocks
  with arbitrary binary content.

### Encoding Considerations

**binary**

A2ML documents are primarily UTF-8 text but MAY include opaque payload blocks
(via the `@opaque` directive) containing arbitrary binary data. Because opaque
blocks may contain any octet sequence, the encoding is classified as "binary"
per RFC 6838 Section 4.8.

Implementations MUST preserve byte-for-byte fidelity of opaque blocks across
parsing and serialisation. Line endings are LF (U+000A) by convention; parsers
MUST accept CR+LF (U+000D U+000A) and normalise to LF internally.

---

## Security Considerations

A2ML is a document markup format comparable to Markdown and AsciiDoc. It is
not executable by itself and does not contain active content.

**Executable content:** A2ML MAY embed opaque payload blocks (using the
`@opaque` directive) and code blocks (using fenced code blocks) that can
contain code, scripts, or other executable content. Processors MUST treat
opaque payloads and code blocks as untrusted data and MUST NOT execute
embedded content by default. If an implementation offers execution or
evaluation features (e.g., running code blocks in a REPL environment), it
MUST:

- (a) Operate in a sandboxed context with restricted privileges
- (b) Require explicit user consent before execution
- (c) Clearly indicate which content is being executed
- (d) Provide mechanisms to disable execution entirely

**Privacy:** A2ML documents may contain personally identifiable information
(PII) in author metadata, abstracts, or content blocks. Implementations
SHOULD provide mechanisms to redact or strip metadata when sharing documents.
Opaque payloads may contain sensitive data and SHOULD be inspected before
transmission across trust boundaries.

**Integrity and cryptographic attestation:** A2ML documents support
cryptographic attestation via Ed25519 signatures for opaque payloads and
document structure. Implementations that verify signatures MUST validate:

- (a) Signature correctness against the stated public key
- (b) Timestamp freshness (to prevent replay)
- (c) Public-key trust (via a known-keys list or certificate chain)

Documents without signatures SHOULD be treated as unverified.

**Compression:** A2ML does not define a compression layer. If documents are
compressed for transport, standard HTTP Content-Encoding or Transfer-Encoding
mechanisms (RFC 9110) should be used.

**External references:** A2ML link syntax (`[label](url)`) and `@ref()`
directives may reference external resources. Implementations MUST NOT
automatically fetch external resources without user consent.

---

## Interoperability Considerations

A2ML is designed for cross-platform interoperability with progressive
strictness modes:

- **Lax mode**: Permissive parsing, warnings only
- **Checked mode**: Structural validation required (unique IDs, valid
  cross-references, well-formed directives)
- **Attested mode**: Cryptographic attestation required, enforced by
  dependent-type proofs in the Idris2 reference implementation

Character encoding is UTF-8 (RFC 3629). Byte Order Marks (U+FEFF) at the
start of a document are permitted but not required; parsers MUST accept and
silently consume a leading BOM.

Opaque payloads are preserved byte-for-byte across parsing and serialisation.
Renderers MAY transform opaque content for display but MUST retain the
original bytes for attestation and round-trip fidelity.

A2ML is renderer-agnostic and can be converted to HTML5, LaTeX/PDF,
Markdown (CommonMark), Djot, or plain text.

Implementations SHOULD support all three strictness modes.

---

## Published Specification

- **Primary specification (v1.0.0, Stable):**
  https://github.com/hyperpolymath/standards/blob/main/a2ml/SPEC-v1.0.adoc

- **Surface grammar specification (v0, Draft):**
  https://github.com/hyperpolymath/standards/blob/main/a2ml/SPEC.adoc

- **Formal verification (Idris2 typed core):**
  https://github.com/hyperpolymath/a2ml/tree/main/src/A2ML

---

## Application Usage

A2ML is used by:

- A2ML compilers and validators (the `a2ml` command-line tool)
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

**Syntax:** `#<id>` where `<id>` matches `[A-Za-z][A-Za-z0-9:_-]*`

**Examples:**
- `#intro` -- references a section with id="intro"
- `#fig:results` -- references a figure with id="fig:results"
- `#tab:data` -- references a table with id="tab:data"

**Resolution:** Fragment MUST match an element with the specified ID. If no
match, the user agent SHOULD treat it as unresolvable without raising an
error. ID uniqueness enforcement depends on the strictness mode
(attested > checked > lax).

---

## Restrictions on Usage

None.

---

## Provisional Registration

**No.** (Vendor-tree registration; provisional applies only to standards tree.)

---

## Additional Information

| Field | Value |
|-------|-------|
| **Deprecated alias names** | None |
| **Magic number(s)** | None (text-based format; identified by file extension or content detection of A2ML directives such as `@abstract:`, `@refs:`, `@opaque:`) |
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
| **Address** | Milton Keynes, MK7 6AA, United Kingdom |
| **Author/Change Controller** | Jonathan D.A. Jewell, The Open University |

---

## References

1. RFC 6838 -- Media Type Specifications and Registration Procedures
   https://www.rfc-editor.org/rfc/rfc6838.html

2. RFC 3629 -- UTF-8, a transformation format of ISO 10646
   https://www.rfc-editor.org/rfc/rfc3629.html

3. RFC 9110 -- HTTP Semantics
   https://www.rfc-editor.org/rfc/rfc9110.html

4. A2ML Specification (v1.0.0, Stable)
   https://github.com/hyperpolymath/standards/blob/main/a2ml/SPEC-v1.0.adoc

5. A2ML Idris2 Core Implementation
   https://github.com/hyperpolymath/a2ml/tree/main/src/A2ML

---

## Submission Checklist

- [x] Review all fields for accuracy
- [x] Verify published specification links are accessible
- [x] Confirm no naming conflict with existing registrations
- [x] Clarify distinction from ASAM A2L (application/A2L)
- [ ] Submit via IANA web form at https://www.iana.org/form/media-types
- [ ] Monitor IANA email for review feedback
- [ ] Update specification with assigned media type upon approval

---

*Prepared: 2026-01-30*
*Revised: 2026-04-03 (Revision 2)*
*Status: Draft -- ready for submission*
