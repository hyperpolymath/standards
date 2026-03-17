<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
# IANA Media Type Registration Applications: K9 SVC

> Prepared for submission to IANA per RFC 6838 (Vendor Tree)
> Submission URL: https://www.iana.org/form/media-types
>
> This document contains **two** registration applications:
> 1. `application/vnd.k9+nickel` -- Nickel-based K9 files (`.k9.ncl`)
> 2. `application/vnd.k9` -- General K9 SVC files (`.k9`)

---

# Registration 1: application/vnd.k9+nickel

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
| **Subtype** | vnd.k9+nickel |
| **Tree** | Vendor (vnd.) |
| **Structured Syntax Suffix** | +nickel (Nickel configuration language) |

---

## Technical Parameters

### Required Parameters

N/A

### Optional Parameters

- **security-level**: One of `kennel`, `yard`, or `hunt`. Indicates the K9
  leash level of the document. If absent, processors MUST default to
  `kennel` (pure data, no execution). This parameter is advisory; the
  authoritative security level is declared inside the Nickel pedigree
  contract.

### Encoding Considerations

**8-bit text**

K9 Nickel files are UTF-8 text containing Nickel configuration language
expressions. They begin with the magic number `K9!` (0x4B 0x39 0x21) at
byte offset 0. Line endings are LF (U+000A) by convention.

---

## Security Considerations

K9 SVC is a Self-Validating Component format built on the must-just-nickel
triad (environment detection, task orchestration, and typed validation).

**Executable content:** K9 files at `'Yard` level permit Nickel evaluation
(functionally pure, no side effects). K9 files at `'Hunt` level permit full
triad execution including Just recipes that MAY perform filesystem and
network operations. Processors MUST enforce the leash system:

- **`'Kennel` (Pure Data):** No execution. Read-only. Safe to open anywhere.
  Processors MUST NOT evaluate Nickel expressions or execute Just recipes.
- **`'Yard` (Validation Only):** Nickel evaluation permitted in a sandboxed
  context. No filesystem or network access. No Just recipe execution.
- **`'Hunt` (Full Execution):** Complete triad execution. REQUIRES a
  cryptographic handshake (signed pedigree) before execution. Processors
  MUST verify the Ed25519 signature chain before permitting `'Hunt` level.

**Privacy and integrity:** K9 pedigree files may contain metadata about
authors, target architectures, and deployment environments. Implementations
SHOULD provide mechanisms to redact metadata when sharing. Nickel contracts
provide typed validation of all fields.

**External services:** At `'Hunt` level, Just recipes may invoke external
services (package managers, deployment targets). All external interactions
are declared in Nickel contracts and subject to the leash system. Network
access is forbidden at `'Kennel` and `'Yard` levels.

**Compression:** K9 does not define a compression layer. Standard HTTP
content-encoding mechanisms should be used for transport.

**Dependability collapse prevention:** K9 uses contract isolation (even if a
Just recipe is compromised, it can only act on resources explicitly granted
by Nickel contracts), fail-fast validation (invalid components refuse to
execute), and sandboxed evaluation (Nickel evaluation is functionally pure).

---

## Interoperability Considerations

K9 SVC files are designed for multi-architecture permanence, targeting
platforms from ASICs and edge nodes to full desktop/server environments.

The format is built on Nickel (a typed configuration language) and Just (a
command runner). Both are open-source tools with cross-platform support.

K9 files begin with the magic number `K9!` (3 bytes) for immediate
kernel-level identification. Freedesktop MIME XML and macOS UTI plist
definitions are provided in the K9 specification repository.

The three-level leash system (`'Kennel`, `'Yard`, `'Hunt`) ensures that K9
files degrade gracefully on constrained platforms: a system that cannot
evaluate Nickel can still identify and read `'Kennel`-level files as data.

---

## Published Specification

- **K9 SVC Specification (v1.0.0-alpha):**
  https://github.com/hyperpolymath/standards/blob/main/k9-svc/SPEC.adoc

- **Nickel configuration language:**
  https://nickel-lang.org/

- **Just command runner:**
  https://just.systems/

---

## Application Usage

K9 SVC files are used by:

- K9 validators and scanners (`k9-scan`, `k9-sign`)
- Self-validating deployment components for multi-architecture targets
- Environment-aware configuration files that carry their own validation
  contracts
- Build systems that need typed, verifiable configuration
- Archival systems requiring active document permanence

Reference implementation: https://github.com/hyperpolymath/standards/tree/main/k9-svc

---

## Fragment Identifier Considerations

Fragment identifiers are not defined for K9 files. K9 documents are
configuration components, not hypertext documents. If fragment identifiers
are needed in the future, they will be defined in a specification update.

---

## Restrictions on Usage

None. However, implementations MUST enforce the leash system security model
described in the security considerations. Files at `'Hunt` level MUST NOT
be executed without cryptographic verification.

---

## Provisional Registration

**No.** (Vendor-tree registration.)

---

## Additional Information

| Field | Value |
|-------|-------|
| **Deprecated alias names** | None |
| **Magic number(s)** | `K9!` (0x4B 0x39 0x21) at byte offset 0 |
| **File extension(s)** | `.k9.ncl` |
| **Macintosh file type code(s)** | None |
| **Object Identifier(s) / OID(s)** | None |
| **Intended usage** | COMMON |

### Other Comments

The `+nickel` structured syntax suffix indicates the file uses the Nickel
configuration language as its underlying syntax. Note that the `+nickel`
suffix is not yet registered in the IANA Structured Syntax Suffixes
registry; a separate registration may be pursued for the Nickel suffix if
adoption warrants it.

---

## Contact Information

| Field | Value |
|-------|-------|
| **Contact Name** | Jonathan D.A. Jewell |
| **Contact Email** | j.d.a.jewell@open.ac.uk |
| **Affiliation** | The Open University |
| **Author/Change Controller** | Jonathan D.A. Jewell, The Open University |

---

---

# Registration 2: application/vnd.k9

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
| **Subtype** | vnd.k9 |
| **Tree** | Vendor (vnd.) |

---

## Technical Parameters

### Required Parameters

N/A

### Optional Parameters

- **security-level**: One of `kennel`, `yard`, or `hunt`. Indicates the K9
  leash level. If absent, defaults to `kennel` (pure data, no execution).

### Encoding Considerations

**8-bit text**

General K9 files (`.k9`) are UTF-8 text that may use YAML-style or
Nickel-style syntax depending on the component. They begin with the magic
number `K9!` (0x4B 0x39 0x21) at byte offset 0.

---

## Security Considerations

Same security model as `application/vnd.k9+nickel` (see Registration 1
above). The three-level leash system (`'Kennel`, `'Yard`, `'Hunt`) applies
identically. Processors MUST enforce the leash system and MUST NOT execute
`'Hunt`-level files without cryptographic verification.

---

## Interoperability Considerations

`application/vnd.k9` is the general-purpose media type for K9 SVC files
that may not use Nickel syntax exclusively. When a K9 file is known to use
Nickel syntax, `application/vnd.k9+nickel` SHOULD be preferred.

The magic number `K9!` identifies both types at the byte level.
Freedesktop and UTI definitions are provided in the K9 specification
repository. The `.k9` extension maps to this general type; `.k9.ncl` maps
to `application/vnd.k9+nickel`.

---

## Published Specification

- **K9 SVC Specification (v1.0.0-alpha):**
  https://github.com/hyperpolymath/standards/blob/main/k9-svc/SPEC.adoc

---

## Application Usage

Same as `application/vnd.k9+nickel` (see Registration 1 above). This type
is used when the K9 file syntax is not exclusively Nickel-based or when the
specific syntax is unknown.

---

## Fragment Identifier Considerations

Fragment identifiers are not defined for K9 files.

---

## Restrictions on Usage

None, subject to leash system enforcement (see security considerations).

---

## Provisional Registration

**No.** (Vendor-tree registration.)

---

## Additional Information

| Field | Value |
|-------|-------|
| **Deprecated alias names** | None |
| **Magic number(s)** | `K9!` (0x4B 0x39 0x21) at byte offset 0 |
| **File extension(s)** | `.k9` |
| **Macintosh file type code(s)** | None |
| **Object Identifier(s) / OID(s)** | None |
| **Intended usage** | COMMON |

### Other Comments

`application/vnd.k9` serves as the base type for all K9 SVC files.
`application/vnd.k9+nickel` is a more specific type for Nickel-syntax K9
files. Content negotiation should prefer the more specific type when the
syntax is known.

---

## Contact Information

| Field | Value |
|-------|-------|
| **Contact Name** | Jonathan D.A. Jewell |
| **Contact Email** | j.d.a.jewell@open.ac.uk |
| **Affiliation** | The Open University |
| **Author/Change Controller** | Jonathan D.A. Jewell, The Open University |

---

## References (Both Registrations)

1. RFC 6838 -- Media Type Specifications and Registration Procedures
   https://www.rfc-editor.org/rfc/rfc6838.html

2. RFC 6839 -- Additional Media Type Structured Syntax Suffixes
   https://www.rfc-editor.org/rfc/rfc6839.html

3. K9 SVC Specification (v1.0.0-alpha)
   https://github.com/hyperpolymath/standards/blob/main/k9-svc/SPEC.adoc

4. Nickel Configuration Language
   https://nickel-lang.org/

5. Just Command Runner
   https://just.systems/

6. IANA Media Type Registration Form
   https://www.iana.org/form/media-types

---

## Submission Checklist

- [ ] Review all fields for accuracy in both registrations
- [ ] Verify published specification links are accessible
- [ ] Submit Registration 1 (`application/vnd.k9+nickel`) via IANA web form
- [ ] Submit Registration 2 (`application/vnd.k9`) via IANA web form
- [ ] Consider separate registration for `+nickel` structured syntax suffix
- [ ] Monitor IANA email for review feedback
- [ ] Update specification with assigned media types upon approval

---

*Prepared: 2026-03-16*
*Status: Draft -- ready for submission review*
