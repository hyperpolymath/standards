<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
# IANA Media Type Registration Application: application/vnd.k9

> Prepared for submission to IANA per RFC 6838 (Vendor Tree)
> Submission URL: https://www.iana.org/form/media-types
> Revision 2 -- 2026-04-03

---

## Important: Change from +nickel suffix

Earlier drafts registered `application/vnd.k9+nickel`. The `+nickel`
structured syntax suffix is **not registered** in the IANA Structured Syntax
Suffixes registry, and RFC 6838 Section 4.2.8 states that unregistered
suffixes SHOULD NOT be used.

This revision registers **`application/vnd.k9`** without a structured syntax
suffix. A separate `+nickel` suffix registration may be pursued if broader
Nickel ecosystem adoption warrants it.

The file extension `.k9.ncl` continues to be recognised as a conventional
indicator that the K9 file uses Nickel syntax.

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
| **Subtype** | vnd.k9 |
| **Tree** | Vendor (vnd.) |

---

## Technical Parameters

### Required Parameters

N/A

### Optional Parameters

- **security-level**: One of `kennel`, `yard`, or `hunt` (case-insensitive).
  Indicates the maximum execution privilege the component requests. Defaults
  to `kennel` if absent. This parameter is advisory; the authoritative
  security level is declared inside the component's pedigree metadata.
  Processors MUST NOT grant privileges beyond the level declared in the
  pedigree.

- **version**: K9 specification version. Syntax: `major.minor.patch`
  (e.g., `1.0.0`). If absent, processors SHOULD assume the latest version
  they support.

### Encoding Considerations

**8bit**

K9 files are UTF-8 text using Nickel configuration language syntax.
They begin with the ASCII magic bytes `K9!` (0x4B 0x39 0x21) at byte
offset 0. No binary payload blocks are defined in the K9 format itself,
though pedigree metadata may reference external binary artefacts by hash.

Line endings are LF (U+000A) by convention; parsers MUST accept CR+LF
(U+000D U+000A) and normalise to LF internally.

---

## Security Considerations

K9 implements a tiered security model called the Leash System. The security
implications differ significantly by level.

### Kennel level (Pure Data)

No execution. Read-only. The file contains only structured data (metadata,
content blocks, tags). Processors MUST NOT execute any content or evaluate
any expressions. Safe to open in any environment, including constrained
devices and edge nodes.

### Yard level (Validation Only)

Nickel contract evaluation is permitted. Nickel is a functionally pure
configuration language with no side effects, no filesystem access, and no
network access. Evaluation verifies that the component's data satisfies its
declared type contracts.

Processors MUST evaluate Nickel contracts in a sandboxed context with bounded
resource limits (CPU time, memory). No external resource access is permitted.

### Hunt level (Full Execution)

Complete must-just-nickel triad execution is permitted:

- **must** (POSIX shell): Environment detection and prerequisite validation.
  MUST be idempotent and MUST NOT modify system state.
- **just** (Just recipes): Task orchestration. MAY perform filesystem
  operations, invoke external commands, and deploy components.
- **nickel** (Nickel contracts): Typed validation of all configuration and
  outputs.

Hunt level REQUIRES:

- (a) A valid Ed25519 cryptographic signature over the component's pedigree
  and recipes
- (b) Signature verification BEFORE any recipe execution
- (c) Explicit user consent before execution
- (d) Containerised execution (Podman-first) where available
- (e) Fail-fast on signature verification failure

Unsigned or improperly signed Hunt-level components MUST be rejected without
execution.

### Threat Mitigations

- **Malicious payload**: Crafted .k9 files with hostile Just recipes are
  mitigated by Hunt-level signature requirements. Kennel and Yard levels
  cannot execute recipes.
- **Contract isolation**: Even if a Just recipe is compromised, it can only
  act on resources explicitly granted by the Nickel contracts.
- **Complexity exhaustion**: Nickel evaluation is bounded by configurable
  resource limits. Kennel mode is available where any evaluation is too
  expensive.
- **Replay prevention**: Ed25519 signatures include a timestamp field.
  Processors SHOULD reject signatures outside an acceptable freshness window.
- **Dependability collapse prevention**: Tiered controls scale with
  requested privileges, preventing bypass of all security due to excessive
  restrictions.

### Privacy

K9 pedigree metadata may contain author information, version history, and
deployment targets. Implementations SHOULD provide mechanisms to strip
metadata before sharing across trust boundaries.

---

## Interoperability Considerations

K9 is designed for multi-architecture permanence. Components are portable
across:

- Linux (all architectures)
- Minix
- macOS
- Android
- Embedded systems and ASICs (Kennel level only)

The must-just-nickel triad provides three complementary layers:

- Environment detection ("must" script, POSIX shell)
- Task orchestration (Just recipes)
- Typed validation (Nickel contracts)

The "must" bootstrap script detects the host environment and ensures
prerequisites are available before any task execution.

**Interoperability by level:**

- Kennel: Requires only magic-byte detection and metadata extraction
- Yard: Requires a Nickel evaluator
- Hunt: Requires Nickel, Just, and Ed25519 signature verification

The format is self-describing: conforming components include a pedigree
metadata block declaring the component's type ("breed"), version, security
level, and target architectures.

The underlying syntax is Nickel (https://nickel-lang.org), a typed
configuration language. Standard Nickel tooling can parse K9 files, though
K9-aware processors provide additional security enforcement.

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

- K9 component validators and deployers (`k9-scan`, `k9-sign`)
- CI/CD pipelines with self-validating configuration
- Container orchestration systems (Podman-first)
- Multi-architecture deployment tools
- Configuration management systems requiring typed contracts
- Edge and ASIC deployment (Kennel-level data components)
- The Contractile CLI system (must/trust/dust/intend/k9)

Reference implementation: https://github.com/hyperpolymath/standards/tree/main/k9-svc

---

## Fragment Identifier Considerations

Fragment identifiers for K9 documents use dot-separated paths into the
component's data structure.

**Syntax:** `#<path>` where `<path>` is a dot-separated key sequence.
Each key matches `[A-Za-z0-9_-]+`.

**Examples:**
- `#pedigree.name` -- references the component name
- `#config.server.port` -- references a nested config value

**Resolution:** Fragment MUST resolve to a key in the component's data
structure. If no match, the user agent SHOULD treat it as unresolvable
without raising an error.

---

## Restrictions on Usage

None. However, implementations MUST enforce the Leash System security model.
Files at Hunt level MUST NOT be executed without cryptographic signature
verification and explicit user consent.

---

## Provisional Registration

**No.** (Vendor-tree registration.)

---

## Additional Information

| Field | Value |
|-------|-------|
| **Deprecated alias names** | None |
| **Magic number(s)** | `K9!` (0x4B 0x39 0x21) at byte offset 0 |
| **File extension(s)** | `.k9`, `.k9.ncl` |
| **Macintosh file type code(s)** | None |
| **Object Identifier(s) / OID(s)** | None |
| **Intended usage** | COMMON |

### Other Comments

K9 (Self-Validating Components) treats files as active, self-validating
entities rather than passive data containers. The must-just-nickel triad
enables environment-aware deployment: environment detection via POSIX shell
("must"), task orchestration via Just recipes, and typed validation via
Nickel contracts.

The three-tier Leash System (Kennel/Yard/Hunt) ensures that security controls
are proportional to the component's requested privileges, preventing
dependability collapse while maintaining strong guarantees for executable
components.

The `K9!` magic bytes (3 octets) enable immediate file type identification
at the byte level, including by kernel-level detectors and freedesktop
shared-mime-info rules.

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

2. RFC 8032 -- Edwards-Curve Digital Signature Algorithm (EdDSA)
   https://www.rfc-editor.org/rfc/rfc8032.html

3. K9 SVC Specification (v1.0.0-alpha)
   https://github.com/hyperpolymath/standards/blob/main/k9-svc/SPEC.adoc

4. Nickel Configuration Language
   https://nickel-lang.org/

5. Just Command Runner
   https://just.systems/

---

## Submission Checklist

- [x] Review all fields for accuracy
- [x] Verify published specification links are accessible
- [x] Confirm no naming conflict with existing registrations
- [x] Remove unregistered +nickel suffix (RFC 6838 Section 4.2.8)
- [x] Unify file extensions under base type
- [ ] Submit via IANA web form at https://www.iana.org/form/media-types
- [ ] Monitor IANA email for review feedback
- [ ] Update specification with assigned media type upon approval
- [ ] Consider future +nickel suffix registration if warranted

---

## Future Work: +nickel Structured Syntax Suffix

If the Nickel configuration language achieves sufficient ecosystem adoption,
a registration of `+nickel` in the IANA Structured Syntax Suffixes registry
(per RFC 6838 Section 6.1) should be pursued. This would enable a companion
media type `application/vnd.k9+nickel` for K9 files that are valid Nickel
programs.

Prerequisites:
- Nickel language specification published as a stable reference
- Demonstrated use of Nickel syntax across multiple independent media types
- Community consensus on suffix semantics

---

*Prepared: 2026-01-30*
*Revised: 2026-04-03 (Revision 2)*
*Status: Draft -- ready for submission*
