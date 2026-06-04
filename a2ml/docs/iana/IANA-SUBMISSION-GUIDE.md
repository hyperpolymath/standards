<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# IANA Media Type Submission Guide

Guide for submitting A2ML and K9 media type registrations to IANA.

---

## Overview

We are registering two media types with IANA:

1. **application/vnd.a2ml** -- For A2ML (Attested Markup Language) documents
2. **application/vnd.k9** -- For K9 SVC (Self-Validating Component) files

Both are vendor-tree registrations (vnd.) per RFC 6838.

**Note:** Earlier drafts used `application/vnd.k9+nickel`, but the `+nickel`
structured syntax suffix is not registered with IANA. Per RFC 6838
Section 4.2.8, unregistered suffixes SHOULD NOT be used. The base type
`application/vnd.k9` is registered instead.

---

## Pre-Submission Checklist

- [x] Registration templates completed
  - [x] application/vnd.a2ml (format-registrations/iana/a2ml-media-type.txt)
  - [x] application/vnd.k9 (format-registrations/iana/k9-media-type.txt)
- [x] Specifications publicly available
  - [x] A2ML: github.com/hyperpolymath/standards/blob/main/a2ml/SPEC-v1.0.adoc
  - [x] K9: github.com/hyperpolymath/standards/blob/main/k9-svc/SPEC.adoc
- [x] Reference implementations published
  - [x] A2ML: github.com/hyperpolymath/standards/tree/main/a2ml
  - [x] K9: github.com/hyperpolymath/standards/tree/main/k9-svc
- [x] No naming conflicts with existing IANA registrations
  - [x] Confirmed A2ML is distinct from ASAM A2L (application/A2L)
  - [x] No existing vnd.k9 registration
- [x] Removed unregistered +nickel suffix (RFC 6838 Section 4.2.8)
- [ ] arXiv paper published (optional, recommended for credibility)

---

## Submission Process

### Step 1: Submit via IANA Web Form

IANA vendor-tree media type registrations are submitted through the web form:

**URL:** https://www.iana.org/form/media-types

Fill in the fields from the registration template. The form requires:

| Form Field | A2ML Value | K9 Value |
|-----------|-----------|---------|
| Type name | application | application |
| Subtype name | vnd.a2ml | vnd.k9 |
| Required parameters | N/A | N/A |
| Optional parameters | charset (utf-8 only) | security-level, version |
| Encoding | binary | 8bit |
| Security considerations | (see template) | (see template) |
| Interoperability | (see template) | (see template) |
| Published specification | (spec URLs) | (spec URLs) |
| Application usage | (see template) | (see template) |
| Intended usage | COMMON | COMMON |
| Contact | Jonathan D.A. Jewell | Jonathan D.A. Jewell |

### Step 2: Separate Submissions

Submit each registration **separately** (do not combine).

**Recommended order:**
1. Submit `application/vnd.a2ml` first
2. Submit `application/vnd.k9` a few days later (or after first acknowledgement)

---

### Step 3: IANA Review Process

**Timeline:** Typically 2-4 weeks for vendor-tree registrations.

**Process:**

1. **Initial review** (1-3 days):
   IANA checks template completeness. May request clarifications.

2. **Expert review** (1-2 weeks):
   Media types designated expert reviews technical details.
   May ask about security, interoperability, or specification.

3. **Approval** (1-2 days):
   IANA approves registration and publishes to registry.

**Common reviewer questions:**

- "Can you expand the security considerations?" -- Add specific attack
  vectors and mitigations.
- "Is the specification permanently accessible?" -- Ensure GitHub repo
  is public; consider a permanent domain.
- "Are the optional parameters widely used?" -- Explain use cases for
  each parameter.
- "How do fragment identifiers resolve?" -- Provide step-by-step
  algorithm (already included in our templates).

---

### Step 4: Respond to Feedback

If IANA or the designated expert requests changes:

1. Read the request carefully
2. Update the registration template
3. Reply promptly (within 1 week)
4. Re-submit updated template via the web form or by replying to the
   review email

---

### Step 5: After Approval

**Registry entries will appear at:**
- https://www.iana.org/assignments/media-types/application/vnd.a2ml
- https://www.iana.org/assignments/media-types/application/vnd.k9

**Post-approval actions:**

1. Update documentation with official IANA registry links
2. Add IANA badges to repository READMEs
3. Update HTTP Content-Type headers in examples
4. Announce on relevant channels (Nickel, Idris2, markup language
   communities)

---

## Recommended HTTP Usage After Registration

**A2ML:**

```http
Content-Type: application/vnd.a2ml
Content-Type: application/vnd.a2ml; charset=utf-8
```

**K9:**

```http
Content-Type: application/vnd.k9
Content-Type: application/vnd.k9; security-level=kennel
Content-Type: application/vnd.k9; security-level=hunt; version=1.0.0
```

---

## File Locations

| File | Location |
|------|----------|
| A2ML registration (canonical) | format-registrations/iana/a2ml-media-type.txt |
| K9 registration (canonical) | format-registrations/iana/k9-media-type.txt |
| A2ML registration (standards sync) | standards/a2ml/docs/iana/application-vnd.a2ml-registration.txt |
| K9 registration (standards sync) | standards/k9-svc/docs/iana/application-vnd.k9+nickel-registration.txt |
| A2ML application (Markdown) | standards/a2ml/IANA-MEDIA-TYPE-APPLICATION.md |
| K9 application (Markdown) | standards/k9-svc/IANA-MEDIA-TYPE-APPLICATION.md |

---

## Contacts

**IANA Media Types Team:**
- Web form: https://www.iana.org/form/media-types
- Email (for follow-up): media-types@iana.org

**Submitter:**
- Jonathan D.A. Jewell
- Email: j.d.a.jewell@open.ac.uk
- Institution: The Open University

---

## References

- RFC 6838 -- Media Type Specifications and Registration Procedures
  https://www.rfc-editor.org/rfc/rfc6838.html

- RFC 6839 -- Additional Media Type Structured Syntax Suffixes
  https://www.rfc-editor.org/rfc/rfc6839.html

- IANA Media Types Registry
  https://www.iana.org/assignments/media-types/

- IANA Structured Syntax Suffixes Registry
  https://www.iana.org/assignments/media-type-structured-suffix/

---

*Created: 2026-01-30*
*Revised: 2026-04-03 (Revision 2 -- web form, +nickel removal, A2L note)*
