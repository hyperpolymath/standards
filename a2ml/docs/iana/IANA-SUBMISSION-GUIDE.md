# IANA Media Type Submission Guide

Guide for submitting A2ML and K9 media type registrations to IANA.

---

## Overview

We are registering two media types with IANA:

1. **application/vnd.a2ml** - For A2ML documents
2. **application/vnd.k9+nickel** - For K9 contractiles

Both are vendor-tree registrations (vnd.) which are appropriate for
organization-specific formats.

---

## Pre-Submission Checklist

- [x] Registration templates completed
  - [x] application/vnd.a2ml (a2ml/docs/iana/application-vnd.a2ml-registration.txt)
  - [x] application/vnd.k9+nickel (k9-svc/docs/iana/application-vnd.k9+nickel-registration.txt)
- [x] Specifications publicly available
  - [x] A2ML: github.com/hyperpolymath/a2ml/blob/main/SPEC.a2ml
  - [x] K9: github.com/hyperpolymath/k9-svc/blob/main/docs/SPECIFICATION.md
- [x] Reference implementations published
  - [x] A2ML: github.com/hyperpolymath/a2ml
  - [x] K9: github.com/hyperpolymath/k9-svc
- [ ] arXiv paper published (optional, recommended)
  - Adds credibility to submission
  - Can be included as reference after acceptance

---

## Submission Process

### Step 1: Prepare Email

**For application/vnd.a2ml:**

```
To: media-types@iana.org
Cc: iana@iana.org
Subject: Media Type Registration: application/vnd.a2ml

Dear IANA Media Types Team,

I am submitting a registration for a new media type in the vendor tree:

Type name: application
Subtype name: vnd.a2ml

A2ML (Attested Markup Language) is a lightweight markup format with formal
verification capabilities, designed for academic papers, specifications, and
documents requiring structural guarantees. It combines ease of authoring
(similar to Markdown) with formal proof obligations (via Idris2 dependent types).

The complete registration template is attached as plain text.

Key references:
- Specification: https://github.com/hyperpolymath/a2ml/blob/main/SPEC.a2ml
- Reference implementation: https://github.com/hyperpolymath/a2ml
- Documentation: https://github.com/hyperpolymath/a2ml/tree/main/docs

Please let me know if you need any additional information or clarifications.

Best regards,
Jonathan D.A. Jewell
The Open University
jonathan.jewell@open.ac.uk
```

**Attachment:** `application-vnd.a2ml-registration.txt`

---

**For application/vnd.k9+nickel:**

```
To: media-types@iana.org
Cc: iana@iana.org
Subject: Media Type Registration: application/vnd.k9+nickel

Dear IANA Media Types Team,

I am submitting a registration for a new media type in the vendor tree:

Type name: application
Subtype name: vnd.k9+nickel

K9 is a format for infrastructure automation contractiles based on the Nickel
configuration language. K9 contractiles combine system validation (Bash),
execution recipes (Justfile), and configuration contracts (Nickel) with
three security levels (kennel/yard/hunt) and cryptographic attestation.

The complete registration template is attached as plain text.

Key references:
- Specification: https://github.com/hyperpolymath/k9-svc/blob/main/docs/SPECIFICATION.md
- Reference implementation: https://github.com/hyperpolymath/k9-svc
- Security documentation: https://github.com/hyperpolymath/k9-svc/blob/main/docs/SECURITY.md

Please let me know if you need any additional information or clarifications.

Best regards,
Jonathan D.A. Jewell
The Open University
jonathan.jewell@open.ac.uk
```

**Attachment:** `application-vnd.k9+nickel-registration.txt`

---

### Step 2: Send Submissions

Send both emails separately (don't combine into one submission).

**Order:** Submit application/vnd.a2ml first, then application/vnd.k9+nickel
after a few days (or after first approval).

---

### Step 3: IANA Review Process

**Timeline:** Typically 2-4 weeks for vendor-tree registrations

**What happens:**

1. **Initial review** (1-3 days):
   - IANA checks template completeness
   - May request clarifications or corrections

2. **Expert review** (1-2 weeks):
   - Media types expert reviews technical details
   - May ask about security, interoperability, or specification

3. **Public review** (optional):
   - Some registrations posted to ietf-types mailing list
   - Community feedback period (usually 1-2 weeks)

4. **Approval** (1-2 days):
   - IANA approves registration
   - Publishes to registry

**Possible requests:**

- **Clarify security considerations**: Expand on execution risks
- **Improve interoperability section**: Add more details on parsers
- **Reference updates**: Include arXiv ID (if paper published)
- **Specification location**: Ensure spec is permanently accessible

---

### Step 4: Respond to Requests

If IANA requests changes:

1. **Read request carefully**: Understand what's needed
2. **Update template**: Make requested changes
3. **Reply promptly**: Respond within 1 week if possible
4. **Attach updated template**: Include revised registration

**Example response:**

```
Dear IANA Media Types Team,

Thank you for your review of the application/vnd.a2ml registration.

I have updated the template to address your feedback:

1. Expanded security considerations to cover execution risks in detail
2. Added additional interoperability notes about parser implementations
3. Updated references to include arXiv ID (arXiv:2601.12345)

The updated registration template is attached.

Please let me know if you need any further information.

Best regards,
Jonathan D.A. Jewell
```

---

### Step 5: After Approval

**You'll receive:**

- Confirmation email from IANA
- Published registry entry URL:
  - https://www.iana.org/assignments/media-types/application/vnd.a2ml
  - https://www.iana.org/assignments/media-types/application/vnd.k9+nickel

**Next steps:**

1. **Update documentation:**
   - Add IANA registry links to READMEs
   - Update IANA-MEDIA-TYPE.adoc with official registry URL
   - Add badges to repositories

2. **Announce:**
   - Blog post: "A2ML and K9 Media Types Officially Registered with IANA"
   - Social media: Share registry links
   - Community: Notify Nickel, Idris2, markup language communities

3. **Update implementations:**
   - Use official media type in HTTP responses
   - Update documentation to reference IANA registry
   - Add Content-Type headers in examples

---

## Common Issues and Solutions

### Issue: Specification Not Accessible

**Problem:** IANA can't access specification at provided URL

**Solution:**
- Ensure GitHub repository is public
- Use permanent URLs (not /blob/main/ which can change)
- Consider hosting spec at permanent domain (e.g., a2ml.org)

### Issue: Security Considerations Insufficient

**Problem:** IANA requests more detail on security risks

**Solution:**
- Expand security section with specific attack vectors
- Include threat model if available
- Reference external security documentation
- Describe sandboxing and execution safeguards

### Issue: Too Many Optional Parameters

**Problem:** IANA suggests reducing optional parameters

**Solution:**
- Move non-essential parameters to specification
- Keep only widely-used parameters in registration
- Explain rationale for each parameter

### Issue: Fragment Identifier Syntax Unclear

**Problem:** IANA requests clearer fragment identifier specification

**Solution:**
- Provide formal syntax (ABNF or regex)
- Give multiple examples
- Explain resolution algorithm step-by-step

---

## Timeline Estimates

| Stage | Duration | Notes |
|-------|----------|-------|
| Submission | 1 day | Prepare and send emails |
| Initial review | 1-3 days | IANA checks completeness |
| Expert review | 1-2 weeks | Technical review |
| Public review | 1-2 weeks | If required (vendor-tree usually skips) |
| Revisions | 1-7 days | If changes requested |
| Approval | 1-2 days | Final approval + publication |
| **Total** | **2-4 weeks** | Typical for vendor-tree |

---

## After Registration

### Update A2ML Documentation

```bash
# Add IANA badge to README.adoc
echo "https://img.shields.io/badge/IANA-application%2Fvnd.a2ml-blue" >> README.adoc

# Update IANA-MEDIA-TYPE.adoc with official registry link
```

### Update K9 Documentation

```bash
# Add IANA badge to README.adoc
echo "https://img.shields.io/badge/IANA-application%2Fvnd.k9%2Bnickel-blue" >> README.adoc
```

### Use Official Media Types

**HTTP responses:**

```http
Content-Type: application/vnd.a2ml; charset=utf-8
```

```http
Content-Type: application/vnd.k9+nickel; security-level=hunt; validation-mode=validated
```

**HTML embedding:**

```html
<script type="application/vnd.a2ml" id="doc">
# A2ML Document
...
</script>
```

---

## Contacts

**IANA Media Types Team:**
- Email: media-types@iana.org, iana@iana.org
- Web: https://www.iana.org/form/media-types

**Submitter:**
- Jonathan D.A. Jewell
- Email: jonathan.jewell@open.ac.uk
- Institution: The Open University

---

## References

- RFC 6838 - Media Type Specifications and Registration Procedures
  https://www.rfc-editor.org/rfc/rfc6838.html

- IANA Media Types Registry
  https://www.iana.org/assignments/media-types/

- IANA Registration Forms
  https://www.iana.org/form/media-types

---

**Created:** 2026-01-30
**Status:** Ready for submission (pending arXiv acceptance)
