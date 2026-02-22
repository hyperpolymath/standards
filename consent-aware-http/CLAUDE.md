# CLAUDE.md

## Project Overview

**Consent-Aware HTTP** is a standards-based initiative that defines protocols for declaring and enforcing AI usage boundaries on the web. It consists of two complementary Internet-Drafts:

1. **HTTP Status Code 430 (Consent Required)** - A new HTTP status code enabling servers to reject requests when AI-specific consent requirements are not met
2. **AI Boundary Declaration Protocol (AIBDP)** - A machine-readable manifest format (`.well-known/aibdp.json`) for declaring what forms of AI engagement are permitted

This is primarily a **specification and documentation repository**, not a code implementation. It contains formal Internet-Draft XML documents, educational materials, and outreach resources.

## Core Concepts

### HTTP 430: Consent Required

- New HTTP status code for refusing access based on AI consent violations
- Distinct from 403 (Forbidden), 428 (Precondition Required), or 451 (Legal)
- Provides procedural clarity: "Your AI agent violated declared boundaries"
- Example response includes link to the site's AIBDP manifest

### AIBDP (AI Boundary Declaration Protocol)

- Machine-readable JSON manifest hosted at `/.well-known/aibdp.json`
- Declares acceptable/prohibited AI uses (training, indexing, generation, etc.)
- Self-sovereign: content creators control their boundaries
- Federated-friendly: works with IndieWeb, personal sites, CDNs

### Cultural Philosophy

This project is rooted in ethics and cultural theory, not just technical specification:
- **Bell Hooks**: "Boundary is where meaning begins"
- **Virginia Woolf**: Architectural refusal as dignity
- **Journalism ethics**: Right to decline co-option
- **IndieWeb principles**: Self-authorship and federated control

The goal is **not anti-AI** but **pro-boundary** - restoring consent and procedural clarity to web interactions.

## Repository Structure

```
consent-aware-http/
├── draft-jewell-http-430-consent-required-00.xml  # Main Internet-Draft (430 status code)
├── drafts/                                        # Additional draft versions
│   └── draft-jewell-http-430-consent-required-00.xml
├── docs/                                          # Human-readable documentation
│   ├── explainer.md                               # Architectural overview
│   ├── technical.md                               # Developer guide
│   ├── start-here.md                              # Quick adoption guide
│   ├── ethics.md                                  # Cultural/philosophical framing
│   ├── governance.md                              # Organizational implications
│   ├── conformance.md                             # Implementation requirements
│   ├── references.md                              # Citations and influences
│   ├── directory-structure.md                     # This repository's layout
│   └── example-aibdp.json                         # Sample AIBDP manifest
├── assets/                                        # Visual and template resources
│   ├── badges/                                    # SVG badges for adopters
│   ├── error-pages/                               # WCAG-compliant 430 error page
│   └── outreach/                                  # Template letters and announcements
│       ├── install-guidance.md
│       ├── disclosure-template-letter-*.md
│       ├── org-list.md
│       └── badge-announcement.md
├── scripts/                                       # Build tools
│   ├── build-drafts.ps1                          # PowerShell script for xml2rfc
│   └── README-FIRST.md
└── .github/                                       # Community guidelines
    ├── CONTRIBUTING.md
    ├── CODE_OF_CONDUCT.md
    └── ISSUE_TEMPLATE/
```

## Key Files to Understand

### Internet-Drafts (IETF Standards Track)
- `draft-jewell-http-430-consent-required-00.xml` - Formal specification for HTTP 430
- Written in RFC XML format (RFC 7991)
- Intended for submission to IETF for standardization

### Documentation
- `docs/explainer.md` - Best starting point for understanding the "why"
- `docs/technical.md` - Implementation guidance for developers
- `docs/ethics.md` - Cultural and philosophical context
- `docs/start-here.md` - Quick adoption guide with examples

### Example Manifest
- `docs/example-aibdp.json` - Template AIBDP manifest showing structure

## Development Workflow

### Current Branch
You are working on: `claude/create-claude-md-018k61kfViJFfuXVhDeqfYok`

### Git Practices
- Always develop on the designated feature branch
- Commit with clear, descriptive messages that reflect the nature of changes
- Push to origin with: `git push -u origin <branch-name>`
- Branch names must start with `claude/` and match the session ID

### This is NOT a code repository
- No application code, libraries, or frameworks
- No build artifacts (except draft rendering tools)
- Focus is on **specification authoring** and **community education**

## Common Tasks

### Working with Internet-Drafts
- XML format follows RFC 7991 (xml2rfc v3)
- Draft naming: `draft-jewell-<topic>-<version>.xml`
- Build scripts in `scripts/` can generate HTML/PDF renderings

### Documentation Updates
- All docs use GitHub-flavored Markdown
- Maintain consistent tone: principled, clear, non-adversarial
- Cross-reference related documents where appropriate

### Adding Outreach Materials
- Template letters go in `assets/outreach/`
- Follow existing naming convention: `disclosure-template-letter-*.md`
- Keep language accessible but precise

## Important Principles

1. **Declarative over Prescriptive** - We define boundaries, not enforcement mechanisms
2. **Protocol over Platform** - Works with any web stack, federated or centralized
3. **Ethical Clarity** - Boundaries are acts of care, not punishment
4. **Cultural Rootedness** - Grounded in journalism ethics, critical theory, and dignity frameworks
5. **Open Standards** - Designed for IETF submission and broad adoption

## Technical Details

### HTTP 430 Response Format
```http
HTTP/1.1 430 Consent Required
Content-Type: application/json
Link: <https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"
Retry-After: 86400

{
  "error": "Consent declaration missing or invalid.",
  "reference": "https://example.org/.well-known/aibdp.json"
}
```

### AIBDP Manifest Location
- **Must** be hosted at `/.well-known/aibdp.json`
- JSON format with declared permissions/prohibitions
- Can include cryptographic signatures (COSE)

## Contributor Context

**Author**: Jonathan D.A. Jewell
- NEC PRC Representative
- NUJ Ethics Council
- Convenor: AI & Data Working Group
- Contact: jonathan@metadatastician.art

**License**: Dual-licensed
- Code/specs: MIT License
- Documentation: CC BY-SA 4.0

**Community**:
- IndieWeb participants
- Federated web advocates
- Ethical AI practitioners
- IETF working groups

## When Helping with This Project

- **For spec changes**: Understand IETF RFC formatting conventions
- **For documentation**: Maintain the cultural and ethical framing
- **For outreach**: Keep language accessible but principled
- **For discussions**: Ground responses in consent theory and web architecture

## Philosophy Summary

> "Without refusal, permission is meaningless."

This project treats **boundary-setting as a cultural practice**, not just a technical feature. It respects:
- Declarative refusal as care
- Transparent infrastructure over implied permissions
- Sanctuary work as procedural and cultural
- Authorship dignity in the age of generative systems

The protocols enable creators to say "no" to unauthorized AI use without legal escalation, using the same web infrastructure that made open publishing possible in the first place.
