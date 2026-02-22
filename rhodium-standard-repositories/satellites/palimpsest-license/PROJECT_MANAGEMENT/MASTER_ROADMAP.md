# Palimpsest License - Master Roadmap

**Version:** 1.0
**Last Updated:** 2025-12-08
**Status:** Active Development

---

## Executive Summary

This roadmap outlines the path from current state (v0.4 incomplete) to full establishment as a recognized, widely-adopted license framework for creative works in the AI age.

---

## Part 1: Lessons from Established Licenses

### Creative Commons Establishment (2001-2006)

| Year | Milestone | Strategy |
|------|-----------|----------|
| 2001 | Founded by Lessig, Abelson, Eldred | Legal expertise + tech vision |
| 2002 | First licenses released | Simple, clear, modular |
| 2003 | 1M works licensed | Early adopter focus |
| 2004 | 5M works licensed | Platform partnerships |
| 2006 | 50M works (Flickr adoption) | **Key:** Major platform integration |

**Key Success Factors:**
1. Strong legal backing (Stanford Law)
2. Simple, understandable license options
3. Web application for license selection
4. Platform partnerships (Flickr was pivotal)
5. International porting (50+ jurisdictions)

### SPDX Recognition Process

**Path to SPDX Identifier:**
1. Submit to [SPDX License List XML repo](https://github.com/spdx/license-list-XML)
2. Provide: Full text, short identifier, canonical URL
3. Community review
4. Use `LicenseRef-Palimpsest-0.4` until approved

### OSI Approval Process

**Requirements:**
1. 60-day public review on license-review mailing list
2. Demonstrate compliance with Open Source Definition
3. Show what gap it fills vs. existing licenses
4. Legal review documentation
5. Examples of use

**Timeline:** Typically 60-90 days

---

## Part 2: Near-Term Roadmap (2025 Q4 - 2026 Q2)

### Phase 1: Foundation (Now - 2025 Q4)

| Week | Milestone | Owner | Status |
|------|-----------|-------|--------|
| W1-2 | Complete v0.4 license text (EN) | Legal Lead | BLOCKED |
| W1-2 | Port Haskell validator to OCaml | Tech Lead | PLANNED |
| W3-4 | Dutch translation + legal review | NL Legal | WAITING |
| W3-4 | Create metadata examples | Tech Lead | PLANNED |
| W5-6 | RSR Silver compliance | QA Lead | IN PROGRESS |
| W7-8 | Stewardship Council recruitment | Governance | BLOCKED |

### Phase 2: Recognition (2026 Q1)

| Month | Milestone | Dependencies |
|-------|-----------|--------------|
| Jan | Submit to SPDX License List | v0.4 complete |
| Jan | Submit IETF Internet-Draft | v0.4 complete |
| Feb | OSI license review submission | v0.4 complete |
| Feb | Charter W3C Community Group | Council formed |
| Mar | Platform partnership outreach | Recognition progress |

### Phase 3: Adoption (2026 Q2)

| Month | Milestone | Target |
|-------|-----------|--------|
| Apr | GitHub pilot program | 100 repos |
| May | Hugging Face integration | AI model cards |
| Jun | v1.0 release candidate | Council vote |

---

## Part 3: Mid-Term Roadmap (2026 Q3 - 2027)

### v1.0 Launch (2026 Q3)

- 90-day community review
- 6/7 Council supermajority vote
- Press campaign launch
- Platform integrations live

### Growth Targets (2027)

| Metric | Target |
|--------|--------|
| Works protected | 100,000+ |
| Platform integrations | 10+ |
| Jurisdictions | 5+ |
| Community contributors | 50+ |

---

## Part 4: Long-Term Vision (2028-2030)

### 2028: Standards Track

- IETF RFC publication
- W3C Working Group formation
- EU AI Act citation

### 2029: Scale

- 1M+ works protected
- DAO governance operational
- Consent Registry live

### 2030: Ubiquity

- 100M+ works protected
- W3C Recommendation or IETF Standard
- Post-quantum cryptography deployed
- "Consent metadata as common as HTTPS"

---

## Part 5: Critical Path

```
v0.4 License Text ──────┐
        │               │
        ▼               │
Bilingual Alignment     │
        │               │
        ▼               ▼
OCaml Consolidation   SPDX/OSI Submission
        │               │
        ▼               │
RSR Silver             │
        │               │
        ▼               ▼
Council Formation ◄────Platform Partnerships
        │
        ▼
90-day Review
        │
        ▼
v1.0 Launch
```

---

## Part 6: Risk Register

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| v0.4 text delayed | HIGH | CRITICAL | Prioritize legal drafting |
| Council recruitment fails | MEDIUM | HIGH | Expand network, offer stipends |
| OSI rejection | LOW | MEDIUM | Pre-submission consultation |
| Platform disinterest | MEDIUM | HIGH | Demonstrate value proposition |
| Legal challenge | LOW | HIGH | Dutch + Scottish law backup |

---

## Sources & References

- [Creative Commons Timeline](https://creativecommons.org/timeline/)
- [CC Certificate Story](https://certificates.creativecommons.org/cccertedu/chapter/1-1-the-story-of-creative-commons/)
- [SPDX License List](https://spdx.org/licenses/)
- [OSI License Review Process](https://opensource.org/licenses/review-process)
- [How OSI Checks License Compliance](https://opensource.org/blog/how-the-osi-checks-if-new-licenses-comply-with-the-open-source-definition)
