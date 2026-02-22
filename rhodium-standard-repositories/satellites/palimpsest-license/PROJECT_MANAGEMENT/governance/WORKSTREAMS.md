# Palimpsest License - Workstream Structure

**Framework:** APM (Association for Project Management) Aligned
**Last Updated:** 2025-12-08

---

## Programme Overview

```
                    PALIMPSEST LICENCE PROGRAMME
                              │
        ┌─────────────┬───────┴───────┬─────────────┐
        │             │               │             │
   WORKSTREAM 1  WORKSTREAM 2   WORKSTREAM 3  WORKSTREAM 4
     LEGAL         TECHNICAL      ADOPTION      GOVERNANCE
```

---

## Workstream 1: Legal & Compliance

**Lead:** TBD (Legal Expert on Council)
**Objective:** Establish legally sound, internationally enforceable license

### Work Packages

| WP | Name | Deliverables | Status |
|----|------|--------------|--------|
| 1.1 | v0.4 License Text | Full EN text, NL translation | 🔴 BLOCKED |
| 1.2 | Legal Review | External counsel sign-off | ⬜ NOT STARTED |
| 1.3 | SPDX Registration | SPDX identifier approved | ⬜ NOT STARTED |
| 1.4 | OSI Submission | OSI review process | ⬜ NOT STARTED |
| 1.5 | Jurisdiction Guide | Multi-jurisdiction analysis | ⬜ NOT STARTED |

### Dependencies
- WP 1.1 blocks all other work packages
- WP 1.2 must complete before 1.3, 1.4

### Milestones
- [ ] M1.1: v0.4 EN text complete
- [ ] M1.2: v0.4 NL text complete
- [ ] M1.3: Legal review passed
- [ ] M1.4: SPDX identifier assigned
- [ ] M1.5: OSI review submitted

---

## Workstream 2: Technical Implementation

**Lead:** TBD (Technologist on Council)
**Objective:** Build robust, maintainable tooling in OCaml

### Work Packages

| WP | Name | Deliverables | Status |
|----|------|--------------|--------|
| 2.1 | OCaml Consolidation | Migrate Haskell → OCaml | ⬜ NOT STARTED |
| 2.2 | Metadata Parser | JSON-LD parser (complete) | ✅ COMPLETE |
| 2.3 | Validator Suite | License, bilingual, reference validation | 🔄 MIGRATING |
| 2.4 | Browser Build | Melange → JS output | ✅ COMPLETE |
| 2.5 | CLI Tool | Command-line validator | ⬜ NOT STARTED |
| 2.6 | Consent Registry | MVP backend | ⬜ NOT STARTED |
| 2.7 | Test Coverage | 8/10 for RSR Silver | 🟡 IN PROGRESS |

### Dependencies
- WP 2.1 before 2.3 (migration)
- WP 2.3 before 2.5 (CLI needs validators)
- WP 2.7 ongoing

### Milestones
- [ ] M2.1: Haskell fully deprecated
- [ ] M2.2: OCaml test coverage 80%+
- [ ] M2.3: CLI v1.0 released
- [ ] M2.4: RSR Silver achieved

---

## Workstream 3: Adoption & Outreach

**Lead:** TBD (Communications on Council)
**Objective:** Drive awareness and adoption

### Work Packages

| WP | Name | Deliverables | Status |
|----|------|--------------|--------|
| 3.1 | Branding | Logo, badges, style guide | 🟡 PARTIAL |
| 3.2 | Website | palimpsestlicense.org | ⬜ NOT STARTED |
| 3.3 | Press Kit | Media materials | ✅ COMPLETE |
| 3.4 | Platform Partnerships | GitHub, HuggingFace, etc. | ⬜ NOT STARTED |
| 3.5 | Creator Advocacy | Testimonials, tutorials | ⬜ NOT STARTED |
| 3.6 | Conference Presence | Talks, booths | ⬜ NOT STARTED |

### Dependencies
- WP 3.1 before 3.2 (branding needed for website)
- WP 1.1 before 3.4 (need complete license for partnerships)

### Milestones
- [ ] M3.1: Website launched
- [ ] M3.2: First platform partnership signed
- [ ] M3.3: 1,000 works licensed
- [ ] M3.4: 10,000 works licensed

---

## Workstream 4: Governance & Standards

**Lead:** TBD (Council Chair)
**Objective:** Establish governance and standards recognition

### Work Packages

| WP | Name | Deliverables | Status |
|----|------|--------------|--------|
| 4.1 | Council Formation | 7 members recruited | 🔴 BLOCKED |
| 4.2 | IETF Draft | Internet-Draft submitted | ⬜ NOT STARTED |
| 4.3 | W3C Community Group | CG chartered | ⬜ NOT STARTED |
| 4.4 | DAO Preparation | Governance token design | ⬜ NOT STARTED |
| 4.5 | Community Building | Discord, contributors | 🟡 PARTIAL |

### Dependencies
- WP 4.1 before 4.2, 4.3, 4.4 (council needed for authority)
- WP 1.1 before 4.2, 4.3 (license text needed for submissions)

### Milestones
- [ ] M4.1: Council seated
- [ ] M4.2: IETF draft published
- [ ] M4.3: W3C CG chartered
- [ ] M4.4: First community contributor

---

## Cross-Workstream Dependencies

```
WP 1.1 (v0.4 text)
    │
    ├──► WP 1.2 (Legal Review)
    │       │
    │       ├──► WP 1.3 (SPDX)
    │       └──► WP 1.4 (OSI)
    │
    ├──► WP 3.4 (Platform Partnerships)
    │
    ├──► WP 4.2 (IETF)
    │
    └──► WP 4.3 (W3C)

WP 4.1 (Council)
    │
    ├──► WP 1.2 (Legal Review - council sign-off)
    ├──► WP 4.2 (IETF - authority to submit)
    └──► WP 4.3 (W3C - authority to charter)
```

---

## Resource Allocation

| Role | Workstream | Allocation |
|------|------------|------------|
| Legal Expert | WS1 | 80% |
| Technologist | WS2 | 80% |
| Comms Lead | WS3 | 60% |
| Council Chair | WS4 | 40% |
| All Council | WS1-4 | 20% each |

---

## Reporting Structure

**Weekly:** Workstream leads → Programme Board
**Monthly:** Programme Board → Council
**Quarterly:** Council → Community (public report)

---

## Status Key

- 🔴 BLOCKED - Cannot proceed, external dependency
- 🟡 IN PROGRESS / PARTIAL - Work underway
- ⬜ NOT STARTED - Planned but not begun
- ✅ COMPLETE - Finished
