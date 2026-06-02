<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-04-04 -->

# Hyperpolymath Standards — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              EPISTEMIC FLEET            │
                        │        (275+ Target Repositories)       │
                        └───────────────────┬─────────────────────┘
                                            │ Policy Enforcement
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           STANDARDS HUB LAYER           │
                        │                                         │
                        │  ┌───────────┐  ┌───────────────────┐  │
                        │  │ Language  │  │  Governance       │  │
                        │  │ Policy    │  │  Templates        │  │
                        │  │ (CCCP)    │  │ (RSR Scaffolding) │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        │        │                 │              │
                        │  ┌─────▼─────┐  ┌────────▼──────────┐  │
                        │  │ Build     │  │  A2ML Metadata    │  │
                        │  │ System    │  │  Family (7)       │  │
                        │  │ (Mustfile)│  │ (STATE, META, etc)│  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        └────────│─────────────────│──────────────┘
                                 │                 │
                                 ▼                 ▼
                        ┌─────────────────────────────────────────┐
                        │        A2ML SPECIFICATION MODULES       │
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ meta-a2ml │  │ agentic-  │  │neuro- ││
                        │  │           │  │ a2ml      │  │sym    ││
                        │  └───────────┘  └───────────┘  └───────┘│
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │state-a2ml │  │ playbook- │  │anchor-││
                        │  │           │  │ a2ml      │  │a2ml   ││
                        │  └───────────┘  └───────────┘  └───────┘│
                        │  ┌───────────────────────────────────┐  │
                        │  │        ecosystem-a2ml             │  │
                        │  └───────────────────────────────────┘  │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │           PROTOCOL SPECIFICATIONS       │
                        │  ┌───────┐ ┌──────┐ ┌──────┐ ┌──────┐  │
                        │  │Groove │ │ AVOW │ │ AXEL │ │  K9  │  │
                        │  └───────┘ └──────┘ └──────┘ └──────┘  │
                        │  ┌──────────────┐ ┌─────────────────┐   │
                        │  │Consent-HTTP  │ │Overlay Protocol │   │
                        │  └──────────────┘ └─────────────────┘   │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │         INTEGRATION & TOOLING           │
                        │  VeriSimDB (:8097)   Hypatia Scan       │
                        │  ECHIDNA Proofs      PanLL Panels       │
                        │  Groove Registry     K9 Coordination    │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Justfile Automation  .machine_readable/  │
                        │  CI/CD Workflows      0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
CORE STANDARDS
  Language Policy (CCCP)            ██████████ 100%    Approved/Banned list verified
  SCM Format Family (7)             ██████████ 100%    All 7 specs stable & absorbed
  RSR Compliance Framework          ██████████ 100%    Rhodium tiers defined
  Governance Templates              ██████████ 100%    CODE_OF_CONDUCT/etc verified

SPECIFICATION MODULES
  meta-a2ml / state-a2ml             ██████████ 100%    ABNF & IANA specs stable
  agentic-a2ml (Execution)          ██████████ 100%    Entropy budgets verified
  neurosym-a2ml                     ██████████ 100%    Proof obligations active
  playbook-a2ml / anchor-a2ml       ██████████ 100%    Realign/Plan specs stable
  ecosystem-a2ml                    ██████████ 100%    Ecosystem positioning stable

PROTOCOL SPECIFICATIONS
  Groove Protocol                   ████████░░  80%    WIP: reference impls needed
  AVOW Protocol                     ██████░░░░  60%    Draft spec; 0 tests
  AXEL Protocol                     ████████░░  80%    Beta; 14 tests
  K9 Self-Validating Components     ██████████ 100%    Stable; 45 tests
  K9 Coordination Protocol          ████████░░  80%    Phase 1 (Defensive)
  Consent-Aware HTTP                ██████░░░░  60%    Draft; spec-focused
  Overlay Protocol                  ████░░░░░░  40%    Spec only; sparse impl

INTEGRATION & DOGFOODING
  VeriSimDB Instance                ░░░░░░░░░░   0%    Not yet configured
  Hypatia Self-Scan                 ░░░░░░░░░░   0%    Workflow missing
  ECHIDNA Proof Verification        ░░░░░░░░░░   0%    Proofs exist but unverified
  PanLL Panels                      ░░░░░░░░░░   0%    No standards panels yet
  CRG Self-Assessment               ░░░░░░░░░░   0%    Standards not self-graded

REPO INFRASTRUCTURE
  Justfile / Mustfile               ██████████ 100%    Standard build tasks verified
  .machine_readable/                ██████████ 100%    STATE tracking active
  Multi-Forge Enforcement           ██████████ 100%    CI/CD quality gates verified

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ████████░░  80%    Core stable; integration layer needed
```

## Key Dependencies

```
Philosophy ──────► Standards Spec ──────► Implementation ─────► Audit
     │                 │                      │                 │
     ▼                 ▼                      ▼                 ▼
CCCP Policy ─────► 6SCM Family ────────► Repository ────────► Compliance
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
