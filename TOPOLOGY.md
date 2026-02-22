<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

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
                        │  │ Build     │  │  SCM Metadata     │  │
                        │  │ System    │  │  Family (7)       │  │
                        │  │ (Mustfile)│  │ (STATE, META, etc)│  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        └────────│─────────────────│──────────────┘
                                 │                 │
                                 ▼                 ▼
                        ┌─────────────────────────────────────────┐
                        │          SPECIFICATION MODULES          │
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ meta-scm  │  │ agentic-  │  │ neuro-││
                        │  │           │  │ scm       │  │ sym   ││
                        │  └───────────┘  └───────────┘  └───────┘│
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ state-scm │  │ playbook- │  │ anchor││
                        │  │           │  │ scm       │  │ scm   ││
                        │  └───────────┘  └───────────┘  └───────┘│
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
  meta-scm / state-scm              ██████████ 100%    ABNF & IANA specs stable
  agentic-scm (Execution)           ██████████ 100%    Entropy budgets verified
  neurosym-scm                      ██████████ 100%    Proof obligations active
  playbook-scm / anchor-scm         ██████████ 100%    Realign/Plan specs stable

REPO INFRASTRUCTURE
  Justfile / Mustfile               ██████████ 100%    Standard build tasks verified
  .machine_readable/                ██████████ 100%    STATE tracking active
  Multi-Forge Enforcement           ██████████ 100%    CI/CD quality gates verified

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ██████████ 100%    Canonical Standards Stable
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
