<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- TOPOLOGY.md — DERIVED architecture map (generated from REGISTRY.a2ml + STATE.a2ml) -->
<!-- GENERATED FILE — DO NOT EDIT BY HAND. Run: just topology (scripts/build-registry.sh) -->

# Hyperpolymath Standards — Topology (derived)

> This file is **generated** from `.machine_readable/REGISTRY.a2ml` and
> `.machine_readable/6a2/STATE.a2ml` by `scripts/build-registry.sh`.
> It cannot freeze: every regeneration re-reads ground truth. Do not edit by hand.

- **Phase:** active  &nbsp;|&nbsp; **Maturity:** experimental  &nbsp;|&nbsp; **STATE last-updated:** 2026-06-03T00:00:00Z
- **Registry entries:** 33 specs across 6 streams
- **Front door:** human → [README.adoc](README.adoc); machine → [0-AI-MANIFEST.a2ml](0-AI-MANIFEST.a2ml)
- **Registry:** [.machine_readable/REGISTRY.a2ml](.machine_readable/REGISTRY.a2ml) (index + source hashes) · prose: [REGISTRY.adoc](REGISTRY.adoc)

## Specs by stream

### Foundation — A2ML family + K9 + contractiles (Stream 1)

| Spec | Home | If you want… |
|---|---|---|
| A2ML — Attested Markup Language | [`a2ml/`](a2ml/) | the typed/verified machine-readable document format |
| K9 Self-Validating Components | [`k9-svc/`](k9-svc/) | self-validating components with embedded contracts + deploy logic |
| Contractiles (Must/Trust/Dust/Intend) | [`contractiles/`](contractiles/) | policy-enforcement primitives the K9 layer is built from |
| META.a2ml spec | [`meta-a2ml/`](meta-a2ml/) | architecture decisions / governance metadata format |
| STATE.a2ml spec | [`state-a2ml/`](state-a2ml/) | project-state metadata format (drives this registry's topology) |
| ECOSYSTEM.a2ml spec | [`ecosystem-a2ml/`](ecosystem-a2ml/) | ecosystem-positioning metadata format |
| AGENTIC.a2ml spec | [`agentic-a2ml/`](agentic-a2ml/) | AI-agent operational gating / entropy budgets |
| NEUROSYM.a2ml spec | [`neurosym-a2ml/`](neurosym-a2ml/) | symbolic semantics / proof obligations |
| PLAYBOOK.a2ml spec | [`playbook-a2ml/`](playbook-a2ml/) | executable operational runbooks |
| ANCHOR.a2ml spec | [`anchor-a2ml/`](anchor-a2ml/) | project-recalibration intervention format |

### Language — AffineScript + language policy (Stream 2)

| Spec | Home | If you want… |
|---|---|---|
| AffineScript .affine (faces / source documents) | [`hyperpolymath/affinescript`](https://github.com/hyperpolymath/affinescript/blob/main/spec/affine.adoc) `@ v2.0.0` ⇗ | faces, canonical-lowering invariant, canonical islands, idiom packs, mimicry bindings, project face policy |
| AffineScript .affex (face-interop manifest) | [`hyperpolymath/affinescript`](https://github.com/hyperpolymath/affinescript/blob/main/spec/affex.adoc) `@ v2.0.0` ⇗ | derived regenerable manifest; declaration heads not full bodies; format_version bumps independently |
| AffineScript .affmap (provenance) | [`hyperpolymath/affinescript`](https://github.com/hyperpolymath/affinescript/blob/main/spec/affmap.adoc) `@ v2.0.0` ⇗ | provenance format; own pointer for independent staleness tracking |

### Protocols

| Spec | Home | If you want… |
|---|---|---|
| 0-AI Gatekeeper Protocol | [`0-ai-gatekeeper-protocol/`](0-ai-gatekeeper-protocol/) | the AI-agent entry/gating protocol behind 0-AI-MANIFEST |
| K9 Coordination Protocol | [`k9-coordination-protocol/`](k9-coordination-protocol/) | multi-agent coordination on top of K9 |
| AVOW Protocol | [`avow-protocol/`](avow-protocol/) | consent-attested messaging / origin attribution |
| AXEL Protocol | [`axel-protocol/`](axel-protocol/) | age-gating + explicit-content enforcement |
| Overlay Protocol | [`overlay-protocol/`](overlay-protocol/) | layered overlay composition spec |
| Consent-Aware HTTP | [`consent-aware-http/`](consent-aware-http/) | consent headers / AI-usage boundaries for HTTP |

### Governance — RSR, gates, session standards

| Spec | Home | If you want… |
|---|---|---|
| Hyperpolymath Estate Constitution | [`constitution/`](constitution/) | the highest estate-level rules, authority precedence, assurance, contribution, exceptions, and known tensions |
| RSR — Rhodium Standard Repositories | [`rhodium-standard-repositories/`](rhodium-standard-repositories/) | the repository-compliance standard every repo is graded against |
| Session Management Standards | [`session-management-standards/`](session-management-standards/) | continuity / verify / handover protocols |
| DYADT — Did-You-Actually-Do-That | [`did-you-actually-do-that/`](did-you-actually-do-that/) | post-action agent-claim verification (Tier 4 accountability) |
| ENSAID Config | [`ensaid-config/`](ensaid-config/) | the ensaid configuration standard |
| Accessibility Standard | [`accessibility/`](accessibility/) | estate accessibility requirements |
| Publication Pre-Flight | [`publication-pre-flight/`](publication-pre-flight/) | submission gate (HOL + Zenodo checklists) |
| Release Pre-Flight (V1 Gate) | [`release-pre-flight/`](release-pre-flight/) | hard v1.0.0 audit requirements |

### Readiness grading — ARG / FRG / CRG / TRG

| Spec | Home | If you want… |
|---|---|---|
| ARG — Adoption Readiness Grades | [`adoption-readiness-grades/`](adoption-readiness-grades/) | per-language adoption-maturity profile templates |
| FRG — Foundations Readiness Grades | [`foundations-readiness-grades/`](foundations-readiness-grades/) | per-language foundational-maturity profile templates |
| CRG — Component Readiness Grades | [`component-readiness-grades/`](component-readiness-grades/) | the X..A grading system for components |
| TRG — Toolchain Readiness Grades | [`toolchain-readiness-grades/`](toolchain-readiness-grades/) | per-toolchain readiness profile templates |

### Integration — registry, hypatia rules, templates (Stream 3)

| Spec | Home | If you want… |
|---|---|---|
| Standards Hypatia Rules | [`hypatia-rules/`](hypatia-rules/) | the dogfooding rules that scan THIS repo (incl. drift detection) |
| A2ML Templates | [`a2ml-templates/`](a2ml-templates/) | copy-in templates for the 7 A2ML files |

## How this map stays honest

```
file tree + STATE.a2ml ──► scripts/build-registry.sh ──► REGISTRY.a2ml ──► TOPOLOGY.md
                                      ▲                        │
                                      │                        ▼
                          just registry / CI            HYP-S006 (registry-staleness)
                          (registry-verify.yml)         emits doc.drift on hash mismatch
```

Regenerate after any spec change: `just registry` (writes REGISTRY.a2ml + TOPOLOGY.md).
CI (`registry-verify.yml`) runs `--check` and fails the build if either is stale.
