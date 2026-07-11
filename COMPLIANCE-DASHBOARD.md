<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- COMPLIANCE-DASHBOARD.md — DERIVED from .machine_readable/scorecards/*.scorecard.a2ml -->
<!-- GENERATED FILE — DO NOT EDIT BY HAND. Run: just scorecards (scripts/build-scorecards.sh) -->

# Standards Compliance Dashboard (derived)

> Generated from `.machine_readable/scorecards/<spec-id>.scorecard.a2ml` by
> `scripts/build-scorecards.sh`. One scorecard per LOCAL spec in
> `.machine_readable/REGISTRY.a2ml`. Do not edit by hand — edit the scorecards.
>
> **How to read this.** Each spec is audited as MUST / SHOULD / COULD
> requirements. **MUST-status** is the compliance verdict: ✅ met (every MUST
> passes or is manual-only) or ❌ gap (some MUST fails). **Systems coverage**
> is the share of requirements with a real mechanical check (`system` ≠ `none`)
> — the honest measure of *enforcement vs. assertion*. **Aspirational**
> requirements (intentionally-unreachable reach targets) are never counted as
> passing.

## Per-spec scorecards

| Spec | MUST status | MUST (pass/total) | SHOULD (pass/total) | COULD (pass/total) | Systems coverage | Assessed |
|---|---|---|---|---|---|---|
| `estate-constitution` | ❌ gap | 2/4 | 1/1 | 0/0 | 60% | 2026-07-11 |
| `a2ml` | ✅ met | 4/5 | 4/5 | 0/3 | 84% | 2026-07-03 |
| `k9-svc` | ❌ gap | 3/6 | 3/5 | 2/3 | 100% | 2026-07-03 |
| `contractiles` | ❌ gap | 0/5 | 0/3 | 0/3 | 54% | 2026-07-03 |
| `meta-a2ml` | ❌ gap | 1/5 | 1/4 | 1/3 | 83% | 2026-07-03 |
| `state-a2ml` | ❌ gap | 1/5 | 1/4 | 1/3 | 50% | 2026-07-03 |
| `ecosystem-a2ml` | ❌ gap | 2/5 | 0/4 | 0/3 | 41% | 2026-07-03 |
| `agentic-a2ml` | ❌ gap | 1/5 | 0/4 | 0/3 | 100% | 2026-07-03 |
| `neurosym-a2ml` | ❌ gap | 1/5 | 0/4 | 0/3 | 75% | 2026-07-03 |
| `playbook-a2ml` | ❌ gap | 1/5 | 0/4 | 0/3 | 0% | 2026-07-03 |
| `anchor-a2ml` | ❌ gap | 0/5 | 0/5 | 0/3 | 15% | 2026-07-03 |
| `0-ai-gatekeeper-protocol` | ❌ gap | 3/5 | 0/4 | 0/2 | 54% | 2026-07-03 |
| `k9-coordination-protocol` | ❌ gap | 3/5 | 2/4 | 0/3 | 100% | 2026-07-03 |
| `avow-protocol` | ❌ gap | 1/5 | 2/4 | 0/3 | 58% | 2026-07-03 |
| `axel-protocol` | ❌ gap | 0/5 | 4/5 | 0/3 | 92% | 2026-07-03 |
| `overlay-protocol` | ❌ gap | 1/5 | 0/4 | 0/3 | 50% | 2026-07-03 |
| `consent-aware-http` | ❌ gap | 1/5 | 1/5 | 0/3 | 69% | 2026-07-03 |
| `adoption-readiness-grades` | ❌ gap | 1/5 | 1/4 | 0/4 | 84% | 2026-07-03 |
| `foundations-readiness-grades` | ❌ gap | 2/5 | 0/4 | 0/2 | 72% | 2026-07-03 |
| `component-readiness-grades` | ❌ gap | 2/5 | 2/4 | 0/3 | 66% | 2026-07-03 |
| `toolchain-readiness-grades` | ❌ gap | 1/5 | 2/4 | 0/3 | 83% | 2026-07-03 |
| `rhodium-standard-repositories` | ❌ gap | 2/3 | 1/2 | 0/1 | 50% | 2026-07-03 |
| `session-management-standards` | ❌ gap | 1/5 | 1/4 | 0/3 | 41% | 2026-07-03 |
| `did-you-actually-do-that` | ✅ met | 5/5 | 2/3 | 0/2 | 90% | 2026-07-03 |
| `ensaid-config` | ❌ gap | 0/5 | 0/3 | 0/3 | 90% | 2026-07-03 |
| `accessibility` | ❌ gap | 2/5 | 0/5 | 0/3 | 100% | 2026-07-03 |
| `publication-pre-flight` | ❌ gap | 0/5 | 0/4 | 0/2 | 36% | 2026-07-03 |
| `release-pre-flight` | ❌ gap | 4/5 | 3/4 | 0/2 | 72% | 2026-07-03 |
| `hypatia-rules` | ❌ gap | 2/4 | 1/3 | 0/3 | 100% | 2026-07-03 |
| `a2ml-templates` | ❌ gap | 1/5 | 1/3 | 0/2 | 10% | 2026-07-03 |

## Estate rollup

- **Specs registered (local):** 30
- **Specs with a scorecard:** 30 / 30
- **MUST requirements:** 48 passing / 147 total (75 failing)
- **Estate systems coverage:** 67% of 343 graded requirements have a mechanical check

## How this dashboard stays honest

```
scorecards/*.scorecard.a2ml ──► scripts/build-scorecards.sh ──► COMPLIANCE-DASHBOARD.md
        (hand-authored)                      │
   validated vs scorecard.schema.json        ▼
                                    just scorecards-check (CI)
```

- A `pass` requires cited `evidence`; the generator rejects a pass without it.
- `aspirational` requirements never count as passing (no intuition-plucked
  Grade-A gate can inflate a score — standards#446).
- `system = "none"` is legal but visible, and lowers systems coverage.
- Regenerate after editing any scorecard: `just scorecards`.
