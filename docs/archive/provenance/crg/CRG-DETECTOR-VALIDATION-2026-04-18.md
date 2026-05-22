# CRG Detector Validation — 2026-04-18

Validated Bucket-A self-consistency assumptions against current `STATE.a2ml` values for the 17 repos listed in `CRG-BULK-TRIAGE-2026-04-18.md`.

## Result Summary
- 15/17 still match auto-demotion conditions (including 3 repos with missing `overall-completion`).
- 2/17 no longer match (stale triage values):
  - `developer-ecosystem/valence-shell` (`version=0.9.0`, `completion=74`)
  - `007-lang` (`version=0.1.0`, `completion=55`)
- Rule updates applied in `HYP-S005` to treat missing `overall-completion` as a high-severity self-consistency failure for `C/B/A` claims.

## Validation Table
| Repo | version | completion | dogfooding-status | Bucket-A match now |
|---|---:|---:|---|---|
| aerie | 0.1.0 | 40 | absent | yes |
| systems-ecosystem/flatracoon/netstack/modules/zerotier-k8s-link | 0.1.0 | 40 | absent | yes |
| document-management-toolset/universal-chat-extractor | 0.1.0 | 5 | absent | yes |
| verification-ecosystem/thunderbird-template-reloaded | 0.1.0 | 5 | absent | yes |
| fleet-ecosystem/boinc-boinc | 0.1.0 | 0 | absent | yes |
| social-media-ecosystem/social-media-tools | 0.1.0 | 10 | absent | yes |
| verification-ecosystem/zerotier-k8s-link | 0.1.0 | 40 | absent | yes |
| fleet-ecosystem/infrastructure-automation | 0.1.0 | 0 | absent | yes |
| verification-ecosystem/rrecord-verity | 0.1.0 | 35 | absent | yes |
| developer-ecosystem/rescript-ecosystem/idaptik-rescript13-staging | 0.1.0 | 0 | absent | yes |
| verification-ecosystem/tropical-resource-typing | 0.1.0 | 30 | absent | yes |
| verification-ecosystem/a2ml-showcase | 0.1.0 | absent | absent | yes (missing overall-completion) |
| developer-ecosystem/nextgen-languages/anvomidav | 0.1.0 | absent | absent | yes (missing overall-completion) |
| developer-ecosystem | 0.1.0 | 45 | absent | yes |
| verification-ecosystem/k9-ecosystem/k9-showcase | 0.1.0 | absent | absent | yes (missing overall-completion) |
| developer-ecosystem/valence-shell | 0.9.0 | 74 | absent | no |
| 007-lang | 0.1.0 | 55 | absent | no |
