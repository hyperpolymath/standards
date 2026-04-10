# PAPER-STATUS — 2026-03-30

| Paper / Repo | Venue Target | Evidence Status | Next Steps |
|-------------|--------------|-----------------|------------|
| `007-lang-private-docs/paper/007-agent-meta-language.tex` | HOL / Zenodo | Proofs outstanding (soundness/budget/isolation), tests missing → not ready. | Finish declaring theorems in Idris2/Lean4, run e2e and panic-attack, then rerun PPPPP gate. |
| `typed-wasm/docs/arxiv/typed-wasm.tex` | arXiv/HAL | Claims: cross-module memory safety, linearity levels; proof modules exist but not fully audited; tests/benchmarks insufficient. | Extend proofs (multi-module, lifetime/linear) and bench pipeline, rerun PPPPP. |
| `vql-ut/arcvix-10-level-query-safety.tex` | HOL / Zenodo | Idris2 core proofs still labelled `needs proving`; 10-level type system lacking level-specific coverage. | Finish Idris2 proofs, add LSP/DAP/E2E tests, align PPPPP content, re-run audit. |
| `stapeln/arcvix-logic-driven-container-security.tex` | HOL / Zenodo | Accessibility audit present; release tagging depends on bigger proofs/tests in other repos. | Highlight PPPPP pipeline outputs and cite `ACCESSIBILITY-AUDIT-2026-03-29`; ensure contractiles logged. |
| `wokelang/arxiv-consent-aware-programming.tex` | arXiv | Release pending once VQL-UT proofs settle (shared infrastructure). | Wait for VQL-UT to hit beta stable; then confirm docs reference the final PPPPP evidence. |
| Additional candidates (`verisimdb/WHITEPAPER.md`, `valence-shell/arcvix-formally-verified-reversible-shell.tex`, `ephapax/arcvix-code-as-matter.tex`) | HAL / Zenodo | Varies; mostly behind standard release gating (proof/test/bench). | Collect PPPPP evidence, ensure `PAPER-STATUS` updated before submission. |

## Action items
- No publication (paper, blog, release note) is allowed until the PPPPP pipeline in `AUDIT-V2.adoc` is green.  
- When a paper cites LLM assistance, attach the mechanised proof logs as required by `LLM-PROOF-TRUST.md`.  
- Mark any paper that still describes conjectures as “DRAFT” and keep it in the special backlog until the proof/test audits are complete.
