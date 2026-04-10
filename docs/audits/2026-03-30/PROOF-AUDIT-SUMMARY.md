# PROOF-AUDIT-SUMMARY — 2026-03-30

## Emergency tranche (still blocking any release/publication)
- **007-lang**: Type soundness progress/preservation, Harvard separation without `believe_me`, session duality, linear resource safety, budget monotonicity, actor isolation, and the Elixir codegen bisimulation theorem all still live in `PROOF-NEEDS.md` (Idris2/Lean4 primary). None of these have machine-checked replacements yet, so the compound “Proven” pillar in the PPPPP gate is incomplete.  
- **typed-wasm**: Multi-module safety, lifetime/region interaction, tropical type semantics, and linear consumption proofs are flagged as “needs finishing” inside `PROOF-NEEDS.md`; the repo already has 11 Idris2 modules, but they need completion and CI proof logs before any claim about memory safety can stand.  
- **vql-ut**: The Idris2 core (Checker, Grammar, Levels, Schema) needs total verifier proofs, and the ReScript bridge must be formally linked to the Idris2 semantics; the `PROOF-NEEDS` page calls these “high priority.”  
- **patch-bridge**: ABI folder is empty; CVE classification, reachability, registry lookup, and patch decision gate proofs still await Idris2 definitions.

## Pillars to shore up next
- **panic-attacker**, **verisimdb**, **echidna**, **hypatia**, **absolute-zero**, **januskey**, **panll**, etc. — each repo needs contractile-triggered proofs (k9 + intent) before we can bump the CRG grade. Continue to feed their proof debt into `CLAUDE-WORK.md`.

## Action items
- Ensure every proof release includes the command + tool version that generated it (Idris2/Lean4/Agda logs) so we can cite the “LLM Proof Trust Statement.”  
- Keep `PROOF-NEEDS.md` up to date when we replace `believe_me`/`postulate` with real proofs.  
- When a proof is complete, log it in the PPPPP pipeline (per `AUDIT-V2.adoc`) before touching `PAPER-STATUS`.
