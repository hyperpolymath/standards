# TEST-AUDIT-SUMMARY — 2026-03-30

## Emergency tranche (blocking the PPPPP gate)
- **007-lang** (`TEST-NEEDS.md`): 728 unit tests mostly cover parser/evaluator; zero P2P tests for modules like codegen, optimizer, JIT, module system, etc.; no E2E pipeline, no panic-attack/Hypatia runs, no benchmarks covering multi-module or JIT performance, no `panic-attack assail`, and no accessible self-tests.  
- **typed-wasm**: Only one parser unit test (`ParserTests.res`), a 43-assertion smoke E2E, and a placeholder fuzz file labelled as fake — still no benchmarks, no coverage for the 10-level type system, no multi-module linking test, and no security/performance aspects.  
- **vql-ut**: 49 unit tests for 27 modules; zero E2E workflows (parse→typecheck→execute), zero LSP/DAP/formatter integration, no concurrency/error handling/bench aspect tests, and the same fake fuzz placeholder.  
- **patch-bridge**: ~14 inline tests, zero E2E/cross-format lockfile multi-stage pipelines, no benchmarks, no panic-attack/Hypatia, no security/performance/execution tests, and the placeholder fuzz file flagged as “fake.”  

## Next tier (paper-worthy) tests
- Pillar repos (panic-attacker, verisimdb, echidna, hypatia, etc.) must each run aspect tests (security, performance, concurrency, accessibility) plus the release-level benchmarks described in their `TEST-NEEDS` page before being allowed to claim B-level maturity.  
- `statist ease` (once located) needs its own test plan; use `STATISTEASE-PLAN.md` to map the bench/analysis matrix once the document is available.

## Action items
- Add panic-attack/Hypatia/bench logs to the PPPPP pipeline for each release candidate before upgrading the CRG grade.  
- Replace any `tests/fuzz/placeholder.txt` with real harnesses or delete the files to avoid fake coverage.  
- Each repo flagged as “beta unstable” must still have an accessible regression suite before it graduates to “beta stable.”
