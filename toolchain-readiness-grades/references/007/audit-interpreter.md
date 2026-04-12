# 007 MK2 Audit Target: Interpreter
# Status: DESIGN TARGET — check against actual implementation

## Must Have

| Category | Feature |
|---|---|
| Architecture | Single interpreter core with modular language backends |
| Syntax | Stable lexer/parser as single source of truth |
| Context | Discourse-bound context tracking |
| Diagnostics | Error reporting tailored to active language |
| Execution | Support for interpreted and JIT execution paths |

## Should Have

| Category | Feature |
|---|---|
| Interop | Interoperability between languages |
| Selection | Dynamic semantic selection via annotations |
| Performance | Optimization hints for execution context |
| Tooling | Semantic-aware tooling |

## Could Have

| Category | Feature |
|---|---|
| Concurrency | Concurrency models per language |
| Codegen | Semantic-aware codegen |
| Runtime | Runtime semantic switching |

## Aspirational

| Category | Feature |
|---|---|
| Inference | Automated semantic inference |
| Verification | Formal proofs of semantic equivalence |
| Consistency | Language-wide consistency checks |

## Ecosystem Integration
- Package Manager: OPSM
- IDE: PanLL + Visual Codium (ENSAID)
