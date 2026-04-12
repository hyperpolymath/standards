# 007 MK2 Audit Target: AST
# Status: DESIGN TARGET — check against actual implementation

## Must Have

| Category | Feature |
|---|---|
| Architecture | Unified AST with modular node types (Expr/Stmt/Decl) |
| Syntax | Stable syntax/semantics as single source of truth |
| Context | Discourse-bound context tracking |
| Diagnostics | Error reporting for AST construction |
| Serialization | Serialization/deserialization (JSON/binary) |

## Should Have

| Category | Feature |
|---|---|
| Interop | Cross-language interop |
| Selection | Dynamic semantic selection (@dynamic_ast) |
| Performance | Optimization hints (dead code elimination) |
| Tooling | Semantic-aware tooling |

## Could Have

| Category | Feature |
|---|---|
| Concurrency | Parallel AST visitors |
| Transforms | Semantic-aware AST transformations |
| Runtime | Runtime semantic switching |

## Aspirational

| Category | Feature |
|---|---|
| Inference | Automated inference |
| Verification | Formal proofs of AST equivalence |
| Consistency | Language-wide consistency checks |

## Ecosystem Integration
- Package Manager: OPSM
- IDE: PanLL + Visual Codium (ENSAID)
