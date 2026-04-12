# 007 MK2 Audit Target: Aggregate Library
# Status: DESIGN TARGET — check against actual implementation

## Must Have

| Category | Feature |
|---|---|
| Syntax | Unified source of truth for ALL tools |
| Architecture | Modular architecture with clear separation |
| Context | Cross-tool discourse-bound context |
| Diagnostics | Unified error reporting system |
| Analysis | Static and dynamic analysis support |

## Should Have

| Category | Feature |
|---|---|
| Interop | Tool interoperability (lexer→parser→AST→typechecker→compiler→linker→interpreter) |
| Selection | Dynamic semantic selection |
| Performance | Optimization hints per tool |
| Tooling | Adaptive semantic-aware tooling |

## Could Have

| Category | Feature |
|---|---|
| Concurrency | Parallel tooling |
| Codegen | Semantic-aware codegen per target |
| Runtime | Runtime tool switching |

## Aspirational

| Category | Feature |
|---|---|
| Inference | Cross-tool automated inference |
| Verification | Cross-tool formal proofs |
| Consistency | Cross-tool consistency checks |

## Ecosystem Integration
- Package Manager: OPSM
- IDE: PanLL + Visual Codium (ENSAID)
