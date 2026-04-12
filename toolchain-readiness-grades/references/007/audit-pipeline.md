# 007 MK2 Audit Target: Pipeline
# Status: DESIGN TARGET — check against actual implementation

## Must Have

| Category | Feature |
|---|---|
| Architecture | Unified end-to-end pipeline (source→executable) |
| Syntax | Single source of truth for all stages |
| Context | Cross-stage discourse-bound context |
| Diagnostics | Unified error reporting for all stages |
| Builds | Incremental builds |

## Should Have

| Category | Feature |
|---|---|
| Interop | Seamless stage interop |
| Selection | Dynamic semantic selection |
| Performance | Stage-specific optimization hints |
| Tooling | Adaptive tooling per stage |

## Could Have

| Category | Feature |
|---|---|
| Concurrency | Parallel stages |
| Codegen | Semantic-aware codegen |
| Runtime | Runtime stage switching |

## Aspirational

| Category | Feature |
|---|---|
| Inference | Cross-pipeline inference |
| Verification | Cross-pipeline formal proofs |
| Consistency | Cross-pipeline consistency checks |

## Ecosystem Integration
- Package Manager: OPSM
- IDE: PanLL + Visual Codium (ENSAID)
