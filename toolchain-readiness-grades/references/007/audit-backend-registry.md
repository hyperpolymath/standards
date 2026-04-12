# 007 MK2 Audit Target: Backend Registry
# Status: DESIGN TARGET — check against actual implementation

## Must Have

| Category | Feature |
|---|---|
| Syntax | Single source of truth for backend registrations |
| Architecture | Modular architecture (hardware/software/hybrid) |
| Context | Discourse-bound context (target arch/optimization flags/ABI) |
| Diagnostics | Unified error reporting (unsupported arch/missing backend) |
| Registration | Static and dynamic registration |

## Should Have

| Category | Feature |
|---|---|
| Interop | Backend interop |
| Selection | Dynamic backend selection |
| Performance | Optimization hints per backend |
| Tooling | Semantic-aware tooling |

## Could Have

| Category | Feature |
|---|---|
| Concurrency | Parallel initialization |
| Switching | Semantic-aware switching |
| Runtime | Runtime backend switching |

## Aspirational

| Category | Feature |
|---|---|
| Inference | Automated backend inference |

## Ecosystem Integration
- Package Manager: OPSM
- IDE: PanLL + Visual Codium (ENSAID)
