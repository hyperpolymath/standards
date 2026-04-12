# 007 MK2 Audit Target: Standard Library
# Status: DESIGN TARGET — check against actual implementation

## Must Have

| Category | Feature |
|---|---|
| Syntax | Single source of truth for utilities (I/O/collections/math/concurrency) |
| Architecture | Modular design (io/collections/concurrency) |
| Context | Discourse-bound context (thread safety/async/memory safety) |
| Diagnostics | Error reporting for stdlib (FileNotFoundError/KeyError) |
| APIs | Sync and async APIs |

## Should Have

| Category | Feature |
|---|---|
| Interop | Third-party interop |
| Selection | Feature flags (std::fs vs std::async) |
| Performance | Optimization hints (Vec::reserve) |
| Tooling | Semantic-aware tooling |

## Could Have

| Category | Feature |
|---|---|
| Concurrency | Lock-free collections/async I/O |
| Targets | Target-specific implementations |
| Runtime | Runtime feature switching |

## Aspirational

| Category | Feature |
|---|---|
| Inference | Automated inference |
| Verification | Formal proofs of correctness (HashMap collision-free) |
| Consistency | Language-wide consistency checks (UnsafeCell soundness) |

## Ecosystem Integration
- Package Manager: OPSM
- IDE: PanLL + Visual Codium (ENSAID)
