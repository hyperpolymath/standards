# 007 MK2 Audit Target: Linker
# Status: DESIGN TARGET — check against actual implementation

## Must Have

| Category | Feature |
|---|---|
| Core | Robust symbol resolution/relocation across all targets |
| Syntax | Stable syntax/semantics for linking |
| Formats | Modular library formats (.a/.so/.dll) |
| Context | Discourse-bound context tracking (shared libs/static linking) |
| Diagnostics | Error reporting for linking (undefined symbols/multiple definitions) |

## Should Have

| Category | Feature |
|---|---|
| Visibility | Cross-platform symbol visibility |
| Interop | Cross-language interop |
| Performance | Optimization hints (symbol table compression) |
| Builds | Incremental linking |

## Could Have

| Category | Feature |
|---|---|
| Tooling | Semantic-aware tooling |
| Concurrency | Parallel symbol resolution |
| Scripts | Semantic-aware linker scripts |

## Aspirational

| Category | Feature |
|---|---|
| Inference | Automated inference for linking |
| Verification | Formal proofs of linking equivalence |
| Runtime | Runtime semantic switching for linking |

## Ecosystem Integration
- Package Manager: OPSM
- IDE: PanLL + Visual Codium (ENSAID)
