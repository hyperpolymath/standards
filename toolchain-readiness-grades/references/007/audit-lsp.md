# 007 MK2 Audit Target: LSP
# Status: DESIGN TARGET — check against actual implementation

## Must Have

| Category | Feature |
|---|---|
| Syntax | Single source of truth for LSP semantics |
| Architecture | Modular language servers |
| Context | Discourse-bound context (project scope/open files/cursor) |
| Diagnostics | Unified error reporting for LSP |
| Features | Real-time diagnostics/hover/navigation |

## Should Have

| Category | Feature |
|---|---|
| Interop | Multi-client interop (VS Code/Neovim/Emacs) |
| Selection | Dynamic semantic selection |
| Performance | Incremental parsing hints |
| Tooling | Semantic-aware tooling |

## Could Have

| Category | Feature |
|---|---|
| Concurrency | Parallel diagnostics |
| Codegen | Semantic-aware codegen |
| Runtime | Runtime semantic switching |

## Aspirational

| Category | Feature |
|---|---|
| Inference | Automated inference |
| Verification | Formal proofs of LSP equivalence |
| Consistency | Language-wide consistency checks |

## Ecosystem Integration
- Package Manager: OPSM
- IDE: PanLL + Visual Codium (ENSAID)
