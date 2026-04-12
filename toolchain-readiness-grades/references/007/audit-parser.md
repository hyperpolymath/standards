# 007 MK2 Audit Target: Parser
# Status: DESIGN TARGET — check against actual implementation

## Must Have

| Category | Feature |
|---|---|
| Syntax | Stable lexer as single source of truth |
| Architecture | Modular parser backends for different grammars |
| Context | Discourse-bound context (precedence/associativity/macros) |
| Diagnostics | Error reporting (unexpected tokens/incomplete expressions) |
| Strategies | Multiple parsing strategies (recursive descent/Pratt/GLR) |

## Should Have

| Category | Feature |
|---|---|
| Interop | Mixed-language/embedded DSL parsing |
| Selection | Dynamic semantic selection (@dynamic_parser) |
| Performance | Optimization hints per grammar |
| Tooling | Semantic-aware tooling (highlighters/formatters/LSP) |

## Could Have

| Category | Feature |
|---|---|
| Concurrency | Parallel parsing |
| Output | Semantic-aware parser outputs |
| Runtime | Runtime semantic switching |

## Aspirational

| Category | Feature |
|---|---|
| Inference | Automated inference |
| Verification | Formal proofs of parsing equivalence |
| Consistency | Language-wide consistency checks |

## Ecosystem Integration
- Package Manager: OPSM
- IDE: PanLL + Visual Codium (ENSAID)
