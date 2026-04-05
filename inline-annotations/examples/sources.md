# Inline Annotation Examples — Per Language

Worked examples of `@trust`, `@contract`, `@grade` annotations in the
languages used across the hyperpolymath estate.

## Rust

```rust
// @trust(level=tested, prover=cargo-test, since=2026-04-01)
// @grade(value=B, basis="47 tests pass, 2 external consumers", assessed=2026-04-04)
pub fn parse_document(input: &str) -> Result<Document, ParseError> {
    // ...
}

// @contract(obligation=must, clause="SHA-pinned action references only", severity=critical)
fn validate_workflow(wf: &Workflow) -> Result<(), Error> {
    // ...
}
```

## Zig

```zig
// @trust(level=proven, prover=idris2, since=2026-03-12)
// @contract(obligation=must, clause="memory layout matches C ABI", severity=critical)
pub export fn groove_encode(buf: [*]u8, len: usize) callconv(.C) i32 {
    // ...
}
```

## Idris2

```idris
-- @trust(level=proven, prover=idris2, since=2026-03-30)
-- @grade(value=A, basis="dependent-type proof, no postulates", assessed=2026-04-01)
export
parseConfig : (s : String) -> Either ParseError Config
parseConfig s = ?hole
```

## Agda

```agda
-- @trust(level=postulate, prover=agda)
-- @contract(obligation=dust, clause="provable constructively — remove before v1.0", severity=high)
postulate
  entropy-nonneg : ∀ (p : Distribution) → 0 ≤ entropy p
```

## ReScript

```rescript
// @trust(level=tested, prover=deno-test)
// @contract(obligation=intent, clause="UI component — accessibility label required")
let make = (~label: string, ~onClick: unit => unit) => {
  // ...
}
```

## Nickel

```nickel
# @grade(value=C, basis="works in staging, not field-validated")
# @contract(obligation=trust, clause="schema matches A2ML v0.1 surface grammar")
{
  surface_grammar = import "grammar.ncl",
}
```

## Bash

```bash
# @trust(level=reviewed, prover=manual, since=2026-04-04)
# @contract(obligation=must, clause="exit non-zero on any sub-step failure", severity=high)
set -euo pipefail
```

## Gleam

```gleam
// @trust(level=tested, prover=gleam-test)
// @grade(value=B, basis="BEAM runtime, 23 property tests", assessed=2026-04-02)
pub fn handle_request(req: Request) -> Response {
  // ...
}
```
