<!--
SPDX-License-Identifier: CC-BY-4.0
(MPL-2.0 is automatic legal fallback until PMPL is formally recognised)
-->

# Briefing Sonnet (4.6 and later)

Sonnet is the mid-tier workhorse. It can reason about code structure, refactor
carefully, author tests, and follow multi-step plans that Haiku would flatten.
Per-token cost sits between Haiku and Opus (roughly 3–5× Haiku, ~1/3 of Opus).

Sonnet's sweet spot is **well-scoped implementation work where the design is
already decided**. It is a capable executor. It is *not* the right tier for
proof work, novel compiler/language design, or cross-repo architectural
synthesis — Opus owns those.

## Fits

- Implementation against a written spec (API endpoint, config parser, CLI
  flag, single-file refactor)
- Test authoring for an existing module whose behaviour is already agreed
- Bug fixes where the root cause is already localised
- Translating prose specs or A2ML into code
- CI workflow authoring where the policy is stated (SHA pinning, permissions,
  triggers)
- Small/medium refactors inside one repo (rename, extract, consolidate) with
  clear scope
- Running an existing build/test/bench pipeline and reporting structured
  results with judgement about "is this a regression?"
- Reviewing a PR against a checklist (Sonnet can actually *read* the code,
  unlike Haiku's pattern-match)
- Multi-file greps that require *some* judgement about relevance (e.g. "find
  callers of X that pass a non-default Y")

## Does not fit

- Formal proofs in Idris2 / Agda / Lean / Coq / TLA+ / L4 / Iz / F\* — Opus
- Grammar / type-system / compiler design decisions — Opus
- Novel architecture or cross-repo synthesis — Opus
- Paper review, publication-pre-flight judgement calls — Opus or human
- Anything involving Idris2 dependent-type reasoning, linear/affine type
  juggling, or region-based memory proofs — Opus
- Any task where the spec is genuinely ambiguous and needs the prompter's
  priority order to resolve — Opus (who has access to the "dependability >
  security > interop > usability > performance > versatility > functional
  extension" convention)

## Cost model vs. alternatives

- vs. Haiku: Sonnet is ~3–5× more expensive per token but dramatically less
  likely to confabulate summaries. For tasks that need *one pass of reading
  with comprehension*, Sonnet's lower rework rate usually beats Haiku's
  cheaper-but-unreliable pass.
- vs. Opus: Sonnet is ~1/3 the per-token cost. Use Sonnet when the design is
  decided and the task is "execute this correctly". Save Opus for the design
  and for tasks where mid-flight replanning is expected.

Supervisor overhead (Opus briefing Sonnet, reading the result) is similar to
the Haiku delegation pattern.

## Prompt scaffold

```markdown
You are implementing <one-line task statement> in <repo path>.

# Context

<3–7 bullets of what the prompter has already decided:
- The chosen design / API shape
- The file(s) that need to change
- What exists already that this plugs into
- What is explicitly out of scope for this session
- Any priority-order tradeoff that has already been resolved>

# Task

<Specific deliverable. State acceptance criteria:
- What file(s) should exist / be modified
- What `just <recipe>` or `cargo test` / `deno test` must pass
- What behaviour a test or manual check should demonstrate>

# Hard rules

- <License headers: SPDX-License-Identifier: MPL-2.0 unless otherwise
  stated (AGPL-3.0-or-later for IDApTIK / Airborne Submarine Squadron)>
- <Language policy: AffineScript / Rust(+SPARK) / Deno / Zig / Idris2 / Agda / Gleam; no
  TypeScript, ReScript, Node, npm/bun/yarn, Go, general Python (RS/TS/JS → AffineScript → typed-wasm; Agda for foundational proofs incl. echo-types)>
- <Testing: run tests before claiming completion; "dune build passes" is NOT
  the same as "behaviourally correct" — cite a behavioural check>
- <No dangerous patterns: believe_me, assert_total, Admitted, sorry,
  unsafeCoerce, Obj.magic — these are banned estate-wide>
- <Annotations on all code; per-directory READMEs where new directories are
  introduced>
- <If you hit a design question the context doesn't resolve, STOP and ask.
  Do not invent an answer.>

# Report at end of session

- Files changed (path + one-line summary each)
- Commands run + their exit status
- Any assumptions you made that weren't in the brief
- Anything that felt out of scope but is worth flagging for a follow-up
- For UI changes: what you manually verified in a browser (typecheck != feature
  correctness)
```

## Hard rules to include verbatim

Sonnet reads context better than Haiku, but it still does not have access to
memory, global CLAUDE.md, or prior conversation. Include:

1. **License and SPDX** — PMPL-1.0-or-later baseline; AGPL-3.0-or-later for
   IDApTIK / ASS; third-party code preserves its original licence. Every new
   file gets a header.
2. **Language policy** — the allowed/banned list, plus "Rust" always means
   "Rust/SPARK" per estate convention.
3. **Architectural defaults** — Idris2 for ABI, Zig for FFI, Chainguard base
   images, Containerfile not Dockerfile, Podman not Docker, Deno first
   (pnpm fallback only when forced). V-lang is banned.
4. **Dangerous-pattern ban** — `believe_me`, `assert_total`, `Admitted`,
   `sorry`, `unsafeCoerce`, `Obj.magic`, and the rest of the estate-wide
   banned list. Sonnet will otherwise reach for escape hatches under pressure.
5. **Testing expectation** — behavioural check, not build-passes. For UI, a
   real browser verification of the golden path and one edge case.
6. **"Ask, don't invent"** — when the brief underspecifies, STOP. Sonnet's
   failure mode here is to fabricate a reasonable-looking design decision that
   the rest of the estate then has to unwind.
7. **Commit hygiene** — do not commit unless explicitly asked; if asked,
   follow the single-main-branch + signed-commit + co-author convention from
   the estate's git standards.
8. **Seam check** — when two items complete together, verify their integration
   boundary works end-to-end before moving on.
9. **No stubbing by default** — wire it through; do not leave
   `unimplemented!()` / `todo!()` / placeholder returns unless the prompter
   explicitly said to stub.

## Trust level & verification

**Default trust: medium.** Verify the seams, not every line.

Before accepting Sonnet's work:

- Run the test/bench/build recipe Sonnet says passed. Trust but verify.
- Re-read any file Sonnet claims to have "refactored for clarity" — this is
  where invented design decisions hide.
- Check git diff before committing; Sonnet's edits to unrelated files have
  been observed (dependency tree rewrites, gitignore tweaks, config drift).
- For anything touching a seam (ABI, FFI, IPC, protocol), run the opposite
  side's tests too.

Empirically observed failure modes:

- **Scope creep into "free variants".** Sonnet, told to add a linear variant,
  will happily add an affine one "since it's trivial". That is a bot-scope-creep
  anti-pattern; refuse it or revert.
- **Plausible but wrong type assumptions.** In Idris2/Agda/Lean code, Sonnet
  will write code that typechecks but whose invariants do not match the
  underlying theorem. This is a handoff signal: lift to Opus.
- **Fabricated test passes.** If Sonnet reports `just test` green without
  copying the actual output, re-run it yourself.
- **Premature commits.** If your brief does not forbid commits, Sonnet has
  been observed to commit + push. State commit policy in every brief.

## Anti-patterns in briefing

- Leaving the design open ("figure out the best way to..."). Decide before
  delegating; Sonnet executes, it does not architect.
- Stacking unrelated deliverables in one brief. Sonnet degrades when the task
  list has more than ~3 independent items. Split them.
- Omitting the test/acceptance gate. Without one, Sonnet will call the task
  done when the build passes.
- Assuming Sonnet knows about recent estate decisions (V-lang ban, VQL→VCL
  rename, SCM→A2ML migration, etc.). State anything post-January-2026 in the
  brief explicitly.

## When to escalate to Opus

- Type errors Sonnet cannot resolve after one reasonable attempt — especially
  in Idris2 / Agda / Lean.
- Any time Sonnet proposes to silence a compiler error with `believe_me`,
  `unsafe`, `any`, or an equivalent escape hatch. That is the escalation
  signal.
- Design questions the brief did not resolve.
- Anything that requires understanding multi-repo invariants the brief did
  not restate.

## Parallelism

Same estate cap as Haiku: **3 parallel subagents, 2 parallel Bash**. Sonnet is
more expensive than Haiku, so parallel fan-out is less attractive; prefer
one focused Sonnet over three shallow ones.

## License

PMPL-1.0-or-later (MPL-2.0 automatic legal fallback).
