# Tier 3 gate probe

Positive control for ruleset 23359343 `Optimus-Branch`, armed 2026-09-22.

This file exists only to give the gate something to evaluate. It is
documentation-only by design: it touches no workflow, no action pin and no
registry, so a red result here is a fault in the gate, not in the change.

What this probe is meant to establish:

1. The four required contexts (`CodeQL`, `SonarCloud Code Analysis`,
   `governance / Code quality + docs`, `uses ⊆ actions.lock`) all reach a
   terminal state on a real pull-request head.
2. Whether the `code_scanning` rule passes when Scorecard has published no
   analysis to `refs/pull/N/merge` — measured at 2 of 696 pull-ref analyses,
   so this is the expected case rather than an edge case.
3. That `mergeStateStatus` reflects the ruleset, now that
   `current_user_can_bypass` reads `never`.

Delete the branch once read. The measurement lives in `dev-notes`, not here.
