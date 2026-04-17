# SPDX-License-Identifier: PMPL-1.0-or-later
# Lust — aspirational/roadmap runner
#
# Pairs with: Lustfile.a2ml (same directory)
# Verb:       lust
# Semantics:  wishes / aspirations / horizon roadmap. NEVER gates. Distinct
#             from `intend` which tracks committed next-actions. `lust` is
#             where the project's unrealised desires live so humans and
#             agents can see the horizon without confusing it with
#             declared intent.
# CLI:        `contractile lust run`      → print wishlist
#             `contractile lust horizon`  → group by target-horizon
#
# Anything else in this directory is human-only notes/archive; machines ignore.
#
# Base: ../_base.ncl provides pedigree_schema, run_defaults, probe_schema.
# See: docs/CONTRACTILE-SPEC.adoc

let base = import "../_base.ncl" in

{
  pedigree = base.pedigree_schema & {
    contractile_verb = "lust",
    semantics = "aspirational / wishlist / horizon",
    security = {
      leash = 'Kennel,
      trust_level = "read-only reporting",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,     # pure reporting; no probes run
    },
    metadata = {
      name = "lust-runner",
      version = "1.0.0",
      description = "Reports project aspirations grouped by horizon. Non-gating. Distinct from intend (commitment) — lust is wish.",
      paired_xfile = "Lustfile.a2ml",
      author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
    },
  },

  schema = {
    wishes
      | Array {
          id | String,
          description | String,
          horizon | [| 'near, 'mid, 'far |] | default = 'mid,
          why | String | optional,
          # lust has its own non-standard status progression (non-gating):
          #   'declared → 'in_progress → 'achieved | 'abandoned
          status | [| 'declared, 'in_progress, 'achieved, 'abandoned |] | default = 'declared,
          notes | String | optional,
        },
  },

  # Runner behaviour — inherits from base.run_defaults.
  # lust never blocks; emit_grouped_by_horizon is lust-specific.
  run = base.run_defaults & {
    on_pass = "continue",
    on_any_fail = "continue",       # never blocks; it's a report
    report_format = "a2ml",
    emit_summary = true,
    emit_grouped_by_horizon = true,
  },
}
