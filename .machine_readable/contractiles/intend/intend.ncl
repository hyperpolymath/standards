# SPDX-License-Identifier: PMPL-1.0-or-later
# Intend — north-star runner (verb is `intend`, not `intent`)
#
# Pairs with: Intendfile.a2ml (same directory)
# Verb:       intend
# Semantics:  declarative aspirations. Not a gate — reports progress toward
#             target state. Items can be declared / in-progress / done /
#             deferred / retired.
# CLI:        `contractile intend run`      → print status table
#             `contractile intend progress` → diff declared-vs-observed
#
# Anything else in this directory is human-only notes/archive; machines ignore.

{
  pedigree = {
    schema_version = "1.0.0",
    contractile_verb = "intend",
    semantics = "north-star / aspirational",
    security = {
      leash = 'Kennel,
      trust_level = "read-only reporting",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = true,      # probe commands may shell out
    },
    metadata = {
      name = "intend-runner",
      version = "1.0.0",
      description = "Reports progress toward declared future-state intents. Non-gating.",
      paired_xfile = "Intendfile.a2ml",
      author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
    },
  },

  schema = {
    intents
      | Array {
          id | String,
          description | String,
          probe | String | optional,     # shell command that indicates done-ness
          status | [| 'declared, 'in_progress, 'done, 'deferred, 'retired |] | default = 'declared,
          notes | String | optional,
          target_date | String | optional,
        },
  },

  run = {
    on_pass = "continue",
    on_any_fail = "continue",         # never blocks; it's a report
    report_format = "a2ml",
    emit_summary = true,
    emit_diff = true,                 # declared vs observed
  },
}
