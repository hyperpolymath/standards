# SPDX-License-Identifier: PMPL-1.0-or-later
# Bust — error-handling / failure-recovery runner
#
# Pairs with: Bustfile.a2ml (same directory)
# Verb:       bust
# Semantics:  every declared failure mode must have a recovery path that has
#             been exercised. Runner injects failures (via declared probes)
#             and verifies the recovery path works. Hard gate on any
#             failure-mode with missing or broken recovery.
# CLI:        `contractile bust check`  → list failure modes + recovery status
#             `contractile bust drill`  → inject declared failures, verify recovery
#
# Anything else in this directory is human-only notes/archive; machines ignore.

{
  pedigree = {
    schema_version = "1.0.0",
    contractile_verb = "bust",
    semantics = "error handling + failure recovery",
    security = {
      leash = 'Kennel,
      trust_level = "controlled failure injection; scoped to system-under-test",
      allow_network = false,
      allow_filesystem_write = true,      # drills may write transient state (tmp dirs, test DBs)
      allow_subprocess = true,
      injection_scope = "system-under-test-only",
    },
    metadata = {
      name = "bust-runner",
      version = "1.0.0",
      description = "Exercises declared failure modes and verifies recovery paths. Hard-gates on any failure mode without working recovery.",
      paired_xfile = "Bustfile.a2ml",
      author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
    },
  },

  schema = {
    failure_modes
      | Array {
          id | String,
          description | String,
          class | [| 'network, 'disk_full, 'oom, 'timeout, 'partial_write, 'panic, 'crash, 'rollback, 'concurrency |],
          injection_probe | String,          # command that deterministically causes this failure
          recovery_probe | String,           # command that verifies recovery (exit 0 = recovered)
          expected_recovery_time_seconds | Number | default = 30,
          status | [| 'declared, 'drilled, 'verified, 'failing |] | default = 'declared,
          notes | String | optional,
        },
  },

  run = {
    on_pass = "continue",
    on_any_fail = "exit-nonzero",         # missing or broken recovery blocks merge
    report_format = "a2ml",
    emit_summary = true,
    record_recovery_times = true,         # feeds the performance tier
  },
}
