K9!
# SPDX-License-Identifier: MPL-2.0
# bust.k9.ncl — K9 trust-tier component of the bust trident
# Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Pairs with: Bustfile.a2ml (declaration) + bust.ncl (runner).
#
# Verb:       bust            (hard-stop / expiry / "must-not-run")
# Tier:       Hunt-read-only  (subprocess probes for expiry/state checks)
# Authority:  blocking        (HARD GATE on declared broken states)
#
# bust is the "this is broken, this has expired, this must not run"
# declarative surface. Where must asserts invariants that must hold,
# bust asserts failure states that must not be re-entered. Complement
# to must: together they bound the "acceptable operating state" from
# above (must) and below (bust).
#
# Cardinality: ONE bust trident per repo.
#
# Failure-mode focus: bust catches deprecated-path-still-called
# patterns (C2 capability collapse attempts where an AI reintroduces
# retired code), expiry-exceeded state (certificates / tokens / grants
# past their expiry), and the "it works, ship it" pattern where a
# caller silently starts using a must-not-run API.

# §11.4/K9-S013 — `pedigree_schema` lives in ../_base.ncl. There is no k9/
# subdirectory under .machine_readable/contractiles/, so the old
# `import "../k9/template-hunt.k9.ncl"` never resolved — and the pedigree a
# reviewer read was not the pedigree a host would evaluate.
let base = import "../_base.ncl" in

{
  pedigree = base.pedigree_schema & {
    # §5.2/§6.2 — schema_version, and the reserved-prefix component_type. The
    # `contractile:` prefix is the whole of the disambiguation between this
    # family's verbs (must/trust/dust/intend/...) and K9's own execution triad
    # (must/just/nickel): the K9 contract never names a contractile verb, it
    # only ever sees this string.
    schema_version   = "1.0.0",
    component_type   = "contractile:bust",
    semantics        = "hard-stop-and-expiry-declaration",
    contractile_verb = "bust",
    paired_xfile     = "../bust/Bustfile.a2ml",
    paired_runner    = "../bust/bust.ncl",

    tier      = 'Hunt,
    authority = 'blocking,

    metadata = {
      name = "bust-k9",
      version = "1.0.0",
      description = "Hard-stop / expiry / must-not-run gate. Fourth trident instance. Completes the blocking-authority triple (must + trust + bust).",
      paired_xfile = "Bustfile.a2ml",
      paired_runner = "bust.ncl",
      author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
    },

    security = {
      leash = 'Hunt,
      trust_level = "read-only expiry + state-check with subprocess",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = true,
      signature_required = true,
      # §8.3/§8.4 — the grant. allow_subprocess is a REQUEST for a capability,
      # and a request the grant does not cover is a contradiction inside the
      # pedigree, not a preference (K9-S007).
      capabilities = [
        "process.spawn",
      ],
      probe_scope = 'read_only,
    },

    # §6.5/K9-S010 — mandatory at 'Hunt, and none of it may be a placeholder:
    # §9 requires a dry_run precondition, i.e. a plan that was produced AND
    # reviewed, and a reviewer cannot review a plan for a component that
    # shells out while describing none of what it does.
    side_effects = [
      "spawns read-only expiry and state-check probes as child processes (probe_scope = 'read_only; no mutation, no network)",
      "records trigger outcomes in the host's evidence sinks (contractile_executions, bust_triggers_history); the drift-log append is indirected by the host, not performed by this component",
      "a triggered hard stop fails the gate and blocks the merge — the gate is this component's effect, and no filesystem write is performed by it",
    ],

    # §10.1/K9-S009 — a 'Hunt component MUST carry a signature block. This is
    # the claim the file-format requires, and it is not a verified signature:
    # with no external verifier the verdict is 'Present_Unverified (§10.2),
    # and 'Present_Unverified does not satisfy the Hunt `signature`
    # precondition (§10.4). So the file is conforming and still NOT authorised
    # to run until the signing ceremony replaces this block.
    signature = {
      algorithm = "Ed25519",
      key_id = "bust-k9",
      payload_hash = "sha256:0000000000000000000000000000000000000000000000000000000000000000",
      signature = "NOT-A-REAL-SIGNATURE",
    },
  },

  # A contract, not payload. The fields below are declarations with no
  # definition, and Nickel's export refuses to serialize a record containing
  # one ("missing field definition for `entry_id`"). `not_exported` says what
  # is true: this block documents the variance shape and is not data to
  # serialize. Reading `variance_schema` as a contract is unaffected.
  variance_schema | not_exported = {
    entry_id     | String,
    reason       | String,
    approved_by  | String,
    scope        | String,
    expires      | String,
    review_notes | String | optional,
    severity_acknowledged | [| 'critical, 'high, 'medium |],
    waived_consequence_description | String,
  },

  execution = {
    triggers = [ 'session_close, 'on_demand, 'pre_push, 'pre_merge ],

    per_hard_stop = {
      run_probe = true,
      record_outcome = true,
      respect_variance = true,
      on_triggered = 'fail,        # BLOCKING — bust condition hit = block
      severity_escalation = 'honour,
      # bust-specific: detect re-introduction of deprecated calls
      flag_deprecated_reintroduction = true,
    },

    evidence_sinks = [
      { kind = 'verisimdb, table = "contractile_executions",
        schema = "contractile_execution_v1",
        aux_tables = [ "bust_triggers_history" ] },
      { kind = 'drift_log, path = ".machine_readable/descriptiles/DRIFT.a2ml",
        append_only = true },
    ],

    on_close = {
      re_execute_all_hard_stops = true,
      diff_against_last_ratification = true,
      emit_drift_entries_for_new_triggers = true,
      surface_expired_variances = true,
      block_session_close_on_critical_bust = true,
    },

    on_open = {
      render_summary = 'plain_language,
      include_drift_log_from_last_close = true,
      include_active_variances = true,
      include_recent_anchors = true,
      anchor_lookback_weeks = 8,
      include_silent_regressions = true,

      negotiation = {
        required = true,
        ai_required_inputs = [
          'timeline_realism,
          'industry_standards,
          'audience_feasibility,
          'resulting_invariants,
          'ecosystem_dependencies,
        ],
        user_engagement_required = true,
        user_engagement_mode = 'per_input_response,
        specification_translation = {
          ai_produces_spec_form = true,
          user_reviews_in_domain_language = true,
          schema_authoring_is_ai_responsibility = true,
          translation_faithfulness_auditable = true,
        },
      },

      accountability_pledge = {
        required = true,
        parties = [
          {
            role = 'user,
            pledge = "I have reviewed the declared hard-stop / expiry / must-not-run conditions. I accept accountability for not calling into must-not-run code paths, not ignoring expired tokens/grants, and not silently reintroducing deprecated patterns. I will raise a variance with severity acknowledgement if an exception is needed.",
            signature_required = true,
          },
          {
            role = 'ai_agent,
            pledge = "I will refuse suggestions that reintroduce must-not-run patterns; I will surface bust triggers at session close; I will require variance-with-severity for any legitimate reintroduction of a deprecated path rather than silently allowing it.",
            signature_required = true,
          },
        ],
        signed_record_destination = ".machine_readable/descriptiles/ratification-<session-id>.a2ml",
        must_precede_work = true,
      },

      ratification_record_shape = {
        includes_negotiation_transcript = true,
        includes_both_pledges = true,
        signed = true,
        dated = true,
        session_id = 'required,
        contract_hash = 'required,
      },
    },
  },

  failure_mode_defenses = [
    'A1_enthusiasm_capture,
    'C2_capability_collapse,       # prevents reintroduction of retired capability
    'C4_modernization_drift,       # bust prevents silent re-adoption of deprecated libs
    'D4_error_hiding,
    'E1_refactor_stampede,         # refactor that reintroduces deprecated path caught
    'F1_across_session_forgetting, # bust triggers persist across sessions
  ],
}
