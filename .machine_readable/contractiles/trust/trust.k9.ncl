K9!
# SPDX-License-Identifier: MPL-2.0
# trust.k9.ncl — K9 trust-tier component of the trust trident
# Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Pairs with: Trustfile.a2ml (declaration) + trust.ncl (runner).
# Trident completeness is a hard precondition — a repo shipping
# Trustfile without this file AND its runner is an invalid trident;
# the contractile CLI's verify gate refuses partial publication.
#
# Verb:       trust           (security + provenance + safe-hacking)
# Tier:       Hunt            (capability: subprocess probes may shell out,
#                              active probes in safe_hacking section)
# Authority:  blocking        (HARD GATE — opposite of intend's reporting)
#
# trust is the concrete + ephemeral + transactional verb per user
# 2026-04-18: port use, BLAKE3 hashing, auth challenges, TLS state,
# session tokens. Every probe has instant binary ground truth.
# This is the α two-axis complement to intend: both Hunt-tier, opposite
# authority poles. Validating the architecture on both exercises the
# full (tier, authority) surface.
#
# Cardinality: ONE trust trident per repo (see feedback_contractile_
# layout_rules.md). ANCHOR.a2ml is the sole multi-instance exception —
# it is NOT a verb contractile.
#
# Design commitments baked in (full memory trail under
# ~/.claude/projects/-var-mnt-eclipse-repos/memory/ 2026-04-18):
# * α two-axis (Hunt, blocking) — trust is where the contractile system
#   grows teeth. Failed verification = failed CI = blocked merge.
# * Variance schema first-class — scoped exceptions structural, not
#   comment markers.
# * Sessional drift detection hooks — re-verify every close.
# * Ratification negotiation with threat-model foregrounded
#   (feedback_ai_failure_mode_catalog.md B1 — threat-model
#   misclassification is the PRIMARY defense trust provides).
# * Accountability pledge — both parties sign before security-affecting
#   work proceeds.
# * Plain-language translation — user never authors a Nickel schema for
#   a cipher suite; AI does the spec work, user reviews in domain
#   language ("TLS 1.3 with PQ key exchange, HSTS preload, 1yr").
# * Evidence sinks: VeriSimDB (queryable) + descriptiles/DRIFT.a2ml (repo-local).
# * Failure-mode defenses cross-referenced — trust carries the most
#   defenses of any verb because the threat surface is widest.

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
    component_type   = "contractile:trust",
    semantics        = "security-verification-and-authorised-probe",
    contractile_verb = "trust",
    paired_xfile     = "../trust/Trustfile.a2ml",
    paired_runner    = "../trust/trust.ncl",

    # α two-axis declaration — capability × authority.
    # trust is Hunt-capable (active probes shell out, safe-hacking section
    # runs real fuzz/injection/auth-bypass attempts scoped to the repo)
    # AND blocking-authority (failed verification = failed CI).
    # Contrast with intend = (Hunt, reporting). The two verbs exercise
    # the full α surface.
    tier      = 'Hunt,
    authority = 'blocking,

    metadata = {
      name = "trust-k9",
      version = "1.0.0",
      description = "Executes security verifications + authorised safe-hacking probes. HARD GATE: failed verification blocks merge. Catches the 'turn off the firewall' class of drift directly. Implements negotiation-ratification-accountability protocol inherited from intend.k9.ncl v2.0.0.",
      paired_xfile = "Trustfile.a2ml",
      paired_runner = "trust.ncl",
      author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
    },

    security = {
      leash = 'Hunt,
      trust_level = "verification + authorised-probe + hard-gate",
      allow_network = false,            # verifications offline by default
      allow_filesystem_write = false,   # evidence sinks are indirected
      allow_subprocess = true,
      signature_required = true,
      # §8.3/§8.4 — the grant. allow_subprocess is a REQUEST for a capability,
      # and a request the grant does not cover is a contradiction inside the
      # pedigree, not a preference (K9-S007).
      capabilities = [
        "process.spawn",
      ],
      authorised_probes_only = true,    # probe section explicitly lists allowed targets + probe classes
      probe_scope_enforcement = 'this_repo_only,  # probes NEVER hit external systems
    },

    # §6.5/K9-S010 — mandatory at 'Hunt, and none of it may be a placeholder:
    # §9 requires a dry_run precondition, i.e. a plan that was produced AND
    # reviewed, and a reviewer cannot review a plan for a component that
    # shells out while describing none of what it does.
    side_effects = [
      "spawns verification probes as child processes, including the authorised active probes (fuzz/injection/auth-bypass) — scoped to this repository only by probe_scope_enforcement = 'this_repo_only and authorised_probes_only = true",
      "records verification outcomes in the host's evidence sinks (contractile_executions); the drift-log append is indirected by the host, not performed by this component",
      "a failed verification fails the gate and blocks the merge (HARD GATE) — the gate is this component's effect, and no filesystem write is performed by it",
    ],

    # §10.1/K9-S009 — a 'Hunt component MUST carry a signature block. This is
    # the claim the file-format requires, and it is not a verified signature:
    # with no external verifier the verdict is 'Present_Unverified (§10.2),
    # and 'Present_Unverified does not satisfy the Hunt `signature`
    # precondition (§10.4). So the file is conforming and still NOT authorised
    # to run until the signing ceremony replaces this block.
    signature = {
      algorithm = "Ed25519",
      key_id = "trust-k9",
      payload_hash = "sha256:0000000000000000000000000000000000000000000000000000000000000000",
      signature = "NOT-A-REAL-SIGNATURE",
    },
  },

  # -------------------------------------------------------------------
  # Variance schema — P-shape scoped exceptions per verification.
  # A variance suppresses a specific verification's obligation for a
  # reason, with approver + expiry. Because trust is BLOCKING authority,
  # variances on trust entries are SIGNIFICANTLY more consequential than
  # variances on intend (reporting) entries — variance approver MUST
  # be the repo maintainer or above for critical-severity entries.
  #
  # A contract, not payload. The fields below are declarations with no
  # definition, and Nickel's export refuses to serialize a record containing
  # one ("missing field definition for `entry_id`"). `not_exported` says what
  # is true: this block documents the variance shape and is not data to
  # serialize. Reading `variance_schema` as a contract is unaffected.
  # -------------------------------------------------------------------
  variance_schema | not_exported = {
    entry_id     | String,   # which verification / probe id
    reason       | String,
    approved_by  | String,   # maintainer or above for critical entries
    scope        | String,   # path glob | session-id | "until-<date>"
    expires      | String,   # absolute date; trust variances cannot be open-ended
    review_notes | String | optional,
    # Additional trust-specific guardrails:
    severity_acknowledged | [| 'critical, 'high, 'medium, 'low |],
    waived_risk_description | String,  # plain language — what is being accepted
  },

  # -------------------------------------------------------------------
  # Execution policy
  # -------------------------------------------------------------------
  execution = {
    # When the component runs.
    # pre_push + pre_commit on anything touching security-adjacent files
    # + session_close (drift check) + on_demand.
    triggers = [ 'session_close, 'on_demand, 'pre_push, 'pre_commit_security_adjacent ],

    # Per-verification execution. Failed verification = blocked merge.
    per_verification = {
      run_probe = true,
      record_outcome = true,
      respect_variance = true,    # active variance suppresses the gate
      on_unmet = 'fail,           # BLOCKING — the opposite of intend's 'log_drift
      severity_escalation = 'honour,  # critical > high > medium > low in gate decisions
    },

    # Per-safe-hacking-probe execution.
    # If a probe FINDS what it was supposed to prevent finding
    # (e.g. injection succeeds, auth-bypass works), that's an EXPLOIT
    # demonstration — hard fail, regardless of other status.
    per_probe = {
      run_probe = true,
      record_outcome = true,
      honour_expected_outcome = true,
      on_unexpected_exploit_success = 'fail,  # exploit found where it shouldn't be
      scope_enforcement = 'this_repo_only,    # never touch external systems
      timeout_honouring = 'strict,
    },

    # Evidence sinks — BOTH written, every execution.
    evidence_sinks = [
      {
        kind   = 'verisimdb,
        table  = "contractile_executions",
        schema = "contractile_execution_v1",
        # trust-specific sub-table for probe outcomes (for threat-model audit)
        aux_tables = [ "trust_verifications", "trust_probes" ],
      },
      {
        kind        = 'drift_log,
        path        = ".machine_readable/descriptiles/DRIFT.a2ml",
        append_only = true,
      },
    ],

    # Session-close hook — re-verify EVERYTHING, re-run probes, diff
    # against last ratification. The "turn off the firewall" scenario
    # must be caught here if it wasn't caught at pre-push.
    on_close = {
      re_execute_all_verifications = true,
      re_run_all_safe_hacking_probes = true,
      diff_against_last_ratification = true,
      emit_drift_entries_for_new_failures = true,
      surface_expired_variances = true,
      # trust-specific: if any blocking-severity verification is newly
      # failing, the session close is BLOCKED from completing. User
      # cannot close a session with unresolved critical trust drift.
      block_session_close_on_critical_drift = true,
    },

    # -----------------------------------------------------------------
    # Session-open hook — NEGOTIATION + RATIFICATION + ACCOUNTABILITY
    # (inherited shape from intend.k9.ncl v2.0.0; trust-specific
    # additions around threat-model foregrounding below)
    # -----------------------------------------------------------------
    on_open = {
      # --- Context presentation ---
      render_summary = 'plain_language,    # metaphor-capture defense
      include_drift_log_from_last_close = true,
      include_active_variances = true,
      include_recent_anchors = true,
      anchor_lookback_weeks = 8,

      # trust-specific: the threat model is rendered FIRST, before any
      # negotiation, so the adversary and stakes are fresh in both minds.
      # This directly defends against B1 (threat-model misclassification)
      # — the "war reporter, generic personal-website priors" scenario.
      threat_model_foregrounding = {
        required = true,
        render_adversaries = true,   # from Trustfile [THREAT_MODEL]
        render_stakes = true,
        render_compliance_regimes = true,
        render_audience_sensitivity = true,
        # If the AI is about to suggest a trust-weakening action, it
        # must re-render the threat model before the suggestion lands.
        re_render_before_weakening_suggestion = true,
      },

      # --- Negotiation phase (five mandatory inputs, inherited) ---
      negotiation = {
        required = true,
        ai_required_inputs = [
          'timeline_realism,
          'industry_standards,        # especially relevant for trust: OWASP, NIST, PCI-DSS, GDPR
          'audience_feasibility,      # who is the adversary? who is protected?
          'resulting_invariants,      # what trust entries the work creates/amends
          'ecosystem_dependencies,    # TLS libs, crypto primitives, signing infra
        ],
        user_engagement_required = true,
        user_engagement_mode = 'per_input_response,
        specification_translation = {
          ai_produces_spec_form = true,
          user_reviews_in_domain_language = true,
          schema_authoring_is_ai_responsibility = true,
          translation_faithfulness_auditable = true,
          # trust-specific: the AI's translation includes rendering
          # cipher suites, key exchange choices, rate-limit numbers in
          # domain language ("strong encryption, PQ-resistant, 60 req/min")
          # rather than forcing the user into Nickel-schema authoring.
        },
      },

      # --- Accountability pledge (both parties, explicit) ---
      # trust's pledge is MORE stringent than intend's because the
      # authority is blocking. A user accepting accountability here is
      # accepting that security-affecting decisions have blocking consequence.
      accountability_pledge = {
        required = true,
        parties = [
          {
            role = 'user,
            pledge = "I have reviewed the threat model, the declared trust obligations, and the audience/stakes consequences. I accept accountability for meeting these obligations and understand that failed verification will block merges until resolved or varied. I will not attempt to disable verification to unblock a merge; I will raise a variance or amendment instead.",
            signature_required = true,
          },
          {
            role = 'ai_agent,
            pledge = "I will hold the line on declared trust obligations. I will refuse to 'disable' verifications to unblock merges; I will refuse security-weakening suggestions that contradict the threat model even when the user is enthusiastic; I will surface drift at session close; I will re-render the threat model before proposing any weakening action. If a legitimate scope shift demands security reduction, I will require a variance with severity acknowledgement or an amendment, not silent acceptance.",
            signature_required = true,
          },
        ],
        signed_record_destination = ".machine_readable/descriptiles/ratification-<session-id>.a2ml",
        must_precede_work = true,
      },

      ratification_record_shape = {
        includes_negotiation_transcript = true,
        includes_both_pledges = true,
        includes_threat_model_snapshot = true,   # trust-specific
        signed = true,
        dated = true,
        session_id = 'required,
        contract_hash = 'required,
      },
    },
  },

  # -------------------------------------------------------------------
  # Failure-mode defenses — trust is the widest-coverage verb.
  # See feedback_ai_failure_mode_catalog.md for the full catalog.
  # -------------------------------------------------------------------
  failure_mode_defenses = [
    # Category A — enthusiasm / narrative capture
    'A1_enthusiasm_capture,            # scope breach blocks via blocking authority
    'A2_metaphor_capture,              # render_summary + re_render_before_weakening
    # Category B — threat-model misclassification (trust's flagship defense)
    'B1_threat_model_misclass,         # threat_model_foregrounding = required
    'B2_audience_sensitivity_collapse, # audience_feasibility in negotiation
    'B3_compliance_prior_drift,        # industry_standards in negotiation
    # Category C — scope/capability erosion (the "firewall off" scenario)
    'C2_capability_collapse,           # blocking gate prevents silent capability drop
    'C3_helpfulness_inflation,         # trust-affecting changes need variance/amendment
    'C4_modernization_drift,           # unrequested crypto-lib upgrade caught
    # Category D — epistemic failures
    'D4_error_hiding,                  # on_unmet = 'fail makes hiding impossible
    'D5_sycophancy,                    # pledge forces AI to hold line against enthusiasm
    'D6_false_pessimism,               # negotiation requires AI to cite constraint, not assert impossibility
    # Category E — refactor/churn
    'E4_cargo_cult_security,           # probes VERIFY the claimed protection actually runs
    # Category F — session drift
    'F1_across_session_forgetting,     # on_open reads last-ratification, drift log, recent ANCHORs
  ],
}
