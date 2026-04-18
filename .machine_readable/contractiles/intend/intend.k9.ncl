# SPDX-License-Identifier: PMPL-1.0-or-later
# intend.k9.ncl — K9 trust-tier component of the intend trident
# Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Pairs with: Intentfile.a2ml (declaration) + intend.ncl (runner).
# Trident completeness is a hard precondition — a repo shipping
# Intentfile without this file AND its runner is an invalid trident;
# the contractile CLI's verify gate refuses partial publication.
#
# Verb:       intend          (north star — commitments + aspirations)
# Tier:       Hunt            (capability: subprocess probes may shell out)
# Authority:  reporting       (never blocks; drift-log only)
#
# Cardinality: ONE intend trident per repo (see feedback_contractile_
# layout_rules.md). ANCHOR.a2ml is the sole multi-instance exception —
# it is NOT a verb contractile.
#
# Design commitments baked in (see memory trail 2026-04-18 for full
# context; key files referenced by name in annotations below):
# * α two-axis (tier × authority) — structurally separate capability
#   from authority so "Hunt tier = can override" is impossible.
# * Variance schema first-class, not comment markers
#   (feedback_audit_tool_suppression_design.md — structural > markers).
# * Sessional drift detection hooks (on_close, on_open).
# * Ratification at session open; drift log at session close.
# * Evidence sinks: VeriSimDB (queryable) + 6a2/DRIFT.a2ml (repo-local).
# * Failure-mode defenses cross-referenced to the AI failure catalog.

let base_k9 = import "../k9/template-hunt.k9.ncl" in
let base    = import "../_base.ncl" in

{
  pedigree = base_k9.pedigree_schema & {
    contractile_verb = "intend",
    paired_xfile     = "../intend/Intentfile.a2ml",
    paired_runner    = "../intend/intend.ncl",

    # α two-axis declaration — capability × authority.
    # intend is Hunt-capable (probes shell out) but reporting-authority
    # (never blocks). must/trust/bust will declare (Hunt, blocking);
    # adjust/dust will declare (Yard, advisory). Splitting the axes
    # means "I'm Hunt-tier so I can override everything" is structurally
    # impossible — authority is a separate field.
    tier      = 'Hunt,
    authority = 'reporting,

    metadata = {
      name = "intend-k9",
      version = "1.0.0",
      description = "Executes Intentfile probes + emits drift log. Non-gating; reporting authority only.",
      paired_xfile = "Intentfile.a2ml",
      paired_runner = "intend.ncl",
      author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>",
    },

    security = {
      leash = 'Hunt,
      trust_level = "subprocess + filesystem-read",
      allow_network = false,
      allow_filesystem_write = false,   # evidence sinks are indirected
      allow_subprocess = true,
    },
  },

  # -------------------------------------------------------------------
  # Variance schema — P-shape scoped exceptions per entry.
  # A variance suppresses a specific intent's or wish's obligation for a
  # reason, with approver + expiry. Expired variance = effective
  # re-imposition of the obligation. Unmet intent without a variance =
  # drift, logged to the drift log.
  # Per user 2026-04-18: variances are structural, not magic-comment
  # markers — markers are gameable.
  # -------------------------------------------------------------------
  variance_schema = {
    entry_id     | String,   # which intent/wish id the variance applies to
    reason       | String,
    approved_by  | String,
    scope        | String,   # path glob | session-id | "until-<date>"
    expires      | String,   # absolute date or condition
    review_notes | String | optional,
  },

  # -------------------------------------------------------------------
  # Execution policy
  # -------------------------------------------------------------------
  execution = {
    # When the component runs.
    # session_close is mandatory (the "picked up sessionally" check).
    triggers = [ 'session_close, 'on_demand, 'pre_push ],

    # Per-intent execution.
    per_intent = {
      run_probe = true,
      record_outcome = true,
      respect_variance = true,    # active variance suppresses failure
      on_unmet = 'log_drift,      # never 'fail — authority = reporting
    },

    # Per-wish execution (wishes are non-probeable; horizon-group only).
    per_wish = {
      run_probe = false,
      emit_horizon_summary = true,
      # Vertical alignment soft-check per user_6a2_is_contractile_ought.md:
      # highest-level alignment is meta ↔ north-star (soft), not hard gate.
      check_alignment_with_META = true,
    },

    # Evidence sinks — BOTH written, every execution.
    # VeriSimDB = queryable machine record (feedback_verisimdb_policy.md).
    # 6a2/DRIFT.a2ml = repo-local append-only drift log (feedback_sessional_
    # drift_detection.md + user_6a2_is_contractile_ought.md descriptive role).
    evidence_sinks = [
      {
        kind   = 'verisimdb,
        table  = "contractile_executions",
        schema = "contractile_execution_v1",
      },
      {
        kind        = 'drift_log,
        path        = ".machine_readable/6a2/DRIFT.a2ml",
        append_only = true,
      },
    ],

    # Session-close hook — the "picked up sessionally" requirement.
    # Re-execute, diff against the last ratification, surface expired
    # variances, emit drift entries for new failures.
    on_close = {
      re_execute_all_intents = true,
      diff_against_last_ratification = true,
      emit_drift_entries_for_new_failures = true,
      surface_expired_variances = true,
    },

    # Session-open hook — ratification protocol
    # (user_contractiles_agreed_at_session_start.md).
    # Summary must be plain language (metaphor-capture defense —
    # user_metaphor_capture_break_frame_at_operational_boundaries.md).
    on_open = {
      render_summary = 'plain_language,
      require_user_agreement = true,
      include_drift_log_from_last_close = true,
      include_active_variances = true,
      # Recent ANCHORs (user_anchor_a2ml_recalibration_record.md) may
      # have shifted the contract since last session — surface them.
      include_recent_anchors = true,
      anchor_lookback_weeks = 8,
    },
  },

  # -------------------------------------------------------------------
  # Failure-mode defenses — explicit cross-reference to the catalog
  # (feedback_ai_failure_mode_catalog.md). New catalog entries that
  # shift this verb's defenses must update this list, not narrative.
  # -------------------------------------------------------------------
  failure_mode_defenses = [
    'A1_enthusiasm_capture,            # scope breach → drift log
    'A2_metaphor_capture,              # render_summary = 'plain_language
    'A3_allegory_drift,                # intents cite concrete obligations
    'C1_scope_creep,                   # feature-adjacent change needs intent_id
    'C3_helpfulness_inflation,         # changes without intent_id flagged
    'C4_modernization_drift,           # upgrade cannot cite intent → drift
    'D5_sycophancy,                    # ratification compares user framing vs contract
    'F1_across_session_forgetting,     # on_open reads last-ratification record
  ],
}
