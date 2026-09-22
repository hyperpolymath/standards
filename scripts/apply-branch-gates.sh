#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# apply-branch-gates.sh — fill the `required_status_checks` rule of a
# repository's active branch ruleset with contexts DERIVED from what its
# default-branch runs actually emitted.
#
# WHY THIS EXISTS
#   config/rulesets/README.adoc has said so outright since it was written:
#   "the propagation mechanism is still missing by design, and this note is the
#   reminder that a template fix without an applier is a half fix." Nothing in
#   this repository read config/rulesets/gates.json as DATA — every reference
#   was prose, or tests/test_governance_reusable_shape.sh asserting the file's
#   contents. gates.json specified a derivation that no code performed.
#   apply-tag-ruleset-canon.sh is the TAG applier; this is its branch sibling.
#
# CONTEXTS ARE DERIVED, NEVER TYPED
#   config/rulesets/gates.json: "Contexts are never typed by hand." A typed
#   context that nothing emits is a PHANTOM — it can never turn green, so it
#   blocks the branch forever. This script therefore reads the check names the
#   latest default-branch run of each gate workflow ACTUALLY produced:
#     GET /repos/{o}/{r}/actions/workflows/{file}/runs?branch=<default>&per_page=1
#     GET /repos/{o}/{r}/actions/runs/{id}/jobs
#   and uses the job `name` verbatim. For a reusable caller GitHub already
#   renders that as "<caller job id> / <reusable job name>".
#
# THE TWO REFUSALS THAT MATTER
#   if_no_run_yet            -> omit that file's contexts and REPORT it.
#                               Never write a context nothing has emitted.
#   if_zero_contexts_overall -> do NOT write the rule at all; report UNGATED.
#                               A required_status_checks rule with an empty list
#                               is a VACUOUS GATE: it reports "protected" while
#                               requiring nothing. That is worse than no rule,
#                               because it is indistinguishable from a real one
#                               in every summary view.
#
# EXACTNESS GUARD
#   A ruleset PUT REPLACES the whole object. This script therefore refuses to
#   write unless the planned body, normalised over the required_status_checks
#   rule alone, is byte-identical to the source. Anything else moved => refuse.
#   Without this, one jq slip silently strips required_signatures estate-wide.
#
# WHAT IT DELIBERATELY DOES NOT DO
#   * It never CREATES a ruleset. A repo with no active branch ruleset is
#     reported NORULESET. Creating branch protection where none exists is a
#     policy act, not a gate-fill.
#   * It never DELETES or rewrites another rule. Repos carrying the retired
#     types (update, required_deployments, code_quality, code_coverage) are
#     REPORTED, not repaired: the estate census was ruled report-only. Pass
#     --strip-retired to opt in, one repo at a time.
#   * It never emits a retired rule type itself. The exactness guard makes that
#     structurally impossible, not merely intended.
#   * Two active REPO-LEVEL branch rulesets => AMBIGUOUS, fail closed. Rulesets
#     are ADDITIVE (see apply-tag-ruleset-canon.sh): writing one of a pair
#     leaves the other enforcing, and the repo stays blocked by a rule nothing
#     announced. Guessing which to fill is how that happens silently.
#   * It never tries to write an ORG-INHERITED ruleset. repos/{r}/rulesets
#     RETURNS the org's rulesets alongside the repo's own, and one of them is
#     readable IN FULL at repos/{r}/rulesets/{id} -- so every read succeeds and
#     nothing warns you. The PUT to that same path 404s. That was measured 67
#     times, once for every metadatastician repo reached by the org-level
#     EstateBranching (18225024). The cure for an inherited ruleset is at
#     /orgs/{org}/rulesets/{id}, applied ONCE, not per repo -- so this script
#     reports ORG-INHERITED and stops rather than issuing 67 doomed writes.
#     The discriminator is .source_type, which the LIST endpoint does return
#     (verified against the live API: every entry carries it). An entry WITHOUT
#     it is reported UNKNOWN, never assumed repo-level: writability is exactly
#     what that field decides, and guessing it wrong is a silent 404.
#
# Inputs (environment):
#   GH_TOKEN       required for writes; needs administration:write on targets.
#   ESTATE_ORGS    optional, space-separated. Default "metadatastician".
#
# Flags:
#   --apply            perform writes. WITHOUT IT THIS SCRIPT ONLY REPORTS.
#   --repo OWNER/NAME  process exactly one repository (repeatable).
#   --limit N          process at most N repositories (pilot runs).
#   --gates-file F     default config/rulesets/gates.json
#   --strip-retired    also remove the 4 retired rule types. Off by default.
#   --require-green N  drop any derived context that is not green across the
#                      last N default-branch runs of its workflow, and REFUSE
#                      the repo outright if any of those runs cannot be read.
#                      Off (0) by
#                      default. Owner ruling on #956: "require the reliably-
#                      green set" -- a required context that is currently red
#                      blocks the branch the moment it is required, so gating
#                      on it converts a visible red into a merge deadlock.
#                      `skipped` and `neutral` COUNT AS GREEN: GitHub treats
#                      both as satisfying a required status check.
#   --skip-user        do not enumerate user/repos; use only ESTATE_ORGS.
#
# Output: TSV on stdout  repo <TAB> state <TAB> detail
#         per-class summary on stderr.
#
# Exit codes: 0 ok · 1 usage/refusal · 2 at least one repo FAILED
set -uo pipefail

RETIRED_TYPES='update required_deployments code_quality code_coverage'
ACTIONS_INTEGRATION_ID=15368

APPLY=0 LIMIT=0 STRIP_RETIRED=0 SKIP_USER=0 REQUIRE_GREEN=0
GATES_FILE='config/rulesets/gates.json'
REPOS_EXPLICIT=()

die() { printf '%s\n' "$*" >&2; exit 1; }

while [ $# -gt 0 ]; do
  case "$1" in
    --apply)         APPLY=1 ;;
    --repo)          shift; [ $# -gt 0 ] || die 'usage: --repo OWNER/NAME'; REPOS_EXPLICIT+=("$1") ;;
    --limit)         shift; [ $# -gt 0 ] || die 'usage: --limit N'; LIMIT="$1" ;;
    --gates-file)    shift; [ $# -gt 0 ] || die 'usage: --gates-file PATH'; GATES_FILE="$1" ;;
    --strip-retired) STRIP_RETIRED=1 ;;
    --require-green) shift; [ $# -gt 0 ] || die 'usage: --require-green N'; REQUIRE_GREEN="$1" ;;
    --skip-user)     SKIP_USER=1 ;;
    -h|--help)       sed -n '2,70p' "$0"; exit 0 ;;
    *)               die "unknown flag: $1" ;;
  esac
  shift
done

[ -r "$GATES_FILE" ] || die "gates file not readable: $GATES_FILE"
command -v gh >/dev/null || die 'gh is required'
command -v jq >/dev/null || die 'jq is required'

WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT

jq -e . "$GATES_FILE" >/dev/null 2>&1 || die "gates file is not valid JSON: $GATES_FILE"

jq -r '.never_required_workflows[]?' "$GATES_FILE" | sort -u > "$WORK/never_wf"
jq -r '.never_required_contexts[]?'  "$GATES_FILE" | sort -u > "$WORK/never_ctx"

emit() { printf '%s\t%s\t%s\n' "$1" "$2" "$3"; printf '%s\n' "$2" >> "$WORK/states"; }

# --- normalise away ONLY the required_status_checks rule, so a diff of the
#     normalised forms proves nothing outside it moved.
# The required_status_checks rule is the INTENDED change, so it is removed from
# both sides before comparison -- including when it is being ADDED, where the
# two rule arrays legitimately differ in length. Everything else must match
# exactly. The rule's own contents are verified separately, after the write,
# by comparing the planned context set against what the API returns.
norm_rsc() {
  jq -S '.rules = ((.rules // []) | map(select(.type!="required_status_checks")))' "$1"
}

# --- is $1 listed in never_required_contexts, whole or after the first " / "?
is_never_ctx() {
  local c="$1" tail="${1#* / }"
  command grep -qxF -- "$c" "$WORK/never_ctx" && return 0
  [ "$tail" != "$c" ] && command grep -qxF -- "$tail" "$WORK/never_ctx" && return 0
  return 1
}

# ---------------------------------------------------------------- repo list
if [ "${#REPOS_EXPLICIT[@]}" -gt 0 ]; then
  printf '%s\n' "${REPOS_EXPLICIT[@]}" > "$WORK/repos"
else
  : > "$WORK/repos"
  [ "$SKIP_USER" = 1 ] || gh repo list --limit 1000 --json nameWithOwner,isArchived \
      --jq '.[]|select(.isArchived|not)|.nameWithOwner' >> "$WORK/repos" 2>/dev/null
  printf '%s\n' ${ESTATE_ORGS:-metadatastician} | while IFS= read -r ORG; do
    [ -n "$ORG" ] || continue
    gh repo list "$ORG" --limit 1000 --json nameWithOwner,isArchived \
      --jq '.[]|select(.isArchived|not)|.nameWithOwner' >> "$WORK/repos" 2>/dev/null
  done
  sort -u "$WORK/repos" -o "$WORK/repos"
fi

[ -s "$WORK/repos" ] || die 'refusing to report a clean sweep over nothing: target list is empty'
[ "$LIMIT" -gt 0 ] 2>/dev/null && head -n "$LIMIT" "$WORK/repos" > "$WORK/r2" && mv "$WORK/r2" "$WORK/repos"

printf '# mode=%s repos=%d gates=%s\n' \
  "$( [ "$APPLY" = 1 ] && echo APPLY || echo REPORT-ONLY )" "$(wc -l < "$WORK/repos")" "$GATES_FILE" >&2
printf 'repo\tstate\tdetail\n'
: > "$WORK/states"

# ---------------------------------------------------------------- main loop
while IFS= read -r R; do
  [ -n "$R" ] || continue

  gh api "repos/$R" > "$WORK/repo.json" 2>"$WORK/e" \
    || { emit "$R" "UNKNOWN" "repo GET failed: $(head -c 100 "$WORK/e" | tr -d '\n')"; continue; }
  DEF=$(jq -r '.default_branch // empty' "$WORK/repo.json")
  [ -n "$DEF" ] || { emit "$R" "UNKNOWN" 'no default branch (empty repo?)'; continue; }

  # ---- 1. which gate workflow FILES apply (profiles) --------------------
  gh api "repos/$R/contents/.github/workflows" --jq '.[]?|.name' 2>/dev/null | sort -u > "$WORK/wf" || : > "$WORK/wf"
  gh api "repos/$R/contents" --jq '.[]?|.name' 2>/dev/null | sort -u > "$WORK/root" || : > "$WORK/root"

  : > "$WORK/gatewf"
  jq -r '.profiles | to_entries[] | @base64' "$GATES_FILE" > "$WORK/profiles"
  while IFS= read -r P64; do
    P=$(printf '%s' "$P64" | base64 -d)
    KEY=$(printf '%s' "$P" | jq -r '.key')
    ACTIVE=0
    if [ "$KEY" = base ]; then
      ACTIVE=1
    else
      # detect: root-level files/globs
      while IFS= read -r PAT; do
        [ -n "$PAT" ] || continue
        while IFS= read -r F; do
          case "$F" in $PAT) ACTIVE=1 ;; esac
        done < "$WORK/root"
      done < <(printf '%s' "$P" | jq -r '.value.detect[]?')
      # detect_workflows: presence of a workflow file
      while IFS= read -r WFN; do
        [ -n "$WFN" ] || continue
        command grep -qxF -- "$WFN" "$WORK/wf" && ACTIVE=1
      done < <(printf '%s' "$P" | jq -r '.value.detect_workflows[]?')
    fi
    [ "$ACTIVE" = 1 ] || continue
    printf '%s' "$P" | jq -r '.value.gate_workflows[]?' >> "$WORK/gatewf"
  done < "$WORK/profiles"

  sort -u "$WORK/gatewf" -o "$WORK/gatewf"
  # a gate workflow must exist in the repo AND not be never-required
  : > "$WORK/gatewf2"
  while IFS= read -r WFN; do
    [ -n "$WFN" ] || continue
    command grep -qxF -- "$WFN" "$WORK/never_wf" && continue
    command grep -qxF -- "$WFN" "$WORK/wf" || continue
    printf '%s\n' "$WFN" >> "$WORK/gatewf2"
  done < "$WORK/gatewf"

  # ---- 2. DERIVE contexts from real runs --------------------------------
  # FAIL CLOSED.  A swallowed API error here raises nothing -- it silently
  # SHORTENS the list, and the gate is written weaker than intended while
  # every other line of output still reports success.  (Measured 2026-09-22:
  # this dropped 2 of 18 required contexts on standards/main.)  So capture
  # the exit status of every fetch and refuse to write if any one failed.
  : > "$WORK/ctx"; : > "$WORK/gatewf3"; NORUN=''; DERIVEFAIL=''
  while IFS= read -r WFN; do
    [ -n "$WFN" ] || continue
    if ! RID=$(gh api "repos/$R/actions/workflows/$WFN/runs?branch=$DEF&per_page=1" \
                 --jq '.workflow_runs[0].id // empty'); then
      DERIVEFAIL="${DERIVEFAIL:+$DERIVEFAIL,}$WFN(runs-query-failed)"; continue
    fi
    if [ -z "$RID" ]; then NORUN="${NORUN:+$NORUN,}$WFN"; continue; fi
    if ! gh api "repos/$R/actions/runs/$RID/jobs?per_page=100" --paginate \
           --jq '.jobs[]?|.name' > "$WORK/jobs1"; then
      DERIVEFAIL="${DERIVEFAIL:+$DERIVEFAIL,}$WFN(jobs-query-failed)"; continue
    fi
    # a run that EXISTS but reports zero jobs is a failed read, not an empty gate
    if [ ! -s "$WORK/jobs1" ]; then
      DERIVEFAIL="${DERIVEFAIL:+$DERIVEFAIL,}$WFN(run $RID returned zero jobs)"; continue
    fi
    cat "$WORK/jobs1" >> "$WORK/ctx"
    # Record the workflows that actually CONTRIBUTED contexts.  The greenness
    # probe below must iterate these, not gatewf2: gatewf2 still holds every
    # NORUN workflow, for which an empty runs query is the LEGITIMATE state.
    printf '%s\n' "$WFN" >> "$WORK/gatewf3"
  done < "$WORK/gatewf2"

  sort -u "$WORK/ctx" -o "$WORK/ctx"
  : > "$WORK/ctx2"; EXCLUDED=''
  while IFS= read -r C; do
    [ -n "$C" ] || continue
    if is_never_ctx "$C"; then EXCLUDED="${EXCLUDED:+$EXCLUDED,}$C"; continue; fi
    printf '%s\n' "$C" >> "$WORK/ctx2"
  done < "$WORK/ctx"

  # ---- optional: keep only contexts that are RELIABLY green ------------
  # The same rule in the other direction: an UNREAD run cannot prove a
  # context green, so a failed fetch here must refuse, never silently admit.
  NOTGREEN=''
  if [ "$REQUIRE_GREEN" -gt 0 ] 2>/dev/null; then
    : > "$WORK/bad"
    while IFS= read -r WFN; do
      [ -n "$WFN" ] || continue
      if ! gh api "repos/$R/actions/workflows/$WFN/runs?branch=$DEF&per_page=$REQUIRE_GREEN" \
             --jq '.workflow_runs[]?.id' > "$WORK/rids"; then
        DERIVEFAIL="${DERIVEFAIL:+$DERIVEFAIL,}$WFN(green-runs-query-failed)"; continue
      fi
      # Third instance of the same class.  A runs query that SUCCEEDS with an
      # empty list leaves rids empty, the loop below never runs, nothing lands
      # in "bad", and every context this workflow contributed is admitted as
      # green.  We are iterating gatewf3 -- workflows that DID contribute
      # contexts, hence had a run -- so zero runs here is a failed read.
      if [ ! -s "$WORK/rids" ]; then
        DERIVEFAIL="${DERIVEFAIL:+$DERIVEFAIL,}$WFN(green-runs-query-returned-none)"; continue
      fi
      while IFS= read -r RID2; do
        [ -n "$RID2" ] || continue
        # Ask for NAME + CONCLUSION of every job and classify locally.
        # Asking the API only for the NON-green jobs cannot tell "this run is
        # all green" apart from "this run was not read": both return ZERO
        # lines, and zero lines is read as greenness. That is the same
        # fail-open as the derivation loop above, pointing the other way --
        # there it SHORTENS the gate, here it ADMITS a context that was never
        # shown to be green. An unread run proves nothing in either direction.
        if ! gh api "repos/$R/actions/runs/$RID2/jobs?per_page=100" --paginate \
               --jq '.jobs[]? | [.name, (.conclusion // "pending")] | @tsv' \
               > "$WORK/jobs2"; then
          DERIVEFAIL="${DERIVEFAIL:+$DERIVEFAIL,}$WFN(run $RID2 green-check-failed)"; continue
        fi
        # a run that EXISTS but reports zero jobs is a failed read, not a green run
        if [ ! -s "$WORK/jobs2" ]; then
          DERIVEFAIL="${DERIVEFAIL:+$DERIVEFAIL,}$WFN(run $RID2 returned zero jobs)"; continue
        fi
        # a job is acceptable when success/skipped/neutral, or still running
        while IFS=$'\t' read -r JN JC; do
          [ -n "$JN" ] || continue
          case "$JC" in
            success|skipped|neutral|pending) ;;
            *) printf '%s\n' "$JN" >> "$WORK/bad" ;;
          esac
        done < "$WORK/jobs2"
      done < "$WORK/rids"
    done < "$WORK/gatewf3"
    sort -u "$WORK/bad" -o "$WORK/bad"
    : > "$WORK/ctx3"
    while IFS= read -r C; do
      [ -n "$C" ] || continue
      if command grep -qxF -- "$C" "$WORK/bad"; then NOTGREEN="${NOTGREEN:+$NOTGREEN,}$C"; continue; fi
      printf '%s\n' "$C" >> "$WORK/ctx3"
    done < "$WORK/ctx2"
    mv "$WORK/ctx3" "$WORK/ctx2"
  fi

  NCTX=$(wc -l < "$WORK/ctx2")
  DETAIL="branch=$DEF gate_files=$(wc -l < "$WORK/gatewf2") contexts=$NCTX"
  [ -n "$NOTGREEN" ] && DETAIL="$DETAIL not_green=[$NOTGREEN]"
  [ -n "$NORUN" ]   && DETAIL="$DETAIL no_run=[$NORUN]"
  [ -n "$EXCLUDED" ] && DETAIL="$DETAIL excluded=[$EXCLUDED]"

  # ---- THE OTHER REFUSAL: a gate derived from an incomplete read --------
  # An unread run is not an absent context.  Writing here would produce a
  # real, plausible, permanent ruleset that is simply WEAKER than intended,
  # reported as success, with nothing anywhere to say so.
  if [ -n "$DERIVEFAIL" ]; then
    emit "$R" "REFUSED" "$DETAIL derive_failed=[$DERIVEFAIL] — refusing to write a gate derived from an incomplete read"
    continue
  fi

  # ---- THE REFUSAL: a rule with an empty list is a vacuous gate ---------
  if [ "$NCTX" -eq 0 ]; then
    emit "$R" "UNGATED" "$DETAIL — refusing to write an empty required_status_checks rule"
    continue
  fi

  # ---- 3. locate the one active REPO-LEVEL branch ruleset ----------------
  # This listing includes the ORG's rulesets as well as the repo's own, and an
  # inherited one reads back in full at repos/{r}/rulesets/{id} while the PUT
  # to that same path 404s. Fetch the population and classify LOCALLY -- a
  # server-side select whose empty result is also its success result cannot
  # fail closed.
  gh api "repos/$R/rulesets" > "$WORK/rs.json" 2>"$WORK/e" \
    || { emit "$R" "UNKNOWN" "rulesets GET failed"; continue; }
  jq -r '.[]|select(.target=="branch" and .enforcement=="active")
         |[(.source_type // "MISSING"), (.id|tostring)]|@tsv' "$WORK/rs.json" > "$WORK/active"
  # awk, not grep -P: -P is a GNU extension and this script must not depend on
  # which grep the runner ships.
  awk -F'\t' '$1=="Repository"{print $2}'                "$WORK/active" > "$WORK/ids"
  awk -F'\t' '$1!="Repository" && $1!="MISSING"{print $2}' "$WORK/active" > "$WORK/inherited"
  NMISS=$(awk -F'\t' '$1=="MISSING"{c++} END{print c+0}'  "$WORK/active")
  NIDS=$(wc -l < "$WORK/ids")
  NINH=$(wc -l < "$WORK/inherited")

  # An absent discriminator REFUSES; it never defaults to the writable arm.
  [ "${NMISS:-0}" -gt 0 ] && { emit "$R" "UNKNOWN" "$DETAIL — $NMISS active branch ruleset(s) carry no .source_type; cannot tell repo-level from org-inherited, refusing to guess"; continue; }
  if [ "$NIDS" -eq 0 ] && [ "$NINH" -gt 0 ]; then
    emit "$R" "ORG-INHERITED" "$DETAIL — the only active branch ruleset(s) here are org-level ($(paste -sd, "$WORK/inherited")); writable ONLY at /orgs/{org}/rulesets/{id} with an admin:org credential (a repo token reads it and cannot write it), cured once at the org, never per repo"
    continue
  fi
  [ "$NIDS" -eq 0 ] && { emit "$R" "NORULESET" "$DETAIL — no active branch ruleset; this script never creates one"; continue; }
  [ "$NIDS" -gt 1 ] && { emit "$R" "AMBIGUOUS" "$DETAIL — $NIDS active repo-level branch rulesets ($(paste -sd, "$WORK/ids")); rulesets are additive, refusing to guess"; continue; }
  ID=$(cat "$WORK/ids")
  # Additive: an inherited ruleset still enforces alongside the one being filled.
  [ "$NINH" -gt 0 ] && DETAIL="$DETAIL org_inherited=[$(paste -sd, "$WORK/inherited")]"

  gh api "repos/$R/rulesets/$ID" > "$WORK/live.json" 2>/dev/null \
    || { emit "$R" "UNKNOWN" "ruleset $ID GET failed"; continue; }

  FOUND_RETIRED=$(jq -r --arg rt "$RETIRED_TYPES" \
      '[.rules[]?.type] as $t | ($rt|split(" ")) - (($rt|split(" ")) - $t) | join(",")' "$WORK/live.json")
  [ -n "$FOUND_RETIRED" ] && DETAIL="$DETAIL retired_present=[$FOUND_RETIRED]"

  # ---- 4. build the PUT body -------------------------------------------
  jq -R -s --argjson iid "$ACTIONS_INTEGRATION_ID" \
    'split("\n")|map(select(length>0))|map({context:., integration_id:$iid})' "$WORK/ctx2" > "$WORK/checks.json"

  jq --slurpfile ck "$WORK/checks.json" '
      {name,target,enforcement,conditions,bypass_actors,rules}
      | .rules = ((.rules // []) | map(select(.type!="required_status_checks")))
      | .rules += [{ type:"required_status_checks",
                     parameters:{ strict_required_status_checks_policy:false,
                                  do_not_enforce_on_create:false,
                                  required_status_checks:$ck[0] } }]
    ' "$WORK/live.json" > "$WORK/put.json"

  if [ "$STRIP_RETIRED" = 1 ]; then
    jq --arg rt "$RETIRED_TYPES" '($rt|split(" ")) as $r | .rules |= map(select(.type as $t | ($r|index($t))|not))' \
      "$WORK/put.json" > "$WORK/p2" && mv "$WORK/p2" "$WORK/put.json"
  fi

  # structural assertion: we never emit a retired type that was not already there
  EMITTED_RETIRED=$(jq -r --arg rt "$RETIRED_TYPES" \
      '[.rules[]?.type] as $t | ($rt|split(" ")) - (($rt|split(" ")) - $t) | join(",")' "$WORK/put.json")
  if [ "$STRIP_RETIRED" = 1 ] && [ -n "$EMITTED_RETIRED" ]; then
    emit "$R" "REFUSED" "$DETAIL — --strip-retired left [$EMITTED_RETIRED] in the body"; continue
  fi

  # ---- EXACTNESS GUARD (skipped when --strip-retired deliberately differs)
  if [ "$STRIP_RETIRED" = 0 ]; then
    jq '{name,target,enforcement,conditions,bypass_actors,rules}' "$WORK/live.json" > "$WORK/src.json"
    if ! diff -q <(norm_rsc "$WORK/src.json") <(norm_rsc "$WORK/put.json") >/dev/null; then
      emit "$R" "REFUSED" "$DETAIL — exactness guard: change outside the required_status_checks rule"
      continue
    fi
  fi

  if [ "$APPLY" = 0 ]; then
    emit "$R" "WOULD-GATE" "$DETAIL ruleset=$ID :: $(tr '\n' '|' < "$WORK/ctx2")"
    continue
  fi

  if ! gh api -X PUT "repos/$R/rulesets/$ID" --input "$WORK/put.json" >/dev/null 2>"$WORK/e"; then
    emit "$R" "FAILED" "$DETAIL — PUT: $(head -c 160 "$WORK/e" | tr -d '\n')"; continue
  fi

  # ---- 5. post-write verification --------------------------------------
  gh api "repos/$R/rulesets/$ID" > "$WORK/after.json" 2>/dev/null \
    || { emit "$R" "WROTE-UNVERIFIED" "$DETAIL — re-GET failed"; continue; }
  PRE_BY=$(jq -cS '.bypass_actors//[]' "$WORK/live.json");  POST_BY=$(jq -cS '.bypass_actors//[]' "$WORK/after.json")
  WANT=$(jq -cS '[.rules[]?|select(.type=="required_status_checks")|.parameters.required_status_checks[].context]|sort' "$WORK/put.json")
  GOT=$(jq  -cS '[.rules[]?|select(.type=="required_status_checks")|.parameters.required_status_checks[].context]|sort' "$WORK/after.json")
  if [ "$PRE_BY" != "$POST_BY" ]; then emit "$R" "DRIFT" "$DETAIL — bypass_actors changed across the write"; continue; fi
  if [ "$WANT" != "$GOT" ];       then emit "$R" "DRIFT" "$DETAIL — contexts after write != planned"; continue; fi
  emit "$R" "GATED" "$DETAIL ruleset=$ID :: $(tr '\n' '|' < "$WORK/ctx2")"
done < "$WORK/repos"

echo '# summary' >&2
sort "$WORK/states" | uniq -c | sort -rn >&2
command grep -qx 'FAILED' "$WORK/states" && exit 2
exit 0
