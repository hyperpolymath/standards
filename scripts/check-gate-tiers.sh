#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Gate-tier invariant lint (docs/CICD-SIGNAL-DISCIPLINE.adoc).
#
#   🔴 GATE:     MUST be a required status check
#   🟡 CHECK:    MUST NOT be required
#   ⚪ ADVISORY: MUST NOT be required
#   📅 PERIODIC: MUST NOT be required
#
# A required status check matches on the CONTEXT, which is the JOB name — not
# the workflow name. The first version of this lint compared workflow names to
# required contexts and reported every correctly-wired gate as a violation.
# It now reads each workflow and resolves its actual job ids and job names.
#
# Reusable-workflow callers emit `<caller job id> / <reusable job name>`, which
# cannot be known without a run, so a caller job is treated as satisfied when
# any required context starts with "<job id> / ".
#
# ── TWO PASSES, because neither direction can see what the other misses ──
#
#   FORWARD  (workflow → required?)  Is every 🔴 wired, and every 🟡/⚪/📅 left
#            unwired? This pass can only judge workflows that CARRY a tier
#            marker; an untiered workflow is skipped by construction.
#
#   REVERSE  (required → workflow?)  Does every live required context resolve to
#            a workflow that exists and is tiered 🔴? Branch protection asks THIS
#            question, and the forward pass structurally cannot answer it.
#            standards#680 evicted the a2ml/lol proof corpus but left the
#            untiered `echidna-verify.yml` still emitting the required context
#            `Idris2 — a2ml proofs`; the forward pass reported 0 discrepancies
#            throughout. The reverse pass is what names that.
#
# ── ANTI-VACUITY ──
#
# "🔴 GATE MUST be required" is trivially satisfied when NOTHING is marked 🔴.
# Measured on hyperpolymath/standards 2026-09-04: 45 of 46 workflows carried no
# tier marker, so the forward pass judged ONE workflow and reported clean. A
# coverage line is therefore printed on EVERY run, and a repo that has required
# contexts but declares no 🔴 workflow at all is itself a discrepancy
# (NO_TIERS_DECLARED) — silence must never be mistaken for compliance.
#
# ── FINDINGS ──
#
#   GATE_NOT_REQUIRED    🔴 workflow whose jobs are not required        (forward)
#   <TIER>_IS_REQUIRED   🟡/⚪/📅 workflow whose job IS required        (forward)
#   UNTIERED_REQUIRED    required context from an untiered workflow     (reverse)
#   ORPHAN_REQUIRED      nothing emits this context at all — a phantom  (reverse)
#   NO_TIERS_DECLARED    repo is gated but declares no 🔴 workflow      (coverage)
#
#   EXTERNAL_REQUIRED    required context emitted by a GitHub App rather than a
#                        workflow in this repo (CodeQL default setup, SonarCloud,
#                        …). Printed for visibility, NOT counted: it is outside
#                        the tier system, not a breach of it. Distinguishing this
#                        from ORPHAN needs live evidence, so the lint asks what
#                        actually reported on the default branch head.
#
# A mistiered-but-required context is already reported by the forward pass, so
# the reverse pass stays silent on it rather than counting one defect twice.
#
# Report-only by default; --strict exits non-zero.
# Usage: check-gate-tiers.sh [--strict] OWNER/REPO [OWNER/REPO ...]
set -uo pipefail
STRICT=0
[ "${1:-}" = "--strict" ] && { STRICT=1; shift; }
[ $# -eq 0 ] && { echo "usage: $0 [--strict] OWNER/REPO..." >&2; exit 2; }

# Job extraction is awk, not python3: LANGUAGE-POLICY.adoc bans Python with no
# exceptions, and a lint that enforces estate policy must not itself breach it.
# Scope is deliberately narrow — `jobs:` at column 0, job ids at indent 2, and a
# job-level `name:` at indent EXACTLY 4 (a step name lives at 6 or deeper).
# Verified against PyYAML across all 42 standards workflows: 0 mismatches.
# shellcheck disable=SC2016  # deliberate: $0/$3/$5 below are awk fields, not shell expansions
JOBS_AWK='
BEGIN { injobs = 0; id = "" }
/^[^[:space:]#]/ { if ($0 !~ /^jobs:/) { if (injobs && id != "") emit(); injobs = 0; id = "" } }
/^jobs:[[:space:]]*$/ { if (injobs && id != "") emit(); injobs = 1; id = ""; next }
injobs && /^  [A-Za-z_][A-Za-z0-9_-]*:[[:space:]]*$/ {
  if (id != "") emit()
  id = $0; sub(/^  /, "", id); sub(/:[[:space:]]*$/, "", id); name = ""
  next
}
injobs && id != "" && /^    name:[[:space:]]/ {
  name = $0; sub(/^    name:[[:space:]]*/, "", name)
  sub(/[[:space:]]+$/, "", name)
  if (name ~ /^".*"$/) { sub(/^"/, "", name); sub(/"$/, "", name) }
  else if (name ~ /^'"'"'.*'"'"'$/) { sub(/^'"'"'/, "", name); sub(/'"'"'$/, "", name) }
  next
}
END { if (injobs && id != "") emit() }
function emit() { print id "\t" (name != "" ? name : id); name = "" }
'

TOTAL=0
for R in "$@"; do
  REQ=$(mktemp); DEF=$(mktemp); MAP=$(mktemp); EMIT=$(mktemp)
  DB=$(gh api "repos/$R" -q .default_branch 2>/dev/null)

  # gh writes the error BODY to STDOUT on 404, so an unguarded append lands
  # `{"message":"Branch not protected"...}` in the context list and the reverse
  # pass then reports that JSON as a phantom context. Guard on exit status.
  if PROT=$(gh api "repos/$R/branches/$DB/protection" 2>/dev/null); then
    printf '%s' "$PROT" | jq -r '.required_status_checks.checks[]?.context // empty' 2>/dev/null >> "$REQ"
  fi
  for ID in $(gh api "repos/$R/rulesets" -q '.[].id' 2>/dev/null); do
    if RS=$(gh api "repos/$R/rulesets/$ID" 2>/dev/null); then
      printf '%s' "$RS" | jq -r '.rules[]?|select(.type=="required_status_checks")|.parameters.required_status_checks[].context // empty' 2>/dev/null >> "$REQ"
    fi
  done
  sort -u -o "$REQ" "$REQ"

  # What actually REPORTED recently. Used ONLY to tell an app-provided
  # context (external, fine) from a genuine phantom (nothing emits it).
  #
  # WINDOW, not a single commit: a context that reports intermittently — or
  # only on pull_request — is absent from any one commit. Measured on
  # standards 2026-09-04, `CodeQL` reported on 5db75ff7 but on none of the
  # five commits around it, so a head-only lookback called it a phantom.
  # Ten commits is a heuristic: it can only ever UNDER-report ORPHAN, which
  # is the safe direction — a missed phantom is quieter than a false one.
  for SHA in $(gh api "repos/$R/commits?sha=$DB&per_page=10" -q '.[].sha' 2>/dev/null); do
    gh api "repos/$R/commits/$SHA/check-runs?per_page=100" -q '.check_runs[]?.name' 2>/dev/null >> "$EMIT"
    gh api "repos/$R/commits/$SHA/status" -q '.statuses[]?.context' 2>/dev/null >> "$EMIT"
  done
  sort -u -o "$EMIT" "$EMIT"

  gh api "repos/$R/actions/workflows?per_page=100" -q '.workflows[]|[.name,.path]|@tsv' 2>/dev/null > "$DEF"

  # Build the job map ONCE, over EVERY workflow — tiered or not. The reverse
  # pass needs the untiered ones, which is exactly what the forward pass drops.
  # MAP rows: <tier>\t<workflow name>\t<path>\t<job id>\t<job name>
  NGATE=0; NTIERED=0; NWF=0
  while IFS=$'\t' read -r NAME PATHW; do
    [ -z "${PATHW:-}" ] && continue
    NWF=$((NWF+1))
    case "$NAME" in
      "🔴"*) TIER=GATE; NGATE=$((NGATE+1)); NTIERED=$((NTIERED+1)) ;;
      "🟡"*) TIER=CHECK; NTIERED=$((NTIERED+1)) ;;
      "⚪"*) TIER=ADVISORY; NTIERED=$((NTIERED+1)) ;;
      "📅"*) TIER=PERIODIC; NTIERED=$((NTIERED+1)) ;;
      *)     TIER=UNTIERED ;;
    esac
    gh api "repos/$R/contents/$PATHW" -q .content 2>/dev/null | base64 -d 2>/dev/null \
      | awk "$JOBS_AWK" 2>/dev/null \
      | while IFS=$'\t' read -r JID JNAME; do
          [ -z "${JID:-}" ] && continue
          printf '%s\t%s\t%s\t%s\t%s\n' "$TIER" "$NAME" "$PATHW" "$JID" "$JNAME"
        done >> "$MAP"
  done < "$DEF"

  # ---- FORWARD: every tiered workflow, correctly wired or not ----
  while IFS=$'\t' read -r NAME PATHW; do
    [ -z "${PATHW:-}" ] && continue
    case "$NAME" in
      "🔴"*) TIER=GATE ;; "🟡"*) TIER=CHECK ;;
      "⚪"*) TIER=ADVISORY ;; "📅"*) TIER=PERIODIC ;; *) continue ;;
    esac
    JOBS=$(awk -F'\t' -v p="$PATHW" '$3==p{print $4"\t"$5}' "$MAP")
    [ -z "$JOBS" ] && continue
    WIRED=no
    while IFS=$'\t' read -r JID JNAME; do
      [ -z "${JID:-}" ] && continue
      grep -Fxq "$JNAME" "$REQ" && { WIRED=yes; break; }
      grep -Fq "$JID / " "$REQ" && { WIRED=yes; break; }
    done <<< "$JOBS"
    case "$TIER:$WIRED" in
      GATE:no)  printf '%s\tGATE_NOT_REQUIRED\t%s\t%s\n' "$R" "$NAME" "$PATHW"; TOTAL=$((TOTAL+1)) ;;
      CHECK:yes|ADVISORY:yes|PERIODIC:yes)
                printf '%s\t%s_IS_REQUIRED\t%s\t%s\n' "$R" "$TIER" "$NAME" "$PATHW"; TOTAL=$((TOTAL+1)) ;;
    esac
  done < "$DEF"

  # ---- REVERSE: every live required context, back to its source workflow ----
  NREQ=0
  while read -r CTX; do
    [ -z "${CTX:-}" ] && continue
    NREQ=$((NREQ+1))
    ROW=$(awk -F'\t' -v c="$CTX" '$5==c{print; exit}' "$MAP")
    if [ -z "$ROW" ]; then
      # Reusable caller shape: "<caller job id> / <reusable job name>".
      case "$CTX" in
        *" / "*) PFX=${CTX%% / *}
                 ROW=$(awk -F'\t' -v p="$PFX" '$4==p{print; exit}' "$MAP") ;;
      esac
    fi
    if [ -z "$ROW" ]; then
      if grep -Fxq "$CTX" "$EMIT"; then
        # Something really reports it, just not a workflow in this repo.
        printf '%s\tEXTERNAL_REQUIRED\t%s\t(app-provided, not counted)\n' "$R" "$CTX"
      else
        printf '%s\tORPHAN_REQUIRED\t%s\t(nothing emits this context)\n' "$R" "$CTX"
        TOTAL=$((TOTAL+1))
      fi
      continue
    fi
    RTIER=$(printf '%s' "$ROW" | cut -f1)
    RPATH=$(printf '%s' "$ROW" | cut -f3)
    if [ "$RTIER" = UNTIERED ]; then
      printf '%s\tUNTIERED_REQUIRED\t%s\t%s\n' "$R" "$CTX" "$RPATH"
      TOTAL=$((TOTAL+1))
    fi
  done < "$REQ"

  # ---- COVERAGE: never let an unjudged repo look compliant ----
  if [ "$NREQ" -gt 0 ] && [ "$NGATE" -eq 0 ]; then
    printf '%s\tNO_TIERS_DECLARED\t%d required context(s) but no 🔴 GATE workflow\t(forward pass judged %d of %d)\n' \
      "$R" "$NREQ" "$NTIERED" "$NWF"
    TOTAL=$((TOTAL+1))
  fi
  echo "$R: tier coverage $NTIERED/$NWF workflows (🔴=$NGATE), $NREQ required context(s)" >&2

  rm -f "$REQ" "$DEF" "$MAP" "$EMIT"
done

echo "gate-tier invariant: $TOTAL discrepanc$([ "$TOTAL" = 1 ] && echo y || echo ies)" >&2
[ "$STRICT" = 1 ] && [ "$TOTAL" -gt 0 ] && exit 1
exit 0
