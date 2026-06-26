#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
set -eo pipefail

# Staleness checker script for hyperpolymath estate repositories.
#
# Ensures consumer workflows do not use retired patterns or *genuinely* stale
# pins of the standards reusable workflows (governance / hypatia-scan /
# scorecard).
#
# ── Why this is a *window*, not an exact-HEAD match ─────────────────────────
# `standards` hosts reusable workflows that consumer repos pin by full SHA.
# The original gate failed a consumer whenever its pin != standards HEAD. That
# turned every standards commit into a fleet-wide red: HEAD can move several
# times an hour, and each move forced a manual pin bump in every consumer
# (observed live in stapeln — 5a93d9d→d72fe5a→4ddc926 in ~30 min, three
# forced 3-file bumps). The gate became a treadmill instead of a guard.
#
# The fix mirrors the estate's existing staleness idiom (Hypatia HYP-S006
# `registry-staleness`, which tolerates drift for `stale_after_days` before it
# escalates): a pin PASSES if it is a genuine ancestor of standards HEAD that
# is *within a recency window* — at most STALENESS_WINDOW_COMMITS commits
# behind HEAD, OR at most STALENESS_WINDOW_DAYS days old (union; either
# qualifies). A pin still FAILS if it is older than the window (so pins cannot
# rot forever and the known-bad historical pins — pre-cache-fix Hypatia, the
# SARIF-publishing Scorecard — remain blocked), or if it is not a real
# standards commit at all (supply-chain integrity: forged / unrelated SHAs are
# rejected).
#
# Deliberately jumping a consumer to HEAD (e.g. to pick up a fix early) is the
# job of the propagation path — `scripts/propagate-workflow-pins.sh` plus the
# Hypatia `sha_bump_propagation` rule + gitbot-fleet — which opens an
# audit-first bump PR. This gate no longer *forces* that bump; it only blocks
# pins that have fallen out of the window. See
# docs/decisions/ADR-003-workflow-pin-staleness-window.adoc.
#
# Environment knobs (all optional):
#   STALENESS_EXPECTED_SHA    — override the "current" standards HEAD SHA.
#   STALENESS_STANDARDS_DIR   — path to a git checkout carrying standards
#                               history (defaults to this script's own repo;
#                               in CI the governance reusable clones standards
#                               with `--filter=tree:0` so the full commit graph
#                               is available for ancestry/age math).
#   STALENESS_WINDOW_COMMITS  — max commits-behind-HEAD a pin may be (default 50).
#   STALENESS_WINDOW_DAYS     — max age in days a pin may be (default 14, matching
#                               HYP-S006 stale_after_days).

REPO_ROOT="${1:-.}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

WINDOW_COMMITS="${STALENESS_WINDOW_COMMITS:-50}"
WINDOW_DAYS="${STALENESS_WINDOW_DAYS:-14}"

# The standards reusables a consumer pins by SHA. The set is intentionally the
# trio named in the governance contract; extend here if a new reusable joins
# the freshness gate. Commented-out `uses:` examples are skipped (see below).
STANDARDS_REUSABLES=(
  "governance-reusable.yml"
  "hypatia-scan-reusable.yml"
  "scorecard-reusable.yml"
)

# Determine if we are in standards repo
IS_STANDARDS=false
if [ "$GITHUB_REPOSITORY" = "hyperpolymath/standards" ]; then
  IS_STANDARDS=true
else
  # Fallback: check Git remote origin URL
  if [ -d "$REPO_ROOT/.git" ]; then
    REMOTE_URL=$(git -C "$REPO_ROOT" remote get-url origin 2>/dev/null || echo "")
    if [[ "$REMOTE_URL" =~ "hyperpolymath/standards" ]]; then
      IS_STANDARDS=true
    fi
  fi
fi

# ── Locate a git checkout that carries standards history ────────────────────
# We need the commit graph (not just HEAD) to answer "is this pin an ancestor
# of HEAD, and how far behind / how old is it".
STANDARDS_DIR="${STALENESS_STANDARDS_DIR:-}"
if [ -z "$STANDARDS_DIR" ]; then
  if git -C "$SCRIPT_DIR/.." rev-parse --git-dir >/dev/null 2>&1; then
    STANDARDS_DIR="$SCRIPT_DIR/.."
  elif git -C "$SCRIPT_DIR" rev-parse --git-dir >/dev/null 2>&1; then
    STANDARDS_DIR="$SCRIPT_DIR"
  fi
fi

HAVE_HISTORY=false
if [ -n "$STANDARDS_DIR" ] && git -C "$STANDARDS_DIR" rev-parse --git-dir >/dev/null 2>&1; then
  HAVE_HISTORY=true
fi

# ── Determine current approved standards SHA ────────────────────────────────
CURRENT_SHA="${STALENESS_EXPECTED_SHA:-}"
if [ -z "$CURRENT_SHA" ] && [ "$HAVE_HISTORY" = true ]; then
  CURRENT_SHA=$(git -C "$STANDARDS_DIR" rev-parse HEAD 2>/dev/null || echo "")
fi

if [ -z "$CURRENT_SHA" ]; then
  echo "::error::Could not determine current standards SHA. Set STALENESS_EXPECTED_SHA."
  exit 1
fi

echo "Staleness Check against Standards SHA: $CURRENT_SHA"
echo "Recency window: <= ${WINDOW_COMMITS} commits behind HEAD OR <= ${WINDOW_DAYS} days old."

# If no root .github/workflows exists, pass.
if [ ! -d "$REPO_ROOT/.github/workflows" ]; then
  echo "No .github/workflows directory found. Passing."
  exit 0
fi

FAILED=0

# ── Helpers for the recency-window check ────────────────────────────────────

# Ensure the standards checkout contains a given commit; best-effort fetch if
# not (handles partial clones). Returns 0 if the commit is present afterwards.
ensure_commit() {
  local sha="$1"
  git -C "$STANDARDS_DIR" cat-file -e "${sha}^{commit}" 2>/dev/null && return 0
  git -C "$STANDARDS_DIR" fetch -q origin "$sha" 2>/dev/null || true
  git -C "$STANDARDS_DIR" cat-file -e "${sha}^{commit}" 2>/dev/null
}

# Classify a pin relative to CURRENT_SHA. Echoes one of:
#   FRESH | AHEAD | IN_WINDOW <behind> <age_days> | OUT_OF_WINDOW <behind> <age_days> | UNKNOWN
classify_pin() {
  local pin="$1"

  # Legacy/degraded mode: no standards history available -> exact match only.
  if [ "$HAVE_HISTORY" != true ]; then
    [ "$pin" = "$CURRENT_SHA" ] && echo "FRESH" || echo "UNKNOWN"
    return
  fi

  if ! ensure_commit "$pin"; then
    echo "UNKNOWN"
    return
  fi

  # Normalise to full SHAs for equality / date lookups.
  local pin_full head_full
  pin_full=$(git -C "$STANDARDS_DIR" rev-parse "${pin}^{commit}" 2>/dev/null || echo "$pin")
  head_full=$(git -C "$STANDARDS_DIR" rev-parse "${CURRENT_SHA}^{commit}" 2>/dev/null || echo "$CURRENT_SHA")

  if [ "$pin_full" = "$head_full" ]; then
    echo "FRESH"
    return
  fi

  # Consumer pinned a commit that HEAD is an ancestor of => consumer is ahead
  # of the gate's view of standards (e.g. a race during a HEAD move). Not stale.
  if git -C "$STANDARDS_DIR" merge-base --is-ancestor "$head_full" "$pin_full" 2>/dev/null; then
    echo "AHEAD"
    return
  fi

  # Pin behind HEAD: must be a genuine ancestor on standards' mainline.
  if git -C "$STANDARDS_DIR" merge-base --is-ancestor "$pin_full" "$head_full" 2>/dev/null; then
    local behind pin_ts now_ts age_days
    behind=$(git -C "$STANDARDS_DIR" rev-list --count "${pin_full}..${head_full}" 2>/dev/null || echo 999999)
    pin_ts=$(git -C "$STANDARDS_DIR" log -1 --format=%ct "$pin_full" 2>/dev/null || echo 0)
    now_ts=$(date -u +%s)
    if [ "$pin_ts" -gt 0 ]; then
      age_days=$(( (now_ts - pin_ts) / 86400 ))
    else
      age_days=999999
    fi
    if [ "$behind" -le "$WINDOW_COMMITS" ] || [ "$age_days" -le "$WINDOW_DAYS" ]; then
      echo "IN_WINDOW $behind $age_days"
    else
      echo "OUT_OF_WINDOW $behind $age_days"
    fi
    return
  fi

  # Diverged: a real commit, but not on the line between an ancestor and HEAD.
  echo "UNKNOWN"
}

# Apply the freshness verdict for one reusable pin found in one workflow file.
check_reusable_pin() {
  local wf="$1" reusable="$2" pin="$3"
  local verdict behind age
  read -r verdict behind age < <(classify_pin "$pin")

  case "$verdict" in
    FRESH|AHEAD)
      ;; # current (or ahead) — nothing to do
    IN_WINDOW)
      # Non-failing nudge: within the grace window, but a deliberate bump is
      # available via the propagation path.
      echo "::notice file=$wf::${reusable} pin ${pin:0:12} is ${behind} commit(s) / ${age}d behind standards HEAD — within the recency window (<=${WINDOW_COMMITS} commits or <=${WINDOW_DAYS}d). Bump deliberately with scripts/propagate-workflow-pins.sh when convenient."
      ;;
    OUT_OF_WINDOW)
      echo "::error file=$wf::${reusable} pin ${pin:0:12} is ${behind} commit(s) / ${age}d behind standards HEAD — outside the recency window (>${WINDOW_COMMITS} commits AND >${WINDOW_DAYS}d). Refresh toward ${CURRENT_SHA:0:12} (e.g. scripts/propagate-workflow-pins.sh)."
      FAILED=1
      ;;
    UNKNOWN|*)
      echo "::error file=$wf::${reusable} pin ${pin:0:12} is not a recognised ancestor of standards HEAD (${CURRENT_SHA:0:12}). It may be forged, from a fork, or too divergent to verify. Pin a published standards commit."
      FAILED=1
      ;;
  esac
}

# Rule: no_retired_scorecard_enforcer
if [ "$IS_STANDARDS" = "false" ] && [ -f "$REPO_ROOT/.github/workflows/scorecard-enforcer.yml" ]; then
  echo "::error::scorecard-enforcer.yml is retired. Use scorecard.yml -> standards scorecard-reusable.yml instead."
  FAILED=1
fi

for wf in "$REPO_ROOT"/.github/workflows/*.yml "$REPO_ROOT"/.github/workflows/*.yaml; do
  [ -f "$wf" ] || continue

  # Rule: no_scorecard_sarif_code_scanning (structural — independent of pins)
  if grep -q "ossf/scorecard-action@" "$wf" && grep -q "github/codeql-action/upload-sarif@" "$wf"; then
    echo "::error file=$wf::OSSF Scorecard must not upload SARIF to GitHub Code Scanning unless it runs for every PR head commit."
    FAILED=1
  fi

  # Rule: recency-window freshness for each standards reusable pin.
  for reusable in "${STANDARDS_REUSABLES[@]}"; do
    # Active (non-comment) `uses:` lines that pin this reusable by SHA.
    while IFS= read -r line; do
      [ -n "$line" ] || continue
      pin=$(printf '%s\n' "$line" | sed -E "s#.*${reusable}@([0-9a-fA-F]{7,40}).*#\1#")
      [ -n "$pin" ] || continue
      check_reusable_pin "$wf" "$reusable" "$pin"
    done < <(grep -E "${reusable}@[0-9a-fA-F]{7,40}" "$wf" 2>/dev/null | grep -v '^[[:space:]]*#')
  done
done

if [ $FAILED -ne 0 ]; then
  echo "::error::Remove legacy scorecard-enforcer.yml, refresh out-of-window standards reusable pins toward a recent commit, and keep Scorecard out of GitHub Code Scanning unless it runs for every PR head commit."
  exit 1
fi

echo "All workflow staleness checks passed."
exit 0
