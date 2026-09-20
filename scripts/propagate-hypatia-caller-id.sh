#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# propagate-hypatia-caller-id.sh — deliberate, audit-first standardisation of the
#   Hypatia wrapper's caller job id across consumer repositories.
#
# WHY THIS EXISTS
# ---------------
# The check a reusable-caller job publishes is `<caller job id or name> / <inner
# job display name>`. The estate's canonical required context is
# `hypatia / Hypatia Neurosymbolic Analysis` (docs/audits/audit-hypatia-pin-orphan-2026-05-27.adoc),
# which requires the caller job to be named `hypatia`. In practice the caller is
# named `scan` in the large majority of consumer repositories, so they publish
# `scan / Hypatia Neurosymbolic Analysis` instead. Two names for one gate: any
# required context written against one of them is unsatisfiable in a repository
# that publishes the other — the defect class of hyperpolymath/tropical-types#17.
#
# This is the standards-side action that settles it: rename the caller job id to
# the canonical one, per repository, deliberately.
#
# Mode: READ-ONLY (audit) by default. Pass --fix to stage the rewrite in the
#   consumer checkout. The script NEVER commits, NEVER pushes, and NEVER edits a
#   required-status-check — that is the human's / bot's job (estate guardrail: no
#   unattended mutations; mirrors scripts/propagate-workflow-pins.sh).
#
# Principles (do not violate):
#   * Non-destructive: rewrites one job key, nothing else. The body of the job,
#     its pin, its inputs and its secrets are untouched byte-for-byte.
#   * Idempotent: a caller already at the canonical id is left untouched.
#   * Requirement-aware: a rename silently breaks any repository whose rules name
#     the old prefixed string. Those are reported BLOCKED and refused under --fix
#     until the requirement is updated in the same change.
#   * Honest about blindness: legacy branch protection cannot be read with an
#     ordinary token. If requirements cannot be read, the verdict is UNVERIFIED
#     and --fix refuses. Never rename on an unread requirement set.
#   * Shell-only: no Python, no Ruby.
#
# Usage:
#   bash scripts/propagate-hypatia-caller-id.sh [PATH]
#   bash scripts/propagate-hypatia-caller-id.sh --fix [PATH]
#   bash scripts/propagate-hypatia-caller-id.sh --caller-id <id> [PATH]
#   bash scripts/propagate-hypatia-caller-id.sh --no-network [PATH]
#
#   PATH may be a single consumer repo (has .github/workflows) or a parent
#   directory of many repos. Defaults to the current directory.
#
#   --caller-id <id>   canonical caller job id (default: hypatia)
#   --no-network       never probe the API; requirements count as UNVERIFIED
#   --fix              rewrite the caller job id in place (no commit, no push)
#
# Environment seams (testing / CI):
#   HYPATIA_REQUIRED_JSON  file containing a JSON array of required context
#                          strings for the repositories under PATH. When set, it
#                          replaces the API probe entirely (hermetic).
#   GITHUB_REPOSITORY      used to probe one repository's rulesets when the path
#                          argument is a single repo and the API is reachable.
#
# Output: tab-separated audit lines
#   <repo>\t<workflow-file>\t<caller-id>\t<verdict>
# verdict ∈ canonical | stage | BLOCKED | UNVERIFIED | unchanged-shape | absent
set -uo pipefail

CANONICAL="hypatia"
MODE_FIX=0
OFFLINE=0
TARGET="."

while [ $# -gt 0 ]; do
  case "$1" in
    --fix)         MODE_FIX=1; shift ;;
    --no-network)  OFFLINE=1; shift ;;
    --caller-id)   CANONICAL="${2:-}"; shift 2 ;;
    -h|--help)     sed -n '2,60p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    -*)            printf 'unknown option: %s\n' "$1" >&2; exit 2 ;;
    *)             TARGET="$1"; shift ;;
  esac
done

[ -n "$CANONICAL" ] || { echo "canonical caller id must not be empty" >&2; exit 2; }

say() { printf '%s\n' "$*"; }
note() { printf '::notice::%s\n' "$*" >&2; }

# ---- requirement discovery -------------------------------------------------
# The rename is only safe when nothing requires the old prefixed string.
# Returns "read" / "unreadable" and prints one required context per line.
requirements() {
  if [ -n "${HYPATIA_REQUIRED_JSON:-}" ]; then
    [ -f "$HYPATIA_REQUIRED_JSON" ] || { note "HYPATIA_REQUIRED_JSON not found"; echo unreadable; return 0; }
    if ! command -v jq >/dev/null 2>&1; then echo unreadable; return 0; fi
    jq -r '.[]' "$HYPATIA_REQUIRED_JSON" 2>/dev/null || { echo unreadable; return 0; }
    echo "__read__"
    return 0
  fi
  [ "$OFFLINE" = 1 ] && { echo unreadable; return 0; }
  command -v gh >/dev/null 2>&1 || { echo unreadable; return 0; }
  command -v jq >/dev/null 2>&1 || { echo unreadable; return 0; }
  [ -n "${GITHUB_REPOSITORY:-}" ] || { echo unreadable; return 0; }
  local list body
  list="$(gh api "repos/$GITHUB_REPOSITORY/rulesets" 2>/dev/null || true)"
  printf '%s' "$list" | jq -e 'type == "array"' >/dev/null 2>&1 || { echo unreadable; return 0; }
  for id in $(printf '%s' "$list" | jq -r '.[]? | select(.target=="branch") | .id'); do
    body="$(gh api "repos/$GITHUB_REPOSITORY/rulesets/$id" 2>/dev/null || true)"
    printf '%s' "$body" | jq -r '
      if type == "object" and .enforcement != "disabled" then
        .rules[]? | select(.type=="required_status_checks") | .parameters.required_status_checks[]?
        | (.context|tostring)
      else empty end' 2>/dev/null
  done
  echo "__read__"
}

# Read the requirement set once for the whole run: a parent directory of repos
# cannot be probed per-repo with a single GITHUB_REPOSITORY, so the seam is
# per-run by design and the API path is documented as single-repo only.
REQ_RAW="$(requirements)"
REQ_READABLE=0
REQ_LIST=""
if printf '%s\n' "$REQ_RAW" | grep -qx '__read__'; then
  REQ_READABLE=1
  REQ_LIST="$(printf '%s\n' "$REQ_RAW" | grep -vx '__read__')"
fi

requires_old_name() { # $1 = old caller id
  [ "$REQ_READABLE" = 1 ] || return 2      # 2 = unknown
  printf '%s\n' "$REQ_LIST" | grep -Fxq "$1 / Hypatia Neurosymbolic Analysis"
}

process_repo() { # $1 = repository directory
  local repo="$1" wf caller verdict
  repo="$(cd "$repo" && pwd)"
  wf="$repo/.github/workflows/hypatia-scan.yml"
  if [ ! -f "$wf" ]; then
    printf '%s\t%s\t-\tabsent\n' "$(basename "$repo")" ".github/workflows/hypatia-scan.yml"
    return 0
  fi

  # The caller job is the one whose `uses:` names the estate's scan reusable.
  caller="$(awk '
    /^[[:space:]]*[A-Za-z0-9_.-]+:[[:space:]]*$/ { key=$1; sub(/:$/, "", key) }
    /^[[:space:]]+uses:.*hypatia-scan-reusable\.ya?ml@/    { print key; exit }
  ' "$wf")"

  if [ -z "$caller" ]; then
    printf '%s\t%s\t-\tunchanged-shape\n' "$(basename "$repo")" ".github/workflows/hypatia-scan.yml"
    return 0
  fi
  if [ "$caller" = "$CANONICAL" ]; then
    printf '%s\t%s\t%s\tcanonical\n' "$(basename "$repo")" ".github/workflows/hypatia-scan.yml" "$caller"
    return 0
  fi

  # A rename changes the published check name. Refuse when a rule names the old one.
  if requires_old_name "$caller"; then
    verdict="BLOCKED"
  elif [ "$REQ_READABLE" = 1 ]; then
    verdict="stage"
  else
    verdict="UNVERIFIED"
  fi

  if [ "$verdict" = "stage" ] && [ "$MODE_FIX" = 1 ]; then
    # Rewrite only the job key line: same indentation, same position.
    local tmp; tmp="$(mktemp)"
    awk -v from="$caller" -v to="$CANONICAL" '
      BEGIN { done = 0 }
      !done && $0 ~ "^[[:space:]]*" from ":[[:space:]]*$" { sub(from ":", to ":"); done = 1 }
      { print }
    ' "$wf" > "$tmp" && mv "$tmp" "$wf"
    verdict="staged"
  fi

  printf '%s\t%s\t%s\t%s\n' "$(basename "$repo")" ".github/workflows/hypatia-scan.yml" "$caller" "$verdict"
  return 0
}

if [ -f "$TARGET/.github/workflows/hypatia-scan.yml" ]; then
  process_repo "$TARGET"
else
  found=0
  for d in "$TARGET"/*/; do
    [ -d "$d/.github/workflows" ] || continue
    process_repo "$d"
    found=1
  done
  [ "$found" = 1 ] || { note "no consumer repositories found under $TARGET"; exit 1; }
fi

if [ "$REQ_READABLE" != 1 ]; then
  note "requirements unreadable (no API access, offline, or a multi-repo path): every differing caller id is UNVERIFIED, and --fix refuses. Read the requirements first — a rename is only safe when nothing requires the old name."
fi
if [ "$MODE_FIX" = 1 ]; then
  note "--fix only stages edits. Commit, push and open the PR yourself or via the fleet bot (estate guardrail: no unattended mutations)."
fi
exit 0
