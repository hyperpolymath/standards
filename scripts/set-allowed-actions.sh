#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# set-allowed-actions.sh — apply the canonical Actions allowlist at the level
# that actually governs a repo (repo -> org -> enterprise fallback).
#
# This is the ROOT prevention for the "empty-allowlist startup_failure" class:
# when a repo is CREATED or MIGRATED, its allowlist is reset, blocking every
# estate action/reusable at once so the whole CI dies at startup. Run this on
# onboard/migrate so patterns_allowed is never empty.
#
# The allowlist may be enforced at the REPO, the ORG, or the ENTERPRISE level. A
# repo-level PUT returns 409 "already set at the organization or enterprise level"
# when a higher level governs it — so this escalates automatically:
#   repo  -> (409) -> org (owner)  -> (409) -> enterprise (if slug given)
# Fixing it at the org/enterprise level repairs ALL repos beneath it at once.
#
# Requires an admin token for the level reached: repo Administration:write, or
# admin:org, or admin:enterprise — the farm ADMIN PAT, not the workflow
# GITHUB_TOKEN. (gh auth refresh -h github.com -s admin:org / admin:enterprise)
#
# Usage:  set-allowed-actions.sh <owner/repo> [ALLOWED_ACTIONS_JSON] [ENTERPRISE_SLUG]
#         ENTERPRISE=<slug> may be given via env instead of the 3rd arg.
set -uo pipefail

REPO="${1:?usage: set-allowed-actions.sh <owner/repo> [allowed-actions.json] [enterprise-slug]}"
CANON="${2:-rhodium-standard-repositories/actions-allowlist/allowed-actions.json}"
ENTERPRISE="${3:-${ENTERPRISE:-}}"
OWNER="${REPO%%/*}"
[ -f "$CANON" ] || { echo "!! canonical allowlist not found: $CANON" >&2; exit 2; }
N="$(python3 -c "import json;print(len(json.load(open('$CANON'))['patterns_allowed']))")"

# Apply the patterns at one scope. Returns 0 = applied, 42 = 409 (escalate), 1 = other error.
apply() {
  local label="$1" prefix="$2" out rc
  out="$(gh api -X PUT "$prefix/actions/permissions/selected-actions" --input "$CANON" 2>&1)"; rc=$?
  if [ $rc -eq 0 ]; then
    echo "✔ applied $N patterns at the $label level ($prefix) — governs everything beneath it"
    return 0
  fi
  if printf '%s' "$out" | grep -qiE 'organization or enterprise level|enterprise level'; then
    echo "   $label level is governed higher up (409) — escalating…"
    return 42
  fi
  echo "!! $label-level apply failed:" >&2; printf '%s\n' "$out" >&2
  return 1
}

# Best-effort: ensure the repo is on allowed_actions=selected, preserving `enabled`.
enabled="$(gh api "repos/$REPO/actions/permissions" --jq '.enabled' 2>/dev/null || echo true)"
gh api -X PUT "repos/$REPO/actions/permissions" -F enabled="$enabled" -f allowed_actions=selected >/dev/null 2>&1 || true

echo "==> repo level: $REPO"
apply "repo" "repos/$REPO"; rc=$?
[ $rc -eq 0 ] && exit 0
[ $rc -ne 42 ] && exit 1

echo "==> org level: $OWNER (fixes every repo in the org)"
apply "org" "orgs/$OWNER"; rc=$?
[ $rc -eq 0 ] && exit 0
[ $rc -ne 42 ] && exit 1

if [ -n "$ENTERPRISE" ]; then
  echo "==> enterprise level: $ENTERPRISE (fixes every org in the enterprise)"
  apply "enterprise" "enterprises/$ENTERPRISE"; rc=$?
  [ $rc -eq 0 ] && exit 0
  exit 1
fi

cat >&2 <<EOF
!! Enforced at the ENTERPRISE level. Re-run with the enterprise slug:
     set-allowed-actions.sh $REPO $CANON <enterprise-slug>
   (needs admin:enterprise — gh auth refresh -h github.com -s admin:enterprise)
EOF
exit 3
