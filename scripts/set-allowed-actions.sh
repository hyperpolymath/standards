#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# set-allowed-actions.sh — apply the canonical Actions allowlist to a repo.
#
# This is the ROOT prevention for the "empty-allowlist startup_failure" class:
# when a repo is CREATED or MIGRATED to a new org, its `allowed_actions` allowlist
# is reset (often to empty patterns), so every estate action/reusable is blocked
# and the whole CI dies at startup. Run this on onboard/migrate so the allowlist
# is never empty. This is what the repo-automaton (farm sweep) must call with an
# admin token — the default GITHUB_TOKEN CANNOT change Actions policy.
#
# Requires: a token with repo Administration:write (fine-grained) or classic
# `repo` + org owner — i.e. the farm ADMIN PAT, not the workflow GITHUB_TOKEN.
#
# Usage:  set-allowed-actions.sh <owner/repo> [ALLOWED_ACTIONS_JSON]
set -euo pipefail
REPO="${1:?usage: set-allowed-actions.sh <owner/repo> [allowed-actions.json]}"
CANON="${2:-rhodium-standard-repositories/actions-allowlist/allowed-actions.json}"
[ -f "$CANON" ] || { echo "!! canonical allowlist not found: $CANON"; exit 2; }

echo "==> ensuring allowed_actions=selected + sha_pinning_required on $REPO"
gh api -X PUT "repos/$REPO/actions/permissions" \
  -F enabled=true -f allowed_actions=selected >/dev/null

echo "==> applying $(python3 -c "import json,sys;print(len(json.load(open('$CANON'))['patterns_allowed']))") patterns to $REPO"
gh api -X PUT "repos/$REPO/actions/permissions/selected-actions" --input "$CANON"

echo "==> verify"
gh api "repos/$REPO/actions/permissions/selected-actions" \
  --jq '{patterns:(.patterns_allowed|length), github_owned:.github_owned_allowed, verified:.verified_allowed}'
echo "✔ canonical allowlist applied to $REPO"
