#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# check-actions-policy.sh — RSR Actions Allowlist Policy gate
#
# Part of Issue #486: Wire allowlist preflight into governance gate.
#
# This script wraps check-allowed-actions.sh to validate that every `uses:`
# in the caller repo's workflows is covered by the canonical allowlist.
# It is designed to run in CI (as part of governance-reusable.yml) with
# minimal dependencies: only actions/checkout (github-owned, always permitted),
# so it can never itself startup-fail due to an allowlist gap.
#
# Exit codes:
#   0 = all `uses:` are covered by the canonical allowlist
#   1 = one or more gaps detected
#   2 = allowlist file not found (should never happen in standards repo)
#
# Usage: check-actions-policy.sh [WORKFLOWS_DIR]
#   WORKFLOWS_DIR defaults to .github/workflows
set -euo pipefail

WF_DIR="${1:-.github/workflows}"

# The canonical allowlist lives in this repo at
# rhodium-standard-repositories/actions-allowlist/allowed-actions.json
# Resolve the canonical allowlist. $ALLOWLIST_JSON lets the caller point at a
# copy staged outside the scanned tree — required for consumer repos, which do
# not have rhodium-standard-repositories/ in their own checkout.
CANON="${ALLOWLIST_JSON:-rhodium-standard-repositories/actions-allowlist/allowed-actions.json}"

if [ ! -f "$CANON" ]; then
  echo "::error::Canonical allowlist not found: $CANON"
  echo "This script must be run from the standards repository or a checkout that includes it."
  exit 2
fi

# Run the actual check
exec bash "${0%/*}/check-allowed-actions.sh" "$CANON" "$WF_DIR"
