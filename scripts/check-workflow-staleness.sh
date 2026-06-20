#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
set -eo pipefail

# Staleness checker script for hyperpolymath estate repositories.
# Ensures that workflows do not use retired patterns or stale pins.

REPO_ROOT="${1:-.}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

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

# Determine current approved standards SHA dynamically, or allow override from environment
CURRENT_SHA="${STALENESS_EXPECTED_SHA:-}"
if [ -z "$CURRENT_SHA" ]; then
  # Look up the Git commit of the standards repo containing this script
  if [ -d "$SCRIPT_DIR/../.git" ]; then
    CURRENT_SHA=$(git -C "$SCRIPT_DIR/.." rev-parse HEAD 2>/dev/null || echo "")
  elif [ -d "$SCRIPT_DIR/.git" ]; then
    CURRENT_SHA=$(git -C "$SCRIPT_DIR" rev-parse HEAD 2>/dev/null || echo "")
  fi
fi

if [ -z "$CURRENT_SHA" ]; then
  echo "::error::Could not determine current standards SHA. Set STALENESS_EXPECTED_SHA."
  exit 1
fi

echo "Staleness Check against Standards SHA: $CURRENT_SHA"

# If no root .github/workflows exists, pass.
if [ ! -d "$REPO_ROOT/.github/workflows" ]; then
  echo "No .github/workflows directory found. Passing."
  exit 0
fi

FAILED=0

# Rule: no_retired_scorecard_enforcer
if [ "$IS_STANDARDS" = "false" ] && [ -f "$REPO_ROOT/.github/workflows/scorecard-enforcer.yml" ]; then
  echo "::error::scorecard-enforcer.yml is retired. Use scorecard.yml -> standards scorecard-reusable.yml instead."
  FAILED=1
fi

for wf in "$REPO_ROOT"/.github/workflows/*.yml "$REPO_ROOT"/.github/workflows/*.yaml; do
  [ -f "$wf" ] || continue

  # Rule: no_scorecard_sarif_code_scanning
  if grep -q "ossf/scorecard-action@" "$wf" && grep -q "github/codeql-action/upload-sarif@" "$wf"; then
    echo "::error file=$wf::OSSF Scorecard must not upload SARIF to GitHub Code Scanning unless it runs for every PR head commit."
    FAILED=1
  fi

  # Rule: no_stale_scorecard_reusable_pin
  if grep -q "scorecard-reusable.yml@" "$wf"; then
    PINNED_SHA=$(grep "scorecard-reusable.yml@" "$wf" | sed -E 's/.*scorecard-reusable.yml@([^ #\r\n]+).*/\1/')
    if [ "$PINNED_SHA" != "$CURRENT_SHA" ]; then
      echo "::error file=$wf::Workflow pins stale Scorecard reusable that publishes SARIF / causes Code Scanning waits. Refresh to current standards SHA."
      FAILED=1
    fi
  fi

  # Rule: no_stale_hypatia_reusable_pin
  if grep -q "hypatia-scan-reusable.yml@" "$wf"; then
    PINNED_SHA=$(grep "hypatia-scan-reusable.yml@" "$wf" | sed -E 's/.*hypatia-scan-reusable.yml@([^ #\r\n]+).*/\1/')
    if [ "$PINNED_SHA" != "$CURRENT_SHA" ]; then
      echo "::error file=$wf::Workflow pins Hypatia reusable before cache/baseline-delay fix. Refresh to current standards SHA."
      FAILED=1
    fi
  fi

  # Rule: estate_pin_freshness for governance.yml
  if grep -q "governance-reusable.yml@" "$wf"; then
    PINNED_SHA=$(grep "governance-reusable.yml@" "$wf" | sed -E 's/.*governance-reusable.yml@([^ #\r\n]+).*/\1/')
    if [ "$PINNED_SHA" != "$CURRENT_SHA" ]; then
      echo "::error file=$wf::Workflow pins stale governance reusable. Refresh to current standards SHA."
      FAILED=1
    fi
  fi
done

if [ $FAILED -ne 0 ]; then
  echo "::error::Remove legacy scorecard-enforcer.yml, refresh standards reusable pins, and keep Scorecard out of GitHub Code Scanning unless it runs for every PR head commit."
  exit 1
fi

echo "All workflow staleness checks passed."
exit 0
