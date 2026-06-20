#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0

set -euo pipefail

ROOT="${1:-.}"
REPO="${GITHUB_REPOSITORY:-unknown/unknown}"
FAILED=0

error() {
  echo "ERROR: $*"
  FAILED=1
}

workflow_files() {
  find "$ROOT/.github/workflows" -maxdepth 1 \( -name '*.yml' -o -name '*.yaml' \) -type f 2>/dev/null | sort
}

if [ ! -d "$ROOT/.github/workflows" ]; then
  echo "No root .github/workflows directory; workflow staleness check skipped."
  exit 0
fi

if [ "$REPO" != "hyperpolymath/standards" ] && [ -f "$ROOT/.github/workflows/scorecard-enforcer.yml" ]; then
  error "$ROOT/.github/workflows/scorecard-enforcer.yml is a retired estate wrapper. Use scorecard.yml -> standards scorecard-reusable.yml instead."
fi

while IFS= read -r file; do
  [ -f "$file" ] || continue

  if grep -q 'ossf/scorecard-action@' "$file" && grep -q 'github/codeql-action/upload-sarif@' "$file"; then
    error "$file uploads OSSF Scorecard SARIF to GitHub Code scanning. Scorecard runs on default-branch/schedule cadence, so this creates stale PR code-scanning waits."
  fi

  if grep -q 'scorecard-reusable.yml@' "$file"; then
    ref="$(sed -n 's/.*scorecard-reusable\.yml@\([A-Za-z0-9._/-]*\).*/\1/p' "$file" | head -1)"
    case "$ref" in
      e0caf11508a3989574713c78f5f444f2ce5e33ef|\
      e03686486e11b662834d7090dffae54c3e96fd59|\
      86ea49fd3c94db3dd61dea40759e729fff356d81|\
      6cd3772824e59c8c9affeab66061e25383544242|\
      19995ace5f1179e9e2d4783fe1d36df0f343492d|\
      ae5b6731e0c1ce192edc14737a226d6b201341aa)
        error "$file pins scorecard-reusable.yml@$ref, which still publishes Scorecard as code-scanning SARIF. Refresh to the no-SARIF reusable revision."
        ;;
    esac
  fi

  if grep -q 'hypatia-scan-reusable.yml@' "$file"; then
    ref="$(sed -n 's/.*hypatia-scan-reusable\.yml@\([A-Za-z0-9._/-]*\).*/\1/p' "$file" | head -1)"
    case "$ref" in
      5eb28d7d8790d5389b7b6a5233fe6265a775e3d0|\
      6cd3772824e59c8c9affeab66061e25383544242|\
      915139d73560e65a8240b8fc7768698658502c89|\
      97df762107501909f50bb770e9bc200b6c415600|\
      f5f0506a6ec88e574753eee701a268e0d4b3a7f2)
        error "$file pins hypatia-scan-reusable.yml@$ref before the Hypatia build-cache fix. Refresh to a cached reusable revision."
        ;;
    esac
  fi
done < <(workflow_files)

if [ "$FAILED" -ne 0 ]; then
  echo
  echo "Workflow staleness policy failed."
  echo "Remediation: remove legacy scorecard-enforcer.yml, refresh standards reusable pins, and keep Scorecard out of GitHub Code scanning unless it runs for every PR head commit."
  exit 1
fi

echo "Workflow staleness policy passed."
