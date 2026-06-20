#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
CHECK="$SCRIPT_DIR/../check-workflow-staleness.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

mkdir -p "$WORK/repo/.github/workflows"

cat > "$WORK/repo/.github/workflows/scorecard.yml" <<'EOF'
name: Scorecards
jobs:
  analysis:
    uses: hyperpolymath/standards/.github/workflows/scorecard-reusable.yml@e0caf11508a3989574713c78f5f444f2ce5e33ef
EOF

if GITHUB_REPOSITORY=hyperpolymath/example bash "$CHECK" "$WORK/repo" >/tmp/stale.out 2>&1; then
  echo "expected stale scorecard reusable pin to fail"
  cat /tmp/stale.out
  exit 1
fi

cat > "$WORK/repo/.github/workflows/scorecard.yml" <<'EOF'
name: Scorecards
jobs:
  analysis:
    steps:
      - uses: ossf/scorecard-action@4eaacf0543bb3f2c246792bd56e8cdeffafb205a
      - uses: github/codeql-action/upload-sarif@8aad20d150bbac5944a9f9d289da16a4b0d87c1e
EOF

if GITHUB_REPOSITORY=hyperpolymath/example bash "$CHECK" "$WORK/repo" >/tmp/sarif.out 2>&1; then
  echo "expected direct Scorecard SARIF upload to fail"
  cat /tmp/sarif.out
  exit 1
fi

cat > "$WORK/repo/.github/workflows/hypatia-scan.yml" <<'EOF'
name: Hypatia
jobs:
  scan:
    uses: hyperpolymath/standards/.github/workflows/hypatia-scan-reusable.yml@5eb28d7d8790d5389b7b6a5233fe6265a775e3d0
EOF

if GITHUB_REPOSITORY=hyperpolymath/example bash "$CHECK" "$WORK/repo" >/tmp/hypatia.out 2>&1; then
  echo "expected stale Hypatia reusable pin to fail"
  cat /tmp/hypatia.out
  exit 1
fi

cat > "$WORK/repo/.github/workflows/hypatia-scan.yml" <<'EOF'
name: Hypatia
jobs:
  scan:
    uses: hyperpolymath/standards/.github/workflows/hypatia-scan-reusable.yml@main
EOF
rm -f "$WORK/repo/.github/workflows/scorecard.yml"

GITHUB_REPOSITORY=hyperpolymath/example bash "$CHECK" "$WORK/repo" >/tmp/clean.out 2>&1

echo "check-workflow-staleness-test: ok"
