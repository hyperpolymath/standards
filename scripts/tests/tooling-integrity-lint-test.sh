#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Proves tooling-integrity-lint.sh can fail on an unversioned family-tool install.
set -euo pipefail
SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/tooling-integrity-lint.sh"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/.github/workflows"

printf 'jobs:\n  a:\n    steps:\n      - uses: extractions/setup-just@v2\n        with:\n          tool: just\n' > "$WORK/.github/workflows/bad.yml"
set +e
( cd "$WORK" && bash "$SCRIPT" ) >/dev/null 2>&1
got=$?
set -e
if [ "$got" -ne 1 ]; then
  echo "FAIL unversioned tool: just should exit 1, got $got"
  exit 1
fi

printf 'jobs:\n  a:\n    steps:\n      - run: echo ok\n' > "$WORK/.github/workflows/ok.yml"
rm -f "$WORK/.github/workflows/bad.yml"
( cd "$WORK" && bash "$SCRIPT" ) >/dev/null
echo "ok tooling-integrity-lint can fail and can pass"
