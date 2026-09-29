#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# triage-2026-09-29-apply-test.sh — regression tests for scripts/triage-2026-09-29-apply.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SUT="$SCRIPT_DIR/../triage-2026-09-29-apply.sh"

[ -x "$SUT" ] || { echo "FAIL: $SUT is not executable" >&2; exit 1; }
bash -n "$SUT"

# 1. #658 must never be closed automatically before D10 on #787 is ruled.
if grep -E 'close_if_open[[:space:]]+658\b' "$SUT" >/dev/null; then
  echo "FAIL: $SUT closes #658 before D10 on #787 is ruled" >&2
  exit 1
fi
echo "PASS: #658 is held open pending D10 on #787"

# 2. #708 must verify the lockfile-drift-report TSV before closing.
grep -q 'lockfile-drift-report' "$SUT"
grep -q '\[drift\] clean' "$SUT"
echo "PASS: #708 verifies artifact TSV before closing"

# 3. --dry-run must execute cleanly against a stub `gh` that refuses any write verb.
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
cat > "$WORK/gh" <<'SHIM'
#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  api)
    case "${2:-}" in
      repos/*/issues/*) printf 'open\n' ;;
      repos/*/rules/branches/main) printf 'deletion,non_fast_forward,required_status_checks\n' ;;
      repos/metadatastician/burble/rulesets/18225024) printf 'CodeQL\n' ;;
      *) printf 'true\n' ;;
    esac
    ;;
  *)
    echo "FAIL: unexpected write call under --dry-run: gh $*" >&2
    exit 99
    ;;
esac
SHIM
chmod +x "$WORK/gh"

PATH="$WORK:$PATH" bash "$SUT" --dry-run >/dev/null
echo "PASS: --dry-run performs zero gh write calls"
