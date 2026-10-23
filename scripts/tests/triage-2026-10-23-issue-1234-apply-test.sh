#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# triage-2026-10-23-issue-1234-apply-test.sh — regression tests for
# scripts/triage-2026-10-23-issue-1234-apply.sh and
# docs/reorg/2026-10-14-tidy/ISSUE-1234-CLOSEOUT-REPORT.adoc.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
SUT="$SCRIPT_DIR/../triage-2026-10-23-issue-1234-apply.sh"
REPORT="$ROOT/docs/reorg/2026-10-14-tidy/ISSUE-1234-CLOSEOUT-REPORT.adoc"

[ -x "$SUT" ] || { echo "FAIL: $SUT is not executable" >&2; exit 1; }
bash -n "$SUT"
[ -f "$REPORT" ] || { echo "FAIL: $REPORT does not exist" >&2; exit 1; }

# 1. Verify A5 (D298) runs ?dry-run=true PUT before live PUT on both 23868655 and 23868851.
grep -q 'orgs/metadatastician/rulesets/\${id}?dry-run=true' "$SUT"
grep -q '23868655 23868851' "$SUT"
echo "PASS: A5 (D298) validates ?dry-run=true PUT before live PUT on 23868655 and 23868851"

# 2. Verify A1 (D16) strips 1561, 85455, and 946600 from all 10 target rulesets.
for id in 15402978 18110196 18874804 18875919 14968573 14968592 14968613 14968721 18110652 18110675; do
  grep -q ":$id" "$SUT" || { echo "FAIL: missing A1 ruleset $id in $SUT" >&2; exit 1; }
done
echo "PASS: all 10 A1 (D16) ruleset IDs are covered"

# 3. Verify close-out report covers A1-A7, D304, D332, and D335-D355.
for marker in "A1 (D16)" "A2 (D14)" "A3 (D24)" "A4 (D97)" "A5 (D298)" "A6 (D67" "A7 (D330)" "B / D304" "B / D332" "D355"; do
  grep -q "$marker" "$REPORT" || { echo "FAIL: missing '$marker' in $REPORT" >&2; exit 1; }
done
echo "PASS: close-out report contains all #1234 sections and ruling row D355"

# 4. --dry-run must execute cleanly against a stub `gh` that refuses any write verb.
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
cat > "$WORK/gh" <<'SHIM'
#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  api)
    if [ "${2:-}" = "-X" ]; then
      echo "FAIL: unexpected write call under --dry-run: gh $*" >&2
      exit 99
    fi
    case "${2:-}" in
      repos/hyperpolymath/*/rulesets/*)
        printf '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"rules":[{"type":"deletion"}],"bypass_actors":[{"actor_type":"Integration","actor_id":1561,"bypass_mode":"always"},{"actor_type":"RepositoryRole","actor_id":5,"bypass_mode":"always"}]}\n'
        ;;
      repos/metadatastician/metadatastician-governance/rulesets/*)
        printf '{"name":"Branch-Floor","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"},{"type":"non_fast_forward"},{"type":"required_linear_history"}]}\n'
        ;;
      repos/metadatastician/knot-knot)
        printf 'false\n'
        ;;
      user/installations)
        printf '1\n'
        ;;
      repos/hyperpolymath/standards/issues/*)
        printf 'open\n'
        ;;
      *)
        printf '{}\n'
        ;;
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
