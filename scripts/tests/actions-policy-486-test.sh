#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Regression fixtures for the live Actions policy detector/setter (#486).
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CHECK="$SCRIPT_DIR/../check-actions-policy.sh"
SET="$SCRIPT_DIR/../set-allowed-actions.sh"
SELF_AUDIT="$SCRIPT_DIR/../rsr-selfaudit.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

CANON="$WORK/allowed-actions.json"
FAKE_GH="$WORK/gh"

printf '%s\n' '{"github_owned_allowed":true,"verified_allowed":true,"patterns_allowed":["owner/action@*"]}' > "$CANON"

cat > "$FAKE_GH" <<'FAKE'
#!/usr/bin/env bash
set -u
scenario="${POLICY_SCENARIO:-all-ok}"
args=" $* "

if [[ "$args" == *" -X PUT "* ]]; then
  printf '%s\n' '{}'
  exit 0
fi

if [[ "$args" == *"/selected-actions "* ]]; then
  case "$scenario" in
    selected-empty) printf '%s\n' '{"github_owned_allowed":true,"verified_allowed":true,"patterns_allowed":[]}' ;;
    selected-missing) printf '%s\n' '{"github_owned_allowed":true,"verified_allowed":true,"patterns_allowed":["other/action@*"]}' ;;
    *) printf '%s\n' '{"github_owned_allowed":true,"verified_allowed":true,"patterns_allowed":["owner/action@*","extra/action@*"]}' ;;
  esac
  exit 0
fi

case "$scenario" in
  api-unavailable) exit 4 ;;
  sha-off|setter-reset) printf '%s\n' '{"enabled":true,"allowed_actions":"all","sha_pinning_required":false}' ;;
  selected-empty|selected-missing|selected-ok|setter-selected)
    printf '%s\n' '{"enabled":true,"allowed_actions":"selected","sha_pinning_required":true}' ;;
  disabled) printf '%s\n' '{"enabled":false,"allowed_actions":"all","sha_pinning_required":true}' ;;
  *) printf '%s\n' '{"enabled":true,"allowed_actions":"all","sha_pinning_required":true}' ;;
esac
FAKE
chmod +x "$FAKE_GH"

pass=0
fail=0
expect() {
  local label="$1" want="$2" scenario="$3"; shift 3
  local out rc
  out="$(POLICY_SCENARIO="$scenario" GH_BIN="$FAKE_GH" "$@" 2>&1)"; rc=$?
  if [ "$rc" -eq "$want" ]; then
    echo "PASS: $label"
    pass=$((pass + 1))
  else
    echo "FAIL: $label (wanted $want, got $rc): $out"
    fail=$((fail + 1))
  fi
}

expect "all + SHA pinning passes" 0 all-ok "$CHECK" owner/repo "$CANON"
expect "API/authentication failure is unavailable, not a policy verdict" 3 api-unavailable "$CHECK" owner/repo "$CANON"
expect "SHA pinning off blocks" 1 sha-off "$CHECK" owner/repo "$CANON"
expect "disabled Actions blocks" 1 disabled "$CHECK" owner/repo "$CANON"
expect "empty selected allowlist blocks" 1 selected-empty "$CHECK" owner/repo "$CANON"
expect "selected allowlist missing canonical entry blocks" 1 selected-missing "$CHECK" owner/repo "$CANON"
expect "selected canonical superset passes" 0 selected-ok "$CHECK" owner/repo "$CANON"
expect "setter applies and verifies estate default" 0 all-ok "$SET" owner/repo "$CANON"
expect "setter refuses a read-back with SHA pinning reset" 1 setter-reset "$SET" owner/repo "$CANON"
expect "setter supports selected high-sensitivity posture" 0 setter-selected \
  env ACTIONS_POSTURE=selected "$SET" owner/repo "$CANON"
expect "RSR self-audit blocks an unpinned live policy" 1 sha-off \
  env RSR_REPOSITORY=owner/repo "$SELF_AUDIT" "$SCRIPT_DIR/../.."
expect "RSR self-audit accepts a compliant live policy" 0 all-ok \
  env RSR_REPOSITORY=owner/repo "$SELF_AUDIT" "$SCRIPT_DIR/../.."

echo "actions-policy-486-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
