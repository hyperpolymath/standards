#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-required-contexts-test.sh — fixture suite for
# scripts/check-required-contexts.sh, the unsatisfiable-required-context gate.
#
# The script carries its own hermetic fixture: a synthetic repository (a
# reusable-wrapper caller, a plain job and a matrix job), a stubbed reusable in a
# cache directory, and required-context sets supplied as JSON. No network, no
# `gh`, no token, no API — so this runs in every environment and cannot be
# green-while-broken because a credential was missing.
#
# Branches driven:
#   * a context the wrapper publishes                -> satisfiable, exit 0
#   * the reusable's bare inner name (the #17 shape) -> unsatisfiable, advisory
#                                                       exit 0, strict exit 1
#   * an unknown name bound to no app integration    -> strict exit 1
#   * a name that is only a PREFIX of a real one     -> strict exit 1
#     (guards against the substring-matching bug this suite was written to catch)
#
# Run: bash scripts/tests/check-required-contexts-test.sh
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GATE="$SCRIPT_DIR/../check-required-contexts.sh"

if [ ! -f "$GATE" ]; then
  echo "FAIL: gate not found at $GATE"
  exit 1
fi

if output="$(bash "$GATE" --self-test 2>&1)"; then
  printf '%s\n' "$output"
  echo "PASS $0"
else
  printf '%s\n' "$output"
  echo "::error file=$GATE::fixture suite failed"
  exit 1
fi

# Exercise live-inspection paths with a fake gh, including failures on the
# second read (data rules) after the context inspection succeeded.
work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/repo/.github/workflows"
cat >"$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
path="${2:-}"
case "$path" in
  repos/example/repo) printf '{"default_branch":"release/current"}' ;;
  */rulesets)
    n="$(cat "$FIXTURES/list-count" 2>/dev/null || echo 0)"
    n=$((n+1)); echo "$n" >"$FIXTURES/list-count"
    if [ "$n" -gt 1 ]; then
      case "$MODE" in
        list-fail) cat "$FIXTURES/list.json"; exit 1 ;;
        list-invalid) echo '{"message":"error"}'; exit 0 ;;
        list-empty) exit 0 ;;
      esac
    fi
    cat "$FIXTURES/list.json" ;;
  */rulesets/*)
    id="${path##*/}"
    n="$(cat "$FIXTURES/detail-$id-count" 2>/dev/null || echo 0)"
    n=$((n+1)); echo "$n" >"$FIXTURES/detail-$id-count"
    if [ "$n" -gt 1 ]; then
      case "$MODE:$id" in
        detail-fail:2) cat "$FIXTURES/2.json"; exit 1 ;;
        detail-invalid:2) echo '{"message":"error"}'; exit 0 ;;
        detail-empty:2) exit 0 ;;
        detail-malformed:2) echo '{'; exit 0 ;;
        detail-shape:2) echo '{"enforcement":"active","rules":{}}'; exit 0 ;;
      esac
    fi
    cat "$FIXTURES/$id.json" ;;
  */protection/required_status_checks) echo '{"message":"not found"}'; exit 1 ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"
rule() { # id enforcement includes excludes
  jq -n --argjson id "$1" --arg e "$2" --argjson inc "$3" --argjson exc "$4" '{
    id: $id, target: "branch", enforcement: $e,
    conditions: {ref_name: {include: $inc, exclude: $exc}},
    rules: [{type:"code_coverage"}, {type:"code_quality"}]
  }' >"$work/$1.json"
}
rule 1 active '["~DEFAULT_BRANCH"]' '[]'
rule 2 active '["refs/heads/release/*"]' '[]'
rule 3 evaluate '["~ALL"]' '[]'
rule 4 disabled '["~ALL"]' '[]'
rule 5 active '["refs/heads/main"]' '[]'
rule 6 active '["~ALL"]' '["refs/heads/release/**"]'
rule 7 active '["refs/heads/*"]' '[]' # * must not cross a slash
rule 8 active '["refs/heads/**/curren?"]' '[]'
rule 9 active '["refs/heads/release/[c]urrent"]' '[]'
rule 10 active '["refs/heads/release/current+"]' '[]' # literal regex metachar
rule 11 active '["~ALL"]' '["~DEFAULT_BRANCH"]'
rule 12 active '["refs/heads/release/current"]' '[]'
rule 13 active '["~ALL"]' '["refs/heads/main"]'
rule 14 active '["~ALL"]' '["refs/heads/release/current"]'
jq -n '[range(1;15) | {id:., target:"branch"}]' >"$work/list.json"
pass=0; fail=0
run() {
  rm -f "$work"/*-count
  (unset REQUIRED_CONTEXTS_JSON
    PATH="$work/bin:$PATH" FIXTURES="$work" MODE="$1" GITHUB_REPOSITORY=example/repo \
      REQUIRED_CONTEXTS_STRICT="$2" bash "$GATE" "$work/repo" --no-network --json "$work/report.json"
  ) >"$work/out" 2>&1
  rc=$?
}
check() { # label expected-exit jq-assertion
  if [ "$rc" = "$2" ] && jq -e "$3" "$work/report.json" >/dev/null; then
    pass=$((pass+1)); echo "PASS $1"
  else
    fail=$((fail+1)); echo "FAIL $1 (exit $rc)"; cat "$work/out" "$work/report.json"
  fi
}
run ok 0
check 'only active rules matching the merge branch are counted in JSON' 0 \
  '.inspected == true and .unsatisfiable == 12 and (.results | length) == 12 and
   ([.results[].context | capture("ruleset (?<id>[0-9]+)").id] | unique) == ["1","12","13","2","8","9"]'
run ok 1
check 'strict mode fails when findings consist only of data rules' 1 '.inspected == true and .unsatisfiable == 12'
for mode in list-fail list-invalid list-empty detail-fail detail-invalid detail-empty detail-malformed detail-shape; do
  run "$mode" 0
  check "$mode does not become an empty success or retain partial findings" 1 '.inspected == false and .results == []'
done
printf '\nAPI fixture tests: %s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
