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
  # Evidence lookups. Defaults are deliberately empty: "no evidence" must be the
  # resting state, or every test would pass by accident.
  */actions/workflows/*/runs\?status=success\&per_page=30)
    case "${RUNS_MODE:-none}" in
      ok|foreign|fork-first|missing-head)
        jq -n --arg mode "$RUNS_MODE" '
          {id:9, path:".github/workflows/coverage.yml", head_sha:"abcdef1234567890",
           head_branch:"main", conclusion:"success",
           head_repository:{full_name:"example/repo"}} as $run
          | ($run | .id = 10 | .head_repository.full_name = "example/fork") as $fork
          | {workflow_runs: (if $mode == "foreign" then [$fork]
              elif $mode == "fork-first" then [$fork, $run]
              elif $mode == "missing-head" then [$run | del(.head_repository)]
              else [$run] end)}' ;;
      invalid) echo '{"workflow_runs":{}}'; exit 0 ;;
      *) echo '{"workflow_runs":[]}' ;;
    esac ;;
  */actions/workflows\?per_page=100)
    # Model gh's --slurp output and require pagination, not just a larger page.
    [[ " $* " == *" --paginate "* && " $* " == *" --slurp "* ]] || exit 1
    case "${WF_MODE:-none}" in
      quality) echo '[{"total_count":1,"workflows":[{"name":"Code Quality","path":"dynamic/github-code-quality/codeql"}]}]' ;;
      local-quality) echo '[{"total_count":1,"workflows":[{"name":"Code Quality","path":".github/workflows/code-quality.yml"}]}]' ;;
      quality-later)
        jq -n '[range(100) | {name:"CI", path:".github/workflows/ci-\(.).yml"}] as $first
          | [{total_count:101, workflows:$first},
             {total_count:101, workflows:[{name:"Renamed analysis", path:"dynamic/github-code-quality/codeql"}]}]' ;;
      incomplete) echo '[{"total_count":2,"workflows":[{"name":"Code Quality","path":"dynamic/github-code-quality/codeql"}]}]' ;;
      inconsistent-total) echo '[{"total_count":1,"workflows":[]},{"total_count":2,"workflows":[{"path":"dynamic/github-code-quality/codeql"}]}]' ;;
      page-fail) echo '[{"total_count":0,"workflows":[]}]'; exit 1 ;;
      empty-pages) echo '[]' ;;
      invalid) echo '[{"total_count":0,"workflows":{}}]' ;;
      *) echo '[{"total_count":0,"workflows":[]}]' ;;
    esac ;;
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
      RUNS_MODE="${RUNS_MODE:-none}" WF_MODE="${WF_MODE:-none}" \
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
# An uploader in the tree must change the EXPLANATION and not the verdict. The
# rule stays unsatisfiable until Code Quality is enabled for the repository, so a
# producer must never turn the finding into a pass; what it must do is stop the
# audit claiming the repository "never uploads" while a workflow does.
cat >"$work/repo/.github/workflows/coverage.yml" <<'YML'
name: Coverage
on: [pull_request]
jobs:
  c:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/upload-code-coverage@0000000000000000000000000000000000000000
YML
run ok 1
if [ "$rc" = 1 ] && grep -q 'has an uploader in the tree (.github/workflows/coverage.yml)' "$work/out" && \
   ! grep -q 'never uploads' "$work/out"; then
  pass=$((pass+1)); echo "PASS an uploader in the tree changes the wording and not the verdict"
else
  fail=$((fail+1)); echo "FAIL an uploader in the tree changes the wording and not the verdict (exit $rc)"; cat "$work/out"
fi
# Negative control: with no uploader the old sentence must come back, or the test
# above would pass for a script that never looked at the tree at all.
rm -f "$work/repo/.github/workflows/coverage.yml"
run ok 1
if [ "$rc" = 1 ] && grep -q 'never uploads' "$work/out"; then
  pass=$((pass+1)); echo "PASS with no uploader the finding still says nothing uploads"
else
  fail=$((fail+1)); echo "FAIL with no uploader the finding still says nothing uploads (exit $rc)"; cat "$work/out"
fi

# Evidence: a successful run is what makes a data rule satisfied, and file
# presence alone must never be enough. Each fixture below holds one variable.
cat >"$work/repo/.github/workflows/coverage.yml" <<'YML'
name: Coverage
on: [pull_request]
jobs:
  c:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/upload-code-coverage@0000000000000000000000000000000000000000
YML
RUNS_MODE=ok run ok 1
if [ "$rc" = 1 ] && jq -e '.inspected == true and .unsatisfiable == 6' "$work/report.json" >/dev/null &&
   grep -q 'satisfied rule:code_coverage' "$work/out"; then
  pass=$((pass+1)); echo "PASS a successful coverage run satisfies only the coverage rule"
else
  fail=$((fail+1)); echo "FAIL a successful coverage run satisfies only the coverage rule (exit $rc)"; cat "$work/out"
fi
RUNS_MODE=fork-first run ok 1
check 'coverage evidence selects a repository run after a newer fork run' 1 \
  '.inspected == true and .unsatisfiable == 6 and
   ([.results[] | select(.verdict == "VERIFIED-RULE" and (.context | startswith("rule:code_coverage")))] | length) == 6'
for mode in foreign missing-head; do
  RUNS_MODE="$mode" run ok 1
  check "$mode coverage runs cannot verify a rule" 1 \
    '.inspected == true and .unsatisfiable == 12 and
     ([.results[] | select(.verdict == "VERIFIED-RULE")] | length) == 0'
done
# Same tree, no successful run: the uploader exists and proves nothing. This is
# the branch's first six runs, which all failed 404 while the feature was off.
run ok 1
if [ "$rc" = 1 ] && jq -e '.inspected == true and .unsatisfiable == 12' "$work/report.json" >/dev/null; then
  pass=$((pass+1)); echo "PASS an uploader with no successful run is not evidence"
else
  fail=$((fail+1)); echo "FAIL an uploader with no successful run is not evidence (exit $rc)"; cat "$work/out"
fi
WF_MODE=quality run ok 1
if [ "$rc" = 1 ] && jq -e '.inspected == true and .unsatisfiable == 6' "$work/report.json" >/dev/null &&
   grep -q 'satisfied rule:code_quality' "$work/out"; then
  pass=$((pass+1)); echo "PASS a Code Quality workflow satisfies the quality rule independently"
else
  fail=$((fail+1)); echo "FAIL a Code Quality workflow satisfies the quality rule independently (exit $rc)"; cat "$work/out"
fi
WF_MODE=local-quality run ok 1
check 'a repository workflow named Code Quality cannot verify the quality rule' 1 \
  '.inspected == true and .unsatisfiable == 12 and
   ([.results[] | select(.verdict == "VERIFIED-RULE")] | length) == 0'
WF_MODE=quality-later run ok 1
check 'a generated quality workflow beyond 100 entries is found by path despite its renamed display name' 1 \
  '.inspected == true and .unsatisfiable == 6 and
   ([.results[] | select(.verdict == "VERIFIED-RULE" and (.context | startswith("rule:code_quality")))] | length) == 6'
RUNS_MODE=ok WF_MODE=quality run ok 1
if [ "$rc" = 0 ] && jq -e '.inspected == true and .unsatisfiable == 0' "$work/report.json" >/dev/null; then
  pass=$((pass+1)); echo "PASS strict mode passes when both data rules are demonstrably fed"
else
  fail=$((fail+1)); echo "FAIL strict mode passes when both data rules are demonstrably fed (exit $rc)"; cat "$work/out"
fi
# A lookup that fails must not be read as "no data" (a silent finding) nor as
# "data present" (a silent pass). It is an inspection failure.
RUNS_MODE=invalid run ok 1
if [ "$rc" = 1 ] && jq -e '.inspected == false and .results == []' "$work/report.json" >/dev/null; then
  pass=$((pass+1)); echo "PASS a malformed runs response is an inspection failure, not a verdict"
else
  fail=$((fail+1)); echo "FAIL a malformed runs response is an inspection failure, not a verdict (exit $rc)"; cat "$work/out"
fi
WF_MODE=invalid run ok 1
if [ "$rc" = 1 ] && jq -e '.inspected == false and .results == []' "$work/report.json" >/dev/null; then
  pass=$((pass+1)); echo "PASS a malformed workflows response is an inspection failure, not a verdict"
else
  fail=$((fail+1)); echo "FAIL a malformed workflows response is an inspection failure, not a verdict (exit $rc)"; cat "$work/out"
fi
for mode in incomplete inconsistent-total page-fail empty-pages; do
  WF_MODE="$mode" run ok 1
  check "$mode workflow catalogue is an inspection failure" 1 '.inspected == false and .results == []'
done
rm -f "$work/repo/.github/workflows/coverage.yml"
for mode in list-fail list-invalid list-empty detail-fail detail-invalid detail-empty detail-malformed detail-shape; do
  run "$mode" 0
  check "$mode does not become an empty success or retain partial findings" 1 '.inspected == false and .results == []'
done
printf '\nAPI fixture tests: %s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
