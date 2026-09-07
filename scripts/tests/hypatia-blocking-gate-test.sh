#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# Execute the actual reusable-workflow steps against success and failure controls.
set -euo pipefail
repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
ruby -ryaml -e '
  workflow = YAML.load_file(ARGV[0])
  steps = workflow.fetch("jobs").fetch("scan").fetch("steps")
  validator = steps.find { |step| step["id"] == "scan" }
  gate = steps.find { |step| step["id"] == "blocking-findings" }
  abort "blocking gate is not opt-in" unless gate.fetch("if") == "inputs.block-on-high"
  File.write(ARGV[1] + "/validate.sh", validator.fetch("run"))
  File.write(ARGV[1] + "/gate.sh", gate.fetch("run"))
' "$repo/.github/workflows/hypatia-scan-reusable.yml" "$tmp"
export GITHUB_OUTPUT="$tmp/output" GITHUB_STEP_SUMMARY="$tmp/summary"
cd "$tmp"
check() {
  local name=$1 expected=$2 payload=$3 actual
  if [[ "$payload" = MISSING ]]; then
    rm -f hypatia-findings.json
  else
    printf '%s' "$payload" > hypatia-findings.json
  fi
  if bash validate.sh >result.log 2>&1; then
    if bash gate.sh >>result.log 2>&1; then actual=0; else actual=$?; fi
  else
    actual=$?
  fi
  if [[ "$actual" -ne "$expected" ]]; then
    printf 'FAIL: %s: expected %s, got %s\n' "$name" "$expected" "$actual"
    cat result.log
    exit 1
  fi
  printf 'PASS: %s\n' "$name"
}
check 'empty findings are valid' 0 '[]'
check 'low and informational findings pass' 0 '[{"severity":"low"},{"severity":"info"}]'
check 'high finding blocks' 1 '[{"severity":"high"}]'
check 'critical finding blocks' 1 '[{"severity":"critical"}]'
check 'missing artifact refuses' 2 MISSING
check 'empty artifact refuses' 2 ''
check 'truncated JSON refuses' 2 '[{"severity":'
check 'object is not a findings array' 2 '{}'
check 'null is not a findings array' 2 'null'
check 'unknown severity refuses' 2 '[{"severity":"unknown"}]'
check 'missing severity refuses' 2 '[{}]'
check 'multiple JSON documents refuse' 2 '[] []'
