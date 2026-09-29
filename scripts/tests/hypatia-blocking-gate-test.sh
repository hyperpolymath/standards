#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# Execute the actual reusable-workflow steps against success and failure controls.
set -euo pipefail
repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
if command -v ruby >/dev/null 2>&1; then
  ruby -ryaml -e '
    workflow = YAML.load_file(ARGV[0])
    steps = workflow.fetch("jobs").fetch("scan").fetch("steps")
    scan_runner = steps.find { |step| step["name"] == "Run Hypatia scan" }
    validator = steps.find { |step| step["id"] == "scan" }
    gate = steps.find { |step| step["id"] == "blocking-findings" }
    abort "blocking gate is not opt-in" unless gate.fetch("if") == "inputs.block-on-high"
    ["Relativize finding paths", "Filter SARIF through the baseline before upload", "Upload SARIF to code scanning", "Upload findings artifacts"].each do |name|
      s = steps.find { |step| step["name"] == name } or abort("missing step: #{name}")
      abort("#{name} must not use if: always() (standards#1050)") if s["if"].to_s.include?("always()")
    end
    File.write(ARGV[1] + "/scan.sh", scan_runner.fetch("run"))
    File.write(ARGV[1] + "/validate.sh", validator.fetch("run"))
    File.write(ARGV[1] + "/gate.sh", gate.fetch("run"))
  ' "$repo/.github/workflows/hypatia-scan-reusable.yml" "$tmp"
else
  awk -v out_dir="$tmp" '
    function flush_step() {
      if (step_name == "") return
      seen[step_name] = 1
      step_if_map[step_name] = step_if
      if (step_name == "Run Hypatia scan") {
        printf "%s", run_body > (out_dir "/scan.sh")
        close(out_dir "/scan.sh")
      }
      if (step_id == "scan") {
        printf "%s", run_body > (out_dir "/validate.sh")
        close(out_dir "/validate.sh")
      }
      if (step_id == "blocking-findings") {
        gate_if = step_if
        printf "%s", run_body > (out_dir "/gate.sh")
        close(out_dir "/gate.sh")
      }
    }
    /^      - name: / {
      flush_step()
      step_name = $0
      sub(/^      - name:[[:space:]]*/, "", step_name)
      step_id = ""; step_if = ""; run_body = ""; in_run = 0
      next
    }
    in_run {
      if ($0 ~ /^          / || $0 == "") {
        line = $0
        sub(/^          /, "", line)
        run_body = run_body line "\n"
        next
      } else {
        in_run = 0
      }
    }
    /^        id:[[:space:]]*/ {
      step_id = $0
      sub(/^        id:[[:space:]]*/, "", step_id)
      next
    }
    /^        if:[[:space:]]*/ {
      step_if = $0
      sub(/^        if:[[:space:]]*/, "", step_if)
      gsub(/^[\x27"]|[\x27"]$/, "", step_if)
      next
    }
    /^        run:[[:space:]]*\|$/ {
      in_run = 1
      next
    }
    END {
      flush_step()
      if (gate_if != "inputs.block-on-high") {
        print "blocking gate is not opt-in" > "/dev/stderr"
        exit 1
      }
      split("Relativize finding paths|Filter SARIF through the baseline before upload|Upload SARIF to code scanning|Upload findings artifacts", req, "|")
      for (i in req) {
        n = req[i]
        if (!(n in seen)) {
          print "missing step: " n > "/dev/stderr"
          exit 1
        }
        if (index(step_if_map[n], "always()") > 0) {
          print n " must not use if: always() (standards#1050)" > "/dev/stderr"
          exit 1
        }
      }
    }
  ' "$repo/.github/workflows/hypatia-scan-reusable.yml"
fi
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
    # Fixture for the authoritative renderer's severity projection. The
    # separate controls below exercise malformed SARIF and historical echoes.
    jq '{version:"2.1.0", runs:[{tool:{driver:{name:"Hypatia"}}, results:
      [.[] | {ruleId:"hypatia/control/planted", level:
        (if .severity == "critical" or .severity == "high" then "error"
         elif .severity == "medium" then "warning" else "note" end)}]}]}' \
      hypatia-findings.json > hypatia.sarif
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
check 'clean scan (empty findings array) passes' 0 '[]'
check 'low, warn, and informational findings pass' 0 '[{"severity":"low"},{"severity":"warn"},{"severity":"info"}]'
check 'high finding blocks' 1 '[{"severity":"high"}]'
check 'critical finding blocks' 1 '[{"severity":"critical"}]'
check 'missing artifact refuses' 2 MISSING
check 'empty artifact refuses' 2 ''
check 'whitespace-only artifact refuses' 2 $'   \n'
check 'truncated JSON refuses' 2 '[{"severity":'
check 'object is not a findings array' 2 '{}'
check 'null is not a findings array' 2 'null'
check 'unknown severity refuses' 2 '[{"severity":"unknown"}]'
check 'missing severity refuses' 2 '[{}]'
check 'multiple JSON documents refuse' 2 '[] []'
check 'newline-separated JSON documents refuse' 2 $'[]\n[{"severity":"high"}]'

sarif_check() {
  local name=$1 expected=$2 payload=$3 actual
  if [[ "$payload" = MISSING ]]; then
    rm -f hypatia.sarif
  else
    printf '%s' "$payload" > hypatia.sarif
  fi
  if bash gate.sh >result.log 2>&1; then actual=0; else actual=$?; fi
  if [[ "$actual" -ne "$expected" ]]; then
    printf 'FAIL: %s: expected %s, got %s\n' "$name" "$expected" "$actual"
    cat result.log
    exit 1
  fi
  printf 'PASS: %s\n' "$name"
}
clean='{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"Hypatia"}},"results":[]}]}'
printf '%s' '[{"rule_module":"code_scanning_alerts","type":"CSA003","severity":"high"}]' > hypatia-findings.json
sarif_check 'historical echo does not block a clean current scan' 0 "$clean"
sarif_check 'missing SARIF refuses' 2 MISSING
sarif_check 'empty SARIF refuses' 2 ''
sarif_check 'truncated SARIF refuses' 2 '{"version":'
sarif_check 'multiple SARIF documents refuse' 2 "$clean $clean"
sarif_check 'empty runs refuse' 2 '{"version":"2.1.0","runs":[]}'
sarif_check 'missing results refuse' 2 '{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"Hypatia"}}}]}'
sarif_check 'unknown result level refuses' 2 '{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"Hypatia"}},"results":[{"ruleId":"control","level":"unknown"}]}]}'
printf '%s' '[]' > .hypatia-baseline.json
sarif_check 'baseline without validator refuses' 2 "$clean"
mkdir scripts
cp "$repo/scripts/apply-baseline.sh" scripts/apply-baseline.sh
printf '%s' '[]' > hypatia-findings.relativized.json
sarif_check 'unconfirmed baseline filtering refuses' 2 "$clean"
export HYPATIA_BASELINE_FILTERED=true
sarif_check 'valid baseline accepted' 0 "$clean"
printf '%s' '{}' > .hypatia-baseline.json
sarif_check 'malformed baseline refuses even without findings' 2 "$clean"
rm -f .hypatia-baseline.json hypatia-findings.relativized.json
unset HYPATIA_BASELINE_FILTERED

# CLI adapter contract controls (standards#1054): empty, warn, high, and crash fixtures
mkdir -p "$tmp/home/hypatia"
cat > "$tmp/home/hypatia/hypatia-cli.sh" <<'CLI'
#!/usr/bin/env bash
set -euo pipefail
case "${FIXTURE_MODE:-}" in
  empty)
    if [[ "${HYPATIA_FORMAT:-}" = "json" ]]; then
      printf '[]\n'
    else
      printf '{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"Hypatia"}},"results":[]}]}\n'
    fi
    ;;
  warn)
    if [[ "${HYPATIA_FORMAT:-}" = "json" ]]; then
      printf '[{"severity":"warn","rule_module":"research_extensions","type":"RE001","file":"ci.yml"}]\n'
    else
      printf '{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"Hypatia"}},"results":[{"ruleId":"hypatia/RE001","level":"warning"}]}]}\n'
    fi
    ;;
  high)
    if [[ "${HYPATIA_FORMAT:-}" = "json" ]]; then
      printf '[{"severity":"high","rule_module":"workflow_hardening","type":"WH001","file":"ci.yml"}]\n'
    else
      printf '{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"Hypatia"}},"results":[{"ruleId":"hypatia/WH001","level":"error"}]}]}\n'
    fi
    ;;
  crash)
    printf '[]\n'
    exit 1
    ;;
esac
CLI
chmod +x "$tmp/home/hypatia/hypatia-cli.sh"

adapter_check() {
  local name=$1 mode=$2 expected=$3 actual
  rm -f hypatia-findings.json hypatia.sarif "$GITHUB_OUTPUT" "$GITHUB_STEP_SUMMARY"
  if HOME="$tmp/home" FIXTURE_MODE="$mode" bash scan.sh >result.log 2>&1; then
    if bash validate.sh >>result.log 2>&1; then
      if bash gate.sh >>result.log 2>&1; then actual=0; else actual=$?; fi
    else
      actual=$?
    fi
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
adapter_check 'CLI adapter empty fixture produces zero-result SARIF and passes' empty 0
adapter_check 'CLI adapter warn fixture produces warning SARIF and passes' warn 0
adapter_check 'CLI adapter high fixture produces error SARIF and blocks' high 1
adapter_check 'CLI adapter crash fixture fails closed even if [] was written' crash 1
