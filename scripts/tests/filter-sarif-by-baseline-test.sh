#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Tests for filter-sarif-by-baseline.sh.
#
# ⚠ THE FAIL-OPEN TESTS ARE THE IMPORTANT ONES. This script decides which
# security alerts reach the security tab. Every failure mode must upload the
# SARIF UNFILTERED — suppressing more than the baseline says, or silently
# emitting an empty SARIF, would hide real findings. "Filtered nothing" is a
# safe failure; "filtered everything" is not.
set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
S="$HERE/../filter-sarif-by-baseline.sh"
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
pass=0; fail=0

ck() { # name expected_count actual_count
  if [ "$2" = "$3" ]; then printf '  ok    %s\n' "$1"; pass=$((pass+1))
  else printf '  FAIL  %s (expected %s results, got %s)\n' "$1" "$2" "$3"; fail=$((fail+1)); fi
}

mk_sarif() {
  cat > "$T/in.sarif" <<'EOF'
{"version":"2.1.0","runs":[{"tool":{"driver":{"name":"Hypatia"}},"results":[
 {"ruleId":"hypatia/code_safety/unwrap_without_check","locations":[{"physicalLocation":{"artifactLocation":{"uri":"src/a.rs"}}}]},
 {"ruleId":"hypatia/code_safety/unwrap_without_check","locations":[{"physicalLocation":{"artifactLocation":{"uri":"src/b.rs"}}}]},
 {"ruleId":"hypatia/cicd_rules/banned_language_file","locations":[{"physicalLocation":{"artifactLocation":{"uri":"tools/x.py"}}}]}
]}]}
EOF
}
mk_findings() {
  cat > "$T/f.json" <<'EOF'
[{"severity":"low","rule_module":"code_safety","type":"unwrap_without_check","file":"src/a.rs"},
 {"severity":"low","rule_module":"code_safety","type":"unwrap_without_check","file":"src/b.rs"},
 {"severity":"critical","rule_module":"cicd_rules","type":"banned_language_file","file":"tools/x.py"}]
EOF
}
count() { jq '[.runs[]?.results[]?]|length' "$1"; }

# ── 1. a baselined finding is removed ───────────────────────────────────
mk_sarif; mk_findings
echo '[{"severity":"low","rule_module":"code_safety","type":"unwrap_without_check","file":"src/a.rs"}]' > "$T/b.json"
bash "$S" "$T/in.sarif" "$T/f.json" "$T/b.json" "$T/out.sarif" >/dev/null 2>&1
ck "a baselined finding is removed from the SARIF" 2 "$(count "$T/out.sarif")"

# ── 2. an UNbaselined finding survives ──────────────────────────────────
ck "unbaselined findings survive" 1 \
   "$(jq '[.runs[].results[]|select(.ruleId=="hypatia/cicd_rules/banned_language_file")]|length' "$T/out.sarif")"

# ── 3. file_pattern matching works ──────────────────────────────────────
mk_sarif; mk_findings
echo '[{"severity":"low","rule_module":"code_safety","type":"unwrap_without_check","file_pattern":"src/*.rs"}]' > "$T/b.json"
bash "$S" "$T/in.sarif" "$T/f.json" "$T/b.json" "$T/out.sarif" >/dev/null 2>&1
ck "file_pattern removes both matching results" 1 "$(count "$T/out.sarif")"

# ── 4. severity is part of the key (baseline says low, finding is critical) ──
mk_sarif; mk_findings
echo '[{"severity":"low","rule_module":"cicd_rules","type":"banned_language_file","file":"tools/x.py"}]' > "$T/b.json"
bash "$S" "$T/in.sarif" "$T/f.json" "$T/b.json" "$T/out.sarif" >/dev/null 2>&1
ck "a severity mismatch does NOT suppress" 3 "$(count "$T/out.sarif")"

# ── 5-7. every failure mode must FAIL OPEN ──────────────────────────────
mk_sarif; mk_findings
bash "$S" "$T/in.sarif" "$T/f.json" "$T/does-not-exist.json" "$T/out.sarif" >/dev/null 2>&1
ck "missing baseline leaves the SARIF untouched" 3 "$(count "$T/out.sarif")"

mk_sarif; mk_findings
echo 'this is not json' > "$T/bad.json"
bash "$S" "$T/in.sarif" "$T/f.json" "$T/bad.json" "$T/out.sarif" >/dev/null 2>&1
ck "an INVALID baseline leaves the SARIF untouched" 3 "$(count "$T/out.sarif")"

mk_sarif
echo '[]' > "$T/empty.json"
echo '[{"severity":"low","rule_module":"code_safety","type":"unwrap_without_check","file":"src/a.rs"}]' > "$T/b.json"
bash "$S" "$T/in.sarif" "$T/empty.json" "$T/b.json" "$T/out.sarif" >/dev/null 2>&1
ck "no findings to match leaves the SARIF untouched" 3 "$(count "$T/out.sarif")"

# ── 8. paths differing only by a ./ prefix still match ──────────────────
mk_sarif
cat > "$T/f.json" <<'EOF'
[{"severity":"low","rule_module":"code_safety","type":"unwrap_without_check","file":"./src/a.rs"}]
EOF
echo '[{"severity":"low","rule_module":"code_safety","type":"unwrap_without_check","file":"./src/a.rs"}]' > "$T/b.json"
bash "$S" "$T/in.sarif" "$T/f.json" "$T/b.json" "$T/out.sarif" >/dev/null 2>&1
ck "a leading ./ does not defeat path matching" 2 "$(count "$T/out.sarif")"

# ── 9. a SARIF with no results is handled ───────────────────────────────
echo '{"version":"2.1.0","runs":[]}' > "$T/in.sarif"
mk_findings
echo '[{"severity":"low","rule_module":"code_safety","type":"unwrap_without_check","file":"src/a.rs"}]' > "$T/b.json"
bash "$S" "$T/in.sarif" "$T/f.json" "$T/b.json" "$T/out.sarif" >/dev/null 2>&1
ck "an empty SARIF survives filtering" 0 "$(count "$T/out.sarif")"

printf '\n  %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
