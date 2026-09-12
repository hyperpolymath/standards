#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Regression test for scripts/check-ts-allowlist.sh, the hand-authored
# JavaScript/TypeScript gate that governance-reusable.yml runs on every estate
# repo. Each case constructs a fresh fixture tree under a tmpdir, runs the gate
# against it, and asserts exit code + key output substrings. Mirrors the
# behaviour the previous inline-Python step was relied on for, so a future
# change cannot silently regress estate-wide policy. The gate was a Deno
# artifact until 2026-09-04; it is now pure bash + awk with no JS runtime.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_TARGETS=(
  "$SCRIPT_DIR/../check-ts-allowlist.sh"
)

for target in "${SCRIPT_TARGETS[@]}"; do
  if [[ ! -f "$target" ]]; then
    echo "FATAL: cannot locate $target" >&2
    exit 2
  fi
done

PASS=0
FAIL=0

run_case() {
  local name="$1"; shift
  local expected_exit="$1"; shift
  local expected_substr="$1"; shift
  local setup_fn="$1"; shift

  local tmp
  tmp="$(mktemp -d)"
  (
    cd "$tmp"
    "$setup_fn"
  )

  local target
  for target in "${SCRIPT_TARGETS[@]}"; do
    set +e
    local out
    out="$(cd "$tmp" && bash "$target" 2>&1)"
    local actual_exit=$?
    set -e

    local ok=true
    if [[ "$actual_exit" -ne "$expected_exit" ]]; then ok=false; fi
    if [[ -n "$expected_substr" && "$out" != *"$expected_substr"* ]]; then ok=false; fi

    if $ok; then
      echo "ok   $(basename "$target") :: $name"
      PASS=$((PASS+1))
    else
      echo "FAIL $(basename "$target") :: $name (exit=$actual_exit, expected=$expected_exit)"
      echo "---- output ----"
      echo "$out"
      echo "----------------"
      FAIL=$((FAIL+1))
    fi
  done
  rm -rf "$tmp"
}

setup_mod_ts() { touch mod.ts; }
setup_bindings_dir() { mkdir -p bindings && touch bindings/foo.ts; }
setup_vendor_dir() { mkdir -p vendor && touch vendor/x.ts; }
setup_bare_violation() { mkdir -p src && touch src/Foo.ts; }
setup_exempted_via_claude() {
  mkdir -p src .claude
  touch src/Foo.ts
  cat > .claude/CLAUDE.md <<'EOF'
# CLAUDE.md

### TypeScript Exemptions (Approved)

| Path | Notes |
|---|---|
| `src/Foo.ts` | Documented exemption |
EOF
}
setup_glob_exemption() {
  mkdir -p src/sub .claude
  touch src/sub/bar.ts
  cat > .claude/CLAUDE.md <<'EOF'
# CLAUDE.md

### TypeScript Exemptions (Approved)

| Path | Notes |
|---|---|
| `src/sub/*.ts` | Glob exemption |
EOF
}
setup_hidden_dir() { mkdir -p .secret && touch .secret/foo.ts; }
setup_bench_file() { mkdir -p src && touch src/parser.bench.ts; }
setup_lsp_file() { touch lsp.ts && touch frontend-lsp.ts; }
setup_dts_file() { mkdir -p types && touch types/global.d.ts; }
setup_vscode_dir() { mkdir -p packages/vscode-ext && touch packages/vscode-ext/extension.ts; }
setup_deno_prefix_dir() { mkdir -p deno-lib && touch deno-lib/index.ts; }
setup_table_after_heading_ends() {
  mkdir -p src .claude
  touch src/A.ts
  cat > .claude/CLAUDE.md <<'EOF'
# CLAUDE.md

### TypeScript Exemptions (Approved)

| Path | Notes |
|---|---|
| `src/B.ts` | Documented exemption |

### Some Other Heading

| Path | Notes |
|---|---|
| `src/A.ts` | This row should NOT count — outside exemption table |
EOF
}
write_estate_claude_table() {
  mkdir -p .claude
  cat > .claude/CLAUDE.md <<'EOF'
# CLAUDE.md

### TypeScript Exemptions (Approved)

| Path / Pattern | Rationale | Unblock condition |
|---|---|---|
| `affinescript-ecosystem/affinescript-deno-test/**` | Bootstrap shim. | Later. |
| `aggregate-library/src/test-runner.ts` | Pre-migration test runner. | Later. |
| `rescript-ecosystem/packages/core/runtime-tools/bin/rrt.ts` | Legacy runtime tool. | Later. |
EOF
}
setup_estate_glob_exemption() {
  write_estate_claude_table
  mkdir -p affinescript-ecosystem/affinescript-deno-test
  touch affinescript-ecosystem/affinescript-deno-test/anything.ts
}
setup_estate_aggregate_exact_exemption() {
  write_estate_claude_table
  mkdir -p aggregate-library/src
  touch aggregate-library/src/test-runner.ts
}
setup_estate_rrt_exact_exemption() {
  write_estate_claude_table
  mkdir -p rescript-ecosystem/packages/core/runtime-tools/bin
  touch rescript-ecosystem/packages/core/runtime-tools/bin/rrt.ts
}
setup_estate_other_ts_blocked() {
  write_estate_claude_table
  mkdir -p some/other/path
  touch some/other/path/new-thing.ts
}
setup_estate_rrt_near_miss_blocked() {
  write_estate_claude_table
  mkdir -p rescript-ecosystem/packages/core/runtime-tools/bin
  touch rescript-ecosystem/packages/core/runtime-tools/bin/rrt.ts.bak
}

run_case "mod.ts is builtin-allowed"               0 "No TypeScript files outside allowlist" setup_mod_ts
run_case "bindings/ is builtin-allowed"            0 "No TypeScript files outside allowlist" setup_bindings_dir
run_case "vendor/ is builtin-allowed"              0 "No TypeScript files outside allowlist" setup_vendor_dir
run_case "bare src/Foo.ts fails without exemption" 1 "src/Foo.ts"                            setup_bare_violation
run_case "CLAUDE.md exemption lets src/Foo.ts pass" 0 "1 per-repo exemption"                  setup_exempted_via_claude
run_case "glob exemption matches src/sub/bar.ts"   0 "1 per-repo exemption"                  setup_glob_exemption
run_case "dotted .secret dir is skipped"           0 "No TypeScript files outside allowlist" setup_hidden_dir
run_case "*.bench.ts is builtin-allowed"           0 "No TypeScript files outside allowlist" setup_bench_file
run_case "lsp.ts + *-lsp.ts are builtin-allowed"   0 "No TypeScript files outside allowlist" setup_lsp_file
run_case "*.d.ts is builtin-allowed"               0 "No TypeScript files outside allowlist" setup_dts_file
run_case "directory containing 'vscode' allowed"   0 "No TypeScript files outside allowlist" setup_vscode_dir
run_case "directory starting 'deno-' allowed"      0 "No TypeScript files outside allowlist" setup_deno_prefix_dir
run_case "later heading closes the exemption table" 1 "src/A.ts"                              setup_table_after_heading_ends
run_case "estate glob exemption allows affinescript-deno-test" 0 "3 per-repo exemption"        setup_estate_glob_exemption
run_case "estate exact exemption allows aggregate test runner" 0 "3 per-repo exemption"        setup_estate_aggregate_exact_exemption
run_case "estate exact exemption allows rescript rrt.ts"      0 "3 per-repo exemption"        setup_estate_rrt_exact_exemption
run_case "estate table still blocks unrelated new TS"         1 "some/other/path/new-thing.ts" setup_estate_other_ts_blocked
run_case "estate exact exemption blocks rrt.ts.bak near miss" 1 "rrt.ts.bak"                  setup_estate_rrt_near_miss_blocked

echo
echo "=== SUMMARY ==="
echo "Pass: $PASS"
echo "Fail: $FAIL"
[[ $FAIL -eq 0 ]]
