#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Regression test for scripts/check-ts-allowlist.ts. Each case constructs a
# fresh fixture tree under a tmpdir, runs the script with `--allow-read`,
# and asserts exit code + key output substrings. Mirrors the behaviour the
# previous inline-python step was relied on for, so a future maintenance
# change to the Deno script cannot silently regress estate-wide policy.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DENO_SCRIPT="$SCRIPT_DIR/../check-ts-allowlist.ts"

if [[ ! -f "$DENO_SCRIPT" ]]; then
  echo "FATAL: cannot locate $DENO_SCRIPT" >&2
  exit 2
fi

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
  set +e
  local out
  out="$(cd "$tmp" && deno run --allow-read "$DENO_SCRIPT" 2>&1)"
  local actual_exit=$?
  set -e

  local ok=true
  if [[ "$actual_exit" -ne "$expected_exit" ]]; then ok=false; fi
  if [[ -n "$expected_substr" && "$out" != *"$expected_substr"* ]]; then ok=false; fi

  if $ok; then
    echo "ok   $name"
    PASS=$((PASS+1))
  else
    echo "FAIL $name (exit=$actual_exit, expected=$expected_exit)"
    echo "---- output ----"
    echo "$out"
    echo "----------------"
    FAIL=$((FAIL+1))
  fi
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

echo
echo "=== SUMMARY ==="
echo "Pass: $PASS"
echo "Fail: $FAIL"
[[ $FAIL -eq 0 ]]
