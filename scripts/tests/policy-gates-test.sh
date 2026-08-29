#!/usr/bin/env bash
set -euo pipefail

repo_root=$(git rev-parse --show-toplevel)
language_gate="$repo_root/tools/policy/check-language-policy.sh"
workflow_gate="$repo_root/tools/policy/check-workflows-parse.sh"
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT

init_fixture() {
  local target=$1
  mkdir -p "$target"
  git -C "$target" init -q
  git -C "$target" config user.email tests@example.invalid
  git -C "$target" config user.name 'Policy gate tests'
}

write_policy() {
  local target=$1 extra=${2-}
  mkdir -p "$target/.claude"
  {
    printf '%s\n' '### ALLOWED' '| **Bun** | JS runtime |'
    printf '%s\n' '### BANNED' '| **Deno** | **Bun** |'
    printf '%s\n' "$extra"
  } > "$target/.claude/CLAUDE.md"
  git -C "$target" add .claude/CLAUDE.md
}

expect_pass() { "$@" >/dev/null; }
expect_fail() { if "$@" >/dev/null 2>&1; then echo "expected failure: $*" >&2; exit 1; fi; }

quoted="$fixture/quoted"
init_fixture "$quoted"
write_policy "$quoted" 'History: "Supports TypeScript" and “JS/TS runtime”.'
(cd "$quoted" && expect_pass "$language_gate")

mixed="$fixture/mixed"
init_fixture "$mixed"
write_policy "$mixed" 'History: "Supports TypeScript"; live policy Supports TypeScript.'
(cd "$mixed" && expect_fail "$language_gate")

blockquote="$fixture/blockquote"
init_fixture "$blockquote"
write_policy "$blockquote" '> Historical policy Supports TypeScript.'
(cd "$blockquote" && expect_pass "$language_gate")

blank="$fixture/blank"
init_fixture "$blank"
write_policy "$blank" '|  | replacement |'
(cd "$blank" && expect_fail "$language_gate")

no_workflows="$fixture/no-workflows"
init_fixture "$no_workflows"
mkdir -p "$no_workflows/bin"
ln -s "$(command -v git)" "$no_workflows/bin/git"
(cd "$no_workflows" && PATH="$no_workflows/bin" expect_pass /bin/bash "$workflow_gate")

without_parser="$fixture/without-parser"
init_fixture "$without_parser"
mkdir -p "$without_parser/.github/workflows" "$without_parser/bin"
printf '%s\n' 'name: test' 'on: push' 'jobs: {}' > "$without_parser/.github/workflows/test.yml"
git -C "$without_parser" add .github/workflows/test.yml
ln -s "$(command -v git)" "$without_parser/bin/git"
(cd "$without_parser" && PATH="$without_parser/bin" expect_fail /bin/bash "$workflow_gate")

valid="$fixture/valid"
init_fixture "$valid"
mkdir -p "$valid/.github/workflows"
printf '%s\n' 'name: test' 'on: push' 'jobs: {}' > "$valid/.github/workflows/test.yml"
git -C "$valid" add .github/workflows/test.yml
(cd "$valid" && expect_pass "$workflow_gate")

invalid="$fixture/invalid"
init_fixture "$invalid"
mkdir -p "$invalid/.github/workflows"
printf '%s\n' 'name: test' 'jobs: [' > "$invalid/.github/workflows/test.yml"
git -C "$invalid" add .github/workflows/test.yml
(cd "$invalid" && expect_fail "$workflow_gate")

echo 'policy gate controls passed'
