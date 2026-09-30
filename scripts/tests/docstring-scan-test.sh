#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# docstring-scan-test.sh — fixture suite for .githooks/docstring-scan.sh.
#
# The fixtures, not the scanner code, are the estate's single source of truth for the docstring
# predicate: the Stop hook, the pre-commit validator and Hypatia's re-implementation must all pass
# them. SCANNER=<path> injects an alternative implementation (or a mutant).

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
SCANNER="${SCANNER:-$ROOT/.githooks/docstring-scan.sh}"
[ -f "$SCANNER" ] || { echo "FATAL: scanner not found at $SCANNER" >&2; exit 2; }

PASS=0; FAIL=0
# Record a passing assertion and print its description.
ok()   { PASS=$((PASS+1)); printf '  ok   %s\n' "$1"; }
# Record a failing assertion and print its expected and actual values.
bad()  { FAIL=$((FAIL+1)); printf '  FAIL %s\n     expected: %s\n     actual:   %s\n' "$1" "$2" "$3"; }
# Compare expected and actual values, then record the assertion result.
check(){ if [ "$2" = "$3" ]; then ok "$1"; else bad "$1" "$2" "$3"; fi; }
# Extract one key's value from the scanner's SUMMARY line.
field(){ printf '%s\n' "$1" | sed -nE "s/^SUMMARY .*\b$2=([^ ]+).*/\1/p"; }
# Print the status column of one symbol's row.
row()  { printf '%s\n' "$1" | awk -F'\t' -v s="$2" '$3==s{print $4 "/" $5}'; }

WORK="$(mktemp -d -t docscan-test.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT

# Create a fresh repository with one committed baseline file and cd into it.
newrepo() {
  rm -rf "$WORK/r"; mkdir -p "$WORK/r"; cd "$WORK/r" || exit 2
  git init -q . && git config user.email t@example.invalid && git config user.name t
  git config commit.gpgsign false && git config core.hooksPath /dev/null
  cat > base.sh <<'EOF'
#!/usr/bin/env bash
# Print a greeting.
greet() {
  echo hello
}

legacy() {
  echo old
}
EOF
  git add base.sh && git commit -qm base
}

# Run the scanner in the current repository.
scan() { bash "$SCANNER" "$@" 2>&1; }

echo "== calibration: standards PR #1034 at 1cc72cdc80c9 (CodeRabbit: 13 functions / 2 files / 0.00% / 3 skipped)"
if git -C "$ROOT" cat-file -e 1cc72cdc80c9 2>/dev/null; then
  out="$(cd "$ROOT" && scan --range 1cc72cdc80c9^..1cc72cdc80c9)"
  check "calibration files"      2      "$(field "$out" files)"
  check "calibration functions"  13     "$(field "$out" functions)"
  check "calibration documented" 0      "$(field "$out" documented)"
  check "calibration skipped"    3      "$(field "$out" skipped)"
  check "calibration coverage"   0.00%  "$(field "$out" coverage)"
else
  # A skip is not a pass: a shallow clone must not report the known-answer control as green.
  bad "calibration commit present (fetch full history)" "1cc72cdc80c9 reachable" "absent"
fi

echo "== planted positive: a new undocumented function blocks under --check"
newrepo
cat > new.sh <<'EOF'
#!/usr/bin/env bash
# Documented helper.
good() { :; }
undoc() { :; }
EOF
out="$(scan --worktree)"; rc=0; scan --worktree --check >/dev/null || rc=$?
check "untracked new file is scanned"   2 "$(field "$out" functions)"
check "good is documented"              added/documented   "$(row "$out" good)"
check "undoc is undocumented"           added/undocumented "$(row "$out" undoc)"
check "--check exits 1"                 1 "$rc"

echo "== negative control: touching only a documented function is silent"
newrepo
sed -i 's/echo hello/echo hello world/' base.sh
out="$(scan --worktree)"; rc=0; scan --worktree --check >/dev/null || rc=$?
check "one touched function"            1 "$(field "$out" functions)"
check "undocumented is zero"            0 "$(field "$out" undocumented)"
check "--check exits 0"                 0 "$rc"

echo "== added vs modified: editing an undocumented legacy body reports but does not block"
newrepo
sed -i 's/echo old/echo older/' base.sh
out="$(scan --worktree)"; rc=0; scan --worktree --check >/dev/null || rc=$?
check "legacy is modified/undocumented" modified/undocumented "$(row "$out" legacy)"
check "untouched greet is not reported" "" "$(row "$out" greet)"
check "--check exits 0 on modified-only" 0 "$rc"
printf 'fresh() {\n  :\n}\n' >> base.sh
rc=0; scan --worktree --check >/dev/null || rc=$?
check "adding an undocumented function beside it blocks" 1 "$rc"

echo "== predicate edges"
newrepo
cat > edge.sh <<'EOF'
#!/usr/bin/env bash
trailing() { :; } # a same-line comment is not a docstring

# shellcheck disable=SC2034
directive() { :; }

# Real documentation.
# shellcheck disable=SC2034
docthendirective() { :; }

function kw_style {
  :
}

cat <<'INNER'
first body line
inheredoc() { :; }
INNER
EOF
out="$(scan --worktree)"
check "trailing comment is not documentation"      added/undocumented "$(row "$out" trailing)"
check "shellcheck directive is not documentation"  added/undocumented "$(row "$out" directive)"
check "doc above a directive still documents"      added/documented   "$(row "$out" docthendirective)"
check "function keyword form is detected"          added/undocumented "$(row "$out" kw_style)"
check "a heredoc body is not code"                 ""                 "$(row "$out" inheredoc)"
check "edge function count"                        4 "$(field "$out" functions)"

echo "== skipped is skipped, never documented"
newrepo
printf 'fn main() {}\n' > main.rs
printf '= Notes\n' > notes.adoc
out="$(scan --worktree)"
check "unsupported source is skipped"  1   "$(field "$out" skipped)"
check "it contributes no functions"    0   "$(field "$out" functions)"
check "coverage is n/a, not 100%"      n/a% "$(field "$out" coverage)"

echo "== mode parity: --staged and --worktree ask the same question"
newrepo
cat > par.sh <<'EOF'
# Documented.
a() { :; }
b() { :; }
EOF
sed -i 's/echo old/echo older/' base.sh
w="$(scan --worktree | sort)"
git add -A
s="$(scan --staged | sort)"
check "staged verdict equals worktree verdict" "$w" "$s"

echo "== awkward paths: spaces and non-ASCII"
newrepo
mkdir -p "dir with space" "naïve"
printf 'x() { :; }\n' > "dir with space/a b.sh"
printf '# Doc.\ny() { :; }\n' > "naïve/ü.sh"
out="$(scan --worktree)"
check "path with spaces is scanned"   "added/undocumented" "$(row "$out" x)"
check "non-ASCII path is scanned"     "added/documented"   "$(row "$out" y)"

echo "== errors are loud"
rc=0; (cd "$WORK" && bash "$SCANNER" --worktree >/dev/null 2>&1) || rc=$?
check "outside a git repository exits 2" 2 "$rc"
rc=0; scan >/dev/null || rc=$?
check "no mode exits 2" 2 "$rc"

echo
printf 'docstring-scan-test: %d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
