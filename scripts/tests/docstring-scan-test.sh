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
# Run the scanner with leg B disarmed (threshold 0), so a leg-A assertion holds on either side of
# the leg-B cutoff instead of flipping red on ENFORCE_DOCSTRINGS_FROM.
scan_a() { DOCSTRING_THRESHOLD=0 scan "$@"; }

# Run the scanner with --check under a pinned "today" and print its exit code.
rc_on() { local today="$1" r=0; shift; DOCS_TODAY="$today" bash "$SCANNER" "$@" --check >/dev/null 2>&1 || r=$?; echo "$r"; }

# Write n documented and m undocumented shell functions to the named file.
mkfns() {
  local file="$1" n="$2" m="$3" i
  : > "$file"
  for ((i = 0; i < n; i++)); do printf '# Documented %d.\nd%d() { :; }\n' "$i" "$i" >> "$file"; done
  for ((i = 0; i < m; i++)); do printf 'u%d() { :; }\n' "$i" >> "$file"; done
}

# Known-answer control: scan a range of another public estate repo and compare with the
# CodeRabbit Docstring Coverage verdict measured at the head CodeRabbit actually reviewed.
# Both commits are fetched by full SHA into a throwaway repo (so --depth=1 harms nothing);
# an unfetchable commit is a FAILURE, never a skip.
remote_control() {
  local label="$1" repo="$2" base="$3" head="$4" want_fn="$5" want_doc="$6" d="$WORK/ctl-$1" o
  rm -rf "$d"; git init -q "$d" && git -C "$d" config core.hooksPath /dev/null
  git -C "$d" fetch --quiet --no-tags --depth=1 "https://github.com/$repo" "$base" "$head" 2>/dev/null || true
  if git -C "$d" cat-file -e "$base^{commit}" 2>/dev/null && git -C "$d" cat-file -e "$head^{commit}" 2>/dev/null; then
    o="$(cd "$d" && scan --range "$base..$head")"
    check "$label functions"  "$want_fn"  "$(field "$o" functions)"
    check "$label documented" "$want_doc" "$(field "$o" documented)"
    check "$label leg B fails" fail       "$(field "$o" legb)"
  else
    # A skip is not a pass: an unfetchable calibration commit must not report the known-answer control as green.
    bad "$label commits present (fetch by SHA from $repo failed)" "${base:0:12}..${head:0:12} reachable" "absent"
  fi
}

echo "== calibration: standards PR #1034 at 1cc72cdc80c9 (CodeRabbit: 13 functions / 2 files / 0.00% / 3 skipped)"
# The calibration commit is PR #1034's PRE-SQUASH head: it lives only under refs/pull/1034/head,
# so no clone of main contains it, however deep. Fetch it by full SHA on a miss. No --depth: in a
# complete clone that would write .git/shallow and truncate main's history for every later test.
CALIBRATION=1cc72cdc80c9c60b2a857df7d8753a7dfb7e87fb
git -C "$ROOT" cat-file -e "$CALIBRATION^{commit}" 2>/dev/null ||
  git -C "$ROOT" fetch --quiet --no-tags origin "$CALIBRATION" 2>/dev/null || true
if git -C "$ROOT" cat-file -e "$CALIBRATION^{commit}" 2>/dev/null; then
  out="$(cd "$ROOT" && scan --range 1cc72cdc80c9^..1cc72cdc80c9)"
  check "calibration files"      2      "$(field "$out" files)"
  check "calibration functions"  13     "$(field "$out" functions)"
  check "calibration documented" 0      "$(field "$out" documented)"
  check "calibration skipped"    3      "$(field "$out" skipped)"
  check "calibration coverage"   0.00%  "$(field "$out" coverage)"
else
  # A skip is not a pass: an unfetchable calibration commit must not report the known-answer control as green.
  bad "calibration commit present (fetch by SHA from refs/pull/1034/head failed)" "1cc72cdc80c9 reachable" "absent"
fi

echo "== calibration: leg-B ratio against CodeRabbit (2 of 22 touched functions documented, both)"
remote_control "panll#136" hyperpolymath/panll \
  964f9563070129b2860b9e9e627a28b6dca05b02 61141fb353225ceacb42fd502f21bfd0b0074893 22 2
remote_control "nextgen-databases#107" hyperpolymath/nextgen-databases \
  fc12a2eb856ff2bed2b91f6df38dbac5d6663257 e12e4c71855c1314f528b7fdf833786c6c06630a 22 2

echo "== planted positive: a new undocumented function blocks under --check"
newrepo
cat > new.sh <<'EOF'
#!/usr/bin/env bash
# Documented helper.
good() { :; }
undoc() { :; }
EOF
out="$(scan --worktree)"; rc=0; scan_a --worktree --check >/dev/null || rc=$?
check "untracked new file is scanned"   2 "$(field "$out" functions)"
check "good is documented"              added/documented   "$(row "$out" good)"
check "undoc is undocumented"           added/undocumented "$(row "$out" undoc)"
check "--check exits 1"                 1 "$rc"

echo "== negative control: touching only a documented function is silent"
newrepo
sed -i 's/echo hello/echo hello world/' base.sh
out="$(scan --worktree)"; rc=0; scan_a --worktree --check >/dev/null || rc=$?
check "one touched function"            1 "$(field "$out" functions)"
check "undocumented is zero"            0 "$(field "$out" undocumented)"
check "--check exits 0"                 0 "$rc"

echo "== added vs modified: editing an undocumented legacy body reports but does not block"
newrepo
sed -i 's/echo old/echo older/' base.sh
out="$(scan --worktree)"; rc=0; scan_a --worktree --check >/dev/null || rc=$?
check "legacy is modified/undocumented" modified/undocumented "$(row "$out" legacy)"
check "untouched greet is not reported" "" "$(row "$out" greet)"
check "--check exits 0 on modified-only" 0 "$rc"
printf 'fresh() {\n  :\n}\n' >> base.sh
rc=0; scan_a --worktree --check >/dev/null || rc=$?
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

# Create a repo whose change MODIFIES n documented and m undocumented functions (none added),
# so leg A stays silent and only leg B can decide the exit code.
legb_repo() {
  newrepo
  mkfns t.sh "$1" "$2"
  git add t.sh && git commit -qm fns
  sed -i 's/{ :; }/{ : touched; }/' t.sh
}

echo "== leg B: touched-function ratio, phased in by a self-flipping date"
legb_repo 1 1
out="$(scan --worktree)"
check "1 of 2 touched documented is a leg-B fail"       fail "$(field "$out" legb)"
check "no added function, so leg A is silent"           0    "$(field "$out" added_undocumented)"
check "SUMMARY carries the threshold"                   80   "$(field "$out" threshold)"
check "before the cutoff a violation does not block"    0 "$(rc_on 2026-10-31 --worktree)"
warn="$(DOCS_TODAY=2026-10-31 bash "$SCANNER" --worktree --check 2>&1 >/dev/null)"
case "$warn" in *"WARN leg B"*"2026-11-01"*) ok "before the cutoff it WARNs on stderr naming the cutoff" ;;
  *) bad "before the cutoff it WARNs on stderr naming the cutoff" "WARN leg B ... 2026-11-01" "$warn" ;; esac
check "ON the shipped cutoff date it blocks"            1 "$(rc_on 2026-11-01 --worktree)"
check "after the cutoff it blocks"                      1 "$(rc_on 2027-03-01 --worktree)"
check "ENFORCE_DOCSTRINGS_FROM moves the cutoff"        1 "$(ENFORCE_DOCSTRINGS_FROM=2026-01-01 rc_on 2026-06-01 --worktree)"
rc=0; DOCS_TODAY=2027-03-01 bash "$SCANNER" --worktree >/dev/null 2>&1 || rc=$?
check "without --check a leg-B fail never changes rc"   0 "$rc"

legb_repo 4 1
out="$(scan --worktree)"
check "exactly 80% (4 of 5) passes"                     pass "$(field "$out" legb)"
check "exactly 80% does not block after the cutoff"     0 "$(rc_on 2027-03-01 --worktree)"

legb_repo 19 5
out="$(scan --worktree)"
check "79.17% (19 of 24) coverage"                      79.17% "$(field "$out" coverage)"
check "79.17% fails leg B"                              fail "$(field "$out" legb)"
check "79.17% blocks after the cutoff"                  1 "$(rc_on 2027-03-01 --worktree)"
check "DOCSTRING_THRESHOLD=75 lets 79.17% pass"         0 "$(DOCSTRING_THRESHOLD=75 rc_on 2027-03-01 --worktree)"

echo "== leg B: zero touched functions is no verdict, not a pass or a fail"
newrepo
printf 'fn main() {}\n' > main.rs
out="$(scan --worktree)"
check "zero touched gives legb=n/a"                     n/a "$(field "$out" legb)"
check "zero touched does not block after the cutoff"    0 "$(rc_on 2027-03-01 --worktree)"
nv="$(DOCS_TODAY=2027-03-01 bash "$SCANNER" --worktree --check 2>&1 >/dev/null)"
case "$nv" in *"leg B: no verdict: 0 touched functions"*) ok "zero touched says 'no verdict' explicitly" ;;
  *) bad "zero touched says 'no verdict' explicitly" "leg B: no verdict: 0 touched functions" "$nv" ;; esac

echo "== leg B: a skipped file never counts as documented"
# 3 documented + 1 undocumented = 75% (fail). Counting the skipped file as documented would give
# 4 of 5 = 80% (pass), so this fixture flips if skipped ever leaks into the ratio.
legb_repo 3 1
printf 'fn main() {}\n' > main.rs
out="$(scan --worktree)"
check "the skipped file is reported"                    1 "$(field "$out" skipped)"
check "it adds nothing to the denominator"              4 "$(field "$out" functions)"
check "3 of 4 fails leg B despite the skipped file"     fail "$(field "$out" legb)"
check "and blocks after the cutoff"                     1 "$(rc_on 2027-03-01 --worktree)"
# The other direction: 4 of 5 = 80% (pass). Counting the skipped file as UNdocumented would give
# 4 of 6 = 66.67% (fail).
legb_repo 4 1
printf 'fn main() {}\n' > main.rs
out="$(scan --worktree)"
check "4 of 5 passes leg B beside a skipped file"       pass "$(field "$out" legb)"
check "and does not block after the cutoff"             0 "$(rc_on 2027-03-01 --worktree)"

echo "== leg B: malformed configuration is an error, never a silent disarm"
for bad_date in 2026-11 11/01/2026 tomorrow ' 2026-11-01'; do
  rc=0; ENFORCE_DOCSTRINGS_FROM="$bad_date" bash "$SCANNER" --worktree --check >/dev/null 2>&1 || rc=$?
  check "ENFORCE_DOCSTRINGS_FROM='$bad_date' exits 2" 2 "$rc"
done
rc=0; DOCS_TODAY=20261101 bash "$SCANNER" --worktree --check >/dev/null 2>&1 || rc=$?
check "DOCS_TODAY='20261101' exits 2"                   2 "$rc"
for bad_t in abc 101 080 -1 80.5 ' 80'; do
  rc=0; DOCSTRING_THRESHOLD="$bad_t" bash "$SCANNER" --worktree --check >/dev/null 2>&1 || rc=$?
  check "DOCSTRING_THRESHOLD='$bad_t' exits 2"         2 "$rc"
done
rc=0; DOCSTRING_THRESHOLD=abc bash "$SCANNER" --worktree >/dev/null 2>&1 || rc=$?
check "a malformed threshold is an error without --check too" 2 "$rc"

echo "== errors are loud"
rc=0; (cd "$WORK" && bash "$SCANNER" --worktree >/dev/null 2>&1) || rc=$?
check "outside a git repository exits 2" 2 "$rc"
rc=0; scan >/dev/null || rc=$?
check "no mode exits 2" 2 "$rc"

echo
printf 'docstring-scan-test: %d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
