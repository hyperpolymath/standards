#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Proves check-descriptile-policy.sh CAN FAIL, and fails for the right reasons.
#
# The checker scans `scripts/*.sh`, and as a git pathspec that glob also matches
# scripts/tests/ — so it scans THIS file. The retired descriptile paths below are
# therefore ASSEMBLED FROM FRAGMENTS and never appear literally, or the real gate
# would turn red on its own test.
set -euo pipefail
SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/check-descriptile-policy.sh"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT

MR=".machine_readable"
STATE="$MR/STATE.a2ml"            # retired flat path
META6="$MR/6a2/META.a2ml"         # retired 6a2/ path
ANCHOR="$MR/ANCHOR.a2ml"          # retired flat path
LIVE="$MR/descriptiles/STATE.a2ml"  # the live location

pass=0; fail=0
expect() { # expect <wanted-exit> <label>   (runs the checker in the repo $WORK/t)
  local want="$1" label="$2" got=0
  git -C "$WORK/t" add -A >/dev/null
  (cd "$WORK/t" && bash "$SCRIPT") >"$WORK/out" 2>&1 || got=$?
  if [ "$got" = "$want" ]; then pass=$((pass+1)); echo "  ok    $label"
  else fail=$((fail+1)); echo "  FAIL  $label (wanted exit $want, got $got)"; sed 's/^/        /' "$WORK/out"; fi
}
fresh() {
  rm -rf "$WORK/t"; mkdir -p "$WORK/t/.github/workflows" "$WORK/t/scripts"
  git -C "$WORK/t" init -q
}
wf() { printf '%s\n' "on: push" "jobs:" "  a:" "    runs-on: ubuntu-latest" "    steps:" "      - run: $1" > "$WORK/t/.github/workflows/ci.yml"; }

fresh; wf "echo hello"
expect 0 "a workflow that tests no descriptile path is clean"

fresh; wf "[ -f $STATE ]"
expect 1 "a workflow requiring the retired flat path with -f is rejected"
grep -q '::error file=.github/workflows/ci.yml::' "$WORK/out" \
  && { pass=$((pass+1)); echo "  ok    the rejection is a ::error annotation naming the file"; } \
  || { fail=$((fail+1)); echo "  FAIL  the rejection does not annotate the file"; }

fresh; wf "test -e $META6"
expect 1 "a workflow requiring the retired 6a2/ path with -e is rejected"

fresh; printf '%s\n' '#!/usr/bin/env bash' "check_file \"$ANCHOR\"" > "$WORK/t/scripts/gate.sh"
expect 1 "a script calling check_file on a quoted retired path is rejected"

fresh; printf '%s\n' '#!/usr/bin/env bash' "echo \"\$( [ -f $STATE ] && echo yes )\"" > "$WORK/t/scripts/gate.sh"
expect 1 "a file test hidden inside a double-quoted command substitution is rejected"

fresh; printf '%s\n' "check:" "	test -f $STATE" > "$WORK/t/Justfile"
expect 1 "a Justfile recipe requiring a retired path is rejected"

fresh; wf "[ -f $LIVE ]"
expect 0 "a file test on the live descriptiles/ location is accepted"

fresh; printf '%s\n' '#!/usr/bin/env bash' "# old: [ -f $STATE ]" > "$WORK/t/scripts/gate.sh"
expect 0 "a commented-out example is not enforcement"

fresh; printf '%s\n' '#!/usr/bin/env bash' "echo \"we used to run -f on $STATE\"" > "$WORK/t/scripts/gate.sh"
expect 0 "quoted prose that mentions the path is not enforcement"

fresh; printf '%s\n' '#!/usr/bin/env bash' "[ -f $STATE ]" > "$WORK/t/scripts/gate.sh"
git -C "$WORK/t" add -A >/dev/null; git -C "$WORK/t" rm -q --cached scripts/gate.sh
(cd "$WORK/t" && bash "$SCRIPT") >"$WORK/out" 2>&1 && got=0 || got=$?
if [ "$got" = 0 ]; then pass=$((pass+1)); echo "  ok    an untracked file is outside the gate's scope"
else fail=$((fail+1)); echo "  FAIL  an untracked file was scanned (got $got)"; fi

echo "PASS=$pass FAIL=$fail"
[ "$fail" -eq 0 ]
