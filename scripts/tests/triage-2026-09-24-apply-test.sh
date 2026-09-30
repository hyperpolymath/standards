#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Exercise the triage applier with an offline gh stub; never contact GitHub.
# Keep shell startup hooks from replacing the offline command stubs.
unset BASH_ENV ENV
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/bin" "$TMP/temp with spaces"
export TRIAGE_TEST_LOG="$TMP/calls.jsonl"
export TMPDIR="$TMP/temp with spaces"
cat > "$TMP/bin/gh" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
case "$1 $2" in
  'auth status') exit "${TRIAGE_TEST_AUTH_RC:-0}" ;;
  'label list') printf '%s\n' scope:estate scope:repo priority:p1 ;;
  'issue view')
    if [[ " $* " == *' --json title '* ]]; then
      echo 'Document the standard'
    else
      case "$3" in 89|913|956) echo OPEN ;; *) echo CLOSED ;; esac
    fi ;;
  'api repos/hyperpolymath/standards/issues/1/comments') echo 0 ;;
  'api -X'|'issue comment'|'issue close')
    jq -cn --args '$ARGS.positional' -- "$@" >> "$TRIAGE_TEST_LOG"
    # Read payloads during the call, before the applier's cleanup.
    args=("$@")
    for ((i=0; i<${#args[@]}; i++)); do
      case "${args[i]}" in
        --input) jq -e '.labels | length > 0' "${args[i+1]}" >/dev/null ;;
        --body-file) test -s "${args[i+1]}" ;;
      esac
    done ;;
  *) echo "Unexpected gh invocation: $*" >&2; exit 2 ;;
esac
STUB
cat > "$TMP/bin/sleep" <<'STUB'
#!/usr/bin/env bash
exit 0
STUB
chmod +x "$TMP/bin/gh" "$TMP/bin/sleep"
export PATH="$TMP/bin:$PATH"
cd "$ROOT"

: > "$TRIAGE_TEST_LOG"
DRY_RUN=1 bash scripts/triage-2026-09-24-apply.sh > "$TMP/dry.log"
test ! -s "$TRIAGE_TEST_LOG"
test -z "$(find "$TMPDIR" -mindepth 1 -print -quit)"
grep -qF '[dry-run] gh api -X POST' "$TMP/dry.log"
grep -qF '[dry-run] gh issue comment 913' "$TMP/dry.log"
grep -qF '[dry-run] gh api -X PATCH' "$TMP/dry.log"
echo 'PASS: dry run previews commands, makes no write calls and cleans temporary files' 

for run in 1 2; do
  : > "$TRIAGE_TEST_LOG"
  DRY_RUN=0 bash scripts/triage-2026-09-24-apply.sh > "$TMP/run.log"
  jq -se '
    ([.[] | select(.[0:3] == ["api", "-X", "POST"])] | length) == 2 and
    ([.[] | select(.[0:2] == ["issue", "close"])] | length) == 1 and
    ([.[] | select(.[0:2] == ["issue", "comment"])] | length) == 1 and
    ([.[] | select(.[0:3] == ["api", "-X", "PATCH"])] | length) == 1 and
    any(.[]; .[0:3] == ["issue", "close", "956"] and
      any(.[]; startswith("Closed as fixed by #1034") and contains("\n\n"))) and
    any(.[]; .[0:3] == ["api", "-X", "PATCH"] and
      any(.[]; startswith("description=Canonical standards, specifications")) and
      index("topics[]=policy-as-code") != null)
  ' "$TRIAGE_TEST_LOG" >/dev/null
  jq -sr '[.[] | select(.[0:3] == ["api", "-X", "POST"])][0][-1]' \
    "$TRIAGE_TEST_LOG" > "$TMP/path-$run"
  test -z "$(find "$TMPDIR" -mindepth 1 -print -quit)"
done
! cmp -s "$TMP/path-1" "$TMP/path-2"
echo 'PASS: argument boundaries and payloads survive spaces; runs use distinct, cleaned directories'

: > "$TRIAGE_TEST_LOG"
if TRIAGE_TEST_AUTH_RC=1 DRY_RUN=0 bash scripts/triage-2026-09-24-apply.sh > "$TMP/fail.log"; then
  echo 'FAIL: authentication failure was ignored' >&2; exit 1
fi
test ! -s "$TRIAGE_TEST_LOG"
test -z "$(find "$TMPDIR" -mindepth 1 -print -quit)"
echo 'PASS: preflight failure makes no writes and cleans its temporary directory'
