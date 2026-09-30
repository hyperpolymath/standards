#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Proves check-mustfile-structure.sh CAN FAIL, and fails for the right reasons.
#
# Its whole job is to reject a HOLLOW CHECK — a '### <id>' block that looks like
# enforcement but carries neither a `- run:` nor a `- verification:`, or that
# has no severity. A Mustfile with no checks at all is also a failure, never a
# vacuous pass.
set -euo pipefail
SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/check-mustfile-structure.sh"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
cd "$WORK"

pass=0; fail=0
expect() { # expect <wanted-exit> <label>   (Mustfile content on stdin)
  local want="$1" label="$2" got=0
  cat > Mustfile.a2ml
  bash "$SCRIPT" Mustfile.a2ml >out 2>&1 || got=$?
  if [ "$got" = "$want" ]; then pass=$((pass+1)); echo "  ok    $label"
  else fail=$((fail+1)); echo "  FAIL  $label (wanted exit $want, got $got)"; sed 's/^/        /' out; fi
}

expect 0 "a check with severity + run is valid" <<'EOF'
### alpha
- severity: critical
- run: test -f README.adoc
EOF

expect 0 "a check with severity + verification is valid" <<'EOF'
### alpha
- severity: high
- verification: governance — reviewed by the owner each release
EOF

expect 0 "several valid checks are valid" <<'EOF'
### alpha
- severity: critical
- run: true
### beta
  - severity: low
  - verification: manual
EOF

expect 1 "a check with neither run nor verification is rejected (hollow)" <<'EOF'
### alpha
- severity: critical
- description: looks like enforcement, discharges nothing
EOF
grep -q 'hollow check' out \
  && { pass=$((pass+1)); echo "  ok    the hollow rejection says 'hollow check'"; } \
  || { fail=$((fail+1)); echo "  FAIL  the hollow rejection does not say 'hollow check'"; }

expect 1 "a check with no severity is rejected" <<'EOF'
### alpha
- run: true
EOF

expect 1 "one hollow check among valid ones is rejected" <<'EOF'
### alpha
- severity: critical
- run: true
### beta
- severity: high
### gamma
- severity: low
- verification: manual
EOF

expect 1 "the LAST check is validated too (flush at end of file)" <<'EOF'
### alpha
- severity: critical
- run: true
### omega
- description: trailing hollow check
EOF

expect 1 "a Mustfile that declares no checks is rejected" <<'EOF'
# Mustfile
- severity: critical
- run: true
EOF

expect 1 "an empty Mustfile is rejected" </dev/null

got=0; bash "$SCRIPT" "$WORK/does-not-exist.a2ml" >/dev/null 2>&1 || got=$?
if [ "$got" = 2 ]; then pass=$((pass+1)); echo "  ok    a missing Mustfile exits 2"
else fail=$((fail+1)); echo "  FAIL  a missing Mustfile (wanted exit 2, got $got)"; fi

echo "PASS=$pass FAIL=$fail"
[ "$fail" -eq 0 ]
