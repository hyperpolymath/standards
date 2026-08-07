#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Proves check-debtfile-structure.sh CAN FAIL, and fails for the right reasons.
#
# The structural validator's whole job is to reject a HOLLOW ENTRY — a number
# with no probe behind it. That is the shape the estate's stale issues already
# have ("~45 banned .py files", true value 176, unchallenged for ten weeks).
# If this validator could not reject it, the Debtfile would just be a slower
# way of writing the same folklore down.
set -euo pipefail
SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/check-debtfile-structure.sh"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
cd "$WORK"

pass=0; fail=0
expect() { # expect <wanted-exit> <label>   (Debtfile content on stdin)
  local want="$1" label="$2" got=0
  cat > Debtfile.a2ml
  bash "$SCRIPT" Debtfile.a2ml >/dev/null 2>&1 || got=$?
  if [ "$got" = "$want" ]; then pass=$((pass+1)); echo "  ok    $label"
  else fail=$((fail+1)); echo "  FAIL  $label (wanted exit $want, got $got)"; fi
}

expect 0 "a complete entry is valid" <<'EOF'
### alpha
- description: d
- probe: echo 1
- count: 1
- ceiling: 1
- severity: high
- policy: remediable
- accepted-until: 2030-01-01
EOF

expect 1 "an entry with no probe is rejected (a number nothing re-measures)" <<'EOF'
### alpha
- description: d
- count: 1
- ceiling: 1
- severity: high
- policy: remediable
- accepted-until: 2030-01-01
EOF

expect 1 "an entry with no ceiling is rejected (the ratchet would have nothing to hold)" <<'EOF'
### alpha
- description: d
- probe: echo 1
- count: 1
- severity: high
- policy: remediable
- accepted-until: 2030-01-01
EOF

expect 1 "count above ceiling is rejected (records more debt than it tolerates)" <<'EOF'
### alpha
- description: d
- probe: echo 9
- count: 9
- ceiling: 4
- severity: high
- policy: remediable
- accepted-until: 2030-01-01
EOF

expect 0 "count below ceiling is fine (debt paid down, ceiling not yet lowered)" <<'EOF'
### alpha
- description: d
- probe: echo 2
- count: 2
- ceiling: 4
- severity: high
- policy: remediable
- accepted-until: 2030-01-01
EOF

expect 1 "a non-integer count is rejected" <<'EOF'
### alpha
- description: d
- probe: echo 1
- count: several
- ceiling: 4
- severity: high
- policy: remediable
- accepted-until: 2030-01-01
EOF

expect 1 "an unknown severity is rejected" <<'EOF'
### alpha
- description: d
- probe: echo 1
- count: 1
- ceiling: 1
- severity: catastrophic
- policy: remediable
- accepted-until: 2030-01-01
EOF

expect 1 "a missing policy is rejected (say whether a bot may touch this)" <<'EOF'
### alpha
- description: d
- probe: echo 1
- count: 1
- ceiling: 1
- severity: high
- accepted-until: 2030-01-01
EOF

expect 1 "a missing accepted-until is rejected (debt nobody revisits)" <<'EOF'
### alpha
- description: d
- probe: echo 1
- count: 1
- ceiling: 1
- severity: high
- policy: remediable
EOF

expect 1 "a malformed date is rejected" <<'EOF'
### alpha
- description: d
- probe: echo 1
- count: 1
- ceiling: 1
- severity: high
- policy: remediable
- accepted-until: next spring
EOF

expect 1 "duplicate ids are rejected" <<'EOF'
### alpha
- description: d
- probe: echo 1
- count: 1
- ceiling: 1
- severity: high
- policy: remediable
- accepted-until: 2030-01-01

### alpha
- description: d2
- probe: echo 2
- count: 2
- ceiling: 2
- severity: low
- policy: flag-only
- accepted-until: 2030-01-01
EOF

# An empty file and an unparsed file look identical from inside the validator,
# so "no entries" is a defect rather than a clean bill of health.
expect 1 "a Debtfile with no entries is a defect, not zero debt" <<'EOF'
# SPDX-License-Identifier: MPL-2.0
# nothing here
EOF

# Indented list items must still parse — the Mustfile parser trims leading
# whitespace and this one must agree, or a reformatted file silently empties.
expect 0 "indented list items still parse" <<'EOF'
### alpha
  - description: d
  - probe: echo 1
  - count: 1
  - ceiling: 1
  - severity: high
  - policy: remediable
  - accepted-until: 2030-01-01
EOF

got=0; bash "$SCRIPT" no-such-file.a2ml >/dev/null 2>&1 || got=$?
if [ "$got" = 2 ]; then pass=$((pass+1)); echo "  ok    a missing Debtfile exits 2, distinct from a structural defect"
else fail=$((fail+1)); echo "  FAIL  missing file (wanted exit 2, got $got)"; fi

echo
echo "debtfile-structure-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
