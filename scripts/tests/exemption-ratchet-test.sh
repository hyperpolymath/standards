#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Proves check-exemption-ratchet.sh CAN FAIL, and fails for the right reasons.
#
# This estate's recurring defect is the gate that cannot fail — `just proof-check`
# recipes exiting 0 with no prover installed, `continue-on-error` on the primary
# secret scanner, a linter that never parsed its input. A ratchet that always
# passes would be worse than no ratchet, because it would license exactly the
# silent growth it claims to prevent. So every branch is exercised here, in both
# directions.
set -euo pipefail
SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/check-exemption-ratchet.sh"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
cd "$WORK"; git init -q .; git config user.email t@example.com; git config user.name T

printf '[{"severity":"high","rule_module":"m","type":"t","file":"a.rs","note":"why"}]\n' > .hypatia-baseline.json
printf 'cicd_rules/banned_language_file:src/A.res\n' > .hypatia-ignore
git add -A; git commit -q -m base
BASE="$(git rev-parse HEAD)"

pass=0; fail=0
expect() { # expect <wanted-exit> <label>
  local want="$1" label="$2" got=0
  bash "$SCRIPT" "$BASE" >/dev/null 2>&1 || got=$?
  if [ "$got" = "$want" ]; then pass=$((pass+1)); echo "  ok    $label"
  else fail=$((fail+1)); echo "  FAIL  $label (wanted exit $want, got $got)"; fi
}

expect 0 "unchanged ledgers pass"

python3 - <<'PY'
import json; d=json.load(open('.hypatia-baseline.json'))
d.append({"severity":"low","rule_module":"m","type":"t2","file":"b.rs","note":"ok"})
json.dump(d,open('.hypatia-baseline.json','w'))
PY
git commit -aqm "grow"
expect 1 "silent growth fails"

git commit -q --amend -m "grow

Ratchet-exception: vendored upstream corpus"
expect 0 "declared growth passes"

python3 - <<'PY'
import json; d=json.load(open('.hypatia-baseline.json'))
d.append({"severity":"low","rule_module":"m","type":"t3","file":"c.rs"})
json.dump(d,open('.hypatia-baseline.json','w'))
PY
git commit -aqm "anonymous

Ratchet-exception: declared"
expect 1 "anonymous entry fails even when growth is declared"

git checkout -q "$BASE" -- .hypatia-baseline.json
printf 'cicd_rules/banned_language_file:src/**\n' > .hypatia-ignore
git commit -aqm "wildcard"
expect 1 "wildcard in the migration ledger fails"

git checkout -q "$BASE" -- .hypatia-ignore
: > .hypatia-ignore
git commit -aqm "paid down"
expect 0 "shrinking passes"

echo
echo "  ${pass} passed, ${fail} failed"
[ "$fail" = "0" ]
