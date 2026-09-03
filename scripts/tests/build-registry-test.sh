#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
set -uo pipefail
#
# Regression cover for scripts/build-registry.sh.
#
# REGISTRY.a2ml and TOPOLOGY.adoc are generated artefacts, and `--check` is the
# gate behind the required context "Registry + topology in sync". A stale
# registry does not fail quietly in one place: four scorecard rows ground their
# `status = "pass"` on `build-registry.sh --check`, so one stale file also fails
# `build-scorecards.sh --verify` and surfaces as unrelated-looking assertion
# failures in the wave-3 suite, a long way from the cause.
#
# Everything runs inside a throwaway CLONE of the repository. build-registry.sh
# cd's to the git toplevel and reads the INDEX (`git ls-files -s`), so it needs
# a real repo; and planting drift in the developer's own worktree would risk
# destroying uncommitted registry work during cleanup.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

pass=0 fail=0
ok()  { echo "  ✅ $1"; pass=$((pass + 1)); }
bad() { echo "  ❌ $1"; fail=$((fail + 1)); }

REPO="$TMP/repo"
if ! git clone -q "$ROOT" "$REPO" 2>/dev/null; then
  echo "  ❌ could not clone the repository under test"; exit 1
fi
cd "$REPO" || exit 1
git config user.name  "Standards regression test"
git config user.email "standards-regression@example.invalid"

# The clone carries the COMMITTED generator. Overlay the working-tree copy so
# this suite exercises the script UNDER REVIEW — without this, a break
# introduced in a pull request is invisible here and the tests pass for the
# wrong reason.
cp "$ROOT/scripts/build-registry.sh" scripts/build-registry.sh

REG=".machine_readable/REGISTRY.a2ml"
TOP="TOPOLOGY.adoc"

echo "== the committed artefacts are in sync with the committed tree =="
out="$(bash scripts/build-registry.sh --check 2>&1)"; rc=$?
if [ "$rc" -eq 0 ] && printf '%s' "$out" | grep -qF "OK:"; then
  ok "--check reports OK on a clean checkout"
else
  bad "--check on a clean checkout (rc=$rc)"; printf '%s\n' "$out" | sed 's/^/       | /'
fi

echo
echo "== --check detects a mutated artefact =="
printf '\n# planted drift\n' >> "$REG"
out="$(bash scripts/build-registry.sh --check 2>&1)"; rc=$?
if [ "$rc" -eq 1 ] && printf '%s' "$out" | grep -qF "DRIFT: $REG"; then
  ok "a mutated REGISTRY.a2ml is reported as DRIFT"
else
  bad "mutated registry not caught (rc=$rc)"; printf '%s\n' "$out" | sed 's/^/       | /'
fi
git checkout -- "$REG"

printf '\n// planted drift\n' >> "$TOP"
out="$(bash scripts/build-registry.sh --check 2>&1)"; rc=$?
if [ "$rc" -eq 1 ] && printf '%s' "$out" | grep -qF "DRIFT: $TOP"; then
  ok "a mutated TOPOLOGY.adoc is reported as DRIFT"
else
  bad "mutated topology not caught (rc=$rc)"; printf '%s\n' "$out" | sed 's/^/       | /'
fi
git checkout -- "$TOP"


echo
echo "== --check detects a tree change the artefacts do not yet record =="
# The real-world fault: someone adds a tracked file under a spec's home and
# forgets `just registry`. source_hash is sha256 over `git ls-files -s <home>`,
# which reads the INDEX — so the file must be STAGED for this to bite, which is
# also why --check belongs after `git add`, never on a dirty worktree.
printf 'planted registry probe\n' > constitution/zzz-registry-probe.adoc
git add constitution/zzz-registry-probe.adoc
out="$(bash scripts/build-registry.sh --check 2>&1)"; rc=$?
if [ "$rc" -eq 1 ] && printf '%s' "$out" | grep -qF "DRIFT:"; then
  ok "a newly-tracked file under a spec home makes the artefacts stale"
else
  bad "new tracked file under a spec home did not drift (rc=$rc)"; printf '%s\n' "$out" | sed 's/^/       | /'
fi

echo
echo "== the gate's scope is the spec homes, not the whole tree =="
# Worth pinning down so nobody later mistakes this gate for whole-tree coverage:
# a tracked file outside every declared home legitimately does NOT drift the
# registry, because no source_hash covers it.
git rm -q --cached constitution/zzz-registry-probe.adoc >/dev/null 2>&1
rm -f constitution/zzz-registry-probe.adoc
mkdir -p .machine_readable/scorecards
printf '[metadata]\nname = "zzz-probe"\n' > .machine_readable/scorecards/zzz-registry-probe.scorecard.a2ml
git add .machine_readable/scorecards/zzz-registry-probe.scorecard.a2ml
out="$(bash scripts/build-registry.sh --check 2>&1)"; rc=$?
if [ "$rc" -eq 0 ]; then
  ok "a tracked file outside every spec home does not drift the registry"
else
  bad "a file outside every spec home unexpectedly drifted the registry (rc=$rc)"
  printf '%s\n' "$out" | sed 's/^/       | /'
fi
git rm -q --cached .machine_readable/scorecards/zzz-registry-probe.scorecard.a2ml >/dev/null 2>&1
rm -f .machine_readable/scorecards/zzz-registry-probe.scorecard.a2ml

echo
echo "== regenerating clears the drift =="
# Re-plant the in-scope drift, then prove the generator resolves it.
printf 'planted registry probe\n' > constitution/zzz-registry-probe.adoc
git add constitution/zzz-registry-probe.adoc
bash scripts/build-registry.sh >/dev/null 2>&1
out="$(bash scripts/build-registry.sh --check 2>&1)"; rc=$?
if [ "$rc" -eq 0 ] && printf '%s' "$out" | grep -qF "OK:"; then
  ok "a regenerated registry is back in sync"
else
  bad "regeneration did not clear the drift (rc=$rc)"; printf '%s\n' "$out" | sed 's/^/       | /'
fi

echo
echo "== the generator is deterministic =="
# The header promises "Run twice -> byte-identical output" and "intentionally NO
# generation timestamp". If that ever stops holding, the gate becomes a coin
# toss that reddens unrelated pull requests.
bash scripts/build-registry.sh >/dev/null 2>&1
r1="$(sha256sum "$REG" | cut -d' ' -f1)"; t1="$(sha256sum "$TOP" | cut -d' ' -f1)"
bash scripts/build-registry.sh >/dev/null 2>&1
r2="$(sha256sum "$REG" | cut -d' ' -f1)"; t2="$(sha256sum "$TOP" | cut -d' ' -f1)"
[ "$r1" = "$r2" ] && ok "two REGISTRY.a2ml generations are byte-identical" \
                  || bad "REGISTRY.a2ml is not deterministic"
[ "$t1" = "$t2" ] && ok "two TOPOLOGY.adoc generations are byte-identical" \
                  || bad "TOPOLOGY.adoc is not deterministic"

if grep -q 'generated' "$REG" && grep -qiE '^[[:space:]]*generated(_at)?[[:space:]]*=' "$REG"; then
  bad "REGISTRY.a2ml carries a generation timestamp — that would defeat --check"
else
  ok "REGISTRY.a2ml carries no generation timestamp"
fi

echo
echo "build-registry regression: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
