#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
set -uo pipefail
#
# Regression cover for scripts/check-action-pins-resolve.sh.
#
# An unresolvable `uses:` produces NO check run at all — the job silently never
# runs, so the board looks green while the gate is simply absent. This checker
# is what stands between the estate and that failure mode, which means its own
# behaviour has to be pinned down precisely.
#
# The interesting property is not "does it find bad pins" but the DISTINCTION it
# draws between a determinate negative and an indeterminate answer:
#
#   404/422 from the commits endpoint  -> determinate; re-probe to say WHICH
#                                         thing is missing; counts as failure
#   403 / 5xx / network error          -> says NOTHING about the pin; must be
#                                         reported and must NOT fail the build
#
# Get that backwards in either direction and the tool is worthless: fail-closed
# on a rate limit and every consumer reddens for reasons unrelated to its code;
# fail-open silently and a genuinely dead pin sails through. It fails OPEN but
# ANNOUNCES itself, and this suite exists to hold it to that.
#
# The network is driven by a `curl` stub placed ahead of the real binary on
# PATH, keyed on the URL, so every branch is reachable offline and repeatably.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CHK="$ROOT/scripts/check-action-pins-resolve.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

pass=0 fail=0
ok()  { echo "  ✅ $1"; pass=$((pass + 1)); }
bad() { echo "  ❌ $1"; fail=$((fail + 1)); }

# ── the curl stub ───────────────────────────────────────────────────────────
# The script calls: curl -sS -o /dev/null -w '%{http_code}' … <url>
# so the stub simply prints the code the case wants for that URL shape.
mkdir -p "$TMP/bin"
cat > "$TMP/bin/curl" <<'STUB'
#!/usr/bin/env bash
url="${!#}"
case "$url" in
  */commits/*) code="${STUB_COMMITS:-200}" ;;
  *)           code="${STUB_REPO:-200}" ;;
esac
[ "$code" = "NETFAIL" ] && exit 22
printf '%s' "$code"
STUB
chmod +x "$TMP/bin/curl"
export PATH="$TMP/bin:$PATH"

SHA_A=1111111111111111111111111111111111111111
SHA_B=2222222222222222222222222222222222222222

# mk_repo <dir> — a target tree with one SHA-pinned external action
mk_repo() {
  local d="$1"; rm -rf "$d"; mkdir -p "$d/.github/workflows"
  cat > "$d/.github/workflows/ci.yml" <<YAML
name: ci
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@${SHA_A}
YAML
}

# expect <name> <expected-rc> <expected-substring> <target-dir>
expect() {
  local name=$1 want_rc=$2 want_sub=$3 target=$4 out rc
  out="$(bash "$CHK" "$target" 2>&1)"; rc=$?
  if [ "$rc" -ne "$want_rc" ]; then
    bad "$name (rc=$rc, wanted $want_rc)"; printf '%s\n' "$out" | sed 's/^/       | /'; return
  fi
  if [ -n "$want_sub" ] && ! printf '%s' "$out" | grep -qF -- "$want_sub"; then
    bad "$name (rc ok but missing \"$want_sub\")"; printf '%s\n' "$out" | sed 's/^/       | /'; return
  fi
  ok "$name"
}

echo "== offline-decidable exits, taken before any network call =="

mkdir -p "$TMP/no-workflows"
expect "a target with no .github/workflows/ exits 0" 0 "nothing to check" "$TMP/no-workflows"

mkdir -p "$TMP/no-pins/.github/workflows"
cat > "$TMP/no-pins/.github/workflows/ci.yml" <<'YAML'
name: ci
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: ./.github/actions/local-thing
YAML
expect "tag pins and local refs are not SHA pins" 0 "No SHA-pinned external actions found" "$TMP/no-pins"

echo
echo "== determinate answers: the pin really is unresolvable =="

mk_repo "$TMP/r"

STUB_COMMITS=404 STUB_REPO=200 \
  expect "commits 404 + repo 200 is reported as SHA-NOT-FOUND" 1 "SHA-NOT-FOUND" "$TMP/r"

STUB_COMMITS=422 STUB_REPO=200 \
  expect "commits 422 is treated as determinate too" 1 "SHA-NOT-FOUND" "$TMP/r"

STUB_COMMITS=404 STUB_REPO=404 \
  expect "commits 404 + repo 404 is reported as REPO-NOT-FOUND" 1 "REPO-NOT-FOUND" "$TMP/r"

STUB_COMMITS=404 STUB_REPO=404 \
  expect "a determinate failure explains the remedy" 1 "Vendor the logic into this repo" "$TMP/r"

echo
echo "== indeterminate answers must NOT fail the build =="
# This is the property most easily lost in a refactor: an answer that says
# nothing about the pin must not be converted into a verdict about the pin.

STUB_COMMITS=403 \
  expect "a rate limit does not fail the build" 0 "::warning::UNVERIFIED" "$TMP/r"

STUB_COMMITS=403 \
  expect "a rate limit is explicitly not counted as a failure" 0 "NOT counted as failures" "$TMP/r"

STUB_COMMITS=500 \
  expect "a 5xx does not fail the build" 0 "::warning::UNVERIFIED" "$TMP/r"

STUB_COMMITS=NETFAIL \
  expect "a network failure surfaces as HTTP 000, not a verdict" 0 "HTTP 000" "$TMP/r"

STUB_COMMITS=404 STUB_REPO=500 \
  expect "a determinate negative with an unconfirmable repo is indeterminate" 0 "::warning::UNVERIFIED" "$TMP/r"

echo
echo "== the happy path =="

STUB_COMMITS=200 \
  expect "a resolving pin passes" 0 "All 1 verifiable action pin(s) resolve upstream." "$TMP/r"

echo
echo "== pair extraction =="

# The same action pinned twice, plus a subpath pin, must collapse to the
# repository-level pairs actually worth one API call each.
rm -rf "$TMP/dedup"; mkdir -p "$TMP/dedup/.github/workflows"
cat > "$TMP/dedup/.github/workflows/a.yml" <<YAML
jobs:
  j:
    steps:
      - uses: actions/checkout@${SHA_A}
      - uses: actions/checkout@${SHA_A}
YAML
cat > "$TMP/dedup/.github/workflows/b.yml" <<YAML
jobs:
  j:
    steps:
      - uses: actions/checkout@${SHA_A}
YAML
STUB_COMMITS=200 \
  expect "a pin repeated across files is checked once" 0 "Checking 1 unique action pin(s)" "$TMP/dedup"

cat > "$TMP/dedup/.github/workflows/c.yml" <<YAML
jobs:
  j:
    steps:
      - uses: github/codeql-action/init@${SHA_B}
YAML
STUB_COMMITS=200 \
  expect "a subpath pin is resolved at the repository level" 0 "Checking 2 unique action pin(s)" "$TMP/dedup"

echo
echo "check-action-pins-resolve regression: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
