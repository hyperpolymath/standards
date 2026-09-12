#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -euo pipefail

# verify-regextarget-claim.sh — does an anchored VALUE regex suppress a
# `generic-api-key` finding? Measure it; do not reason about it.
#
# ── Why this exists ────────────────────────────────────────────────────────
# `config/gitleaks/estate-baseline.toml` once told the estate that anchored
# value regexes are "silently INERT" for `generic-api-key`, and on that basis
# steered every repository toward PATH entries instead. The two are not
# equivalent: a `regexes` entry suppresses exactly ONE string, so a credential
# planted in the same file is still caught, whereas a `paths` entry blinds the
# whole file to everything. So the claim decided how surgical every repo's
# allowlist was allowed to be — and it was false. This script is the standing
# proof, and a gate: if a future gitleaks changes these semantics, it fails.
#
# ── The instrument trap this script is built to avoid ──────────────────────
# If the gitleaks CONFIG or the JSON REPORT lives inside `--source`, gitleaks
# scans them too. Their own credential-shaped content trips `generic-api-key`,
# the finding count never reaches zero, and a WORKING allowlist entry reads as
# inert. That is the most likely origin of the original false claim. Here both
# live outside the scanned tree, and the control asserts exactly one finding —
# so a contaminated instrument aborts instead of reporting a wrong answer.
#
# Usage:  scripts/verify-regextarget-claim.sh
#         GITLEAKS=/path/to/gitleaks scripts/verify-regextarget-claim.sh
#         GITLEAKS_ALLOW_VERSION_DRIFT=1 ...   (measure on a non-pinned build)

PINNED_VERSION=8.18.4
GL="${GITLEAKS:-$(command -v gitleaks || true)}"

fail() { echo "ABORT: $*" >&2; exit 2; }

[ -n "$GL" ] && [ -x "$GL" ] \
  || fail "gitleaks not found. Set GITLEAKS=/path/to/gitleaks (CI pins $PINNED_VERSION)."
have_version="$("$GL" version 2>&1 | head -1)"
if [ "$have_version" != "$PINNED_VERSION" ]; then
  if [ "${GITLEAKS_ALLOW_VERSION_DRIFT:-0}" != "1" ]; then
    fail "gitleaks is $have_version, CI pins $PINNED_VERSION. These results are version-specific; set GITLEAKS_ALLOW_VERSION_DRIFT=1 to measure anyway."
  fi
  echo "WARNING: measuring on $have_version, not the CI-pinned $PINNED_VERSION." >&2
fi

WORK="$(mktemp -d)"; OUT="$(mktemp -d)"
trap 'rm -rf "$WORK" "$OUT"' EXIT

# An Ada declaration is the canonical estate false positive: `generic-api-key`
# cannot tell `Key : Ed25519_Private_Key;` from `api_key: <base64>`, and the
# MATCH (whole declaration) differs from the SECRET (the type name) — which is
# precisely the case the old claim was about.
cat > "$WORK/sample.ads" <<'ADA'
package Crypto is
   Key : Ed25519_Private_Key;
end Crypto;
ADA

CFG="$OUT/cfg.toml"
mk() {
  {
    echo '[extend]'
    echo 'useDefault = true'
    echo
    echo '[allowlist]'
    echo 'description = "verify-regextarget-claim fixture"'
    printf '%s\n' "$1"
  } > "$CFG"
}
count() {
  if ! "$GL" detect --source "$WORK" --no-git --no-banner --config "$CFG" \
        --report-format json --report-path "$OUT/o.json" --exit-code 0 >/dev/null 2>&1; then
    fail "gitleaks failed to run (config load error?)"
  fi
  jq 'length' "$OUT/o.json"
}

mk ''
base="$(count)"
[ "$base" = "1" ] \
  || fail "control expected exactly 1 finding, got $base — the instrument is contaminated or the fixture no longer measures generic-api-key."
rule="$(jq -r '.[0].RuleID' "$OUT/o.json")"
[ "$rule" = "generic-api-key" ] || fail "control fired '$rule', not generic-api-key"
echo "gitleaks $have_version"
echo "control: rule=$rule"
echo "         MATCH  = [$(jq -r '.[0].Match'  "$OUT/o.json")]"
echo "         SECRET = [$(jq -r '.[0].Secret' "$OUT/o.json")]"
echo "         (match != secret — this is the case the false claim was about)"
echo

status=0
expect() { # expect <label> <wanted-count>
  local label="$1" want="$2" got
  got="$(count)"
  if [ "$got" = "$want" ]; then
    printf '  %-38s %s  ok\n' "$label" "$got"
  else
    printf '  %-38s %s  FAIL (expected %s)\n' "$label" "$got" "$want"
    status=1
  fi
}

printf '  %-38s %s\n' 'ALLOWLIST ENTRY' 'FINDINGS'
mk ''
expect '(none - control)' 1
mk "regexes = ['''^Ed25519_Private_Key\$''']"
expect '^SECRET$, regexTarget unset' 0
mk "regexTarget = \"secret\"
regexes = ['''^Ed25519_Private_Key\$''']"
expect '^SECRET$, regexTarget = "secret"' 0
mk "regexTarget = \"match\"
regexes = ['''^Ed25519_Private_Key\$''']"
expect '^SECRET$, regexTarget = "match"' 1
mk "regexTarget = \"match\"
regexes = ['''^\\s*Key : Ed25519_Private_Key;\$''']"
expect '^MATCH$,  regexTarget = "match"' 0

echo
if [ "$status" = 0 ]; then
  cat <<'VERDICT'
VERDICT: regexTarget DEFAULTS TO THE SECRET. An anchored value regex DOES
suppress generic-api-key and needs no regexTarget at all. This matches what
config/gitleaks/estate-baseline.toml documents. All eight of that file's own
entries are anchored on the value and set regexTarget nowhere — were the old
"silently INERT" claim true, the estate baseline would allowlist nothing.
VERDICT
else
  cat <<'VERDICT' >&2
VERDICT: MEASURED BEHAVIOUR NO LONGER MATCHES WHAT THE BASELINE DOCUMENTS.
Do not edit allowlist entries until this is understood: every `regexes` entry
in config/gitleaks/estate-baseline.toml assumes regexTarget defaults to the
secret. A gitleaks upgrade is the likely cause. Update the file's comment and
this script together, and re-check the entries it protects.
VERDICT
fi
exit "$status"
