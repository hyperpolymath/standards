#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -uo pipefail
#
# Wave-4 DYADT regression test.
#
# The whole point of DYADT is that it can REFUTE a false claim and does not
# confirm on the agent's say-so. This exercises confirm / refute / unverifiable
# and the incompatible-verifier + manual-only guards, plus the conformance suite.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
V="$ROOT/scripts/verify-claims.sh"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

pass=0 fail=0
ok()  { echo "  ✅ $1"; pass=$((pass + 1)); }
bad() { echo "  ❌ $1"; fail=$((fail + 1)); }

# verdict_of <claims-file> <claim-id>  -> prints confirmed|refuted|unverifiable
verdict_of() {
  cd "$ROOT" && DYADT_ALLOW_UNVERIFIABLE=1 bash "$V" "$1" 2>/dev/null \
    | grep -oE "$2  (confirmed|REFUTED|unverifiable)" | awk '{print tolower($2)}' | head -1
}

echo "== confirm / refute (does not trust the statement) =="
cat > "$TMP/t.a2ml" <<'EOF'
[claims]
schema = "dyadt/claim@1"
actor = "test"
[[claim]]
id = "C1"
claim_class = "command-ran"
statement = "this command succeeds"
target = "true"
expect = "exit==0"
verifier = "command-transcript"
[[claim]]
id = "C2"
claim_class = "command-ran"
statement = "LIE: this command succeeds (it does not)"
target = "false"
expect = "exit==0"
verifier = "command-transcript"
EOF
[ "$(verdict_of "$TMP/t.a2ml" C1)" = confirmed ] && ok "true command confirmed" || bad "true command not confirmed"
[ "$(verdict_of "$TMP/t.a2ml" C2)" = refuted ]   && ok "false command REFUTED despite honest-sounding statement" || bad "false command not refuted"

echo "== guards =="
# incompatible verifier -> unverifiable
cat > "$TMP/i.a2ml" <<'EOF'
[claims]
schema = "dyadt/claim@1"
actor = "test"
[[claim]]
id = "C1"
claim_class = "pr-merged"
statement = "PR merged"
target = "o/r#1"
expect = "merged==true"
verifier = "command-transcript"
EOF
[ "$(verdict_of "$TMP/i.a2ml" C1)" = unverifiable ] && ok "incompatible verifier -> unverifiable" || bad "incompatible verifier not caught"

# licence claim -> unverifiable (manual-only)
cat > "$TMP/l.a2ml" <<'EOF'
[claims]
schema = "dyadt/claim@1"
actor = "test"
[[claim]]
id = "C1"
claim_class = "file-changed"
statement = "SPDX licence header added"
target = "x.rs"
expect = "contains:SPDX-License-Identifier"
verifier = "git-diff"
EOF
[ "$(verdict_of "$TMP/l.a2ml" C1)" = unverifiable ] && ok "licence claim -> manual-only unverifiable" || bad "licence claim not manual-only"

echo "== loud exit semantics =="
# a refuted claim makes the process exit non-zero (fail loudly)
cat > "$TMP/r.a2ml" <<'EOF'
[claims]
schema = "dyadt/claim@1"
actor = "test"
[[claim]]
id = "C1"
claim_class = "command-ran"
statement = "false succeeds"
target = "false"
expect = "exit==0"
verifier = "command-transcript"
EOF
( cd "$ROOT" && bash "$V" "$TMP/r.a2ml" >/dev/null 2>&1 ); [ $? -ne 0 ] && ok "refuted claim fails loudly (non-zero exit)" || bad "refuted claim did not fail the run"

# an all-confirmed file exits 0
cat > "$TMP/g.a2ml" <<'EOF'
[claims]
schema = "dyadt/claim@1"
actor = "test"
[[claim]]
id = "C1"
claim_class = "command-ran"
statement = "true succeeds"
target = "true"
expect = "exit==0"
verifier = "command-transcript"
EOF
( cd "$ROOT" && bash "$V" "$TMP/g.a2ml" >/dev/null 2>&1 ); [ $? -eq 0 ] && ok "all-confirmed file exits 0" || bad "all-confirmed file did not exit 0"

echo "== conformance suite =="
bash "$ROOT/did-you-actually-do-that/spec/conformance/run-conformance.sh" >/dev/null 2>&1 && ok "conformance vectors pass" || bad "conformance vectors failed"

echo
echo "Wave-4 DYADT regression: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
