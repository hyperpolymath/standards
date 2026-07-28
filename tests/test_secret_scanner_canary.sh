#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# test_secret_scanner_canary.sh — prove the secret scanner can still FAIL.
#
# WHY THIS EXISTS
#
# Every defect this suite guards against was invisible in CI. Ranked by how
# long each went unnoticed:
#
#   * `continue-on-error: true` on the gitleaks step. The scan ran, found a
#     live Cloudflare Global API Key in a PUBLIC repo, and reported success
#     anyway — for months, across ~200 repos.
#   * `rust-secrets` grepped `./src` only. A Cargo workspace keeps code in
#     `crates/*/src`, so `grep` exited 2 on a missing directory and the
#     enclosing `if` read that as "clean". 8 of 64 Rust repos (12.5%) were
#     never scanned at all.
#   * `const.*KEY.*=` never matched `static AUTH_KEY: &str = "…"`. Found only
#     because a canary planted 5 secrets and the job reported 4. Reading the
#     regex would never have shown it.
#
# The common shape: **a gate that cannot fail is indistinguishable from a gate
# that passes.** Nothing in a dashboard distinguishes them. The only defence is
# to plant a known secret and assert the scanner still trips.
#
# A security gate without a canary is an unverified claim.
#
# WHAT IT TESTS AGAINST
#
# The step body is extracted from `secret-scanner-reusable.yml` at run time and
# executed. It is NOT a copy: a copy silently drifts from what actually ships,
# which is exactly the failure mode that let the defects above survive review.
#
# Usage: tests/test_secret_scanner_canary.sh [path-to-reusable.yml]
# Exit:  0 = every canary behaved correctly, 1 = the gate is not trustworthy.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REUSABLE="${1:-${SCRIPT_DIR}/../.github/workflows/secret-scanner-reusable.yml}"

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

pass=0
fail=0

if [ ! -f "$REUSABLE" ]; then
  echo "::error::canary: reusable workflow not found at $REUSABLE"
  exit 1
fi

# ---------------------------------------------------------------------------
# Extract the shipping step body. Any failure here is a HARD failure: silently
# skipping would restore the very "gate that cannot fail" this test exists to
# prevent.
# ---------------------------------------------------------------------------
python3 - "$REUSABLE" "$WORK/rust-secrets.sh" <<'PY' || { echo "::error::canary: could not extract the rust-secrets step from the reusable"; exit 1; }
import sys
try:
    import yaml
except ImportError:
    sys.stderr.write("PyYAML is required to run the canary (pip install pyyaml)\n")
    sys.exit(1)

src, dst = sys.argv[1], sys.argv[2]
doc = yaml.safe_load(open(src))
steps = doc["jobs"]["rust-secrets"]["steps"]
step = next(s for s in steps if s.get("name", "").startswith("Check for hardcoded"))
env = step.get("env", {}) or {}
cutoff = env.get("ENFORCE_RUST_WIDE_SCAN_FROM", "2026-08-21")
with open(dst, "w") as fh:
    fh.write("#!/usr/bin/env bash\nset -uo pipefail\n")
    fh.write('ENFORCE_RUST_WIDE_SCAN_FROM="${ENFORCE_RUST_WIDE_SCAN_FROM:-%s}"\n' % cutoff)
    fh.write(step["run"])
print("extracted rust-secrets step (cutoff %s)" % cutoff)
PY

SCAN="$WORK/rust-secrets.sh"
chmod +x "$SCAN"

# Build a fixture tree. `mk <dir> <relative-file>` then feed stdin.
mk() { mkdir -p "$WORK/$1/$(dirname "$2")"; cat > "$WORK/$1/$2"; }

# `expect <dir> <wanted-exit> <label> [env...]`
expect() {
  local dir="$1" want="$2" label="$3"; shift 3
  ( cd "$WORK/$dir" && env "$@" bash "$SCAN" >"$WORK/out.txt" 2>&1 )
  local rc=$?
  if [ "$rc" = "$want" ]; then
    echo "  PASS  $label"
    pass=$((pass+1))
  else
    echo "  FAIL  $label (exit $rc, expected $want)"
    sed 's/^/          /' "$WORK/out.txt" | head -6
    fail=$((fail+1))
  fi
}

echo "Running secret-scanner canary..."

# ---------------------------------------------------------------------------
# 1. REAL SECRETS MUST BE DETECTED — the load-bearing assertion.
#    `static` forms are included deliberately: `const.*KEY.*=` missed
#    `static AUTH_KEY` in production, and only a planted-secret count exposed it.
# ---------------------------------------------------------------------------
echo '[package]' > "$WORK/.keep"; mkdir -p "$WORK/real"; echo '[package]' > "$WORK/real/Cargo.toml"
mk real src/main.rs <<'EOF'
const API_SECRET: &str = "PLANTED_CANARY_VALUE_NOT_REAL_1";
const SERVICE_TOKEN: &str = "PLANTED_CANARY_VALUE_NOT_REAL_2";
static AUTH_KEY: &str = "abcdef0123456789abcdef0123456789";
let api_key = "PLANTED_CANARY_VALUE_NOT_REAL_3";
let db_password = "PLANTED_CANARY_VALUE_NOT_REAL_4";
EOF
expect real 1 "5 planted secrets in ./src -> BLOCKS" RUST_TODAY=2026-07-21

# Count them: exit 1 only proves >=1 was found, not that all five were.
# The `static AUTH_KEY` defect passed an exit-code assertion but failed a count.
found=$( cd "$WORK/real" && RUST_TODAY=2026-07-21 bash "$SCAN" 2>&1 | grep -cE '^\./src/main\.rs' )
if [ "${found:-0}" -eq 5 ]; then
  echo "  PASS  all 5 planted secrets reported (not just the first)"
  pass=$((pass+1))
else
  echo "  FAIL  only ${found:-0}/5 planted secrets reported — the scanner has a blind spot"
  fail=$((fail+1))
fi

# ---------------------------------------------------------------------------
# 2. WORKSPACE LAYOUT MUST BE SCANNED (regression guard for the ./src-only bug)
# ---------------------------------------------------------------------------
mkdir -p "$WORK/ws"; echo '[workspace]' > "$WORK/ws/Cargo.toml"
mk ws crates/core/src/lib.rs <<'EOF'
pub const SERVICE_TOKEN: &str = "PLANTED_CANARY_VALUE_NOT_REAL_5";
EOF
expect ws 1 "secret in crates/*/src -> BLOCKS after cutoff (no root ./src)" RUST_TODAY=2026-09-01
expect ws 0 "secret in crates/*/src -> advisory before cutoff" RUST_TODAY=2026-07-21
if ( cd "$WORK/ws" && RUST_TODAY=2026-07-21 bash "$SCAN" 2>&1 | grep -q 'NOT YET ENFORCED' ); then
  echo "  PASS  advisory run reports the finding and never claims a clean pass"
  pass=$((pass+1))
else
  echo "  FAIL  advisory run did not report the outstanding finding"
  fail=$((fail+1))
fi

# ---------------------------------------------------------------------------
# 3. CORRECT CODE MUST NOT BE FLAGGED. Flagging `env::var` fails the very
#    practice this job exists to enforce — a gate whose only route to green is
#    to stop reading from the environment is inverted.
# ---------------------------------------------------------------------------
mkdir -p "$WORK/clean"; echo '[package]' > "$WORK/clean/Cargo.toml"
mk clean src/main.rs <<'EOF'
use std::env;
fn main() {
    let api_key = env::var("SOME_API_KEY").ok();
    let password = std::env::var("DB_PASSWORD").unwrap_or_default();
    const TOKEN_ENV: &str = option_env!("BUILD_TOKEN").unwrap_or("");
    const MS_TOKEN_URL: &str = "https://login.microsoftonline.com/common/oauth2/v2.0/token";
    let api_key_file = temp_dir.path().join("config.js");
    let lookup = request["api_key"].as_str().unwrap_or("");
    // const API_SECRET: &str = "documented-example-only";
    let api_key2 = "fixture-value";  // scanner-allow: rust-secrets
}
EOF
expect clean 0 "env::var / URL / lookup / path / comment / pragma -> ALLOWED" RUST_TODAY=2026-09-01

# ---------------------------------------------------------------------------
# 4. EXEMPTIONS MUST NOT BE OVER-BROAD. A literal secret with an unrelated
#    `env::var` mention later on the same line must still trip.
# ---------------------------------------------------------------------------
mkdir -p "$WORK/trap"; echo '[package]' > "$WORK/trap/Cargo.toml"
mk trap src/main.rs <<'EOF'
const API_SECRET: &str = "PLANTED_CANARY_VALUE_NOT_REAL_6"; // fallback when env::var fails
EOF
expect trap 1 "literal secret + later env::var mention -> STILL BLOCKS" RUST_TODAY=2026-07-21

# ---------------------------------------------------------------------------
# 5. THE CUTOFF MUST NOT BE SILENTLY DISARMABLE. An unparseable date would
#    select the warn branch forever, restoring a gate that cannot fail.
# ---------------------------------------------------------------------------
expect clean 1 "malformed cutoff -> REFUSES TO RUN" ENFORCE_RUST_WIDE_SCAN_FROM=soon

# ---------------------------------------------------------------------------
# 6. A repo with no Rust must skip cleanly rather than error.
# ---------------------------------------------------------------------------
mkdir -p "$WORK/norust"; echo 'hello' > "$WORK/norust/README.md"
expect norust 0 "no Cargo.toml -> skips cleanly" RUST_TODAY=2026-07-21

echo
echo "  === $pass passed, $fail failed ==="
if [ "$fail" -gt 0 ]; then
  echo "::error::Secret-scanner canary FAILED — the gate is not trustworthy. Do not merge."
  exit 1
fi
echo "Secret-scanner canary passed: the gate can still fail."
