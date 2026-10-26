#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# check-rsr-profile-test.sh — fixture suite for scripts/check-rsr-profile.sh.
#
# Covers the three defects fixed on 2026-10-25:
#   #1228  the rust source row was root-anchored, so a cargo WORKSPACE could
#          never satisfy it and posted a permanent MISSING;
#   #1227  [baseline] had no reader at all — a repo could pass while missing
#          its LICENSE (the .well-known row also predated the www/ migration);
#   #1229  the baseline demanded 0-AI-MANIFEST.a2ml after the template retired
#          that spelling for .deed.
# Plus the carrier-list parse fix: the [carrier]/[canon] arrays are multi-line
# and were read with a single-line reader, so no entry ever applied.
#
# Run: bash scripts/tests/check-rsr-profile-test.sh

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECK="$SCRIPT_DIR/../check-rsr-profile.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
pass=0; fail=0

assert() { # label want-status needle cmd...
  local label="$1" want="$2" needle="$3"; shift 3
  local out status
  out="$("$@" 2>&1)"; status=$?
  if [ "$status" != "$want" ]; then
    echo "FAIL: $label — expected exit $want, got $status"
    echo "      $(printf '%s' "$out" | grep -E '^  |FAIL' | head -3 | tr '\n' '|')"
    fail=$((fail + 1)); return
  fi
  if [ "$needle" != "-" ] && ! printf '%s' "$out" | grep -qF -- "$needle"; then
    echo "FAIL: $label — exit ok, output lacked '$needle'"
    echo "      $(printf '%s' "$out" | tr '\n' '|')"
    fail=$((fail + 1)); return
  fi
  echo "PASS: $label"; pass=$((pass + 1))
}

# repo <dir> — every baseline artefact, using the ALTERNATE spellings that the
# 2026-10 estate uses (.deed manifest, www/.well-known, district guides), so the
# fixtures also prove the alternations resolve.
repo() {
  local d="$WORK/$1"; mkdir -p "$d/.machine_readable/descriptiles" "$d/www/.well-known" "$d/3-practice"
  : > "$d/README.adoc"; : > "$d/EXPLAINME.adoc"; : > "$d/LICENSE"; : > "$d/SECURITY.md"
  : > "$d/3-practice/CONTRIBUTING.adoc"; : > "$d/3-practice/CODE_OF_CONDUCT.adoc"
  : > "$d/CHANGELOG.adoc"; : > "$d/0-AI-MANIFEST.deed"; : > "$d/.gitignore"; : > "$d/Justfile"
  : > "$d/www/.well-known/security.txt"
  # A CONFORMING rust single-crate scaffold by default, so every fixture starts
  # green and each test mutates exactly the one thing it is about. A repo that
  # declares `rust` owes all four rust rows, because declaring a capability is a
  # claim the module set backs up.
  mkdir -p "$d/src" "$d/.github/workflows"
  : > "$d/Cargo.toml"; : > "$d/Cargo.lock"; : > "$d/src/main.rs"
  : > "$d/.github/workflows/rust-ci.yml"
  printf '[rsr-profile]\nversion = "1.0.0"\ncapabilities = ["rust"]\n' > "$d/.machine_readable/rsr-profile.a2ml"
  printf '%s' "$d"
}

echo "=== #1228 cargo workspace vs root-anchored glob ==="
d=$(repo ws)
mkdir -p "$d/crates/a/src" "$d/crates/b/src"
: > "$d/crates/a/Cargo.toml"; : > "$d/crates/a/Cargo.lock"; : > "$d/crates/a/src/lib.rs"
: > "$d/crates/b/Cargo.toml"; : > "$d/crates/b/Cargo.lock"; : > "$d/crates/b/src/lib.rs"
assert "workspace: nested src+manifest satisfies rust (was a permanent MISSING)" 0 "rsr-profile check: OK" bash "$CHECK" "$d"

d=$(repo single)
assert "single crate at the root still passes (both layouts resolve)" 0 "rsr-profile check: OK" bash "$CHECK" "$d"

d=$(repo ws2); rm -rf "$d/src" "$d/Cargo.toml"
mkdir -p "$d/crates/a/src"; : > "$d/crates/a/Cargo.toml"; : > "$d/crates/a/src/lib.rs"
assert "workspace with NO root manifest still passes (no root src/ required)" 0 "rsr-profile check: OK" bash "$CHECK" "$d"

echo "=== #1227 baseline is enforced ==="
d=$(repo nolicence); rm "$d/LICENSE"
assert "missing LICENSE → MISSING BASELINE, exit 1" 1 "MISSING BASELINE" bash "$CHECK" "$d"

d=$(repo wellknown); rm -rf "$d/www/.well-known"; mkdir -p "$d/.well-known"; : > "$d/.well-known/security.txt"
assert ".well-known/ at the root still conforms (pre-migration repositories)" 0 "rsr-profile check: OK" bash "$CHECK" "$d"

d=$(repo nomanifest); rm "$d/0-AI-MANIFEST.deed"; : > "$d/0-AI-MANIFEST.a2ml"
assert "#1229: 0-AI-MANIFEST.a2ml still conforms" 0 "rsr-profile check: OK" bash "$CHECK" "$d"

d=$(repo nomd); rm "$d/0-AI-MANIFEST.deed"
assert "no AI manifest in either spelling fails" 1 "MISSING BASELINE" bash "$CHECK" "$d"

echo "=== vestigial detection still works ==="
d=$(repo vestigial)
printf '[rsr-profile]\nversion = "1.0.0"\ncapabilities = ["bash"]\n' > "$d/.machine_readable/rsr-profile.a2ml"
assert "rust sources without the rust capability → VESTIGIAL" 1 "VESTIGIAL" bash "$CHECK" "$d"
assert "…and the verdict names the row readably, not as a raw alternation" 1 "Cargo.toml or */Cargo.toml" bash "$CHECK" "$d"

echo "=== carrier lists actually parse (multi-line arrays) ==="
d=$(repo canonrole)
printf '[rsr-profile]\nversion = "1.0.0"\nrole = "canon"\ncapabilities = ["rust"]\n' > "$d/.machine_readable/rsr-profile.a2ml"
rm "$d/.github/workflows/rust-ci.yml"
assert "a canon row listed in [canon] is exempt from MISSING" 0 "served for downstream" bash "$CHECK" "$d"

d=$(repo spinecarrier)
printf '[rsr-profile]\nversion = "1.0.0"\nrole = "spine"\ncapabilities = ["bash"]\n' > "$d/.machine_readable/rsr-profile.a2ml"
rm -f "$d/Cargo.toml" "$d/Cargo.lock" "$d/src/main.rs"
assert "a spine row listed in [carrier] is exempt from VESTIGIAL" 0 "carried for downstream" bash "$CHECK" "$d"

echo "=== setup errors ==="
assert "no profile → 2" 2 "no profile at" bash "$CHECK" "$WORK/does-not-exist"
assert "missing gates table → 2" 2 "gates file not found" env RSR_GATES="$WORK/nope.toml" bash "$CHECK" "$(repo g)"

printf '\npassed=%d failed=%d\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
