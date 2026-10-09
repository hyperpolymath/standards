#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Tests for .githooks/validate-bot-directives.sh in staged mode.
#
# Staged mode used to grep EVERY staged prose file for codex|gci|other-bot,
# while scan mode only looks under .machine_readable/. A README that names the
# AI tool "Codex" therefore could not be committed (owner ruling D316). These
# cases pin both halves: prose outside .machine_readable/ is not the gate's
# business, and a directive file inside it still fails (the planted positive
# control; without it a validator that checks nothing would pass every case).
set -uo pipefail
HOOK="$(cd "$(dirname "$0")/../.." && pwd)/.githooks/validate-bot-directives.sh"
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
pass=0; fail=0

# ck NAME EXPECTED_EXIT STAGED_PATH... — run the hook from $T in staged mode
# over the given relative paths and compare its exit code with EXPECTED_EXIT.
ck() {
  local name="$1" want="$2" out rc; shift 2
  out="$(cd "$T" && INPUT_STAGED_FILES="$(printf '%s\n' "$@")" bash "$HOOK" 2>&1)"; rc=$?
  if [ "$rc" = "$want" ]; then printf '  ok    %s (exit %s)\n' "$name" "$rc"; pass=$((pass+1))
  else printf '  FAIL  %s (expected exit %s, got %s) output=%s\n' "$name" "$want" "$rc" "${out:-<none>}"; fail=$((fail+1)); fi
}

mkdir -p "$T/docs" "$T/.machine_readable/bot_directives" "$T/sub/.machine_readable"
printf 'Agents: Claude, Gemini, Codex.\n' > "$T/docs/README.adoc"
printf '(bot codex)\n' > "$T/.machine_readable/bot_directives/legacy.deed"
printf 'gci = true\n' > "$T/sub/.machine_readable/x.a2ml"
printf '(bot claude)\n' > "$T/.machine_readable/bot_directives/ok.deed"

ck "prose naming Codex outside .machine_readable passes"   0 docs/README.adoc
ck "deprecated directive in .machine_readable .deed fails" 1 .machine_readable/bot_directives/legacy.deed
ck "nested .machine_readable .a2ml directive fails"        1 sub/.machine_readable/x.a2ml
ck "clean directive file passes"                           0 .machine_readable/bot_directives/ok.deed
ck "mixed set fails on the directive file alone"           1 docs/README.adoc .machine_readable/bot_directives/legacy.deed

# The frozen scorecard archive (ruling R5, #837): its records name the tools
# of their day and are skipped; its README and a sibling archive are not
# (planted positives, so a skip of the whole archive tree would be caught).
A=".machine_readable/archive/scorecards-v1"
mkdir -p "$T/$A" "$T/.machine_readable/archive/scorecards-v2"
printf 'evidence = "Codex"\n' > "$T/$A/k.scorecard.a2ml"
printf 'Codex\n' > "$T/$A/README.adoc"
printf 'evidence = "Codex"\n' > "$T/.machine_readable/archive/scorecards-v2/k.scorecard.a2ml"
ck "archived scorecard record naming Codex passes"         0 "$A/k.scorecard.a2ml"
ck "archive README is still checked"                       1 "$A/README.adoc"
ck "sibling archive is still checked"                      1 .machine_readable/archive/scorecards-v2/k.scorecard.a2ml

# cks NAME EXPECTED_EXIT DIR — run the hook in scan mode (no staged list) over
# DIR and compare its exit code with EXPECTED_EXIT.
cks() {
  local name="$1" want="$2" dir="$3" out rc
  out="$(INPUT_PATH="$dir" INPUT_STAGED_FILES="" bash "$HOOK" 2>&1)"; rc=$?
  if [ "$rc" = "$want" ]; then printf '  ok    %s (exit %s)\n' "$name" "$rc"; pass=$((pass+1))
  else printf '  FAIL  %s (expected exit %s, got %s) output=%s\n' "$name" "$want" "$rc" "${out:-<none>}"; fail=$((fail+1)); fi
}

S="$T/scan"
mkdir -p "$S/$A"
printf 'evidence = "Codex"\n' > "$S/$A/k.scorecard.a2ml"
cks "scan: archived scorecard record passes"               0 "$S"
printf '(bot codex)\n' > "$S/.machine_readable/live.deed"
cks "scan: live directive beside the archive still fails"  1 "$S"

printf '%s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ] && [ "$pass" -gt 0 ]
