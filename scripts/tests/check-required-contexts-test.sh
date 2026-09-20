#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-required-contexts-test.sh — fixture suite for
# scripts/check-required-contexts.sh, the unsatisfiable-required-context gate.
#
# The script carries its own hermetic fixture: a synthetic repository (a
# reusable-wrapper caller, a plain job and a matrix job), a stubbed reusable in a
# cache directory, and required-context sets supplied as JSON. No network, no
# `gh`, no token, no API — so this runs in every environment and cannot be
# green-while-broken because a credential was missing.
#
# Branches driven:
#   * a context the wrapper publishes                -> satisfiable, exit 0
#   * the reusable's bare inner name (the #17 shape) -> unsatisfiable, advisory
#                                                       exit 0, strict exit 1
#   * an unknown name bound to no app integration    -> strict exit 1
#   * a name that is only a PREFIX of a real one     -> strict exit 1
#     (guards against the substring-matching bug this suite was written to catch)
#
# Run: bash scripts/tests/check-required-contexts-test.sh
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GATE="$SCRIPT_DIR/../check-required-contexts.sh"

if [ ! -f "$GATE" ]; then
  echo "FAIL: gate not found at $GATE"
  exit 1
fi

if output="$(bash "$GATE" --self-test 2>&1)"; then
  printf '%s\n' "$output"
  echo "PASS $0"
  exit 0
fi

printf '%s\n' "$output"
echo "::error file=$GATE::fixture suite failed"
exit 1
