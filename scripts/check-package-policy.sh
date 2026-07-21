#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# check-package-policy.sh — gate on the Guix-primary / Nix-fallback policy.
#
# Replaces the echo-only "Enforce Guix primary / Nix fallback" step in
# governance-reusable.yml, whose every branch echoed and which terminated with
# an unconditional `✅ Package policy check passed` (standards#505). It could
# not detect a violation, and it claimed a pass over any input.
#
# POLICY (CLAUDE.md, "Package Management"): Guix primary (guix.scm), Nix
# fallback (flake.nix). A repo satisfying neither is the violation.
#
# PREDICATE — deliberately tightened. The previous step accepted *any* `*.scm`
# anywhere in the tree as proof of "Guix package management detected", which a
# stray Guile source file satisfies without any packaging existing. This script
# requires a genuine packaging artefact (guix.scm / manifest.scm / channels.scm
# / .guix-channel) and prunes vendored trees so a dependency's file cannot
# satisfy the policy on the repo's behalf. Measured 2026-07-21 over the 412 real
# repo-root callers, tightening moved only 13 repos (57 -> 70 failing), so the
# honest predicate is nearly free.
#
# GRACE WINDOW: 70/412 callers (17%) currently satisfy neither. They warn until
# the cutoff, then fail. The cutoff is a real date that flips itself with no
# further edit to this file.
#
# NOTE ON LOCKFILES: the replaced step also grepped `git diff HEAD~1` for
# package-lock.json / yarn.lock / Gemfile.lock / Pipfile.lock / poetry.lock.
# That branch was inert — governance checks out at `ref: github.sha` without
# `fetch-depth: 0`, so `HEAD~1` does not resolve and the command was swallowed
# by `2>/dev/null` / `|| true`. It is dropped here rather than kept as a stub:
# the blocking rule for lockfiles is hypatia `cicd_rules/nodejs_detected`.
# Broadening that rule beyond package-lock.json is tracked as follow-up.
#
# Usage: check-package-policy.sh [repo-root]
#
# Environment (test seams — the shipped policy is the default in each case):
#   ENFORCE_PACKAGE_POLICY_FROM  YYYY-MM-DD; enforcement begins ON this date.
#   PKG_TODAY                    YYYY-MM-DD; overrides "now" so the pre-cutoff
#                                and post-cutoff branches are both testable.
#
# Exit: 0 = pass (or in-grace warning), 1 = policy failure or bad configuration.

set -euo pipefail

ROOT="${1:-.}"

ENFORCE_PACKAGE_POLICY_FROM="${ENFORCE_PACKAGE_POLICY_FROM:-2026-08-21}"
TODAY="${PKG_TODAY:-$(date -u +%Y-%m-%d)}"

# An unparseable cutoff would make the comparison below pick the grace branch
# forever, restoring the fake gate this script replaces. Refuse to run.
valid_date() {
  case "$1" in
    [0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]) return 0 ;;
    *) return 1 ;;
  esac
}
require_date() {
  local name="$1" value="$2"
  if ! valid_date "$value"; then
    echo "::error::check-package-policy: $name='$value' is not YYYY-MM-DD."
    echo "Refusing to run: an unparseable cutoff would silently disarm this gate."
    exit 1
  fi
}
require_date ENFORCE_PACKAGE_POLICY_FROM "$ENFORCE_PACKAGE_POLICY_FROM"
require_date PKG_TODAY "$TODAY"

if [ ! -d "$ROOT" ]; then
  echo "::error::check-package-policy: '$ROOT' is not a directory."
  exit 1
fi

# Vendored trees cannot satisfy the policy on the repo's behalf.
PRUNE=( -name .git -o -name node_modules -o -name deps -o -name .lake -o -name vendor )

# NB: `find … | head -1` under `set -o pipefail` is a SIGPIPE race — head exits
# after the first line, find keeps writing, takes SIGPIPE, and pipefail
# propagates the non-zero status, aborting the script under `set -e`. It only
# manifests on trees large enough that find is still running when head exits, so
# small fixtures never catch it (it red two real estate repos intermittently
# while every unit fixture passed). `|| true` neutralises the pipeline status;
# the guix/nix decision is made from the captured value, not the exit code.
find_first() {
  local out
  out="$(find "$ROOT" \( "${PRUNE[@]}" \) -prune -o \( "$@" \) -print 2>/dev/null | head -1 || true)"
  printf '%s' "$out"
}

GUIX="$(find_first -name guix.scm -o -name manifest.scm -o -name channels.scm -o -name .guix-channel)"
NIX="$(find_first -name flake.nix -o -name default.nix -o -name shell.nix)"

if [ -n "$GUIX" ]; then
  echo "✅ Guix package management detected (primary): ${GUIX#"$ROOT"/}"
  exit 0
fi

if [ -n "$NIX" ]; then
  echo "✅ Nix package management detected (fallback): ${NIX#"$ROOT"/}"
  echo "::notice::Guix is the estate primary; Nix is the accepted fallback."
  exit 0
fi

# Violation: neither packaging system is present.
if [[ "$TODAY" < "$ENFORCE_PACKAGE_POLICY_FROM" ]]; then
  echo "::warning::No Guix or Nix packaging found — this becomes a BLOCKING" \
       "failure on $ENFORCE_PACKAGE_POLICY_FROM (today is $TODAY)."
  # Never claim a pass while the policy is unmet.
  echo "NOT YET ENFORCED: package policy unmet but inside the grace window."
  exit 0
fi

echo "::error::Package policy violation: no Guix or Nix packaging found."
echo
echo "Estate policy is Guix primary / Nix fallback. Add one of:"
echo "  guix.scm | manifest.scm | channels.scm | .guix-channel   (primary)"
echo "  flake.nix | default.nix | shell.nix                      (fallback)"
echo
echo "Files inside .git/ node_modules/ deps/ .lake/ vendor/ do not count."
exit 1
