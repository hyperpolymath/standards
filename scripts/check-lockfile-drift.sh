#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -eo pipefail

# check-lockfile-drift.sh — detect the estate's recurring CI killer.
#
# ── The fault this catches ─────────────────────────────────────────────────
# Dependabot bumps an action version *in a workflow file*. Nobody regenerates
# `.github/workflows/actions.lock`. The workflow now requests a version the
# lockfile does not record, and GitHub rejects the run at `startup_failure`
# with ZERO jobs — no log, no annotation, nothing in REST or GraphQL.
#
# It is self-reinflicting: any repo with Dependabot AND a lockfile AND no
# regeneration step re-acquires the fault on every action bump. Measured
# 2026-08-25: 60 of 60 sampled repos have both. That is why repos appear to
# "go bad again" after being fixed — nobody broke them, the clock did.
#
# Proven on hypatia (standards session 2026-08-25): three drifted entries
# (docker/setup-buildx-action, github/codeql-action, taiki-e/install-action)
# discriminated PERFECTLY — every workflow using one was dead, every workflow
# using none was alive. Regeneration revived all four. See hypatia#723.
#
# ── What this is NOT ───────────────────────────────────────────────────────
# Three other lockfile failure modes exist and this script does not cover them
# (it reports mode 4 only, because that is the one that recurs on its own):
#   1. no actions.lock at all in an enforced repo → whole repo dead
#   2. lockfile present but a workflow has NO entry → that workflow dead
#   3. entry exists but UNDER-declares (unresolvable action → false `[]`)
#   4. entry exists but the VERSION has drifted    ← THIS SCRIPT
#
# ── Calibration: what a hit does and does not prove ────────────────────────
# A reported line is a genuine inconsistency — the workflow requests a version
# the lockfile does not record for that action. It is NOT proof the workflow is
# dead. Measured on hypatia after regeneration: `tests.yml` still carried an
# inline `actions/checkout@34e11487…` against a lockfile recording `v7.0.1`
# (→ 3d3c42e5…), and it *started* anyway. So treat output as "reconcile this",
# not "this is why CI is down". The strong claim is the converse and it holds:
# every workflow that WAS dead had a drifted entry.
#
# Usage:  check-lockfile-drift.sh [REPO_DIR]     (default: .)
# Output: TSV — repo <TAB> workflow <TAB> requested <TAB> locked
# Exit:   0 = no drift (or no lockfile — not this script's business)
#         1 = drift found
#         2 = usage / environment error

REPO_DIR="${1:-.}"
LOCK="$REPO_DIR/.github/workflows/actions.lock"
WFDIR="$REPO_DIR/.github/workflows"

[ -d "$WFDIR" ] || { echo "[drift] no .github/workflows in $REPO_DIR — nothing to check"; exit 0; }

# No lockfile is failure mode 1, not mode 4. Report and leave it alone: a repo
# outside the enforcement cohort is legitimately lockfile-free (proof-burrower
# has none and runs fine), so absence is NOT evidence of a fault.
[ -f "$LOCK" ] || { echo "[drift] no actions.lock in $REPO_DIR — out of scope (see mode 1)"; exit 0; }

drift=0
checked=0

for wf in "$WFDIR"/*.yml "$WFDIR"/*.yaml; do
  [ -f "$wf" ] || continue
  base="$(basename "$wf")"
  checked=$((checked + 1))

  # Actions the workflow actually requests. Two normalisations matter:
  #   * drop reusable-workflow calls (owner/repo/.github/workflows/x.yml@ref) —
  #     those legitimately carry a bare `[]` entry and are not drift
  #   * strip sub-action paths: `github/codeql-action/init@v1` is recorded in
  #     the lockfile as `github/codeql-action@v1`
  grep -oE "uses:[[:space:]]*[A-Za-z0-9_.-]+/[A-Za-z0-9_./-]+@[A-Za-z0-9._-]+" "$wf" 2>/dev/null \
    | sed -E 's/uses:[[:space:]]*//' \
    | grep -v '/\.github/workflows/' \
    | sed -E 's#^([^/]+/[^/@]+)(/[^@]*)?@#\1@#' \
    | sort -u > /tmp/_drift_want.$$ || true

  [ -s /tmp/_drift_want.$$ ] || { rm -f /tmp/_drift_want.$$; continue; }

  # Versions the lockfile records for THIS workflow.
  awk -v key="    '.github/workflows/$base':" '
    $0 == key            { on = 1; next }
    on && /^        - / { gsub(/^        - .|.$/, ""); print; next }
    on && NF && $0 !~ /^        / { exit }
  ' "$LOCK" | sort -u > /tmp/_drift_have.$$ || true

  while read -r want; do
    [ -n "$want" ] || continue
    grep -qxF "$want" /tmp/_drift_have.$$ && continue

    name="${want%@*}"
    ref="${want#*@}"

    # Only DRIFT if the lockfile knows this action at a *different* version.
    # Absent entirely is mode 2/3, or a verified-creator action that
    # legitimately needs no entry (e.g. Swatinem/rust-cache) — not drift.
    have="$(grep -m1 -F "$name@" /tmp/_drift_have.$$)" || continue

    # A workflow may pin by 40-char SHA while the lockfile records a TAG.
    # That is the same action in two notations, NOT drift. The lockfile
    # resolves every entry to `commit: 'sha1-<40hex>'`, so compare against
    # that rather than against the tag string.
    if printf '%s' "$ref" | grep -qE '^[0-9a-f]{40}$'; then
      resolved="$(awk -v k="    '$have':" '
        $0 == k { on = 1; next }
        on && /commit:/ { gsub(/.*sha1-|.$/, ""); print; exit }
        on && NF && $0 !~ /^        / { exit }
      ' "$LOCK")"
      [ "$resolved" = "$ref" ] && continue   # same commit, different notation
    fi

    printf '%s\t%s\t%s\t%s\n' "$(basename "$REPO_DIR")" "$base" "$want" "$have"
    drift=$((drift + 1))
  done < /tmp/_drift_want.$$

  rm -f /tmp/_drift_want.$$ /tmp/_drift_have.$$
done

if [ "$drift" -gt 0 ]; then
  echo "[drift] $drift drifted entry/entries across $checked workflow(s) in $REPO_DIR" >&2
  echo "[drift] fix: run \`gh actions-lock\` then restore SPDX to line 1" >&2
  exit 1
fi

echo "[drift] clean — $checked workflow(s) checked in $REPO_DIR"
exit 0
