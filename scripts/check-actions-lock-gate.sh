#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-actions-lock-gate.sh — the `actions-lock-verify` GATE (R2: SHA pins +
# actions.lock everywhere; spec 2026-09-02-cicd-regularisation-design §6.4).
#
# Three outcomes, never a silent pass:
#   lockfile present   → run the authoritative verifier (`gh actions-lock
#                        --verify-local` via scripts/update-actions-lock.sh) and
#                        propagate its exit status. A corrupted lock goes RED.
#   lockfile absent,   → RED: an unpinned `uses:` is a violation today, lock or
#   unpinned refs        no lock. A workflow the gate cannot read is RED too
#                        (UNEXAMINED), never counted as pinned.
#   lockfile absent,   → grace window: `::warning` + "NOT YET ENFORCED" and exit
#   all SHA-pinned       0 until ENFORCE_ACTIONS_LOCK_FROM; `::error` + exit 3
#                        from that date. The sweep (spec §10 step 5) lands the
#                        lockfiles before the date; the date makes the gate
#                        real without red-flooding 300 repos on day one.
#
# Exit contract (consumed by governance-reusable.yml's ledger exemption):
#   0 = pass (verified lock, or lockless+pinned inside the grace window)
#   1 = LIVE VIOLATION (unpinned refs, an UNEXAMINED workflow, or a
#       verifier-rejected lock) — never exempt
#   2 = infrastructure failure (no workflows dir, no verifier, no usable yq)
#       — never exempt
#   3 = missing-lock debt ONLY (lockless, every ref pinned, grace window
#       closed) — the single state the shrink-only ledger may excuse.
# Collapsing 3 into 1 would let the ledger wave unpinned refs and corrupt
# locks through with the debt it was built to excuse.
#
# Reading `uses:` (lockless branch): the YAML parser, never a line grep
# (YAML-POLICY Y-1). The anchored grep this replaces could not read a KYAML
# value: `uses: "a/b@<sha>",` failed its `@<sha>([[:space:]]|$)` test and a
# quoted `"./local"` missed its exemption, so a fully pinned KYAML workflow went
# RED (jaffascript#72, 2026-10-09). It also matched `uses:` text inside a `run:`
# body. Only top-level `*.yml`/`*.yaml` files are read: those are the files
# GitHub runs, and the verifier snapshots the same set. Requires mikefarah yq
# v4, which ships on GitHub-hosted ubuntu runners and which the R5 job of
# governance-reusable.yml already requires. A yq that cannot run the extraction
# on a known input is exit 2, so a wrong yq never reads as "every file broken".
#
# Test seams (used by scripts/tests/check-actions-lock-gate-test.sh):
#   LOCK_TODAY                 override today's date (YYYY-MM-DD)
#   ENFORCE_ACTIONS_LOCK_FROM  override the cutoff (default 2026-10-01)
#   ACTIONS_LOCK_VERIFIER      path to update-actions-lock.sh (default: sibling)
#   YQ_BIN                     yq to run (default: yq on PATH)
#
# Usage: check-actions-lock-gate.sh [WORKFLOWS_DIR]   (default .github/workflows)
set -uo pipefail

WF_DIR="${1:-.github/workflows}"
TODAY="${LOCK_TODAY:-$(date -u +%F)}"
ENFORCE_FROM="${ENFORCE_ACTIONS_LOCK_FROM:-2026-10-01}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VERIFIER="${ACTIONS_LOCK_VERIFIER:-$SCRIPT_DIR/update-actions-lock.sh}"

if [ ! -d "$WF_DIR" ]; then
  echo "::error::actions-lock gate: workflows directory not found: $WF_DIR"
  exit 2
fi

if [ -f "$WF_DIR/actions.lock" ]; then
  if [ ! -f "$VERIFIER" ]; then
    echo "::error::actions-lock gate: lockfile present but verifier not found at $VERIFIER"
    exit 2
  fi
  echo "Lockfile present: running the authoritative verifier ($VERIFIER --verify-local)."
  bash "$VERIFIER" --verify-local
  rc=$?
  if [ "$rc" -ne 0 ]; then
    echo "::error::actions-lock gate: lockfile verification FAILED (exit $rc). Regenerate with scripts/update-actions-lock.sh in the same PR as the uses: change."
    echo "Note: Per estate canon rule 10, actions MUST be SHA-pinned. A bare SHA must be accompanied by a version comment (e.g., '# v2.0.0') for traceability."
    exit "$rc"
  fi
  echo "Immutable direct and transitive lockfile coverage verified."
  exit 0
fi

# No lockfile. Unpinned refs are a violation regardless of the grace window.
YQ_BIN="${YQ_BIN:-yq}"
USES_EXPR='.. | select(tag == "!!map") | select(has("uses")) | .uses | select(tag == "!!str")'

# Prints every string `uses:` value in the workflow file $1, one per line, at
# step and job level alike, in block YAML and KYAML alike. Comments and `run:`
# bodies are never read. Exits non-zero when the file does not parse.
uses_refs() {
  "$YQ_BIN" -r "$USES_EXPR" "$1"
}

# Succeeds when $1 needs no inline SHA: a local action, a container image, or
# one of the two exemptions the grep this replaced carried
# (actions/github-script, and standards' own reusables).
exempt_ref() {
  case "$1" in
    ./* | docker://* | actions/github-script* | hyperpolymath/standards/*) return 0 ;;
    *) return 1 ;;
  esac
}

if ! command -v "$YQ_BIN" >/dev/null 2>&1; then
  echo "::error::actions-lock gate: yq not found ($YQ_BIN). It is required to read workflows (YAML-POLICY Y-1)."
  exit 2
fi
# Positive control: the extraction must find the one ref in a known input, in
# both spellings, before any verdict below is believed.
scratch="$(mktemp)"
trap 'rm -f "$scratch"' EXIT
printf 'jobs:\n  a:\n    steps:\n      - uses: o/r@v1\n' > "$scratch"
probe_block="$(uses_refs "$scratch" 2>&1)"
printf '{ jobs: { a: { steps: [ { uses: "o/r@v1", }, ], }, }, }\n' > "$scratch"
probe_kyaml="$(uses_refs "$scratch" 2>&1)"
if [ "$probe_block" != "o/r@v1" ] || [ "$probe_kyaml" != "o/r@v1" ]; then
  echo "::error::actions-lock gate: $YQ_BIN cannot read uses: refs from a known workflow (got '$probe_block' / '$probe_kyaml'). mikefarah yq v4 is required."
  exit 2
fi

unpinned=""
unexamined=""
checked=0
for wf in "$WF_DIR"/*.yml "$WF_DIR"/*.yaml; do
  [ -f "$wf" ] || continue
  checked=$((checked + 1))
  if ! refs="$(uses_refs "$wf" 2>"$scratch")"; then
    unexamined+="  $wf: not parseable as YAML: $(head -1 "$scratch")"$'\n'
    continue
  fi
  # An unclosed quote can swallow the rest of a file into one scalar and still
  # parse, leaving no jobs and no refs. GitHub cannot run that file, so an
  # empty ref list from it is not "pinned".
  if [ "$("$YQ_BIN" -r '.jobs | tag' "$wf" 2>/dev/null)" != '!!map' ]; then
    unexamined+="  $wf: parses, but has no jobs: map"$'\n'
    continue
  fi
  while IFS= read -r ref; do
    [ -n "$ref" ] || continue
    exempt_ref "$ref" && continue
    [[ "$ref" =~ @[0-9a-f]{40}$ ]] && continue
    unpinned+="  $wf: uses: $ref"$'\n'
  done <<< "$refs"
done
echo "Read $checked workflow file(s) in $WF_DIR with $YQ_BIN."

if [ -n "$unexamined" ]; then
  echo "::error::actions-lock gate: these workflows could not be read, so their refs are UNEXAMINED (never counted as pinned):"
  printf '%s' "$unexamined"
fi
if [ -n "$unpinned" ]; then
  echo "::error::actions-lock gate: no $WF_DIR/actions.lock AND these refs are not SHA-pinned:"
  printf '%s' "$unpinned"
  echo "  Prefer \`gh actions-lock\` (scripts/update-actions-lock.sh): it also locks the"
  echo "  transitive dependencies of composite actions, which an inline SHA cannot express."
fi
if [ -n "$unexamined" ] || [ -n "$unpinned" ]; then
  exit 1
fi

if [[ "$TODAY" < "$ENFORCE_FROM" ]]; then
  echo "::warning::actions-lock gate: no $WF_DIR/actions.lock. All refs are SHA-pinned, but the lockfile becomes REQUIRED on $ENFORCE_FROM (today is $TODAY). Run scripts/update-actions-lock.sh."
  echo "NOT YET ENFORCED: lockfile missing but inside the grace window."
  exit 0
fi

echo "::error::actions-lock gate: no $WF_DIR/actions.lock and the grace window closed on $ENFORCE_FROM (today is $TODAY). MISSING-LOCK DEBT (exit 3): run scripts/update-actions-lock.sh and commit the lockfile."
exit 3
