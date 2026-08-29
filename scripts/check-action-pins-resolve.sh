#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
set -uo pipefail

# check-action-pins-resolve.sh — verify every SHA-pinned action actually EXISTS.
#
# ── Why this gate exists ────────────────────────────────────────────────────
# The governance linter's "Check locked or SHA-pinned actions" step delegates
# lockfile integrity to gh actions-lock. For repositories without a lockfile it
# verifies the *shape* of each direct pin (`@` + 40 hex chars), but cannot tell
# a real commit from an invented one: a fabricated SHA is still well formed.
#
# That gap is not theoretical. Measured across the estate on 2026-07-28:
#
#     613 unique (action, SHA) pins  →  112 (18%) DO NOT RESOLVE
#       ·  80 = real action repo, SHA does not exist
#       ·  32 = the action repository itself is gone (7 distinct actions)
#     …present in 876 COMMITTED workflow files across ~310 repo roots.
#
# The failure mode is silent by design of the platform: Actions only resolves
# a `uses:` ref at RUN time, and an unresolvable ref produces **no check run at
# all** — not a red one. So `gh pr checks` shows nothing, the board looks
# green, and the job never ran. A repo can be "fully green" with its security
# scanning entirely absent. See dev-notes/estate-unresolvable-action-pins-*.md.
#
# This script closes that gap by asking GitHub whether each pin resolves.
#
# ── Failure semantics (deliberate, not handwaving) ──────────────────────────
# HARD FAIL on a *determinate negative* — GitHub answered, and the answer was
# "this does not exist":
#   · 422/404 from the commits endpoint while the repo itself resolves
#     → invented SHA.
#   · 404 from the repo endpoint → dead/renamed/private action repo. (Note
#     that Actions does NOT follow repo renames in `uses:`, so a rename is a
#     genuine break, not a cosmetic one.)
#
# DO NOT FAIL on an *indeterminate* answer — rate limiting, 5xx, network loss.
# Those say nothing about the pin. Failing on them would convert any GitHub
# incident into an estate-wide red treadmill, the same trap the staleness gate
# fell into (see check-workflow-staleness.sh). Instead they are counted and
# reported LOUDLY as UNVERIFIED so the gap is visible rather than silently
# green — a fail-open that announces itself is not a fake gate; a fail-open
# that hides is.
#
# Rate limiting is not expected to bite: with GITHUB_TOKEN the limit is 1,000
# requests/hour/repo, and the largest estate repo carries well under 100 unique
# pins (only unique (repo,sha) pairs are queried, not every occurrence).
#
# USAGE:  check-action-pins-resolve.sh [path]     # default: current directory
#         GH_TOKEN / GITHUB_TOKEN respected for auth.
# EXIT:   0 = all pins resolve (or only indeterminate results)
#         1 = at least one pin determinately does not exist

TARGET="${1:-.}"
WORKFLOW_DIR="$TARGET/.github/workflows"

if [ ! -d "$WORKFLOW_DIR" ]; then
  echo "No .github/workflows/ in $TARGET — nothing to check."
  exit 0
fi

# ── Collect unique (repo, sha) pairs ────────────────────────────────────────
# Handles `owner/repo@sha` and `owner/repo/sub/path@sha` (reusable workflows
# and composite subpaths both pin at the repository level).
# Skips local (`./`) and docker:// refs, which have no upstream commit.
pairs="$(
  grep -rhoE '\buses:[[:space:]]*[A-Za-z0-9_.-]+/[A-Za-z0-9_./-]+@[0-9a-f]{40}' \
    "$WORKFLOW_DIR" 2>/dev/null \
  | sed -E 's/.*uses:[[:space:]]*//' \
  | awk -F'@' '{ split($1, p, "/"); print p[1] "/" p[2] "\t" $2 }' \
  | sort -u
)"

if [ -z "$pairs" ]; then
  echo "No SHA-pinned external actions found — nothing to check."
  exit 0
fi

total=$(printf '%s\n' "$pairs" | wc -l | tr -d ' ')
echo "Checking $total unique action pin(s) resolve upstream…"

api() { # api <path> -> prints body, returns curl-visible HTTP code in $HTTP
  local path="$1" auth=()
  [ -n "${GH_TOKEN:-${GITHUB_TOKEN:-}}" ] && \
    auth=(-H "Authorization: Bearer ${GH_TOKEN:-$GITHUB_TOKEN}")
  HTTP="$(curl -sS -o /dev/null -w '%{http_code}' \
    -H "Accept: application/vnd.github+json" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "${auth[@]}" "https://api.github.com/$path" 2>/dev/null)" || HTTP="000"
}

bad=0
unverified=0
bad_list=""
unver_list=""

while IFS=$'\t' read -r repo sha; do
  [ -z "$repo" ] && continue

  api "repos/$repo/commits/$sha"
  case "$HTTP" in
    200)
      : # resolves — good
      ;;
    404|422)
      # Determinate negative from the commits endpoint. Disambiguate:
      # is the SHA missing, or the whole repository?
      api "repos/$repo"
      if [ "$HTTP" = "200" ]; then
        bad=$((bad + 1))
        bad_list="${bad_list}  SHA-NOT-FOUND   $repo@$sha"$'\n'
      elif [ "$HTTP" = "404" ]; then
        bad=$((bad + 1))
        bad_list="${bad_list}  REPO-NOT-FOUND  $repo (pinned @$sha)"$'\n'
      else
        # Could not confirm the repo either way — treat as indeterminate.
        unverified=$((unverified + 1))
        unver_list="${unver_list}  HTTP $HTTP on repos/$repo — $repo@$sha"$'\n'
      fi
      ;;
    *)
      # 403 (rate limit), 5xx, 000 (network) — says nothing about the pin.
      unverified=$((unverified + 1))
      unver_list="${unver_list}  HTTP $HTTP — $repo@$sha"$'\n'
      ;;
  esac
done <<< "$pairs"

echo

if [ "$unverified" -gt 0 ]; then
  echo "::warning::UNVERIFIED: $unverified of $total pin(s) could not be checked"
  echo "  (rate limit, 5xx, or network — NOT evidence the pins are bad)"
  printf '%s' "$unver_list"
  echo "  These were NOT counted as failures. Re-run to confirm."
  echo
fi

if [ "$bad" -gt 0 ]; then
  echo "::error::$bad of $total action pin(s) DO NOT EXIST upstream."
  printf '%s' "$bad_list"
  echo
  echo "An unresolvable 'uses:' produces NO check run — the job silently never"
  echo "runs, so the board looks green while the gate is absent. Fix by:"
  echo "  · SHA-NOT-FOUND  — repin to a real SHA. Resolve the intended tag with"
  echo "                     git ls-remote <repo> refs/tags/vN   (use the ^{} value)"
  echo "  · REPO-NOT-FOUND — the action is gone. Vendor the logic into this repo"
  echo "                     and call it with 'run:' (see hyperpolymath/tangle#84),"
  echo "                     or repoint at the live repository name."
  exit 1
fi

verified=$((total - unverified))
echo "All $verified verifiable action pin(s) resolve upstream."
exit 0
