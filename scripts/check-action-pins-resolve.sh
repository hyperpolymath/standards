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
# ── Orphan pins: reachable ≠ consumable (issue #782) ───────────────────────
# For REUSABLE-WORKFLOW pins (owner/repo/.github/workflows/*.yml@sha), object
# existence is necessary but NOT sufficient. GitHub resolves a called workflow
# only at a commit reachable from the repo's default branch. A real commit
# object that is an ancestor of nothing — squash-merge orphan, deleted unmerged
# branch, remote-branch-only ref — answers 200 at the commits endpoint AND at
# contents/<path>?ref=…, yet Actions fails at graph resolution with
# "workflow was not found", reporting jobs.total_count == 0: no check run,
# often not even a red one. Measured in-estate 2026-09: four such SHAs
# (7fdc2705…, 892497fe…, 46960521…, plus the non-object 5b1d0022…) account
# for 61 dead workflow-run rows with ZERO alive rows — all passing this gate's
# old existence predicate.
#
# So reusable-workflow pins get a second probe, server-side:
#     compare/<default>...<sha>  = behind|identical → ancestor; consumable
#                                = ahead|diverged   → NOT an ancestor of the
#                                                     default branch; a
#                                                     DETERMINATE negative
# Local forms are unusable: for-each-ref --contains passes remote-branch
# orphans, and merge-base --is-ancestor lies under shallow clones. The compare
# call is one request and needs no clone at all. (The pin-writer,
# scripts/apply-workflow-pins-remote.sh, already enforces the same rule with
# compare/<sha>...main ∈ {identical, ahead} — identical semantics, reversed
# direction.)
#
# The ancestry probe applies ONLY to reusable-workflow pins: ordinary action
# pins (@sha on an action repo) are fetched by object id at run time, and
# non-default-branch commits are a working, legitimate pattern there.
#
# Rate limiting is not expected to bite: with GITHUB_TOKEN the limit is 1,000
# requests/hour/repo, and the largest estate repo carries well under 100 unique
# pins (only unique (repo,sha) pairs are queried, not every occurrence).
#
# USAGE:  check-action-pins-resolve.sh [path]     # default: current directory
#         GH_TOKEN / GITHUB_TOKEN respected for auth.
# EXIT:   0 = all pins consumable (or only indeterminate results)
#         1 = at least one pin determinately unusable: does not exist, or is
#             an orphan no default-branch ref can reach (reusable pins only)

TARGET="${1:-.}"
WORKFLOW_DIR="$TARGET/.github/workflows"

if [ ! -d "$WORKFLOW_DIR" ]; then
  echo "No .github/workflows/ in $TARGET — nothing to check."
  exit 0
fi

# ── Collect unique (repo, sha, kind) pairs ──────────────────────────────────
# Handles `owner/repo@sha` and `owner/repo/sub/path@sha` (reusable workflows
# and composite subpaths both pin at the repository level).
# Skips local (`./`) and docker:// refs, which have no upstream commit.
# kind = R: the ref points at a reusable workflow file (.github/workflows/*.yml
# in the repo) — those get the ancestry probe (see header); kind = A otherwise.
pairs="$(
  grep -rhoE '\buses:[[:space:]]*[A-Za-z0-9_.-]+/[A-Za-z0-9_./-]+@[0-9a-f]{40}' \
    "$WORKFLOW_DIR" 2>/dev/null \
  | sed -E 's/.*uses:[[:space:]]*//' \
  | awk -F'@' '{ split($1, p, "/"); k = ($1 ~ /\.github\/workflows\/[^\/]+\.ya?ml$/) ? "R" : "A"; print p[1] "/" p[2] "\t" $2 "\t" k }' \
  | sort -u
)"

if [ -z "$pairs" ]; then
  echo "No SHA-pinned external actions found — nothing to check."
  exit 0
fi

total=$(printf '%s\n' "$pairs" | wc -l | tr -d ' ')
echo "Checking $total unique action pin(s) resolve upstream…"

api() { # api <path> — HTTP=response code, BODY=response body (both globals).
  # The ancestry probe below needs the body (compare status, default_branch),
  # so the code is captured via -w on the final line of stdout.
  local path="$1" auth=() out
  [ -n "${GH_TOKEN:-${GITHUB_TOKEN:-}}" ] && \
    auth=(-H "Authorization: Bearer ${GH_TOKEN:-$GITHUB_TOKEN}")
  out="$(curl -sS -w $'\n%{http_code}' \
    -H "Accept: application/vnd.github+json" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "${auth[@]}" "https://api.github.com/$path" 2>/dev/null)" || out=""
  # Newline test must be a BUILTIN: `printf | grep -q` under pipefail lets
  # grep exit on the first match and kill printf with SIGPIPE for any body
  # over the 64 KB pipe buffer, turning HTTP into the whole response blob
  # (measured live: 146 KB codeql-action compare body).
  if [ "$out" != "${out%$'\n'*}" ]; then
    HTTP="${out##*$'\n'}"
    BODY="${out%$'\n'*}"
  elif [ -n "$out" ]; then
    HTTP="$out"  # bare code from a minimal server (test stub): no body
    BODY=""
  else
    HTTP="000"; BODY=""
  fi
}

# json_field <name> — first string-valued "name":"value" pair in $BODY.
# Sufficient here: repos/<repo> defines default_branch exactly once, and in
# /compare the top-level status precedes the commits/files arrays, so the
# first occurrence IS the verdict. Deliberately no jq dependency: this gate
# also runs in minimal local shells.
json_field() {
  printf '%s\n' "$BODY" \
    | grep -oE "\"$1\"[[:space:]]*:[[:space:]]*\"[^\"]+\"" \
    | head -1 \
    | sed -E 's/^.*:[[:space:]]*"([^"]+)"$/\1/'
}

# default_branch <repo> — echo the repo's default branch, or "" if the probe
# was indeterminate. pairs arrive repo-sorted (sort -u above), so a one-entry
# cache hits every adjacent repeat exactly.
last_db_repo=""; last_db=""
default_branch() {
  local repo="$1"
  if [ "$last_db_repo" != "$repo" ]; then
    last_db_repo="$repo"; last_db=""
    api "repos/$repo"
    [ "$HTTP" = "200" ] && last_db="$(json_field default_branch)"
  fi
  printf '%s' "$last_db"
}

bad=0
unverified=0
bad_list=""
unver_list=""

while IFS=$'\t' read -r repo sha kind; do
  [ -z "$repo" ] && continue

  api "repos/$repo/commits/$sha"
  case "$HTTP" in
    200)
      if [ "$kind" = "R" ]; then
        # Reusable-workflow pin (issue #782): the object exists, but Actions
        # will still refuse it at graph resolution unless it is an ancestor of
        # the repo's default branch. Probe ancestry server-side.
        db="$(default_branch "$repo")"
        if [ -z "$db" ]; then
          unverified=$((unverified + 1))
          unver_list="${unver_list}  ancestry: default-branch probe indeterminate — $repo@$sha"$'\n'
        else
          api "repos/$repo/compare/$db...$sha"
          status=""
          [ "$HTTP" = "200" ] && status="$(json_field status)"
          case "$status" in
            behind|identical)
              : # ancestor of the default branch — consumable
              ;;
            ahead|diverged)
              # Determinate negative: a real commit the resolver cannot reach.
              # ahead/diverged => NOT an ancestor of $db (an ancestor would
              # report behind/identical).
              bad=$((bad + 1))
              bad_list="${bad_list}  NOT-ANCESTOR    $repo@$sha  (compare $db → ${status})"$'\n'
              ;;
            *)
              unverified=$((unverified + 1))
              unver_list="${unver_list}  ancestry: compare probe indeterminate (HTTP $HTTP) — $repo@$sha  (base: $db)"$'\n'
              ;;
          esac
        fi
      fi
      # Action pins (kind=A): object existence is sufficient at run time.
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
  echo "  · NOT-ANCESTOR   — the reusable-workflow pin names a real commit that"
  echo "                     the resolver cannot reach from the default branch"
  echo "                     (squash-merge orphan, deleted unmerged branch, or"
  echo "                     remote-branch-only ref — 61 estate rows dead this way,"
  echo "                     issue #782). Repin to a commit ON the default branch:"
  echo "                     the merge commit, never a PR head — a PR head orphans"
  echo "                     at squash-merge — and re-run this gate to confirm."
  exit 1
fi

verified=$((total - unverified))
echo "All $verified verifiable action pin(s) resolve upstream."
exit 0
