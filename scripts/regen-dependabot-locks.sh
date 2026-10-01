#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# regen-dependabot-locks.sh — regenerate actions.lock on Dependabot PR branches.
#
# WHY THIS EXISTS
# ---------------
# Dependabot bumps the `uses:` refs in a workflow but never touches
# `.github/workflows/actions.lock`. The bump PR goes red on
# "Governance / Actions lockfile verify", is merged anyway (the check is not
# required), and main then drifts from its own lock — so every LATER pull
# request in that repository inherits a red it did not cause. Measured
# 2026-10-01 on sim-public-relations, cicd-squabbler, burble, stapeln and
# hypatia. This closes the loop at the source: the Dependabot PR is repaired
# before it can land.
#
# WHAT IT DOES, PER PULL REQUEST
# ------------------------------
# Selects open PRs authored by dependabot[bot], from a same-repo branch, that
# touch .github/workflows/, in a repository that already carries actions.lock.
# On a clone of the PR head it runs the estate lock repair order (see
# complete-job-refs.sh):
#   gh actions-lock → relock-sha-keys → complete-job-refs → close-lock → prune-stale
# then RESTORES every file except actions.lock, because `gh actions-lock`
# rewrite mode de-pins inline SHAs and can invent `uses: $/...` local refs.
# The result is committed only when:
#   * the ONLY changed path is .github/workflows/actions.lock,
#   * the lock names no `$/` ref and close-lock resolved every edge, and
#   * update-actions-lock.sh --verify-local (the gate's own verifier) passes.
# Anything else is reported and left for a human; nothing partial is written.
#
# The commit goes through GraphQL createCommitOnBranch with expectedHeadOid,
# authenticated as a GitHub App installation, so it is Verified (satisfies
# required_signatures) and refuses to land if Dependabot moved the branch
# meanwhile. A second run finds the lock current and does nothing.
#
# CREDENTIALS
# -----------
# One installation token per owner, passed as REGEN_TOKENS="owner=token ..."
# (the workflow mints them with actions/create-github-app-token). An empty set
# is reported as a warning and exits 0 — the job must not pretend it swept
# anything, and must not redden every schedule tick before the App exists.
#
# Usage: regen-dependabot-locks.sh [owner/repo]     (default: every repo the
#        installations can see).  DRY_RUN=1 regenerates but never commits.
# Env:   REGEN_TOKENS, DRY_RUN, GH_BIN (test stub), REGEN_TOOLS (dir holding
#        the repair scripts; default: this script's directory).
set -uo pipefail

GH_BIN="${GH_BIN:-gh}"
REGEN_TOOLS="${REGEN_TOOLS:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
LOCK_PATH=.github/workflows/actions.lock

# Print one outcome token for the checkout in $1, leaving actions.lock modified
# only when the token is `changed`: no-lock | composite-unsupported |
# tool-failed | unresolvable | corrupt | dirty | unverified | current | changed.
#
# composite-unsupported: prune-stale reads only workflow files, so it drops the
# per-workflow edges a local composite action (.github/actions/*) contributes,
# and --verify-local still reports such a lock valid (measured 2026-10-02 on
# standards' signed-push-smoke.yml; standards#1122). Left for a human.
regen_lock() {
  local dir="$1" changed
  (
    cd "$dir" || exit 1
    [ -f "$LOCK_PATH" ] || { echo no-lock; exit 0; }
    [ -d .github/actions ] && { echo composite-unsupported; exit 0; }
    if ! "$GH_BIN" actions-lock >&2; then echo tool-failed; exit 0; fi
    # Keep only the lock: undo every tool-authored workflow rewrite.
    git checkout --quiet -- . ":(exclude)$LOCK_PATH"
    git clean -fdq
    bash "$REGEN_TOOLS/relock-sha-keys.sh" >&2 || { echo tool-failed; exit 0; }
    bash "$REGEN_TOOLS/complete-job-refs.sh" >&2 || { echo tool-failed; exit 0; }
    GH_BIN="$GH_BIN" bash "$REGEN_TOOLS/close-lock.sh" >&2
    case $? in 0) ;; 3) echo unresolvable; exit 0 ;; *) echo tool-failed; exit 0 ;; esac
    bash "$REGEN_TOOLS/prune-stale.sh" >&2 || { echo tool-failed; exit 0; }
    if grep -Fq '$/' "$LOCK_PATH"; then echo corrupt; exit 0; fi
    changed="$(git status --porcelain --untracked-files=all)"
    if [ -n "$changed" ] && [ "$changed" != " M $LOCK_PATH" ]; then echo dirty; exit 0; fi
    if ! GH_BIN="$GH_BIN" bash "$REGEN_TOOLS/update-actions-lock.sh" --verify-local >&2; then
      echo unverified; exit 0
    fi
    if [ -z "$changed" ]; then echo current; else echo changed; fi
  )
}

# Commit the checkout's actions.lock onto branch $2 of repo $1, only if the
# branch head is still $3. Prints the new commit oid.
commit_lock() {
  local repo="$1" branch="$2" expected="$3" dir="$4" payload
  payload="$(jq -n \
    --arg repo "$repo" --arg branch "$branch" --arg oid "$expected" \
    --arg path "$LOCK_PATH" --arg contents "$(base64 -w0 < "$dir/$LOCK_PATH")" \
    '{query: "mutation($in: CreateCommitOnBranchInput!) { createCommitOnBranch(input: $in) { commit { oid } } }",
      variables: {in: {
        branch: {repositoryNameWithOwner: $repo, branchName: $branch},
        expectedHeadOid: $oid,
        message: {headline: "chore(deps): regenerate actions.lock for this bump",
                  body: "Dependabot updated workflow refs without the lockfile. Regenerated with the estate repair order (scripts/regen-dependabot-locks.sh in hyperpolymath/standards) and verified with update-actions-lock.sh --verify-local."},
        fileChanges: {additions: [{path: $path, contents: $contents}]}}}}')"
  printf '%s' "$payload" | "$GH_BIN" api graphql --input - --jq '.data.createCommitOnBranch.commit.oid'
}

# Read a pulls-list JSON array on stdin; print "number<TAB>head_ref<TAB>head_sha"
# for each open, non-draft Dependabot PR whose head branch is in the same repo.
select_prs() {
  jq -r '.[]
    | select(.user.login == "dependabot[bot]" and (.draft | not)
             and .head.repo.full_name == .base.repo.full_name)
    | [.number, .head.ref, .head.sha] | @tsv'
}

# Process every candidate PR of repo $1 with token $2; print one
# "repo#N<TAB>outcome" line per PR examined.
process_repo() {
  local repo="$1" token="$2" num ref sha work outcome oid err files prs
  export GH_TOKEN="$token"
  # Only a 404 means "no lock here". A rate limit or 5xx must stay visible, or
  # the repository is silently counted as examined.
  if ! err="$("$GH_BIN" api "repos/$repo/contents/$LOCK_PATH" --silent 2>&1)"; then
    case "$err" in *"HTTP 404"*) return 0 ;; esac
    printf '%s\tapi-error\n' "$repo"; return 0
  fi
  if ! prs="$("$GH_BIN" api --paginate "repos/$repo/pulls?state=open&per_page=100")"; then
    printf '%s\tapi-error\n' "$repo"; return 0
  fi
  while IFS=$'\t' read -r num ref sha; do
    [ -n "$num" ] || continue
    if ! files="$("$GH_BIN" api --paginate "repos/$repo/pulls/$num/files?per_page=100" --jq '.[].filename')"; then
      printf '%s#%s\tapi-error\n' "$repo" "$num"; continue
    fi
    printf '%s\n' "$files" | grep -q '^\.github/workflows/' || continue
    work="$(mktemp -d)"
    if ! git clone --quiet --depth 1 --branch "$ref" \
         "https://x-access-token:$token@github.com/$repo.git" "$work/r" 2>/dev/null; then
      printf '%s#%s\tclone-failed\n' "$repo" "$num"; rm -rf "$work"; continue
    fi
    if [ "$(git -C "$work/r" rev-parse HEAD)" != "$sha" ]; then
      printf '%s#%s\tmoved\n' "$repo" "$num"; rm -rf "$work"; continue
    fi
    outcome="$(regen_lock "$work/r")"
    if [ "$outcome" = changed ] && [ "${DRY_RUN:-0}" != 1 ]; then
      if oid="$(commit_lock "$repo" "$ref" "$sha" "$work/r")" && printf '%s' "$oid" | grep -Eq '^[0-9a-f]{40}$'; then
        outcome="committed $oid"
      else
        outcome="commit-refused"
      fi
    fi
    printf '%s#%s\t%s\n' "$repo" "$num" "$outcome"
    rm -rf "$work"
  done < <(printf '%s' "$prs" | jq -s 'add // []' | select_prs)
}

# Enumerate every repository visible to each owner's installation token (or
# only $1), process them, and print the denominator alongside the outcomes.
main() {
  local only="${1:-}" pair owner token repos total=0 seen tokens="${REGEN_TOKENS:-}"
  if [ -z "${tokens// /}" ]; then
    echo "::warning::regen-dependabot-locks: no App installation token (vars.APP_ID / secrets.APP_PRIVATE_KEY unset or the App is not installed). Nothing was examined."
    return 0
  fi
  for pair in $tokens; do
    owner="${pair%%=*}"; token="${pair#*=}"
    [ -n "$token" ] || { echo "::warning::no installation token for $owner — its repositories were NOT examined"; continue; }
    if [ -n "$only" ]; then
      [ "${only%%/*}" = "$owner" ] || continue
      repos="$only"
    else
      repos="$(GH_TOKEN="$token" "$GH_BIN" api --paginate 'installation/repositories?per_page=100' \
               --jq '.repositories[] | select(.archived | not) | .full_name')" || {
        echo "::error::could not enumerate $owner's installation repositories"; return 1; }
    fi
    seen=$(printf '%s\n' "$repos" | grep -c .)
    total=$((total + seen))
    echo "$owner: $seen repositories in scope"
    while IFS= read -r r; do
      [ -n "$r" ] && process_repo "$r" "$token"
    done <<< "$repos"
  done
  echo "examined $total repositories"
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  main "$@"
fi
