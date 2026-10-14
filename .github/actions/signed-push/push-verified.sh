#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# push-verified.sh — re-push a branch's local commits as GitHub-"Verified"
# commits through the GraphQL createCommitOnBranch mutation.
#
# Replaces Asana/push-signed-commits, which was Python (banned here), had no
# licence file, pulled the tag-pinned actions/setup-python@v2 and ran an
# unpinned `pip install requests` at run time. The contract is the same:
#
#   * the remote branch must already exist and must not have diverged: every
#     commit on it must already be in the local branch;
#   * each local commit not on the remote is recreated, oldest first, with
#     its message, by one createCommitOnBranch call that names the remote
#     head it expects (expectedHeadOid), so a concurrent push fails closed;
#   * on success the local branch is reset to the remote, whose commits now
#     carry GitHub's authorship and verification.
#
# Two things are stricter than the Python action:
#   * file contents are base64'd from the raw blob, so binary files survive
#     (the Python action decoded them as UTF-8 text);
#   * createCommitOnBranch can express neither file modes nor symlinks nor
#     submodules, so a commit that adds or changes one is REFUSED rather than
#     silently flattened, and the pushed tree is compared with the local one.
#
# Usage: push-verified.sh BRANCH
# Env:   GH_TOKEN (App installation token), GITHUB_REPOSITORY (owner/repo),
#        REMOTE (default origin), GH_BIN (default gh; the test suite stubs it).
set -euo pipefail

BRANCH="${1:?usage: push-verified.sh BRANCH}"
REMOTE="${REMOTE:-origin}"
GH_BIN="${GH_BIN:-gh}"
REPO="${GITHUB_REPOSITORY:?GITHUB_REPOSITORY (owner/repo) is required}"

# die: report an error in Actions annotation form and stop.
die() { echo "::error::signed-push: $*" >&2; exit 1; }

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

git fetch --quiet "$REMOTE" "$BRANCH"
REMOTE_HEAD="$(git rev-parse "$REMOTE/$BRANCH")"
LOCAL_HEAD="$(git rev-parse --verify "refs/heads/$BRANCH")"

[ "$(git merge-base "$LOCAL_HEAD" "$REMOTE_HEAD")" = "$REMOTE_HEAD" ] \
  || die "$REMOTE/$BRANCH has commits that $BRANCH lacks; refusing to push over them"

mapfile -t COMMITS < <(git rev-list --reverse "$REMOTE_HEAD..$LOCAL_HEAD")
if [ "${#COMMITS[@]}" -eq 0 ]; then
  echo "signed-push: $BRANCH has no commits that $REMOTE/$BRANCH lacks; nothing to push"
  exit 0
fi

# changes_json: write the commit's file changes as {additions, deletions}
# JSON to $2. Renames arrive as a deletion plus an addition (--no-renames).
changes_json() {
  local c="$1" out="$2" meta path old_mode new_mode new_blob status n=0
  : > "$out.adds"; : > "$out.dels"
  while IFS= read -r -d '' meta && IFS= read -r -d '' path; do
    read -r old_mode new_mode _ new_blob status <<<"${meta#:}"
    case "$status" in
      D) jq -nc --arg p "$path" '{path: $p}' >> "$out.dels" ;;
      A|M|T)
        case "$new_mode" in
          100644|100755) ;;
          120000) die "$c adds or changes the symlink $path; createCommitOnBranch cannot express symlinks" ;;
          160000) die "$c adds or changes the submodule $path; createCommitOnBranch cannot express submodules" ;;
          *) die "$c gives $path the unsupported mode $new_mode" ;;
        esac
        if [ "$status" = A ] && [ "$new_mode" != 100644 ]; then
          die "$c adds $path as executable ($new_mode); createCommitOnBranch would drop the mode"
        fi
        if [ "$status" != A ] && [ "$old_mode" != "$new_mode" ]; then
          die "$c changes the mode of $path ($old_mode -> $new_mode); createCommitOnBranch cannot express modes"
        fi
        n=$((n + 1))
        git cat-file blob "$new_blob" | base64 -w0 > "$out.blob.$n"
        jq -nc --arg p "$path" --rawfile b "$out.blob.$n" '{path: $p, contents: $b}' >> "$out.adds"
        ;;
      *) die "$c has an unsupported change ($status) to $path" ;;
    esac
  done < <(git diff --raw -z --no-renames --no-abbrev "$c^" "$c")
  jq -n --slurpfile a "$out.adds" --slurpfile d "$out.dels" \
    '{additions: $a, deletions: $d}' > "$out"
}

# Validate and prepare EVERY commit before sending any, so a commit that
# cannot be expressed refuses the whole push instead of leaving it partial.
i=0
for c in "${COMMITS[@]}"; do
  [ "$(git rev-list --parents -n 1 "$c" | wc -w)" -eq 2 ] \
    || die "$c is a merge commit; createCommitOnBranch can only create single-parent commits"
  i=$((i + 1))
  changes_json "$c" "$WORK/changes.$i.json"
done

expected="$REMOTE_HEAD"
pushed=0
i=0
for c in "${COMMITS[@]}"; do
  i=$((i + 1))
  jq -n \
    --arg repo "$REPO" --arg branch "$BRANCH" --arg oid "$expected" \
    --arg headline "$(git log -1 --format=%s "$c")" \
    --arg body "$(git log -1 --format=%b "$c")" \
    --slurpfile fc "$WORK/changes.$i.json" \
    '{query: "mutation($in: CreateCommitOnBranchInput!) { createCommitOnBranch(input: $in) { commit { oid } } }",
      variables: {in: {
        branch: {repositoryNameWithOwner: $repo, branchName: $branch},
        expectedHeadOid: $oid,
        message: {headline: $headline, body: $body},
        fileChanges: $fc[0]}}}' > "$WORK/payload.json"
  new="$("$GH_BIN" api graphql --input "$WORK/payload.json" --jq '.data.createCommitOnBranch.commit.oid' 2>"$WORK/gh.err" || true)"
  if ! [[ "$new" =~ ^[0-9a-f]{40}$ ]]; then
    msg="createCommitOnBranch failed for $c: $(tr '\n' ' ' < "$WORK/gh.err")"
    if [ "$pushed" -gt 0 ]; then
      msg="$msg -- PARTIAL PUSH: $pushed of ${#COMMITS[@]} commit(s) are on $REMOTE/$BRANCH; reconcile by hand"
    fi
    die "$msg"
  fi
  expected="$new"
  pushed=$((pushed + 1))
  echo "signed-push: $c -> $new"
done

git fetch --quiet "$REMOTE" "$BRANCH"
[ "$(git rev-parse "$REMOTE/$BRANCH")" = "$expected" ] \
  || die "$REMOTE/$BRANCH moved after the push (expected $expected)"
[ "$(git rev-parse "$REMOTE/$BRANCH^{tree}")" = "$(git rev-parse "$LOCAL_HEAD^{tree}")" ] \
  || die "the pushed tree differs from the local one; $REMOTE/$BRANCH needs inspection"

if [ "$(git symbolic-ref --quiet --short HEAD || true)" = "$BRANCH" ]; then
  git reset --quiet --hard "$REMOTE/$BRANCH"
else
  git branch --quiet --force "$BRANCH" "$REMOTE/$BRANCH"
fi
echo "signed-push: pushed $pushed commit(s) to $REMOTE/$BRANCH as Verified; $BRANCH now matches it"
