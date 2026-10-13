#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# signed-push-test.sh — fixture suite for .github/actions/signed-push/push-verified.sh.
#
# A stub `gh` stands in for GitHub: it applies each createCommitOnBranch payload
# to a local bare "remote", refusing it when expectedHeadOid is not the branch
# head, exactly as the real mutation does. The suite checks that local commits
# arrive in order with their messages and byte-identical contents (binary
# included), that deletions and renames survive, and that every refusal
# (divergence, a moved head, merges, modes, symlinks) happens before anything
# is pushed or is reported as a partial push.
#
# Run: bash scripts/tests/signed-push-test.sh
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET="${TARGET:-$SCRIPT_DIR/../../.github/actions/signed-push/push-verified.sh}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
pass=0; fail=0

# ok: record and print a passing assertion.
ok()  { echo "PASS: $1"; pass=$((pass + 1)); }
# bad: record and print a failing assertion.
bad() { echo "FAIL: $1"; fail=$((fail + 1)); }

export GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@example.invalid GIT_COMMITTER_NAME=t GIT_COMMITTER_EMAIL=t@example.invalid
export GITHUB_REPOSITORY=o/r

# The stub: apply a createCommitOnBranch payload to $STUB_REMOTE, print the oid.
cat > "$WORK/gh" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
[ "$1 $2" = "api graphql" ] || { echo "stub: unexpected call: $*" >&2; exit 2; }
payload="$4"
echo x >> "$STUB_CALLS"
in=$(jq -c '.variables.in' "$payload")
branch=$(jq -r '.branch.branchName' <<<"$in")
want=$(jq -r '.expectedHeadOid' <<<"$in")
head=$(git -C "$STUB_REMOTE" rev-parse "refs/heads/$branch")
[ -n "${STUB_FAIL_ON_CALL:-}" ] && [ "$(wc -l < "$STUB_CALLS")" -eq "$STUB_FAIL_ON_CALL" ] && {
  echo "stub: simulated outage" >&2; exit 1; }
[ "$want" = "$head" ] || { echo "Expected branch to point to $want but it did not" >&2; exit 1; }
wt=$(mktemp -d); git -C "$STUB_REMOTE" worktree add -q --detach "$wt" "$head"
jq -r '.fileChanges.deletions[].path' <<<"$in" | while IFS= read -r p; do rm -f "$wt/$p"; done
n=$(jq '.fileChanges.additions | length' <<<"$in")
for i in $(seq 0 $((n - 1))); do
  p=$(jq -r ".fileChanges.additions[$i].path" <<<"$in"); mkdir -p "$wt/$(dirname "$p")"
  jq -r ".fileChanges.additions[$i].contents" <<<"$in" | base64 -d > "$wt/$p"
done
msg=$(jq -r '.message.headline' <<<"$in"); body=$(jq -r '.message.body' <<<"$in")
git -C "$wt" add -A
git -C "$wt" -c user.name=github -c user.email=noreply@github.com commit -q --allow-empty -m "$msg" ${body:+-m "$body"}
new=$(git -C "$wt" rev-parse HEAD)
git -C "$STUB_REMOTE" update-ref "refs/heads/$branch" "$new" "$head"
git -C "$STUB_REMOTE" worktree remove --force "$wt"
echo "$new"
STUB
chmod +x "$WORK/gh"
export GH_BIN="$WORK/gh"

# fresh: a bare remote with branch b, and a clone with b checked out.
fresh() {
  rm -rf "$WORK/remote.git" "$WORK/clone"; : > "$WORK/calls"
  git init -q --bare -b main "$WORK/remote.git"
  git clone -q "$WORK/remote.git" "$WORK/clone" 2>/dev/null
  ( cd "$WORK/clone" && printf 'keep\n' > keep.txt && printf 'old\n' > gone.txt && printf 'move me\n' > from.txt \
    && git add -A && git commit -q -m base && git push -q origin HEAD:refs/heads/b 2>/dev/null && git checkout -q -b b origin/b 2>/dev/null )
  export STUB_REMOTE="$WORK/remote.git" STUB_CALLS="$WORK/calls"
}
# calls: how many mutations the stub has received since the last fresh.
calls() { wc -l < "$WORK/calls" | tr -d " "; }
# run: push branch b from the clone, capturing all output in $WORK/out.
run() { ( cd "$WORK/clone" && bash "$TARGET" b ) > "$WORK/out" 2>&1; }

# 1. The planted positive: three commits, every change shape.
fresh
( cd "$WORK/clone" || exit 1
  printf 'hello\n' > keep.txt && head -c 300 /dev/urandom > bin.dat && git add -A && git commit -q -m "one: text and binary" -m "a body line"
  git rm -q gone.txt && git commit -q -m "two: delete"
  git mv from.txt sub/to.txt 2>/dev/null || { mkdir sub; git mv from.txt sub/to.txt; }
  git commit -q -m "three: rename" )
LOCAL_TREE=$(git -C "$WORK/clone" rev-parse 'b^{tree}')
run; st=$?
[ "$st" = 0 ] && ok "three commits pushed" || bad "three commits pushed — $(cat "$WORK/out")"
[ "$(calls)" = 3 ] && ok "one mutation per commit" || bad "one mutation per commit (got $(calls))"
[ "$(git -C "$WORK/remote.git" rev-parse 'b^{tree}')" = "$LOCAL_TREE" ] && ok "remote tree identical, binary included" || bad "remote tree identical"
[ "$(git -C "$WORK/remote.git" log --format=%s b -3 | tr '\n' '|')" = "three: rename|two: delete|one: text and binary|" ] \
  && ok "messages kept, in order" || bad "messages kept, in order"
git -C "$WORK/remote.git" log --format=%b -1 'b~2' | grep -qx "a body line" && ok "commit body kept" || bad "commit body kept"
[ "$(git -C "$WORK/clone" rev-parse b)" = "$(git -C "$WORK/remote.git" rev-parse b)" ] && ok "local branch reset to the remote" || bad "local branch reset to the remote"
run && grep -q "nothing to push" "$WORK/out" && [ "$(calls)" = 3 ] && ok "second run pushes nothing" || bad "second run pushes nothing"

# 2. Diverged remote: refused before any mutation.
fresh
( cd "$WORK/clone" && printf 'x\n' > a.txt && git add a.txt && git commit -q -m local )
( d=$(mktemp -d) && git clone -q -b b "$WORK/remote.git" "$d" 2>/dev/null && cd "$d" && printf 'y\n' > other.txt && git add other.txt && git commit -q -m elsewhere && git push -q origin b 2>/dev/null )
run; st=$?
[ "$st" != 0 ] && grep -q "has commits that b lacks" "$WORK/out" && [ "$(calls)" = 0 ] && ok "diverged remote refused, nothing sent" || bad "diverged remote refused — $(cat "$WORK/out")"

# 3. The head moves mid-push: the second mutation is refused, reported as partial.
fresh
( cd "$WORK/clone" && for i in 1 2; do printf '%s\n' "$i" > n.txt && git add n.txt && git commit -q -m "c$i"; done )
STUB_FAIL_ON_CALL=2 run; st=$?
[ "$st" != 0 ] && grep -q "PARTIAL PUSH: 1 of 2" "$WORK/out" && ok "failure after one commit reported as a partial push" || bad "partial push reported — $(cat "$WORK/out")"
unset STUB_FAIL_ON_CALL

# 4. Shapes createCommitOnBranch cannot express are refused before any mutation.
for shape in exec symlink modechange merge; do
  fresh
  ( cd "$WORK/clone" || exit 1
    case $shape in
      exec) printf '#!/bin/sh\n' > run.sh && chmod +x run.sh && git add run.sh && git commit -q -m exec ;;
      symlink) ln -s keep.txt link && git add link && git commit -q -m symlink ;;
      modechange) chmod +x keep.txt && git add keep.txt && git commit -q -m mode ;;
      merge) git checkout -q -b side && printf 's\n' > s.txt && git add s.txt && git commit -q -m side \
             && git checkout -q b && printf 't\n' > t.txt && git add t.txt && git commit -q -m main-side \
             && git merge -q --no-edit side >/dev/null ;;
    esac )
  run; st=$?
  [ "$st" != 0 ] && [ "$(calls)" = 0 ] && [ "$(git -C "$WORK/remote.git" rev-parse b)" = "$(git -C "$WORK/clone" rev-parse origin/b)" ] \
    && ok "$shape refused before anything is pushed" || bad "$shape refused — $(cat "$WORK/out")"
done

echo "signed-push-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
