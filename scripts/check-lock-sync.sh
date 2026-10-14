#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# check-lock-sync.sh — verify .github/workflows/actions.lock is in sync with the
# workflow YAML, in BOTH directions, including job-level reusable-workflow refs.
#
# This is the estate-wide template from hyperpolymath/standards for issue #968.
# Adapted from metadatastician/burble#224.
#
# Why this exists rather than `gh actions-lock --verify-local` alone:
#   `gh actions-lock` v0.1.6 cannot see a job-level `uses:` (a reusable-workflow
#   call). Measured on this repo, 2026-09-22:
#     * mirror.yml's lock entry was stale by a whole SHA and died at startup with
#       jobs=0, yet --verify-local reported "All 30 workflows have complete
#       lockfile coverage" and fix mode did not repair it;
#     * release.yml's job-level slsa-github-generator ref was absent from the
#       lock, again unreported; and once added, --verify-local calls it `stale`
#       with "no uses: in this workflow references it" — about a ref on line 144.
#   GitHub's own startup enforcement DOES check those refs (that is what killed
#   mirror.yml), so the tool is wrong in both directions and cannot be the
#   authority. This script is.
#
# A workflow's `uses:` set includes the `uses:` of every LOCAL composite action
# it calls (`uses: ./.github/actions/x`), transitively: a local action has no
# dependency record of its own, so its actions are locked under the calling
# workflow, which is where `gh actions-lock` writes them (standards#1122).
#
# Exit 0 only when every workflow's `uses:` set equals its lockfile set exactly.
# Any mismatch exits 1. There is no warn-only mode: a desync means GitHub will
# refuse to start the run, so it must fail the job.

set -euo pipefail

WF_DIR="${1:-.github/workflows}"
LOCK="$WF_DIR/actions.lock"

if [ ! -f "$LOCK" ]; then
  echo "check-lock-sync: FATAL: no lockfile at $LOCK" >&2
  exit 1
fi

shopt -s nullglob
mapfile -t WORKFLOWS < <(printf '%s\n' "$WF_DIR"/*.yml "$WF_DIR"/*.yaml | sort -u)
if [ "${#WORKFLOWS[@]}" -eq 0 ]; then
  echo "check-lock-sync: FATAL: no workflow files under $WF_DIR" >&2
  exit 1
fi

# ── local composite actions (identical block in check-lock-sync.sh, ─────────
# ── prune-stale.sh and complete-job-refs.sh; they must agree) ─────────────────
# reach_files WORKFLOW prints WORKFLOW and every local composite action file
# (`uses: ./dir` -> dir/action.yml or dir/action.yaml) it reaches, transitively,
# each once. A local reusable workflow (`./….yml`) is a lock entry of its own,
# so it is not followed. Paths resolve from the repository root, as GitHub
# resolves them from the workspace.
LC_ROOT="$(cd "$WF_DIR/../.." 2>/dev/null && pwd)" || LC_ROOT=.
# local_action_paths FILE: the path after `./` of every local `uses:` in FILE.
local_action_paths() {
  awk -v q="'" '{
    line = $0; sub(/[[:space:]]+#.*$/, "", line)
    if (!match(line, /^[[:space:]]*-?[[:space:]]*uses:[[:space:]]*/)) next
    raw = substr(line, RLENGTH + 1)
    sub("[\"" q "],[[:space:]]*$", "", raw)
    gsub("^[\"" q "]|[\"" q "]$", "", raw); sub(/[[:space:]]+$/, "", raw)
    if (substr(raw, 1, 2) == "./") print substr(raw, 3)
  }' "$1"
}
reach_files() {
  local -A seen=()
  local queue=("$1") f rel a
  while [ "${#queue[@]}" -gt 0 ]; do
    f="${queue[0]}"; queue=("${queue[@]:1}")
    [ -n "${seen[$f]:-}" ] && continue
    seen[$f]=1
    printf '%s\n' "$f"
    while IFS= read -r rel; do
      rel="${rel%/}"
      case "$rel" in *.yml|*.yaml) continue ;; esac
      for a in "$LC_ROOT/$rel/action.yml" "$LC_ROOT/$rel/action.yaml"; do
        if [ -f "$a" ]; then queue+=("$a"); break; fi
      done
    done < <(local_action_paths "$f")
  done
}
# ── end of the shared block ───────────────────────────────────────────────────

# reach map: "file<TAB>workflow" — each file whose uses: count for that workflow.
MAP="$(mktemp)"; trap 'rm -f "$MAP"' EXIT
for wf in "${WORKFLOWS[@]}"; do
  while IFS= read -r f; do printf '%s\t%s\n' "$f" "$wf"; done < <(reach_files "$wf")
done > "$MAP"
mapfile -t SCAN < <(cut -f1 "$MAP" | LC_ALL=C sort -u)

awk -v lockfile="$LOCK" -v mapfile="$MAP" '
BEGIN {
  # owners[file]: the workflows whose uses: set includes this file. WFS: the
  # workflows themselves, in glob order (a workflow is its own first owner).
  while ((getline l < mapfile) > 0) {
    split(l, mp, "\t")
    owners[mp[1]] = owners[mp[1]] SUBSEP mp[2]
    if (mp[1] == mp[2]) WFS[++nw] = mp[2]
  }
}
# owner/repo[/subpath...]@ref  ->  owner/repo@ref   ("" if not an external ref)
function norm(r,   at, path, ref, n, parts) {
  at = 0
  for (n = length(r); n > 0; n--) { if (substr(r, n, 1) == "@") { at = n; break } }
  if (at == 0) return ""
  path = substr(r, 1, at - 1); ref = substr(r, at + 1)
  if (path == "" || ref == "") return ""
  if (substr(path, 1, 2) == "./" || substr(path, 1, 2) == "$/") return ""   # local action
  if (split(path, parts, "/") < 2) return ""
  # GitHub owner/repository names are case-insensitive; refs are not.
  return tolower(parts[1] "/" parts[2]) "@" ref
}

# ---------- pass 1: the lockfile ----------
FILENAME == lockfile {
  if ($0 ~ /^workflows:[[:space:]]*$/) { inwf = 1; next }
  if ($0 ~ /^[a-z_]+:/)                { inwf = 0; next }
  if (!inwf) next

  # "    '.github/workflows/x.yml':"  or  "... : []"
  if (match($0, /^    '"'"'([^'"'"']+)'"'"':/, m)) {
    cur = m[1]
    seen_path[cur] = 1
    if ($0 ~ /\[\][[:space:]]*$/) cur_has_inline_empty = 1
    next
  }
  if (match($0, /^        - '"'"'([^'"'"']+)'"'"'[[:space:]]*$/, m) && cur != "") {
    lock[cur, norm(m[1])] = 1
    lockcount[cur]++
    next
  }
  next
}

# ---------- pass 2: the workflow YAML and the local composites it reaches ----------
# Each file credits its uses: to every workflow that reaches it.
FNR == 1 { nown = split(substr(owners[FILENAME], 2), own, SUBSEP) }
{
  line = $0
  sub(/[[:space:]]+#.*$/, "", line)              # strip trailing comment
  if (match(line, /^[[:space:]]*-?[[:space:]]*uses:[[:space:]]*(.+)$/, m)) {
    raw = m[1]
    # KYAML puts a comma after a quoted scalar. Strip the YAML delimiter
    # before unquoting, otherwise the closing quote becomes part of the ref.
    sub(/["\x27],[[:space:]]*$/, "", raw)
    gsub(/^["'"'"']|["'"'"']$/, "", raw)
    gsub(/[[:space:]]+$/, "", raw)
    if (raw ~ /^\$\//) {                           # known corruption
      for (i = 1; i <= nown; i++) dollar[own[i]] = dollar[own[i]] " " raw
      next
    }
    n = norm(raw)
    if (n != "") for (i = 1; i <= nown; i++) { uses[own[i], n] = 1; useslist[own[i]] = useslist[own[i]] " " n }
  }
}

END {
  bad = 0
  for (w = 1; w <= nw; w++) {
    wf = WFS[w]
    key = wf
    sub(/.*\//, "", key)
    key = ".github/workflows/" key          # the lockfile always uses this canonical path

    if (dollar[wf] != "") {
      printf "FAIL %s\n     invalid local-action rewrite (uses: $/...):%s\n", key, dollar[wf]
      bad = 1
    }

    # --- direction 1: every uses: must be locked under THIS path ---
    nu = split(useslist[wf], u, " ")
    delete uniq; missing = ""
    for (j = 1; j <= nu; j++) {
      if (u[j] == "" || (u[j] in uniq)) continue
      uniq[u[j]] = 1
      if (!((key SUBSEP u[j]) in lock)) missing = missing " " u[j]
    }
    if (missing != "") {
      if (!(key in seen_path))
        printf "FAIL %s\n     not onboarded: no lockfile entry for this path\n     unlocked refs:%s\n", key, missing
      else
        printf "FAIL %s\n     refs missing from the lockfile:%s\n", key, missing
      bad = 1
    }

    # --- direction 2: every lock entry must be referenced by this workflow ---
    orphan = ""
    for (k in lock) {
      split(k, kp, SUBSEP)
      if (kp[1] != key) continue
      if (!((wf SUBSEP kp[2]) in uses)) orphan = orphan " " kp[2]
    }
    if (orphan != "") {
      printf "FAIL %s\n     stale lockfile entries, no uses: references them:%s\n", key, orphan
      bad = 1
    }
  }

  # --- lockfile entries for workflow files that no longer exist ---
  for (p in seen_path) {
    found = 0
    for (w = 1; w <= nw; w++) {
      q = WFS[w]
      sub(/.*\//, "", q); q = ".github/workflows/" q
      if (q == p) { found = 1; break }
    }
    if (!found) { printf "FAIL %s\n     lockfile entry for a workflow file that does not exist\n", p; bad = 1 }
  }

  if (bad) {
    print ""
    print "actions.lock is OUT OF SYNC with the workflow YAML."
    print "GitHub refuses such a run at startup: zero jobs are created and the run"
    print "reports \"This run likely failed because of a workflow file issue.\""
    print "Fix: run `gh actions-lock --no-migrate-local-actions`, then review the diff"
    print "(it does not handle job-level reusable-workflow refs, and it can de-pin"
    print "bare SHAs to floating tags - both must be corrected by hand)."
    exit 1
  }
  print "actions.lock is in sync: every uses: is locked under its own workflow path,"
  print "and every lockfile entry is referenced. Job-level reusable-workflow refs and"
  print "refs used through local composite actions included."
}
' "$LOCK" "${SCAN[@]}"
