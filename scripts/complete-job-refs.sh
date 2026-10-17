#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# complete-job-refs.sh — add job-level reusable-workflow refs, and the refs a
# workflow uses through its local composite actions, to actions.lock.
# `gh actions-lock` v0.1.6 is blind to job-level `uses:` (upstream #129; v0.2.0
# still is, re-measured 2026-10-14), so the
# entries it cannot write are written here. Idempotent; run from a repo root.
#
# Lock repair order (each step idempotent, run from a repo root):
#   gh actions-lock  →  scripts/relock-sha-keys.sh  →  scripts/complete-job-refs.sh
#   →  scripts/close-lock.sh  →  scripts/prune-stale.sh
# relock-sha-keys re-keys tag-spelled entries to the inline SHA the YAML pins
# (GitHub compares lock and YAML as LITERAL strings); complete-job-refs adds the
# job-level reusable refs the tool cannot see, and the refs used through local
# composite actions (standards#1122); close-lock adds a record for every
# dangling edge, and gives each job-level reusable ref's record the `uses:` of
# the workflow it calls; prune-stale drops per-workflow refs nothing uses.
set -euo pipefail
WF="${1:-.github/workflows}"
L="$WF/actions.lock"
[ -f "$L" ] || { echo "complete-job-refs: no $L" >&2; exit 1; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
WANT="$TMP/want"; NEW="$TMP/new.lock"
# Local composite actions (standards#1122). A local action (`uses: ./dir`) has
# no dependency record of its own, so the actions it uses belong in the CALLING
# workflow's entry. gh actions-lock writes them there; prune-stale used to drop
# them. Adding them here makes the chain produce them whatever the tool did.
WF_DIR="$WF"
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

# uses_values FILE: every `uses:` value in FILE, with a trailing comment, YAML
# quotes and a KYAML trailing comma stripped.
uses_values() {
  sed 's/[[:space:]]\+#.*$//' "$1" \
  | grep -E '^[[:space:]]*(-[[:space:]]+)?uses:[[:space:]]' \
  | sed -E 's/^[[:space:]]*(-[[:space:]]+)?uses:[[:space:]]+//' \
  | tr -d '"'"'"'' | awk '{print $1}' | sed 's/,$//'
}

# 1. enumerate each workflow's direct refs as well as reusable/composite refs.
# This completes workflow membership for actions already resolved elsewhere in
# the lock; it never resolves or changes a dependency digest. Offline verification
# must still reject any reference without its own dependency entry.
: > "$WANT"
for f in "$WF"/*.yml "$WF"/*.yaml; do
  [ -e "$f" ] || continue
  b=".github/workflows/$(basename "$f")"
  set +e
  {
    uses_values "$f"
    reach_files "$f" | tail -n +2 | while IFS= read -r a; do uses_values "$a"; done
  } | while read -r u; do
      case "$u" in ./*|\$/*|docker://*|'') continue;; esac
      case "$u" in *@*) : ;; *) continue;; esac
      p="${u%@*}"; r="${u##*@}"
      o=$(printf '%s' "$p" | cut -d/ -f1); n=$(printf '%s' "$p" | cut -d/ -f2)
      printf '%s\t%s/%s@%s\n' "$b" "$o" "$n" "$r"
    done
  set -e
done | LC_ALL=C sort -u >> "$WANT"

# 2. rewrite the workflows: section, merging wanted job refs into each path entry
LC_ALL=C awk -v want="$WANT" '
BEGIN { while ((getline l < want) > 0) { split(l, w, "\t"); WANT[w[1]] = WANT[w[1]] SUBSEP w[2] } }
# flush: write the current workflow entry, merged with its wanted refs, sorted and deduplicated.
function flush(   i, n, arr, j, k, seen, out, c) {
  if (cur == "") return
  SEEN[cur] = 1
  n = split(items, arr, SUBSEP)
  c = 0; delete seen; delete out
  for (i = 1; i <= n; i++) if (arr[i] != "" && !(arr[i] in seen)) { seen[arr[i]] = 1; out[++c] = arr[i] }
  if (cur in WANT) { n = split(WANT[cur], arr, SUBSEP)
    for (i = 1; i <= n; i++) if (arr[i] != "" && !(arr[i] in seen)) { seen[arr[i]] = 1; out[++c] = arr[i] } }
  if (c == 0) { printf "    '\''%s'\'': []\n", cur; cur = ""; items = ""; return }
  for (i = 1; i < c; i++) for (j = i + 1; j <= c; j++) if (out[j] < out[i]) { k = out[i]; out[i] = out[j]; out[j] = k }
  printf "    '\''%s'\'':\n", cur
  for (i = 1; i <= c; i++) printf "        - '\''%s'\''\n", out[i]
  cur = ""; items = ""
}
# rest: create entries for wanted workflow paths the lock has no entry for yet.
function rest(   k, ks, c, i, j, t) {
  c = 0; for (k in WANT) if (!(k in SEEN)) ks[++c] = k
  for (i = 1; i < c; i++) for (j = i + 1; j <= c; j++) if (ks[j] < ks[i]) { t = ks[i]; ks[i] = ks[j]; ks[j] = t }
  for (i = 1; i <= c; i++) { cur = ks[i]; items = ""; flush() }
}
/^workflows:[[:space:]]*$/ { print; inwf = 1; sawwf = 1; next }
inwf && /^[a-z_]+:/ { flush(); rest(); inwf = 0; print; next }
inwf {
  if (match($0, /^    '\''[^'\'']+'\'':/)) { flush()
    cur = $0; sub(/^    '\''/, "", cur); sub(/'\''.*$/, "", cur)
    items = ""
    next
  }
  if (match($0, /^        - '\''[^'\'']+'\''[[:space:]]*$/)) {
    it = $0; sub(/^        - '\''/, "", it); sub(/'\''[[:space:]]*$/, "", it)
    items = items SUBSEP it; next
  }
  next
}
{ print }
END {
  if (inwf) { flush(); rest() }
  if (!sawwf) for (k in WANT) { print "complete-job-refs: lock has no workflows: section" > "/dev/stderr"; exit 2 }
}
' "$L" > "$NEW"

added=$(LC_ALL=C comm -13 <(grep -oE "^        - '[^']+'" "$L" | sed "s/.*- '//;s/'$//" | LC_ALL=C sort -u) \
                 <(grep -oE "^        - '[^']+'" "$NEW" | sed "s/.*- '//;s/'$//" | LC_ALL=C sort -u) | wc -l)
cat "$NEW" > "$L"
echo "complete-job-refs: distinct refs newly present in workflows section: $added"
