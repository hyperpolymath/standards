#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# complete-job-refs.sh — add job-level reusable-workflow refs to actions.lock.
# `gh actions-lock` v0.1.6 is blind to job-level `uses:` (upstream #129), so the
# entries it cannot write are written here. Idempotent; run from a repo root.
#
# Lock repair order (each step idempotent, run from a repo root):
#   gh actions-lock  →  scripts/relock-sha-keys.sh  →  scripts/complete-job-refs.sh
#   →  scripts/close-lock.sh  →  scripts/prune-stale.sh
# relock-sha-keys re-keys tag-spelled entries to the inline SHA the YAML pins
# (GitHub compares lock and YAML as LITERAL strings); complete-job-refs adds the
# job-level reusable refs the tool cannot see; close-lock adds a record for every
# dangling edge; prune-stale drops per-workflow refs nothing uses.
set -euo pipefail
WF="${1:-.github/workflows}"
L="$WF/actions.lock"
[ -f "$L" ] || { echo "complete-job-refs: no $L" >&2; exit 1; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
WANT="$TMP/want"; NEW="$TMP/new.lock"
# 1. enumerate job-level refs actually present in the YAML, as "path<TAB>owner/repo@ref"
: > "$WANT"
for f in "$WF"/*.yml "$WF"/*.yaml; do
  [ -e "$f" ] || continue
  b=".github/workflows/$(basename "$f")"
  set +e
  sed 's/[[:space:]]\+#.*$//' "$f" \
  | grep -E '^[[:space:]]*(-[[:space:]]+)?uses:[[:space:]]' \
  | sed -E 's/^[[:space:]]*(-[[:space:]]+)?uses:[[:space:]]+//' \
  | tr -d '"'"'"'' | awk '{print $1}' \
  | grep -E '/\.github/workflows/' \
  | while read -r u; do
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
function rest(   k, ks, c, i, j, t) {  # wanted paths the lock has no entry for yet
  c = 0; for (k in WANT) if (!(k in SEEN)) ks[++c] = k
  for (i = 1; i < c; i++) for (j = i + 1; j <= c; j++) if (ks[j] < ks[i]) { t = ks[i]; ks[i] = ks[j]; ks[j] = t }
  for (i = 1; i <= c; i++) { cur = ks[i]; items = ""; flush() }
}
/^workflows:[[:space:]]*$/ { print; inwf = 1; sawwf = 1; next }
inwf && /^[a-z_]+:/ { flush(); rest(); inwf = 0; print; next }
inwf {
  if (match($0, /^    '\''[^'\'']+'\'':/)) {
    flush()
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
