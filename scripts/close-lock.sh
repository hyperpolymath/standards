#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# close-lock.sh — make actions.lock TRANSITIVELY CLOSED.
#
# MEASURED on hyperpolymath/cicd-squabbler#101: GitHub rejects a run at startup
# (jobs=0) when the lockfile contains a DANGLING edge — a ref named under
# `workflows:` or inside another record's nested `uses:` that has no top-level
# record of its own. Three workflows that ran with jobs on `pull_request` on
# 2026-09-21 went startup_failure once a job-level ref was listed without
# closure, and adding the record but not its nested refs' records did not cure
# it. metadatastician/burble's working lockfile has zero dangling edges.
#
# Records for actions are written as leaves, without a nested `uses:` list.
#
# A record for a REUSABLE WORKFLOW is not a leaf. MEASURED on
# hyperpolymath/palimpsest-license#177 (2026-10-14): with the job-level
# `hyperpolymath/standards@<sha>` refs listed and LEAF records for them, GitHub
# refused Governance, Hypatia Security Scan and Secret Scanner at startup
# (startup_failure, no jobs) while every step-only workflow started. In the
# estate locks whose reusable callers do start (hyperpolymath/my-lang,
# metadatastician/burble), that record's nested `uses:` is exactly the union of
# the external `uses:` of the workflow files called at that ref, each written as
# the lock key `owner/repo@ref` (a sub-path such as `/init` dropped; local `./`,
# `$/` and `docker://` refs excluded). So every job-level call
# `owner/repo/.github/workflows/<file>@<ref>` in this repository, and every such
# call inside a called file, is read at <ref>, and the record `owner/repo@<ref>`
# is written, or completed where an earlier run left it a leaf or short, with
# that union. The refs it names are then closed by the passes like any other
# edge. A called file that cannot be read is SKIPPED: no record is written or
# changed for its ref and the exit is 3, so a leaf is never written where a list
# is owed.
#
# Lock repair order: gh actions-lock → relock-sha-keys.sh → complete-job-refs.sh
# → close-lock.sh → prune-stale.sh (see complete-job-refs.sh).
#
# Every value taken from the API is SHAPE-checked before it is written: a 40-hex
# commit, two integer ids, and a workflow file with a top-level `jobs:`. On an
# HTTP error `gh api` can put the error body on stdout, and a record built from
# `{"message"…` would be a present-but-unresolvable entry, which is fatal where
# an absent one is merely dangling. A ref that cannot be resolved is SKIPPED,
# reported, and makes the exit 3.
# GH_BIN overrides the `gh` binary (the test suite uses an offline stub).
set -euo pipefail
WF="${1:-.github/workflows}"
L="$WF/actions.lock"
GH_BIN="${GH_BIN:-gh}"
[ -f "$L" ] || { echo "close-lock: no $L" >&2; exit 1; }
SC="$(mktemp -d)"; trap 'rm -rf "$SC"' EXIT
TAB="$(printf '\t')"
# keys: list the dependency record keys already present in the lock.
keys()   { awk '/^dependencies:/{f=1;next} f&&match($0,/^    '\''[^'\'']+'\'':$/){k=$0;sub(/^    '\''/,"",k);sub(/'\'':$/,"",k);print k}' "$L" | LC_ALL=C sort -u; }
# nested: list the refs that dependency records themselves depend on.
nested() { awk '/^dependencies:/{f=1} f&&match($0,/^            - '\''[^'\'']+'\''/){k=$0;sub(/^            - '\''/,"",k);sub(/'\''.*$/,"",k);print k}' "$L" | LC_ALL=C sort -u; }
# wfrefs: list every ref named under the workflows: section.
wfrefs() { awk '/^workflows:/{f=1;next} f&&/^[a-z_]+:/{f=0} f&&match($0,/^        - '\''[^'\'']+'\''/){k=$0;sub(/^        - '\''/,"",k);sub(/'\''.*$/,"",k);print k}' "$L" | LC_ALL=C sort -u; }
# uses_values FILE: every `uses:` value in FILE, with a trailing comment, YAML
# quotes and a KYAML trailing comma stripped (the same reading as
# complete-job-refs.sh).
uses_values() {
  { sed 's/[[:space:]]\+#.*$//' "$1" \
    | grep -E '^[[:space:]]*(-[[:space:]]+)?uses:[[:space:]]' \
    | sed -E 's/^[[:space:]]*(-[[:space:]]+)?uses:[[:space:]]+//' \
    | tr -d '"'"'"'' | awk '{print $1}' | sed 's/,$//'; } || true
}
# external_refs: read `uses:` values on stdin; print each external one as its
# lock key owner/repo@ref.
external_refs() {
  awk '/^\.\// || /^\$\// || /^docker:\/\// || !/@/ { next }
       { at = index($0, "@"); n = split(substr($0, 1, at - 1), a, "/")
         if (n >= 2 && a[1] != "" && a[2] != "") print a[1] "/" a[2] "@" substr($0, at + 1) }'
}
# reusable_calls: read `uses:` values on stdin; print "owner/repo@ref<TAB>path"
# for each call of a workflow file in another repository.
reusable_calls() {
  awk '{ at = index($0, "@"); if (!at) next; p = substr($0, 1, at - 1)
         if (p !~ /^[^.\/$][^\/]*\/[^\/]+\/\.github\/workflows\/[^\/]+\.ya?ml$/) next
         split(p, a, "/"); print a[1] "/" a[2] "@" substr($0, at + 1) "\t" substr(p, length(a[1]) + length(a[2]) + 3) }'
}
# keyfile KEY: a file name for KEY's owed nested refs.
keyfile() { printf '%s' "$1" | sha256sum | cut -c1-32; }

# ── reusable workflows: read each called file once, transitively ─────────────
mkdir -p "$SC/u"; : > "$SC/read"; : > "$SC/bad"; : > "$SC/more"
for f in "$WF"/*.yml "$WF"/*.yaml; do
  [ -e "$f" ] || continue
  uses_values "$f" | reusable_calls
done | LC_ALL=C sort -u > "$SC/calls"
while :; do
  LC_ALL=C comm -23 "$SC/calls" "$SC/read" > "$SC/todo"
  [ -s "$SC/todo" ] || break
  while IFS="$TAB" read -r k p; do
    [ -n "$k" ] || continue
    printf '%s\t%s\n' "$k" "$p" >> "$SC/read"
    owner="${k%%/*}"; rest="${k#*/}"; name="${rest%@*}"; ref="${k##*@}"
    if "$GH_BIN" api "repos/$owner/$name/contents/$p?ref=$ref" -H 'Accept: application/vnd.github.raw' > "$SC/body" 2>/dev/null \
       && grep -qE '^[[:space:]]*\{?[[:space:]]*"?jobs"?[[:space:]]*:' "$SC/body"; then
      uses_values "$SC/body" | external_refs >> "$SC/u/$(keyfile "$k")"
      uses_values "$SC/body" | reusable_calls >> "$SC/more"
    else
      echo "  SKIP $k (cannot read $p at $ref)"; echo "$k" >> "$SC/bad"
    fi
  done < "$SC/todo"
  LC_ALL=C sort -u -o "$SC/read" "$SC/read"
  cat "$SC/calls" "$SC/more" | LC_ALL=C sort -u > "$SC/calls.new"; mv "$SC/calls.new" "$SC/calls"; : > "$SC/more"
done
LC_ALL=C sort -u -o "$SC/bad" "$SC/bad"
# owed: "key<TAB>ref" per nested ref a readable reusable record must list, plus
# "key<TAB>" for each such key, so a reusable that uses no external action is
# still known (its record correctly stays a leaf).
: > "$SC/owed"
cut -f1 "$SC/calls" | LC_ALL=C sort -u | while read -r k; do
  grep -qxF "$k" "$SC/bad" && continue
  printf '%s\t\n' "$k"
  uf="$SC/u/$(keyfile "$k")"
  if [ -f "$uf" ]; then
    LC_ALL=C sort -u "$uf" | while read -r r; do printf '%s\t%s\n' "$k" "$r"; done
  fi
done | LC_ALL=C sort -u > "$SC/owed"
skipped=$(grep -c . "$SC/bad" || true)

# uses_block KEY: print the nested `uses:` lines owed by KEY's record (none for
# an action, or for a reusable that uses no external action).
uses_block() {
  awk -F'\t' -v k="$1" '$1 == k && $2 != "" { if (!h++) print "        uses:"; print "            - '\''" $2 "'\''" }' "$SC/owed"
}

# ── complete existing reusable records whose nested uses: are short or absent ─
awk -F'\t' -v owedf="$SC/owed" -v logf="$SC/rewritten" '
BEGIN { while ((getline l < owedf) > 0) { split(l, w, "\t"); R[w[1]] = 1
          if (w[2] != "") OW[w[1]] = OW[w[1]] "            - '\''" w[2] "'\''\n" } }
# flush: close the current reusable record, writing the owed uses: block.
function flush() {
  if (cur == "") return
  if (OW[cur] != "") printf "        uses:\n%s", OW[cur]
  if (old != OW[cur]) print cur > logf
  cur = ""; old = ""
}
/^dependencies:[[:space:]]*$/ { print; indep = 1; next }
indep && /^[a-z_]+:/ { flush(); indep = 0; print; next }
indep && /^    '\''[^'\'']+'\'':$/ {
  flush(); k = $0; sub(/^    '\''/, "", k); sub(/'\'':$/, "", k)
  if (k in R) cur = k
  print; next
}
cur != "" && /^        uses:[[:space:]]*$/ { next }
cur != "" && /^            - / { old = old $0 "\n"; next }
{ print }
END { flush() }' "$L" > "$SC/completed.lock"
if [ -s "$SC/rewritten" ]; then
  while read -r k; do echo "  ~ $k (nested uses: completed from its called workflow)"; done < "$SC/rewritten"
  cat "$SC/completed.lock" > "$L"
fi

# ── close every dangling edge ─────────────────────────────────────────────────
for pass in 1 2 3 4 5 6; do
  keys > "$SC/have"
  { nested; wfrefs; } | LC_ALL=C sort -u > "$SC/want"
  LC_ALL=C comm -23 "$SC/want" "$SC/have" > "$SC/miss"
  n=$(wc -l < "$SC/miss")
  echo "close-lock: pass $pass — dangling refs: $n"
  [ "$n" -eq 0 ] && break
  : > "$SC/new"
  while read -r k; do
    [ -n "$k" ] || continue
    if grep -qxF "$k" "$SC/bad"; then
      echo "  SKIP $k (its called workflow is unreadable; no leaf record written)"; continue
    fi
    owner="${k%%/*}"; rest="${k#*/}"; name="${rest%@*}"; ref="${k##*@}"
    ids=$("$GH_BIN" api "repos/$owner/$name" --jq '"\(.owner.id) \(.id)"') || ids=""
    if ! printf '%s' "$ids" | grep -qxE '[0-9]+ [0-9]+'; then
      echo "  SKIP $k (repo lookup failed or malformed)"; skipped=$((skipped + 1)); continue
    fi
    sha=""
    if printf '%s' "$ref" | grep -qxE '[0-9a-f]{40}'; then sha="$ref"
    else sha=$("$GH_BIN" api "repos/$owner/$name/commits/$ref" --jq .sha) || sha=""; fi
    if ! printf '%s' "$sha" | grep -qxE '[0-9a-f]{40}'; then
      echo "  SKIP $k (cannot resolve commit)"; skipped=$((skipped + 1)); continue
    fi
    rec_ref="$ref"
    if printf '%s' "$ref" | grep -qxE '[0-9a-f]{40}' && ! grep -qxF "$k$TAB" "$SC/owed"; then
      # Consume all pages before choosing a tag. An early pipeline exit can
      # SIGPIPE gh under pipefail and discard an otherwise valid result.
      tag_ref=""
      if "$GH_BIN" api "repos/$owner/$name/tags?per_page=100" --paginate \
          --jq '.[] | select(.commit.sha == "'"$sha"'") | .name' > "$SC/tags" 2>/dev/null; then
        tag_ref=$(awk '/^[A-Za-z0-9._\/-]+$/ && !(length($0) == 40 && $0 ~ /^[0-9a-f]+$/) { if (!first) first = $0; if (!preferred && $0 ~ /^v?[0-9]+\.[0-9]+\.[0-9]+/) preferred = $0 } END { if (preferred) print preferred; else if (first) print first }' "$SC/tags")
      fi
      if [ -z "$tag_ref" ]; then
        echo "  SKIP $k (cannot resolve a symbolic tag for leaf SHA)"
        skipped=$((skipped + 1))
        continue
      fi
      rec_ref="$tag_ref"
    fi
    { printf "    '%s':\n        ref: '%s'\n        commit: 'sha1-%s'\n        owner_id: %s\n        repo_id: %s\n" \
        "$k" "$rec_ref" "$sha" "${ids%% *}" "${ids##* }"
      uses_block "$k"; } >> "$SC/new"
    echo "  + $k"
  done < "$SC/miss"
  [ -s "$SC/new" ] || break
  awk -v newf="$SC/new" '
  BEGIN { nn=0; while ((getline l < newf) > 0) {
      if (l ~ /^    '\''[^'\'']+'\'':$/) { nn++; k=l; sub(/^    '\''/,"",k); sub(/'\'':$/,"",k); NK[nn]=k; NB[nn]=l }
      else NB[nn] = NB[nn] "\n" l } }
  # emit_lt: print each pending new record whose key sorts before key.
  function emit_lt(key,   i) { for (i=1;i<=nn;i++) if (!used[i] && NK[i] < key) { print NB[i]; used[i]=1 } }
  /^dependencies:[[:space:]]*$/ { print; indep=1; next }
  indep && /^[a-z_]+:/ { for (i=1;i<=nn;i++) if (!used[i]) { print NB[i]; used[i]=1 } indep=0; print; next }
  indep && /^    '\''[^'\'']+'\'':$/ { k=$0; sub(/^    '\''/,"",k); sub(/'\'':$/,"",k); emit_lt(k); print; next }
  { print }
  END { if (indep) for (i=1;i<=nn;i++) if (!used[i]) print NB[i] }' "$L" > "$SC/new.lock" && cat "$SC/new.lock" > "$L"
done
if [ "$skipped" -gt 0 ]; then
  echo "close-lock: $skipped ref(s) could not be resolved or read; lock is NOT closed" >&2
  exit 3
fi
echo "close-lock: done"
