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
# Leaf records are written without a nested `uses:` list, so one pass suffices.
#
# Lock repair order: gh actions-lock → relock-sha-keys.sh → complete-job-refs.sh
# → close-lock.sh → prune-stale.sh (see complete-job-refs.sh).
#
# Every value taken from the API is SHAPE-checked before it is written: a 40-hex
# commit and two integer ids. On an HTTP error `gh api` can put the error body
# on stdout, and a record built from `{"message"…` would be a present-but-
# unresolvable entry, which is fatal where an absent one is merely dangling.
# A ref that cannot be resolved is SKIPPED, reported, and makes the exit 3.
# GH_BIN overrides the `gh` binary (the test suite uses an offline stub).
set -euo pipefail
WF="${1:-.github/workflows}"
L="$WF/actions.lock"
GH_BIN="${GH_BIN:-gh}"
[ -f "$L" ] || { echo "close-lock: no $L" >&2; exit 1; }
SC="$(mktemp -d)"; trap 'rm -rf "$SC"' EXIT
# keys: list the dependency record keys already present in the lock.
keys()   { awk '/^dependencies:/{f=1;next} f&&match($0,/^    '\''[^'\'']+'\'':$/){k=$0;sub(/^    '\''/,"",k);sub(/'\'':$/,"",k);print k}' "$L" | LC_ALL=C sort -u; }
# nested: list the refs that dependency records themselves depend on.
nested() { awk '/^dependencies:/{f=1} f&&match($0,/^            - '\''[^'\'']+'\''/){k=$0;sub(/^            - '\''/,"",k);sub(/'\''.*$/,"",k);print k}' "$L" | LC_ALL=C sort -u; }
# wfrefs: list every ref named under the workflows: section.
wfrefs() { awk '/^workflows:/{f=1;next} f&&/^[a-z_]+:/{f=0} f&&match($0,/^        - '\''[^'\'']+'\''/){k=$0;sub(/^        - '\''/,"",k);sub(/'\''.*$/,"",k);print k}' "$L" | LC_ALL=C sort -u; }

skipped=0
for pass in 1 2 3 4 5; do
  keys > "$SC/have"
  { nested; wfrefs; } | LC_ALL=C sort -u > "$SC/want"
  LC_ALL=C comm -23 "$SC/want" "$SC/have" > "$SC/miss"
  n=$(wc -l < "$SC/miss")
  echo "close-lock: pass $pass — dangling refs: $n"
  [ "$n" -eq 0 ] && break
  : > "$SC/new"
  while read -r k; do
    [ -n "$k" ] || continue
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
    printf "    '%s':\n        ref: '%s'\n        commit: 'sha1-%s'\n        owner_id: %s\n        repo_id: %s\n" \
      "$k" "$ref" "$sha" "${ids%% *}" "${ids##* }" >> "$SC/new"
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
  echo "close-lock: $skipped dangling ref(s) could not be resolved; lock is NOT closed" >&2
  exit 3
fi
echo "close-lock: done"
