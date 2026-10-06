#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# prune-stale.sh — remove workflow-entry refs from .github/workflows/actions.lock
# that no `uses:` in that workflow references.
#
# ── Why pruning, and not demoting the check ─────────────────────────────────
# The per-repo lock-sync gate (check-lock-sync.sh) clause 2 hard-FAILs on
# "stale lockfile entries, no uses: references them". Measured 2026-09-22 on
# hyperpolymath/my-lang, that condition is NOT fatal to GitHub: six workflows there share
# the identical stale entry 'hyperpolymath/standards@08586a12…' and produce
# THREE different outcomes —
# secret-scanner + spark-theatre-gate success x3, governance + hypatia-scan
# failure x3 (jobs>0), mirror + scorecard startup_failure x3. One property,
# three outcomes, so the property is not the cause.
#
# So the gate reds on a harmless condition. There are two cures; this is the
# one that repairs rather than silences: make the lock true, and leave the gate
# strict. A `::warning::` or a demotion to advisory would be a vacuous gate.
#
# ── Why pruning is safe ─────────────────────────────────────────────────────
# A workflow entry is a per-path list of the refs GitHub validates for THAT
# workflow. Transitive dependencies live in the top-level `dependencies:` map,
# never in a per-workflow list, so removing a per-workflow ref cannot open a
# dangling edge — and check-lock-sync clause 3 re-verifies closure afterwards
# regardless. An entry whose ref is referenced by no `uses:` in its own file
# cannot be satisfying anything there.
#
# ── Two independent oracles must agree before a ref is dropped ──────────────
#   1. the PARSER: no `uses:` in that workflow normalises to this owner/repo@ref
#      (same norm()/ck() semantics as check-lock-sync.sh);
#   2. the LITERAL GREP: the exact ref string does not occur anywhere in the
#      workflow file, comments included (`grep -F`).
# Disagreement means KEEP. A parser blind spot therefore cannot delete a needed
# entry (scripts/tests/prune-stale-test.sh plants exactly that blind spot).
#
# ── A key whose workflow file is gone is dropped whole ──────────────────────
# Retiring a workflow is the limit case: every ref under its key is stale, and
# the key itself names a file that does not exist, which check-lock-sync.sh
# rejects ("lockfile entry for a workflow file that does not exist"). Leaving
# `[]` is not enough. `gh actions-lock` would remove the key, but it is blind
# to refs used only by a local composite action and drops those records too
# (#947 deleted asana/push-signed-commits that way; #954 restored it). The
# same two-oracle rule applies: a key is dropped only when it matches no file
# the workflow glob found AND `-f` on that path fails. The pass is scoped to
# the workflows: section, because dependencies: keys share the same shape and
# are never files.
#
# Lock repair order: gh actions-lock → relock-sha-keys.sh → complete-job-refs.sh
# → close-lock.sh → prune-stale.sh (see complete-job-refs.sh).
#
# Exit 0 whether or not anything was pruned; exit 1 only on a structural error.

set -euo pipefail

WF_DIR="${1:-.github/workflows}"
LOCK="$WF_DIR/actions.lock"

AWK=""
for cand in gawk awk; do
  if command -v "$cand" >/dev/null 2>&1 \
     && echo x | "$cand" '{ if (match($0, /(x)/, m) && m[1] == "x") exit 0; exit 1 }' 2>/dev/null; then
    AWK="$cand"; break
  fi
done
[ -n "$AWK" ] || { echo "prune-stale: FATAL: need gawk (3-arg match)" >&2; exit 1; }
[ -f "$LOCK" ] || { echo "prune-stale: FATAL: no lockfile at $LOCK" >&2; exit 1; }

shopt -s nullglob
mapfile -t WORKFLOWS < <(printf '%s\n' "$WF_DIR"/*.yml "$WF_DIR"/*.yaml | sort -u)
[ "${#WORKFLOWS[@]}" -gt 0 ] || { echo "prune-stale: FATAL: no workflows under $WF_DIR" >&2; exit 1; }

read -r -d '' CAND <<'AWK' || true
function norm(r,   at, path, ref, n, parts) {
  at = 0
  for (n = length(r); n > 0; n--) { if (substr(r, n, 1) == "@") { at = n; break } }
  if (at == 0) return ""
  path = substr(r, 1, at - 1); ref = substr(r, at + 1)
  if (path == "" || ref == "") return ""
  if (substr(path, 1, 2) == "./" || substr(path, 1, 2) == "$/") return ""
  if (split(path, parts, "/") < 2) return ""
  return parts[1] "/" parts[2] "@" ref
}
function ck(r,   at, s) {
  at = 0
  for (s = length(r); s > 0; s--) { if (substr(r, s, 1) == "@") { at = s; break } }
  if (at == 0) return tolower(r)
  return tolower(substr(r, 1, at - 1)) substr(r, at)
}
FILENAME == lockfile {
  if ($0 ~ /^workflows:[[:space:]]*$/)    { inwf = 1; next }
  if ($0 ~ /^[a-z_]+:/)                   { inwf = 0; next }
  if (!inwf) next
  if (match($0, /^    '([^']+)':/, m)) { cur = m[1]; next }
  if (match($0, /^        - '([^']+)'[[:space:]]*$/, m) && cur != "") {
    lockref[cur, ck(m[1])] = m[1]
  }
  next
}
FNR == 1 { wf = FILENAME; key = wf; sub(/.*\//, "", key); key = ".github/workflows/" key; pathof[wf] = key }
{
  line = $0
  sub(/[[:space:]]+#.*$/, "", line)
  if (match(line, /^[[:space:]]*-?[[:space:]]*uses:[[:space:]]*(.+)$/, m)) {
    raw = m[1]; gsub(/^["']|["']$/, "", raw); gsub(/[[:space:]]+$/, "", raw)
    n = norm(raw)
    if (n != "") used[pathof[wf], ck(n)] = 1
  }
}
END {
  for (k in lockref) {
    split(k, kp, SUBSEP)
    if (!(k in used)) printf "%s\t%s\n", kp[1], lockref[k]
  }
}
AWK

TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
"$AWK" -v lockfile="$LOCK" "$CAND" "$LOCK" "${WORKFLOWS[@]}" | LC_ALL=C sort -u > "$TMP/cand.tsv"

# ── keys whose workflow file is gone. Both oracles must agree. ─────────────
declare -A present=()
for wf in "${WORKFLOWS[@]}"; do present[".github/workflows/${wf##*/}"]=1; done
: > "$TMP/gone.txt"
while IFS= read -r key; do
  [ -n "${present[$key]:-}" ] && continue
  if [ -f "$WF_DIR/${key##*/}" ]; then
    printf '  ! oracle disagrees, KEEPING key %s (file present, not matched by the glob)\n' "$key"
    continue
  fi
  printf '%s\n' "$key" >> "$TMP/gone.txt"
done < <("$AWK" '
  /^workflows:[[:space:]]*$/ { inwf = 1; next }
  /^[a-z_]+:/                { inwf = 0; next }
  inwf && match($0, /^    '\''([^'\'']+)'\'':/, m) { print m[1] }' "$LOCK")

if [ ! -s "$TMP/cand.tsv" ] && [ ! -s "$TMP/gone.txt" ]; then
  echo "prune-stale: no stale workflow-entry refs; lockfile unchanged"
  exit 0
fi

# ── oracle 2: the literal grep. Disagreement => KEEP. ──────────────────────
: > "$TMP/drop.tsv"
kept_by_oracle=0
while IFS=$'\t' read -r path ref; do
  wf="$WF_DIR/${path##*/}"
  if [ -f "$wf" ] && command grep -Fq -- "$ref" "$wf"; then
    printf '  ! oracle disagrees, KEEPING %s in %s (literal string present)\n' "$ref" "$path"
    kept_by_oracle=$((kept_by_oracle + 1))
    continue
  fi
  printf '%s\t%s\n' "$path" "$ref" >> "$TMP/drop.tsv"
done < "$TMP/cand.tsv"

if [ ! -s "$TMP/drop.tsv" ] && [ ! -s "$TMP/gone.txt" ]; then
  echo "prune-stale: every candidate was vetoed by the literal-grep oracle; lockfile unchanged"
  exit 0
fi

read -r -d '' APPLY <<'AWK' || true
NR == FNR { drop[$1 SUBSEP $2] = 1; next }
{
  if ($0 ~ /^workflows:[[:space:]]*$/) { inwf = 1; print; next }
  if ($0 ~ /^[a-z_]+:/)                { inwf = 0; print; next }
  if (!inwf) { print; next }
  if (match($0, /^    '([^']+)':/, m)) { cur = m[1]; print; next }
  if (match($0, /^        - '([^']+)'[[:space:]]*$/, m) && cur != "") {
    if ((cur SUBSEP m[1]) in drop) { pruned++; next }
  }
  print
}
END { printf "prune-stale: pruned %d stale workflow-entry ref(s)\n", pruned + 0 > "/dev/stderr" }
AWK

if [ -s "$TMP/drop.tsv" ]; then
  "$AWK" -F'\t' "$APPLY" "$TMP/drop.tsv" "$LOCK" > "$TMP/new.lock"
else
  cat "$LOCK" > "$TMP/new.lock"
fi

# An entry left with no items must become an explicit empty list, or the YAML
# key would take a null value and the schema would reject it.
"$AWK" '
  # Scoped strictly to the workflows: section. A dependencies: record uses the
  # same "    '\''key'\'':" shape but its children are "        ref:" style, so an
  # unscoped pass would rewrite every dependency record to ": []" and orphan its
  # children. That bug was caught by the gitbot-fleet test, not by review.
  { lines[NR] = $0 }
  END {
    inwf = 0
    for (i = 1; i <= NR; i++) {
      if (lines[i] ~ /^workflows:[[:space:]]*$/) { inwf = 1; print lines[i]; continue }
      else if (lines[i] ~ /^[a-z_]+:/)           { inwf = 0; print lines[i]; continue }
      if (inwf && match(lines[i], /^    '\''[^'\'']+'\'':[[:space:]]*$/) \
          && (i == NR || lines[i+1] !~ /^        - /)) {
        sub(/:[[:space:]]*$/, ": []", lines[i])
      }
      print lines[i]
    }
  }' "$TMP/new.lock" > "$TMP/new2.lock"

# Drop each gone key with its items, again scoped to the workflows: section.
"$AWK" '
  NR == FNR { gone[$0] = 1; next }
  /^workflows:[[:space:]]*$/ { inwf = 1; skip = 0; print; next }
  /^[a-z_]+:/                { inwf = 0; skip = 0; print; next }
  inwf && match($0, /^    '\''([^'\'']+)'\'':/, m) {
    skip = (m[1] in gone); if (skip) { dropped++; next }
  }
  skip && /^        - / { next }
  { skip = 0; print }
  END { if (dropped) printf "prune-stale: dropped %d key(s) for workflow files that no longer exist\n", dropped > "/dev/stderr" }
' "$TMP/gone.txt" "$TMP/new2.lock" > "$TMP/new3.lock"

if command -v diff >/dev/null 2>&1; then diff -u "$LOCK" "$TMP/new3.lock" | sed 's/^/   | /' || true; fi
cat "$TMP/new3.lock" > "$LOCK"
echo "prune-stale: done (oracle vetoes: $kept_by_oracle)"
