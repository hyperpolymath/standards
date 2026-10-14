#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# relock-standards-pin.sh — move a consumer's actions.lock to the standards
# commit its reusable-workflow pins were just re-pointed at.
#
# WHY: both pin bumpers (propagate-workflow-pins.sh and the scheduled
# apply-workflow-pins-remote.sh) rewrite `uses: hyperpolymath/standards/...@SHA`
# in workflow YAML and nothing else. In a repo with an actions.lock that leaves
# the lock keyed to the OLD standards commit: per-workflow entries name a ref no
# `uses:` carries any more, and the standards dependency record still lists the
# actions the OLD reusables used. check-lock-sync.sh fails, and GitHub refuses
# such runs at startup (jobs=0; mirror.yml, standards 2026-09-22). Measured
# 2026-10-14: 200 of 368 governance callers carry an actions.lock, so a fix-mode
# applier run would have broken their reusable-calling workflows.
# `gh actions-lock` cannot repair it: v0.1.6 is blind to job-level `uses:`.
#
# WHAT IT WRITES, and nothing else:
#   * per-workflow entries hyperpolymath/standards@OLD -> @TARGET;
#   * the hyperpolymath/standards@TARGET dependency record, whose nested `uses:`
#     is every action the called reusables use AT TARGET (owner/repo@ref);
#   * records those actions need and the lock lacks, copied verbatim from
#     standards' own actions.lock at TARGET;
#   * removal of the OLD standards records, and of records that only they
#     kept alive (nothing else is pruned);
#   * dependency records in C-locale key order, as gh actions-lock writes them.
#
# FAIL-CLOSED: it works on a copy and leaves the lock untouched (exit 1) when a
# workflow still pins a standards reusable at another SHA, when the called
# reusable is not in standards at TARGET, when the OLD record is missing, or
# when a needed record is in neither lock. A lock with a dangling edge is
# worse than a stale one: the stale one is at least reported by the gate.
#
# Usage: relock-standards-pin.sh --to <sha> [--standards <dir>] <workflows-dir>
#   <workflows-dir>  holds the workflow files and actions.lock: a repo's
#                    .github/workflows, or the applier's flat fetch directory.
#   --standards      a standards git checkout that contains <sha>; default
#                    $STANDARDS_DIR, else the checkout this script lives in.
# Exit: 0 relocked or nothing to do; 1 refused (lock untouched); 2 usage.
set -euo pipefail

TARGET="" SROOT="${STANDARDS_DIR:-}" WF=""
while [ $# -gt 0 ]; do
  case "$1" in
    --to) TARGET="${2:-}"; shift 2 ;;
    --to=*) TARGET="${1#*=}"; shift ;;
    --standards) SROOT="${2:-}"; shift 2 ;;
    -h|--help) sed -n '2,41p' "$0"; exit 0 ;;
    -*) echo "relock-standards-pin: unknown option $1" >&2; exit 2 ;;
    *) WF="$1"; shift ;;
  esac
done
[ -n "$SROOT" ] || SROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
[[ "$TARGET" =~ ^[0-9a-f]{40}$ ]] || { echo "relock-standards-pin: --to must be a full 40-hex SHA" >&2; exit 2; }
[ -n "$WF" ] && [ -d "$WF" ] || { echo "relock-standards-pin: workflows directory required" >&2; exit 2; }
git -C "$SROOT" cat-file -e "${TARGET}^{commit}" 2>/dev/null \
  || { echo "relock-standards-pin: standards commit $TARGET not in $SROOT" >&2; exit 2; }

L="$WF/actions.lock"
[ -f "$L" ] || { echo "relock-standards-pin: no actions.lock in $WF; nothing to relock"; exit 0; }
# refuse REASON... — report why the relock is refused and exit 1, lock untouched.
refuse() { echo "relock-standards-pin: REFUSED ($*); $L left untouched" >&2; exit 1; }

PIN_RE='hyperpolymath/standards/\.github/workflows/[A-Za-z0-9._-]+\.ya?ml@[0-9a-fA-F]+'
shopt -s nullglob
wfs=("$WF"/*.yml "$WF"/*.yaml)
shopt -u nullglob
pins=$(grep -hoE "$PIN_RE" "${wfs[@]}" 2>/dev/null | sort -u || true)
stale=$(printf '%s\n' "$pins" | awk -F@ -v target="$TARGET" \
  'NF > 1 && $NF != substr(target, 1, length($NF))')
[ -z "$stale" ] || refuse "workflows still pin other standards commits: $(echo $stale)"

olds=$(grep -oE "'hyperpolymath/standards@[0-9a-f]{40}'" "$L" | tr -d "'" | sed 's/.*@//' | sort -u | grep -vx "$TARGET" || true)
if [ -z "$olds" ]; then echo "relock-standards-pin: lock already keyed to standards@${TARGET:0:12}"; exit 0; fi
[ -n "$pins" ] || refuse "lock names standards commits but no workflow calls a standards reusable"

tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
q="'"

# rec_body KEY FILE — print the dependency record KEY (header and body) from FILE.
rec_body() { awk -v k="    ${q}$1${q}:" '$0==k{f=1;print;next} f&&/^    [^ ]/{exit} f' "$2"; }

# Actions the called reusables use at TARGET, normalised to owner/repo@ref.
: > "$tmp/uses"
for p in $pins; do
  path=${p#hyperpolymath/standards/}; path=${path%@*}
  git -C "$SROOT" show "${TARGET}:${path}" > "$tmp/reusable" 2>/dev/null || refuse "$path is not in standards at ${TARGET:0:12}"
  sed 's/[[:space:]]#.*$//' "$tmp/reusable" \
    | grep -oE "^[[:space:]]*-?[[:space:]]*uses:[[:space:]]*['\"]?[A-Za-z0-9_.-]+/[A-Za-z0-9_./-]+@[^[:space:]'\"]+" \
    | sed -E "s/^[[:space:]]*-?[[:space:]]*uses:[[:space:]]*['\"]?//; s#^([^/]+/[^/@]+)[^@]*@#\1@#" >> "$tmp/uses" || true
done
grep -v '^hyperpolymath/standards@' "$tmp/uses" | LC_ALL=C sort -u > "$tmp/uses.s" || true

# owner_id / repo_id come from an OLD record; without one the lock was already dangling.
ids=""
for o in $olds; do ids=$(rec_body "hyperpolymath/standards@$o" "$L" | grep -E '^        (owner_id|repo_id):' || true); [ -n "$ids" ] && break; done
[ -n "$ids" ] || refuse "no hyperpolymath/standards@OLD dependency record to take owner_id/repo_id from"

# Records the OLD standards records kept alive: the pruning candidates.
: > "$tmp/cand"
for o in $olds; do rec_body "hyperpolymath/standards@$o" "$L" | grep -oE "^            - '[^']+'" | sed -E "s/^ +- '|'$//g" >> "$tmp/cand" || true; done

cp "$L" "$tmp/lock"
for o in $olds; do
  sed -i "s|^        - 'hyperpolymath/standards@$o'\$|        - 'hyperpolymath/standards@$TARGET'|" "$tmp/lock"
  awk -v k="    ${q}hyperpolymath/standards@$o${q}:" '$0==k{s=1;next} s&&/^    [^ ]/{s=0} !s' "$tmp/lock" > "$tmp/l" && mv "$tmp/l" "$tmp/lock"
done
awk -v k="    ${q}hyperpolymath/standards@$TARGET${q}:" '$0==k{s=1;next} s&&/^    [^ ]/{s=0} !s' "$tmp/lock" > "$tmp/l" && mv "$tmp/l" "$tmp/lock"
{ printf "    'hyperpolymath/standards@%s':\n        ref: '%s'\n        commit: 'sha1-%s'\n" "$TARGET" "$TARGET" "$TARGET"
  printf '%s\n' "$ids"
  if [ -s "$tmp/uses.s" ]; then echo "        uses:"; sed "s|.*|            - '&'|" "$tmp/uses.s"; fi
} >> "$tmp/lock"

# Records the new pin needs: from this lock, else verbatim from standards' own.
git -C "$SROOT" show "${TARGET}:.github/workflows/actions.lock" > "$tmp/slock" 2>/dev/null || : > "$tmp/slock"
while read -r u; do
  [ -n "$u" ] || continue
  grep -qF "    '$u':" "$tmp/lock" && continue
  r=$(rec_body "$u" "$tmp/slock")
  [ -n "$r" ] || refuse "no record for $u in this lock or in standards' own lock at ${TARGET:0:12}"
  printf '%s\n' "$r" >> "$tmp/lock"
done < "$tmp/uses.s"

# Prune candidates nothing references any more (to a fixed point).
deps_line() { grep -n '^dependencies:$' "$1" | cut -d: -f1; }
while :; do
  n=$(deps_line "$tmp/lock")
  grep -oE "^ {8,12}- '[^']+'" "$tmp/lock" | sed -E "s/^ +- '|'$//g" | LC_ALL=C sort -u > "$tmp/edges"
  pruned=0
  while read -r c; do
    [ -n "$c" ] || continue
    grep -qxF "$c" "$tmp/edges" && continue
    tail -n +"$((n+1))" "$tmp/lock" | grep -qxF "    '$c':" || continue
    rec_body "$c" "$tmp/lock" | grep -oE "^            - '[^']+'" | sed -E "s/^ +- '|'$//g" >> "$tmp/cand" || true
    awk -v k="    ${q}$c${q}:" '$0==k{s=1;next} s&&/^    [^ ]/{s=0} !s' "$tmp/lock" > "$tmp/l" && mv "$tmp/l" "$tmp/lock"
    pruned=1
  done < <(LC_ALL=C sort -u "$tmp/cand")
  [ "$pruned" = 1 ] || break
done

# Dependency records in C-locale key order (portable awk: tag lines, sort, strip).
n=$(deps_line "$tmp/lock")
head -n "$n" "$tmp/lock" > "$tmp/out"
tail -n +"$((n+1))" "$tmp/lock" \
  | awk -v q="$q" 'index($0, "    " q) == 1 { key = $0 } { printf "%s\t%08d\t%s\n", key, NR, $0 }' \
  | LC_ALL=C sort -t "$(printf '\t')" -k1,1 -k2,2n | cut -f3- >> "$tmp/out"

# Closure: every ref the lock names must have a record of its own.
n=$(deps_line "$tmp/out")
grep -oE "^ {8,12}- '[^']+'" "$tmp/out" | sed -E "s/^ +- '|'$//g" | LC_ALL=C sort -u > "$tmp/edges"
tail -n +"$((n+1))" "$tmp/out" | grep -oE "^    '[^']+':" | sed -E "s/^    '|':$//g" | LC_ALL=C sort -u > "$tmp/recs"
dangling=$(LC_ALL=C comm -23 "$tmp/edges" "$tmp/recs")
[ -z "$dangling" ] || refuse "result would have dangling edges: $(echo $dangling)"

cat "$tmp/out" > "$L"
echo "relock-standards-pin: $L keyed to standards@${TARGET:0:12} (from $(echo $olds | sed 's/\([0-9a-f]\{12\}\)[0-9a-f]*/\1/g'))"
