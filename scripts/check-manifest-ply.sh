#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
set -uo pipefail

# check-manifest-ply.sh — assert an AI-MANIFEST's declared ply matches where it
# actually sits in the tree.
#
# ── Why this gate exists ────────────────────────────────────────────────────
# The manifest prefix encodes DEPTH, not version: `0-ply0-` is the repo root,
# `0-ply1-` the layer beneath it, and so on (older deployed form: `0.1-`).
# The depth is therefore stated twice — once in the filename, once by the path
# the file sits at. That redundancy is free, and asserting the two agree is the
# only mechanical detector for the two failures the owner named: manifests
# packing up at the root, and manifests scattered into directories they do not
# govern.
#
# Measured on local checkouts 2026-09-04 (duplicate checkout trees NOT removed,
# see standards#703 — treat these as counts of FILES ON THIS DISK, not as an
# estate census), 15,872 manifests under hyper-repos:
#
#     EQUAL, git-rooted .................  7,684  (48.4%)
#     UNIT-RELATIVE ....................   5,716  (36.0%)
#     DRIFT, declared < actual .........   2,468  (15.6%)
#     DRIFT, declared > actual .........       4  ( 0.0%)
#     UNPARSEABLE ......................       0
#     LEGACY-NAME ......................     766
#
# UNIT-RELATIVE is not drift. A sub-project that considers itself "a different
# repo of its own" numbers its children from ITSELF, and until the `-<unit>`
# tag existed it had no way to say so — it declared `0-` and looked wrong. 36%
# of this disk is in that state. A gate that called those 5,716 files errors
# would be measuring the notation gap, not the tree, and would be switched off
# in a week.
#
# ── What this gate does NOT do ──────────────────────────────────────────────
# It reads FILENAMES ONLY. It never opens a manifest, never parses A2ML, K9 or
# .deed, and has no opinion on any of their grammars — those belong to another
# agent. It is extension-agnostic by construction, so the in-flight
# .a2ml → .deed rename cannot break it and it needs no coordination to land.
#
# ── Failure semantics (deliberate, not handwaving) ──────────────────────────
# HARD FAIL only on what is wrong under EVERY reading of the scheme:
#   UNPARSEABLE  a file named like a manifest whose prefix parses under neither
#                the canonical `0-plyN-` form nor the deployed dotted form. An
#                unrecognised manifest is not a pass; it is an unknown.
#   DRIFT-DEEP   declared ply GREATER than actual depth. This cannot arise from
#                a tree growing beneath a file, so it is always a naming error
#                or a file that moved up. 4 on this disk — enforcing costs
#                nothing and it catches the real ones.
#
# WARN (advisory) unless --strict:
#   DRIFT-SHALLOW  declared ply LESS than actual depth under both frames. This
#                  is the migration debt (2,468). It is real, but hard-failing
#                  15.6% of the estate on day one gets the gate disabled rather
#                  than the files fixed. Ratchet it with --strict once the
#                  migration lands.
#                  KNOWN SOFT SPOT: a file with unit_depth < declared <
#                  git_depth is wrong under BOTH frames — too deep for (b),
#                  too shallow for (a) — but is classified SHALLOW, because
#                  which way it is wrong is undeterminable until the frame is
#                  ruled on. Revisit when the OWNER-CONFIRM lands.
#
# REPORTED, never failed:
#   UNIT-RELATIVE  consistent with a self-rooting ancestor. Pending the
#                  OWNER-CONFIRM in PREFIX-CANON.adoc on how a unit's children
#                  are numbered, this gate does not get to pick a frame.
#   LEGACY-NAME    `AI.<ext>` / `!AI.<ext>` — carries no ply claim, so there is
#                  nothing to check, but silently not seeing them is a blind
#                  spot and blind spots are how the last two schemes decayed.
#
# Finding ZERO manifests under a path that exists is reported LOUDLY, not as a
# green run. A scan that finds nothing and a scan that is broken print the same
# thing otherwise, and this estate has been bitten by exactly that before.

usage() {
  cat <<'USAGE'
usage: check-manifest-ply.sh [--strict] [--quiet] [PATH ...]

  --strict   treat DRIFT-SHALLOW as a failure (default: warn)
  --quiet    suppress the per-file listing, print the summary only
  PATH ...   roots to scan (default: .)

exit 0  no hard failures
exit 1  hard failure (UNPARSEABLE, DRIFT-DEEP, or --strict DRIFT-SHALLOW)
exit 2  usage error, or a PATH that does not exist
USAGE
}

strict=0
quiet=0
roots=()
while [ $# -gt 0 ]; do
  case "$1" in
    --strict) strict=1 ;;
    --quiet)  quiet=1 ;;
    -h|--help) usage; exit 0 ;;
    -*) printf 'unknown option: %s\n' "$1" >&2; usage >&2; exit 2 ;;
    *)  roots+=("$1") ;;
  esac
  shift
done
if [ ${#roots[@]} -eq 0 ]; then roots=("."); fi

# Absolutise the roots ONCE. find then emits absolute paths, so the classifier
# never has to `cd` per file to work out where a manifest really is.
abs_roots=()
for r in "${roots[@]}"; do
  if [ ! -d "$r" ]; then
    printf 'FATAL: not a directory: %s\n' "$r" >&2
    exit 2
  fi
  a=$(cd "$r" && pwd) || exit 2
  abs_roots+=("$a")
done
roots=("${abs_roots[@]}")

# ── repo-root resolution ────────────────────────────────────────────────────
# Nearest ancestor containing `.git`. `.git` may be a FILE, not a directory —
# that is the linked-worktree case, and treating it as dir-only silently
# reparents every worktree onto the wrong root. Memoised: the walk is O(depth)
# and the same directories recur thousands of times.
declare -A ROOT_MEMO=()
GITROOT=""
git_root() {
  local d=$1 probe seen=() up s
  probe=$d
  while :; do
    if [ -n "${ROOT_MEMO[$probe]+x}" ]; then d=${ROOT_MEMO[$probe]}; break; fi
    if [ -e "$probe/.git" ]; then d=$probe; break; fi
    seen+=("$probe")
    up=${probe%/*}
    if [ -z "$up" ]; then up=/; fi
    if [ "$up" = "$probe" ]; then d=""; break; fi
    probe=$up
  done
  for s in "${seen[@]}"; do ROOT_MEMO[$s]=$d; done
  GITROOT=$d
}

# depth of $1 below $2, in path components; 0 when equal.
# Pure parameter expansion on purpose: this runs once per manifest, and a
# subprocess per file turns a 15,871-file estate scan into ~32,000 forks.
# Result lands in $DEPTH rather than on stdout, so there is no subshell either.
DEPTH=0
depth_below() {
  local dir=$1 base=$2 rel slashes
  if [ "$dir" = "$base" ]; then DEPTH=0; return; fi
  rel=${dir#"$base"/}
  slashes=${rel//[!\/]/}
  DEPTH=$(( ${#slashes} + 1 ))
}

# ── pass 1: enumerate, parse, record self-rooting directories ───────────────
# Enumerate from the FILESYSTEM, never from a work list. A completion gate that
# reads the work list is blind to whatever the list omitted.
declare -a M_PATH=() M_DIR=() M_PLY=() M_UNIT=()
declare -A SELFROOT=()
n_legacy=0; n_unparse=0; n_dirs=0
declare -a UNPARSE=()

while IFS= read -r -d '' f; do
  base=${f##*/}; dir=${f%/*}
  ply=""; unit=""
  if [[ $base =~ ^0-ply([0-9]+)(-[A-Za-z0-9]+)?-AI-MANIFEST\. ]]; then
    ply=${BASH_REMATCH[1]}; unit=${BASH_REMATCH[2]#-}
  elif [[ $base =~ ^([0-9]+)\.([0-9]+)(\.([A-Za-z]+))?-AI-MANIFEST\. ]]; then
    ply=${BASH_REMATCH[2]}; unit=${BASH_REMATCH[4]}
  elif [[ $base =~ ^([0-9]+)(\.([A-Za-z]+))?-AI-MANIFEST\. ]]; then
    ply=${BASH_REMATCH[1]}; unit=${BASH_REMATCH[3]}
  else
    n_unparse=$((n_unparse+1)); UNPARSE+=("$f"); continue
  fi
  M_PATH+=("$f"); M_DIR+=("$dir"); M_PLY+=("$ply"); M_UNIT+=("$unit")
  if [ "$ply" = "0" ]; then SELFROOT[$dir]=1; fi
done < <(find "${roots[@]}" -type f -name '*-AI-MANIFEST.*' -print0 2>/dev/null)

while IFS= read -r -d '' f; do
  n_legacy=$((n_legacy+1))
done < <(find "${roots[@]}" -type f \( -name 'AI.*' -o -name '!AI.*' \) -print0 2>/dev/null)

n_dirs=$(find "${roots[@]}" -type d -print 2>/dev/null | wc -l)
n_total=${#M_PATH[@]}

if [ "$n_total" -eq 0 ] && [ "$n_unparse" -eq 0 ]; then
  printf 'WARN: no AI-MANIFEST files found under %s (%s directories scanned).\n' \
    "${roots[*]}" "$n_dirs"
  printf '      This is reported, not passed silently: an empty tree and a\n'
  printf '      broken scan look identical from the outside.\n'
  exit 0
fi

# ── pass 2: classify ────────────────────────────────────────────────────────
n_equal=0; n_unitrel=0; n_shallow=0; n_deep=0; n_noroot=0
declare -a DEEP=() SHALLOW=() UNITREL=() NOROOT=()

i=0
while [ "$i" -lt "$n_total" ]; do
  f=${M_PATH[$i]}; abs=${M_DIR[$i]}; ply=${M_PLY[$i]}
  git_root "$abs"; gr=$GITROOT
  if [ -z "$gr" ]; then
    n_noroot=$((n_noroot+1)); NOROOT+=("$f"); i=$((i+1)); continue
  fi
  depth_below "$abs" "$gr"; gd=$DEPTH

  if [ "$ply" -eq "$gd" ]; then
    n_equal=$((n_equal+1)); i=$((i+1)); continue
  fi

  # unit frame: nearest self-rooting ancestor at or below the git root
  ur=$gr; probe=$abs
  while [ -n "$probe" ] && [ "${probe#"$gr"}" != "$probe" ]; do
    if [ -n "${SELFROOT[$probe]+x}" ]; then ur=$probe; break; fi
    if [ "$probe" = "$gr" ]; then break; fi
    probe=${probe%/*}
    if [ -z "$probe" ]; then break; fi
  done
  depth_below "$abs" "$ur"; ud=$DEPTH

  if [ "$ur" != "$gr" ] && [ "$ply" -eq "$ud" ]; then
    n_unitrel=$((n_unitrel+1))
    UNITREL+=("declared=$ply git=$gd unit=$ud  $f")
  elif [ "$ply" -lt "$gd" ]; then
    n_shallow=$((n_shallow+1))
    SHALLOW+=("declared=$ply actual=$gd  $f")
  else
    n_deep=$((n_deep+1))
    DEEP+=("declared=$ply actual=$gd  $f")
  fi
  i=$((i+1))
done

# ── report ──────────────────────────────────────────────────────────────────
show() {
  local label=$1; shift
  if [ "$#" -eq 0 ]; then return; fi
  if [ "$quiet" -eq 1 ]; then return; fi
  printf '\n  -- %s --\n' "$label"
  printf '     %s\n' "$@"
}

printf 'AI-MANIFEST ply check — %s manifest(s) under %s\n' "$n_total" "${roots[*]}"
printf '  EQUAL (git-rooted) ....... %6s\n' "$n_equal"
printf '  UNIT-RELATIVE ............ %6s   (reported, not failed)\n' "$n_unitrel"
shallow_label=warn
if [ "$strict" -eq 1 ]; then shallow_label='FAIL under --strict'; fi
printf '  DRIFT-SHALLOW ............ %6s   (%s)\n' "$n_shallow" "$shallow_label"
printf '  DRIFT-DEEP ............... %6s   (FAIL)\n' "$n_deep"
printf '  UNPARSEABLE .............. %6s   (FAIL)\n' "$n_unparse"
printf '  LEGACY-NAME .............. %6s   (no ply claim to check)\n' "$n_legacy"
if [ "$n_noroot" -gt 0 ]; then
  printf '  NO-GIT-ROOT .............. %6s   (cannot be checked)\n' "$n_noroot"
fi

show 'DRIFT-DEEP (declared deeper than it sits)' "${DEEP[@]}"
show 'UNPARSEABLE' "${UNPARSE[@]}"
show 'NO-GIT-ROOT' "${NOROOT[@]}"
if [ "$strict" -eq 1 ]; then show 'DRIFT-SHALLOW' "${SHALLOW[@]}"; fi

rc=0
if [ "$n_deep" -gt 0 ] || [ "$n_unparse" -gt 0 ]; then rc=1; fi
if [ "$strict" -eq 1 ] && [ "$n_shallow" -gt 0 ]; then rc=1; fi

printf '\n'
if [ "$rc" -eq 0 ]; then
  printf 'OK: no hard failures.\n'
else
  printf 'FAIL: see above.\n'
fi
exit "$rc"
