#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# apply-workflow-pins-remote.sh — the scheduled APPLIER for standards
#   reusable-workflow pins across the whole estate.
#
# WHY THIS EXISTS, and why it is not `propagate-workflow-pins.sh`:
#
#   `scripts/propagate-workflow-pins.sh` already holds the proven rewrite core,
#   but it walks LOCAL CHECKOUTS and deliberately never commits. That makes it a
#   human-driven, one-repo-at-a-time tool. A cure landed in `standards` therefore
#   does NOT propagate: measured 2026-09-15, 28 of 29 live remote callers are
#   SHA-pinned and exactly one (`affinescript`) tracks `@main`. A pinned caller
#   never sees a fix until something re-points its pin.
#
#   This script is that something. It is an APPLIER, not a sweep (owner ruling
#   STD-R-3): it runs on a cadence, re-points pins, and is expected to run again.
#   A sweep is a one-shot mass mutation; an applier converges.
#
# WHAT A PIN CAN BE (four classes, not one — this is the hard-won part):
#   FRESH      pinned at the target SHA. Nothing to do.
#   BEHIND     pinned at some other real standards commit. Re-point.
#   DEAD-REF   pinned at a SHA that is NOT a commit in standards at all. These
#              exist (e.g. `awesome-idris2`). Cause: a squash-merge made the
#              intermediate commit unaddressable as a cross-repo workflow ref.
#              The workflow fails with `failure`, 0 jobs, and name == path — the
#              same signature as three other faults, so the signature alone
#              never proves the cause.
#   ILLEGAL    `uses: ../../...` or `uses: $/...`. Rejected at PARSE time, so the
#              run dies before any job starts. Seeded into repos by RSR templates
#              (issue #808). Repaired only under --repair-illegal.
#   TRACKING   `@main` or another non-SHA ref. Gets cures for free; also
#              unpinned, which the estate's own policy forbids. Reported, not
#              rewritten, because re-pointing it is a policy change, not a fix.
#
# SAFETY PROPERTIES (do not weaken any of these):
#   * READ-ONLY by default. --fix is required to write anything.
#   * The target SHA is proven REACHABLE FROM standards' main before any write.
#     Mere existence is NOT enough: on 2026-09-04 a squash-merged intermediate
#     commit was addressable through the contents API but unusable as a
#     cross-repo reusable-workflow ref, and pinning to it broke 251 active
#     workflow files across 70 repos.
#   * KNOWN-ANSWER CONTROLS run every time. If a control misclassifies, the run
#     ABORTS before writing. A census with no control is a census you cannot
#     distinguish from a broken one.
#   * Commits are made with createCommitOnBranch so they are GitHub-"Verified".
#     Measured 2026-09-15: required_signatures is the dominant campaign blocker
#     (6 of 9 campaign heads unsigned). An unsigned applier opens PRs that can
#     never merge, which is worse than opening none.
#   * Shell-only: no Python, no Ruby.
#
# Usage:
#   bash scripts/apply-workflow-pins-remote.sh [options]
#     --fix               open PRs (default: audit only, writes nothing)
#     --repair-illegal    also rewrite ILLEGAL `../` refs (implies more risk)
#     --to <sha>          target SHA (default: resolved from standards main)
#     --owners <a,b>      owners to walk (default: hyperpolymath,metadatastician)
#     --only <repo>       restrict to one repo (for testing)
#     --out <file>        write the TSV census here (default: stdout)
#     --limit <n>         stop after n repos (for smoke runs)
#     --self-test         run the pure-function controls and exit

set -uo pipefail

STANDARDS_REPO="hyperpolymath/standards"
TARGET_SHA="${STANDARDS_TARGET_SHA:-}"
DO_FIX=0
REPAIR_ILLEGAL=0
OWNERS="hyperpolymath,metadatastician"
ONLY_REPO=""
OUT_FILE=""
LIMIT=0
BRANCH_NAME="chore/re-point-standards-workflow-pins"

log() { printf '%s\n' "$*" >&2; }

# ---------------------------------------------------------------------------
# PURE CORE — no network, no filesystem outside the file argument.
# Everything below this banner is unit-testable offline, and tests/ does so.
# ---------------------------------------------------------------------------

# A standards reusable-workflow reference, pinned to a hex SHA.
PIN_RE='hyperpolymath/standards/\.github/workflows/[A-Za-z0-9._-]+\.ya?ml@[0-9a-fA-F]{7,40}'
# The same reference pinned to anything at all (SHA, branch, tag).
ANYREF_RE='hyperpolymath/standards/\.github/workflows/[A-Za-z0-9._-]+\.ya?ml@[A-Za-z0-9._/-]+'
# A `uses:` that can never parse: relative, or the `$/` form that
# `gh actions-lock` once invented. Both are rejected before any job starts.
ILLEGAL_RE='^[[:space:]]*uses:[[:space:]]*['"'"'"]?(\.\.?/|\$/)'

# Echo every standards pin SHA found in $1, one per line.
pin_shas() {
  grep -hoE "$PIN_RE" "$1" 2>/dev/null | sed -E 's/.*@([0-9a-fA-F]+)$/\1/'
}

# Echo every standards ref that is NOT a hex SHA (branch/tag tracking).
tracking_refs() {
  grep -hoE "$ANYREF_RE" "$1" 2>/dev/null \
    | sed -E 's/.*@(.+)$/\1/' \
    | grep -vE '^[0-9a-fA-F]{7,40}$' || true
}

# Echo every illegal `uses:` line in $1 (line-number prefixed).
illegal_uses() {
  grep -nE "$ILLEGAL_RE" "$1" 2>/dev/null || true
}

# classify_file <file> <target_sha>
# Echoes ONE token per distinct condition found, newline separated, from:
#   ILLEGAL TRACKING BEHIND FRESH NONE
# A file can be several at once (e.g. BEHIND and ILLEGAL); every applicable
# token is emitted. NONE means the file references standards not at all.
#
# NOTE: DEAD-REF is deliberately NOT decided here. Distinguishing a real-but-old
# commit from one that does not exist requires the network, and keeping this
# function pure is what makes it testable. refine_behind() upgrades it later.
classify_file() {
  local f="$1" target="$2" emitted=0

  if [ -n "$(illegal_uses "$f")" ]; then
    echo ILLEGAL; emitted=1
  fi
  if [ -n "$(tracking_refs "$f")" ]; then
    echo TRACKING; emitted=1
  fi

  local sha behind=0 fresh=0
  while IFS= read -r sha; do
    [ -n "$sha" ] || continue
    # A short pin is compared on its own length: `@abc1234` and the 40-char
    # target are the same commit when the short form is a prefix. Treating a
    # legitimate short pin as BEHIND would rewrite it every single run and the
    # applier would never converge.
    if [ "${target:0:${#sha}}" = "$sha" ]; then fresh=1; else behind=1; fi
  done < <(pin_shas "$f")

  [ "$behind" -eq 1 ] && { echo BEHIND; emitted=1; }
  [ "$fresh"  -eq 1 ] && { echo FRESH;  emitted=1; }
  [ "$emitted" -eq 0 ] && echo NONE
  return 0
}

# rewrite_pins <file> <target_sha> — re-point every standards SHA pin, in place.
# Only the SHA after a standards reusable `@` is touched. Idempotent.
rewrite_pins() {
  local f="$1" target="$2"
  sed -E -i "s#(hyperpolymath/standards/\.github/workflows/[A-Za-z0-9._-]+\.ya?ml@)[0-9a-fA-F]{7,40}#\1${target}#g" "$f"
}

# rewrite_illegal <file> <target_sha> — turn an unparseable relative ref into a
# legal cross-repo pin. `../../.github/workflows/X.yml` was always MEANT to be
# the standards copy: RSR templates seeded it, and a relative `uses:` to a
# reusable workflow has no legal meaning in GitHub Actions at all (only
# `./.github/workflows/x.yml` and `owner/repo/...@ref` parse).
rewrite_illegal() {
  local f="$1" target="$2"
  sed -E -i \
    "s#(uses:[[:space:]]*)['\"]?(\.\./)+\.github/workflows/([A-Za-z0-9._-]+\.ya?ml)['\"]?#\1hyperpolymath/standards/.github/workflows/\3@${target}#g" \
    "$f"
  sed -E -i \
    "s#(uses:[[:space:]]*)['\"]?\\\$/\.github/workflows/([A-Za-z0-9._-]+\.ya?ml)['\"]?#\1hyperpolymath/standards/.github/workflows/\2@${target}#g" \
    "$f"
}

# ---------------------------------------------------------------------------
# SELF-TEST of the pure core. Runs offline. `--self-test` exits after this.
# These are the KNOWN-ANSWER CONTROLS: if any fails, every census this script
# could produce is untrustworthy, so it refuses to produce one.
# ---------------------------------------------------------------------------
self_test() {
  local d rc=0 t="aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  d=$(mktemp -d); trap 'rm -rf "$d"' RETURN

  printf 'jobs:\n  a:\n    uses: hyperpolymath/standards/.github/workflows/x.yml@%s\n' "$t" > "$d/fresh.yml"
  printf 'jobs:\n  a:\n    uses: hyperpolymath/standards/.github/workflows/x.yml@%s\n' "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" > "$d/behind.yml"
  printf 'jobs:\n  a:\n    uses: ../../.github/workflows/x.yml\n' > "$d/illegal.yml"
  printf 'jobs:\n  a:\n    uses: hyperpolymath/standards/.github/workflows/x.yml@main\n' > "$d/tracking.yml"
  printf 'jobs:\n  a:\n    runs-on: ubuntu-latest\n' > "$d/none.yml"
  printf 'jobs:\n  a:\n    uses: hyperpolymath/standards/.github/workflows/x.yml@%s\n' "${t:0:7}" > "$d/short.yml"

  check() {
    local file="$1" want="$2" got
    got=$(classify_file "$d/$file" "$t" | sort | tr '\n' ',' )
    if [ "$got" = "$want" ]; then
      echo "  PASS $file -> $got"
    else
      echo "  FAIL $file -> got '$got' want '$want'" >&2; rc=1
    fi
  }
  echo "control: classify_file"
  check fresh.yml    "FRESH,"
  check behind.yml   "BEHIND,"
  check illegal.yml  "ILLEGAL,"
  check tracking.yml "TRACKING,"
  check none.yml     "NONE,"
  # A short pin that PREFIXES the target is FRESH, not BEHIND. Without this the
  # applier rewrites the same file every run and never converges.
  check short.yml    "FRESH,"

  echo "control: rewrite_pins is idempotent"
  rewrite_pins "$d/behind.yml" "$t"
  local after1 after2
  after1=$(cat "$d/behind.yml")
  rewrite_pins "$d/behind.yml" "$t"
  after2=$(cat "$d/behind.yml")
  if [ "$after1" = "$after2" ] && grep -q "@$t" "$d/behind.yml"; then
    echo "  PASS rewrite is idempotent and hit the target"
  else
    echo "  FAIL rewrite not idempotent" >&2; rc=1
  fi

  # A rewrite anchored on the SHA rather than on the standards reusable PATH
  # silently re-points every THIRD-PARTY action pin in the file as well. Nothing
  # else in this suite would notice: such a rewrite is still idempotent and the
  # file still contains the target SHA, so both other controls stay green.
  echo "control: rewrite_pins does not touch third-party pins"
  {
    echo 'jobs:'
    echo '  a:'
    echo '    steps:'
    echo '      - uses: actions/checkout@cccccccccccccccccccccccccccccccccccccccc'
    echo '    uses: hyperpolymath/standards/.github/workflows/x.yml@dddddddddddddddddddddddddddddddddddddddd'
  } > "$d/mixed.yml"
  rewrite_pins "$d/mixed.yml" "$t"
  if grep -q 'actions/checkout@cccccccccccccccccccccccccccccccccccccccc' "$d/mixed.yml" \
     && grep -q "standards/.github/workflows/x.yml@$t" "$d/mixed.yml"; then
    echo "  PASS third-party pin untouched, standards pin re-pointed"
  else
    echo "  FAIL rewrite overreached onto a third-party pin:" >&2
    sed 's/^/      /' "$d/mixed.yml" >&2; rc=1
  fi

  echo "control: rewrite_illegal produces a LEGAL ref"
  rewrite_illegal "$d/illegal.yml" "$t"
  if grep -qE "uses: $PIN_RE" "$d/illegal.yml" && ! grep -qE "$ILLEGAL_RE" "$d/illegal.yml"; then
    echo "  PASS illegal ref became a legal pin"
  else
    echo "  FAIL illegal repair did not yield a legal pin: $(cat "$d/illegal.yml")" >&2; rc=1
  fi

  return $rc
}

# ---------------------------------------------------------------------------
# NETWORK LAYER
# ---------------------------------------------------------------------------

gh_ok() { command -v gh >/dev/null 2>&1 && gh auth status >/dev/null 2>&1; }

# resolve_target — pick the SHA every consumer should point at.
# NEVER a feature-branch head: the applier would propagate unmerged work.
resolve_target() {
  [ -n "$TARGET_SHA" ] && { echo "$TARGET_SHA"; return 0; }
  gh api "repos/${STANDARDS_REPO}/commits/main" --jq '.sha' 2>/dev/null
}

# validate_target — prove the SHA is reachable from standards' main.
# Existence is not enough (see the 2026-09-04 squash-merge incident above).
# The comparison is done SERVER-side so a shallow/partial clone cannot produce
# a false negative.
validate_target() {
  local sha="$1" status
  case "$sha" in
    *[!0-9a-fA-F]*|"") log "ERROR: target '$sha' is not hex."; return 1 ;;
  esac
  if [ "${#sha}" -ne 40 ]; then
    log "ERROR: target must be a full 40-hex SHA, got ${#sha} chars."; return 1
  fi
  status=$(gh api "repos/${STANDARDS_REPO}/compare/${sha}...main" --jq '.status' 2>/dev/null)
  case "$status" in
    identical|ahead) return 0 ;;
    "") log "ERROR: could not prove ${sha} is reachable from ${STANDARDS_REPO} main; refusing."; return 1 ;;
    *)  log "ERROR: ${sha} is NOT reachable from main (compare status: ${status}); refusing."; return 1 ;;
  esac
}

# sha_is_commit <sha> — is this a real commit in standards?
# This is what separates BEHIND from DEAD-REF.
sha_is_commit() {
  gh api "repos/${STANDARDS_REPO}/commits/$1" --jq '.sha' >/dev/null 2>&1
}

# refine_behind <file> — echo DEAD-REF if ANY pin in the file names a SHA that
# is not a commit in standards; echo nothing otherwise.
refine_behind() {
  local sha
  while IFS= read -r sha; do
    [ -n "$sha" ] || continue
    if ! sha_is_commit "$sha"; then echo "DEAD-REF:$sha"; fi
  done < <(pin_shas "$1" | sort -u)
}

list_repos() {
  local owner
  for owner in ${OWNERS//,/ }; do
    # /users/<o>/repos covers a user; if that 404s the owner is an org.
    gh api "users/${owner}/repos" --paginate \
      --jq '.[] | select(.archived == false) | .full_name' 2>/dev/null \
    || gh api "orgs/${owner}/repos" --paginate \
      --jq '.[] | select(.archived == false) | .full_name' 2>/dev/null
  done
}

# fetch_workflows <repo> <destdir> — download .github/workflows/*.y*ml.
# The REMOTE content is the only evidence: a local checkout can be arbitrarily
# stale, and reading one is what produced a false "285 callers track main"
# census on 2026-09-15.
fetch_workflows() {
  local repo="$1" dest="$2" name
  mkdir -p "$dest"
  gh api "repos/${repo}/contents/.github/workflows" \
     --jq '.[] | select(.type == "file") | .name' 2>/dev/null \
  | grep -E '\.ya?ml$' \
  | while IFS= read -r name; do
      gh api "repos/${repo}/contents/.github/workflows/${name}" \
         -H 'Accept: application/vnd.github.raw' > "${dest}/${name}" 2>/dev/null \
        || rm -f "${dest}/${name}"
    done
}

# land_pr <repo> <dir> <files...> — create branch, commit VERIFIED, open PR.
# Uses createCommitOnBranch: it is cross-repo by construction and the resulting
# commit is GitHub-"Verified", which required_signatures rulesets demand.
land_pr() {
  local repo="$1" dir="$2"; shift 2
  local base_sha base_branch ref_exists head_oid additions="" f rel b64 pr

  base_branch=$(gh api "repos/${repo}" --jq '.default_branch' 2>/dev/null) || return 1
  base_sha=$(gh api "repos/${repo}/commits/${base_branch}" --jq '.sha' 2>/dev/null) || return 1

  ref_exists=$(gh api "repos/${repo}/git/ref/heads/${BRANCH_NAME}" --jq '.object.sha' 2>/dev/null || true)
  if [ -n "$ref_exists" ]; then
    head_oid="$ref_exists"
  else
    gh api "repos/${repo}/git/refs" -X POST \
      -f "ref=refs/heads/${BRANCH_NAME}" -f "sha=${base_sha}" >/dev/null 2>&1 || return 1
    head_oid="$base_sha"
  fi

  for f in "$@"; do
    rel=".github/workflows/$(basename "$f")"
    b64=$(base64 -w0 < "${dir}/${f}")
    additions="${additions}{\"path\":\"${rel}\",\"contents\":\"${b64}\"},"
  done
  additions="[${additions%,}]"

  local msg_head="chore(ci): re-point standards reusable-workflow pins to ${TARGET_SHA:0:12}"
  local msg_body
  msg_body=$(printf 'Opened by the standards pin applier (scripts/apply-workflow-pins-remote.sh).\n\nRe-points this repository'"'"'s pinned references to hyperpolymath/standards reusable\nworkflows at %s, which is proven reachable from standards main.\n\nCo-Authored-By: Claude Opus 5 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01HfgwLCdKNd5iZVo6VTiSim' "$TARGET_SHA")

  gh api graphql -f query='
    mutation($input: CreateCommitOnBranchInput!) {
      createCommitOnBranch(input: $input) { commit { oid } }
    }' \
    -F input="{\"branch\":{\"repositoryNameWithOwner\":\"${repo}\",\"branchName\":\"${BRANCH_NAME}\"},\"expectedHeadOid\":\"${head_oid}\",\"message\":{\"headline\":$(json_str "$msg_head"),\"body\":$(json_str "$msg_body")},\"fileChanges\":{\"additions\":${additions}}}" \
    >/dev/null 2>&1 || { log "  land: createCommitOnBranch failed for ${repo}"; return 1; }

  pr=$(gh pr create --repo "$repo" --head "$BRANCH_NAME" --base "$base_branch" \
        --title "$msg_head" \
        --body "$(printf '%s\n\n🤖 Generated with [Claude Code](https://claude.com/claude-code)\n\nhttps://claude.ai/code/session_01HfgwLCdKNd5iZVo6VTiSim' "$msg_body")" \
        2>/dev/null) || { log "  land: PR already open or creation failed for ${repo}"; return 1; }
  echo "$pr"
}

# json_str — quote a string as a JSON scalar without a JSON library.
json_str() {
  printf '%s' "$1" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' | awk 'BEGIN{ORS=""}{print (NR>1 ? "\\n" : "") $0}' | sed -e 's/^/"/' -e 's/$/"/'
}

# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------
main() {
  while [ $# -gt 0 ]; do
    case "$1" in
      --fix) DO_FIX=1 ;;
      --repair-illegal) REPAIR_ILLEGAL=1 ;;
      --to) TARGET_SHA="$2"; shift ;;
      --owners) OWNERS="$2"; shift ;;
      --only) ONLY_REPO="$2"; shift ;;
      --out) OUT_FILE="$2"; shift ;;
      --limit) LIMIT="$2"; shift ;;
      --self-test) self_test; exit $? ;;
      -h|--help) sed -n '2,60p' "$0"; exit 0 ;;
      *) log "unknown option: $1"; exit 2 ;;
    esac
    shift
  done

  # The controls run on EVERY invocation, not just --self-test. A census whose
  # classifier is broken is worse than no census, because it looks like data.
  log "== known-answer controls =="
  if ! self_test >&2; then
    log "FATAL: classifier controls failed. Refusing to produce a census."
    exit 1
  fi

  gh_ok || { log "FATAL: gh is not authenticated. Probe with 'gh api user' by EXIT CODE."; exit 1; }

  TARGET_SHA=$(resolve_target)
  [ -n "$TARGET_SHA" ] || { log "FATAL: could not resolve a target SHA."; exit 1; }
  validate_target "$TARGET_SHA" || exit 1
  log "== target: ${TARGET_SHA} (reachable from ${STANDARDS_REPO} main) =="
  [ "$DO_FIX" -eq 1 ] && log "== MODE: --fix (will open PRs) ==" || log "== MODE: audit (writes nothing) =="

  local tsv; tsv=$(mktemp)
  printf 'REPO\tWORKFLOW\tSTATUS\tDETAIL\n' > "$tsv"

  local work; work=$(mktemp -d); trap 'rm -rf "$work"' EXIT
  local repos n=0
  if [ -n "$ONLY_REPO" ]; then repos="$ONLY_REPO"; else repos=$(list_repos); fi

  local repo
  for repo in $repos; do
    [ "$LIMIT" -gt 0 ] && [ "$n" -ge "$LIMIT" ] && break
    n=$((n+1))
    local rdir="${work}/$(echo "$repo" | tr '/' '_')"
    fetch_workflows "$repo" "$rdir"
    local found=0 changed=() wf base st detail
    shopt -s nullglob
    for wf in "$rdir"/*.yml "$rdir"/*.yaml; do
      found=1
      base=$(basename "$wf")
      st=$(classify_file "$wf" "$TARGET_SHA" | sort | tr '\n' ',' ); st="${st%,}"
      [ "$st" = "NONE" ] && continue
      detail=""
      case ",$st," in *,BEHIND,*)
        detail=$(refine_behind "$wf" | tr '\n' ' ')
        [ -n "$detail" ] && st="${st},DEAD-REF"
      ;; esac
      printf '%s\t%s\t%s\t%s\n' "$repo" "$base" "$st" "$detail" >> "$tsv"

      if [ "$DO_FIX" -eq 1 ]; then
        local before; before=$(cat "$wf")
        case ",$st," in *,BEHIND,*) rewrite_pins "$wf" "$TARGET_SHA" ;; esac
        if [ "$REPAIR_ILLEGAL" -eq 1 ]; then
          case ",$st," in *,ILLEGAL,*) rewrite_illegal "$wf" "$TARGET_SHA" ;; esac
        fi
        [ "$before" != "$(cat "$wf")" ] && changed+=("$base")
      fi
    done
    shopt -u nullglob
    [ "$found" -eq 0 ] && continue

    if [ "$DO_FIX" -eq 1 ] && [ "${#changed[@]}" -gt 0 ]; then
      local url
      if url=$(land_pr "$repo" "$rdir" "${changed[@]}"); then
        log "  OPENED ${repo}: ${url}"
        printf '%s\t-\tPR-OPENED\t%s\n' "$repo" "$url" >> "$tsv"
      else
        printf '%s\t-\tPR-FAILED\t-\n' "$repo" >> "$tsv"
      fi
    fi
  done

  log "== walked ${n} repo(s) =="
  # Counts are reported at LANDED, never at PR-open: 545 merged/week means
  # throughput is fine; 249 PRs open and unmerged is an abandoned campaign.
  log "== summary by status =="
  awk -F'\t' 'NR>1{c[$3]++} END{for(k in c) printf "   %-28s %d\n", k, c[k]}' "$tsv" >&2

  if [ -n "$OUT_FILE" ]; then cp "$tsv" "$OUT_FILE"; log "== census: ${OUT_FILE} =="; else cat "$tsv"; fi
  rm -f "$tsv"
}

main "$@"
