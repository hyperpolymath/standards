#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# apply-protection-floor.sh -- establish the BASE PROTECTION FLOOR on a repo that has none.
#
# The floor is owner ruling D94: `deletion` + `non_fast_forward`, and nothing else.
#   * branch target -> `~DEFAULT_BRANCH`   (config/rulesets/branch-floor.json)
#   * tag target    -> `~ALL`              (config/rulesets/tag-floor.json)
# Bypass is the empty list, per D96.
#
# WHY A THIRD APPLIER, BESIDE apply-branch-gates.sh AND apply-tag-ruleset-canon.sh
#   apply-branch-gates.sh says so itself: "It never CREATES a ruleset. A repo with no
#   active branch ruleset is reported NORULESET. Creating branch protection where none
#   exists is a policy act, not a gate-fill." That policy act is now ruled (D94-D96), so
#   it gets its own script and the gate-filler stays a gate-filler.
#
# WHY THE FLOOR IS EXACTLY TWO RULES
#   It needs no per-repo derivation and no CI check to be satisfiable, so it can never be
#   a vacuous gate and it cannot block a single PR. It stops precisely two irreversible
#   accidents: deleting the default branch, and force-pushing over it.
#
# WHAT IT DELIBERATELY DOES NOT DO
#   * It never EDITS an existing ruleset. Rulesets are additive (D95); the floor is POSTed
#     alongside. A PUT would replace the whole object, and that is how an applier silently
#     revives what a human switched off (the defect repaired in #1030).
#   * It never re-enables a disabled ruleset. Disabled is a decision, not drift.
#   * It never writes an ORG-INHERITED ruleset. `PUT` to a repo path for an org ruleset
#     404s; the discriminator is `.source_type`, and an ABSENT discriminator REFUSES
#     rather than defaulting to the writable arm.
#   * It never touches a member of the gcrypt-vault class. See below -- this one is not a
#     nicety, it is the difference between a hardened repo and a dead backup.
#
# 🚨 THE GCRYPT-VAULT EXCLUSION (owner ruling D50)
#   git-remote-gcrypt FORCE-PUSHES ON EVERY SYNC. `non_fast_forward` on a vault does not
#   harden it; it stops the hourly backup, silently, at the next timer fire. Vaults carry
#   a deliberate `deletion`-only guard, which means naive floor logic -- "has deletion,
#   lacks non_fast_forward, therefore complete the floor" -- writes exactly the fatal rule.
#   Membership is the EXPLICIT LIST in config/rulesets/gcrypt-vault-class.txt and NEVER a
#   name match: two estate repos match /vault/ and are not members.
#   A missing class file is a REFUSAL, not an empty exclusion set.
#
# USAGE
#   scripts/apply-protection-floor.sh --repos <file>            # report only (default)
#   scripts/apply-protection-floor.sh --repos <file> --apply    # actually create
#   scripts/apply-protection-floor.sh --repos <file> --target tag
#   Optional: --floor-even-if-covered   also floor repos whose cover comes from a RICHER
#                                       ruleset (see COVERED-BY-RICHER below). Off by
#                                       default: that is a policy call, not a gap-fill.
#
# STATES (TSV: repo <TAB> state <TAB> detail)
#   CONVERGED            the exact floor already exists -- nothing to do
#   COVERED-BY-RICHER    both rules are in force, but from a richer ruleset. NOT the same
#                        as converged: that cover vanishes the moment the richer ruleset is
#                        disabled, which is how 230 repos lost protection on 2026-09-22.
#   WOULD-CREATE         report mode; --apply would POST the floor here
#   CREATED              POSTed and verified in force
#   WROTE-UNVERIFIED     POSTed, but the verifying read did not come back. Never assume.
#   EXCLUDED-D50         gcrypt vault; no write, ever
#   ARCHIVED             archived repo; ruleset POST 403s. Skipped, not failed.
#   PLAN-EXCLUDED        403/422 from the rulesets endpoint (private repo / plan limit).
#                        Counted as neither covered nor failed -- this is the honest
#                        denominator for "repos this tooling cannot reach".
#   ORG-INHERITED        covered by an org ruleset; cure once at the org, never per repo
#   AMBIGUOUS            more than one active repo-level floor-shaped ruleset. Fail closed.
#   REFUSED              a ruleset carried no `.source_type`. Fail closed.
#   UNKNOWN              a read was throttled or errored. SKIPPED, never recorded as clean.
#
# A THROTTLED READ IS SKIPPED, NEVER RECORDED. A junk row is indistinguishable from an
# honest one, and resume logic keyed on "repo already present" excludes it forever.
set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONF="$REPO_ROOT/config/rulesets"
VAULT_CLASS="$CONF/gcrypt-vault-class.txt"

APPLY=0
TARGET="branch"
REPOS_FILE=""
FLOOR_EVEN_IF_COVERED=0

TMPDIR_ERR="$(mktemp -t protfloor-err.XXXXXX)"
# Org rulesets are IDENTICAL across every repo in the org, so their bodies are fetched
# once and cached by ruleset id rather than re-read per repo.
ORG_CACHE="$(mktemp -d -t protfloor-org.XXXXXX)"
# Remove the temporary error file and organization-ruleset cache.
cleanup() { rm -f "$TMPDIR_ERR"; rm -rf "$ORG_CACHE"; }
trap cleanup EXIT

# Print a fatal error and exit with the script's refusal status.
die() { printf 'FATAL: %s\n' "$*" >&2; exit 2; }
# Emit one tab-separated repository status row.
report() { printf '%s\t%s\t%s\n' "$1" "$2" "${3:-}"; }

while [ $# -gt 0 ]; do
  case "$1" in
    --apply)                 APPLY=1 ;;
    --target)                TARGET="${2:-}"; shift ;;
    --repos)                 REPOS_FILE="${2:-}"; shift ;;
    --floor-even-if-covered) FLOOR_EVEN_IF_COVERED=1 ;;
    -h|--help)               sed -n '2,60p' "${BASH_SOURCE[0]}"; exit 0 ;;
    *)                       die "unknown argument: $1" ;;
  esac
  shift
done

case "$TARGET" in
  branch) CANON="$CONF/branch-floor.json"; WANT_INCLUDE='["~DEFAULT_BRANCH"]' ;;
  tag)    CANON="$CONF/tag-floor.json";    WANT_INCLUDE='["~ALL"]' ;;
  *)      die "--target must be 'branch' or 'tag', got '$TARGET'" ;;
esac

[ -r "$CANON" ] || die "canon file missing: $CANON"

# FAIL CLOSED. A missing class file must never read as "no vaults to protect".
[ -r "$VAULT_CLASS" ] || die "gcrypt-vault class file missing: $VAULT_CLASS -- refusing to run, because an absent exclusion list is indistinguishable from an empty one and D50 members would be written"

[ -n "$REPOS_FILE" ] || die "--repos <file> is required (one owner/repo per line)"
[ -r "$REPOS_FILE" ] || die "repo list not readable: $REPOS_FILE"

# The floor's rule types, derived FROM THE CANON -- never typed a second time here, so the
# script and the file can never disagree.
FLOOR_TYPES="$(jq -r '[.rules[].type] | sort | join(",")' "$CANON")"
[ -n "$FLOOR_TYPES" ] || die "canon $CANON declares no rules"

# Body actually POSTed. `name` is kept (a POST creates, so the name is ours to set).
CANON_BODY="$(jq -c . "$CANON")"

VAULTS="$(command grep -vE '^[[:space:]]*(#|$)' "$VAULT_CLASS" | tr -d ' \t')"
[ -n "$VAULTS" ] || die "gcrypt-vault class file lists no members -- refusing; D50 names two"

TARGETS="$(command grep -vE '^[[:space:]]*(#|$)' "$REPOS_FILE" | tr -d ' \t' | sort -u)"
[ -n "$TARGETS" ] || die "refusing to report a clean sweep over nothing: $REPOS_FILE yielded no repos"

# Return success when the repository is an explicitly listed gcrypt vault.
is_vault() {
  printf '%s\n' "$VAULTS" | command grep -qxF "$1"
}

# ---------------------------------------------------------------------------
printf 'repo\tstate\tdetail\n'

printf '%s\n' "$TARGETS" | while IFS= read -r repo; do
  [ -n "$repo" ] || continue

  # 1. D50 FIRST, before any read. A vault must not even be a candidate.
  if is_vault "$repo"; then
    report "$repo" "EXCLUDED-D50" "gcrypt vault: force-pushes every sync; non_fast_forward would stop the backup"
    continue
  fi

  # 2. Archived repos 403 on a ruleset write while every GET succeeds.
  meta="$(gh api "repos/$repo" 2>/dev/null)" || { report "$repo" "UNKNOWN" "repos/$repo read failed"; continue; }
  [ -n "$meta" ] || { report "$repo" "UNKNOWN" "repos/$repo returned empty"; continue; }
  if [ "$(printf '%s' "$meta" | jq -r '.archived')" = "true" ]; then
    report "$repo" "ARCHIVED" "ruleset POST 403s on an archived repo; unarchive/write/re-archive is a separate, explicit act"
    continue
  fi
  default_branch="$(printf '%s' "$meta" | jq -r '.default_branch // empty')"

  # 3. List rulesets. 403/422 here is the private-repo / plan-limit arm.
  if ! listing="$(gh api "repos/$repo/rulesets" 2>"$TMPDIR_ERR")"; then
    err="$(cat "$TMPDIR_ERR" 2>/dev/null)"
    case "$err" in
      *"rate limit"*|*"abuse"*) report "$repo" "UNKNOWN"       "rulesets list throttled: ${err%%$'\n'*}" ;;
      *403*|*422*|*"upgrade"*) report "$repo" "PLAN-EXCLUDED" "rulesets endpoint refused: ${err%%$'\n'*}" ;;
      *)                       report "$repo" "UNKNOWN"       "rulesets list failed: ${err%%$'\n'*}" ;;
    esac
    continue
  fi
  [ -n "$listing" ] || { report "$repo" "UNKNOWN" "rulesets list returned empty"; continue; }

  # 4. An entry with no `.source_type` is a REFUSAL, not a default to the writable arm.
  if printf '%s' "$listing" | jq -e 'any(.[]?; has("source_type") | not)' >/dev/null 2>&1; then
    report "$repo" "REFUSED" "a ruleset carried no .source_type; cannot tell repo-level from org-inherited, failing closed"
    continue
  fi

  org_n="$(printf '%s' "$listing" | jq "[.[]? | select(.source_type==\"Organization\" and .target==\"$TARGET\" and .enforcement==\"active\")] | length")"
  repo_ids="$(printf '%s' "$listing" | jq -r ".[]? | select(.source_type==\"Repository\" and .target==\"$TARGET\" and .enforcement==\"active\") | .id")"
  org_ids="$(printf '%s' "$listing" | jq -r ".[]? | select(.source_type==\"Organization\" and .target==\"$TARGET\" and .enforcement==\"active\") | .id")"

  # 5. Walk the active repo-level rulesets of this target and classify.
  exact_n=0; exact_ids=""; union=""
  if [ -n "$repo_ids" ]; then
    while IFS= read -r rid; do
      [ -n "$rid" ] || continue
      body="$(gh api "repos/$repo/rulesets/$rid" 2>/dev/null)" || { body=""; }
      if [ -z "$body" ]; then
        exact_n=-1   # sentinel: a read we could not complete
        break
      fi
      types="$(printf '%s' "$body" | jq -r '[.rules[].type] | sort | join(",")')"
      inc="$(printf '%s' "$body" | jq -c '.conditions.ref_name.include')"
      byp="$(printf '%s' "$body" | jq -c '[.bypass_actors[]?] | length')"
      union="$union,$types"
      if [ "$types" = "$FLOOR_TYPES" ] && [ "$inc" = "$WANT_INCLUDE" ] && [ "$byp" = "0" ]; then
        exact_n=$((exact_n + 1)); exact_ids="$exact_ids $rid"
      fi
    done <<EOF
$repo_ids
EOF
  fi

  if [ "$exact_n" -lt 0 ]; then
    report "$repo" "UNKNOWN" "a ruleset body read was throttled; skipped rather than recorded"
    continue
  fi
  if [ "$exact_n" -gt 1 ]; then
    report "$repo" "AMBIGUOUS" "more than one active repo-level floor-shaped ruleset:$exact_ids"
    continue
  fi
  if [ "$exact_n" -eq 1 ]; then
    report "$repo" "CONVERGED" "floor already present as ruleset$exact_ids"
    continue
  fi

  # 5b. Org-inherited rulesets of this target also put rules IN FORCE. They are never
  # writable per repo, so they are kept in their OWN union: a repo-level cover can be
  # cured here, an org-level cover must be cured once at the org. Omitting this union
  # is what made ORG-INHERITED unreachable and reported 67 covered repos as WOULD-CREATE.
  union_org=""; org_byp_max=0; org_read_ok=1
  if [ -n "$org_ids" ]; then
    while IFS= read -r rid; do
      [ -n "$rid" ] || continue
      cache="$ORG_CACHE/$rid"
      if [ ! -s "$cache" ]; then
        gh api "repos/$repo/rulesets/$rid" > "$cache" 2>/dev/null || :
      fi
      if [ ! -s "$cache" ]; then org_read_ok=0; break; fi
      union_org="$union_org,$(jq -r '[.rules[].type] | sort | join(",")' "$cache")"
      b="$(jq -r '[.bypass_actors[]?] | length' "$cache")"
      [ "$b" -gt "$org_byp_max" ] && org_byp_max="$b"
    done <<EOF
$org_ids
EOF
  fi
  if [ "$org_read_ok" -eq 0 ]; then
    report "$repo" "UNKNOWN" "an org ruleset body read was throttled; skipped rather than recorded"
    continue
  fi

  # 6. Is the floor nevertheless in force, from something richer?
  covered=1
  printf '%s\n' "$FLOOR_TYPES" | tr ',' '\n' | while IFS= read -r t; do
    [ -n "$t" ] || continue
    printf '%s' ",$union," | command grep -q ",$t," || exit 7
  done || covered=0

  covered_org=1
  printf '%s\n' "$FLOOR_TYPES" | tr ',' '\n' | while IFS= read -r t; do
    [ -n "$t" ] || continue
    printf '%s' ",$union,$union_org," | command grep -q ",$t," || exit 7
  done || covered_org=0

  if [ "$covered" -eq 1 ] && [ -n "$union" ] && [ "$FLOOR_EVEN_IF_COVERED" -eq 0 ]; then
    report "$repo" "COVERED-BY-RICHER" "both floor rules in force from a richer active ruleset; cover ends if it is ever disabled. --floor-even-if-covered adds a standalone floor"
    continue
  fi

  # Org cover is reported even under --floor-even-if-covered: a per-repo duplicate of a
  # rule already in force org-wide is noise, and the cure belongs at the org either way.
  if [ "$org_n" -gt 0 ] && [ "$covered_org" -eq 1 ]; then
    report "$repo" "ORG-INHERITED" "covered by $org_n active org ruleset(s) (max bypass_actors=$org_byp_max); cure once at the org, never per repo"
    continue
  fi

  # 7. The write.
  if [ "$APPLY" -eq 0 ]; then
    report "$repo" "WOULD-CREATE" "no floor in force; --apply would POST $(basename "$CANON")"
    continue
  fi

  if ! created="$(printf '%s' "$CANON_BODY" | gh api --method POST "repos/$repo/rulesets" --input - 2>"$TMPDIR_ERR")"; then
    report "$repo" "UNKNOWN" "POST failed: $(head -1 "$TMPDIR_ERR" 2>/dev/null)"
    continue
  fi
  new_id="$(printf '%s' "$created" | jq -r '.id // empty')"

  # 8. VERIFY BY AN INDEPENDENT READ. A ruleset write has returned 200 with an empty body
  #    and not applied -- never trust the write's own response.
  if [ "$TARGET" = "branch" ] && [ -n "$default_branch" ]; then
    eff="$(gh api "repos/$repo/rules/branches/$default_branch" 2>/dev/null)" || eff=""
    if [ -z "$eff" ]; then
      report "$repo" "WROTE-UNVERIFIED" "POSTed id=$new_id; effective-rules read did not return"
      continue
    fi
    missing=""
    printf '%s\n' "$FLOOR_TYPES" | tr ',' '\n' | while IFS= read -r t; do
      [ -n "$t" ] || continue
      printf '%s' "$eff" | jq -e --arg t "$t" 'any(.[]?; .type==$t)' >/dev/null 2>&1 || exit 7
    done || missing="yes"
    if [ -n "$missing" ]; then
      report "$repo" "WROTE-UNVERIFIED" "POSTed id=$new_id but the effective rules on $default_branch do not show both floor types"
      continue
    fi
    report "$repo" "CREATED" "id=$new_id; verified effective on $default_branch"
  else
    back="$(gh api "repos/$repo/rulesets/$new_id" 2>/dev/null)" || back=""
    got="$(printf '%s' "$back" | jq -r '[.rules[].type] | sort | join(",")' 2>/dev/null)"
    if [ "$got" = "$FLOOR_TYPES" ]; then
      report "$repo" "CREATED" "id=$new_id; verified by re-read"
    else
      report "$repo" "WROTE-UNVERIFIED" "POSTed id=$new_id; re-read returned '${got:-<nothing>}'"
    fi
  fi
done
