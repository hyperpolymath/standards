#!/usr/bin/env bash
# lock-selfcheck.sh — is a given `standards` commit SAFE TO PIN A CALLER TO?
#
# WHY THIS EXISTS
# ---------------
# `uses: org/repo/.github/workflows/x.yml@<sha>` is resolved at workflow
# STARTUP, and GitHub validates the callee against **the callee repo's own
# `.github/workflows/actions.lock` AS IT EXISTS AT THAT SHA**. If the reusable
# names an action ref the lock at that same SHA does not key, every caller dies
# before any job exists — and the failure is near-invisible:
#
#   * conclusion is `failure`, NOT `startup_failure`
#   * the run has ZERO jobs
#   * the run's `name` equals its `path`
#   * neither REST nor GraphQL carries a reason — only the run PAGE does
#
# Dependabot causes this routinely: it bumps a `uses:` inside a workflow and
# never regenerates the lockfile. So **capability is not monotonic in time** —
# the newest `standards` commit can be strictly less usable than an older one,
# and "bump every caller to HEAD" is exactly the wrong reflex.
#
# Before a pin sweep repairs N callers onto a target SHA, that SHA must pass
# this check. Otherwise the sweep converts N silently-ungated repos into N
# loudly-broken ones.
#
# USAGE
#   lock-selfcheck.sh <sha> [<sha>...]        # uses $STANDARDS_DIR or default
#   STANDARDS_DIR=/path/to/standards lock-selfcheck.sh <sha>
#
# Reads git objects ONLY (`git show`); never checks out, fetches, or writes.
# Safe against a checkout shared with a live concurrent writer.
#
# EXIT: 0 if every SHA examined self-validates; 1 if any does not; 2 on misuse.
#
# ⚠ SCOPE LIMIT, STATED SO IT IS NOT MISREAD: this proves a commit is
# INTERNALLY CONSISTENT. It does NOT prove the commit is REACHABLE at the
# remote. A local object store can hold commits GitHub has since orphaned
# (a garbage-collected commit is the general case). Reachability is a
# SEPARATE probe and must be made against the remote.
#
# SPDX-License-Identifier: MPL-2.0

set -uo pipefail

STANDARDS_DIR="${STANDARDS_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
LOCK_PATH='.github/workflows/actions.lock'

if [ "$#" -eq 0 ]; then
  echo "usage: $0 <sha> [<sha>...]" >&2
  exit 2
fi

if ! git -C "$STANDARDS_DIR" rev-parse --git-dir >/dev/null 2>&1; then
  echo "FATAL: not a git repository: $STANDARDS_DIR" >&2
  exit 2
fi

TMP="${TMPDIR:-/tmp}/lock-selfcheck.$$"
mkdir -p "$TMP"
trap 'rm -rf "$TMP"' EXIT

# Normalise a `uses:` value to the key shape the lockfile uses:
#   github/codeql-action/upload-sarif@SHA  ->  github/codeql-action@SHA
#   actions/checkout@SHA                   ->  actions/checkout@SHA
# Local refs (./...) and docker:// refs are not lockfile-keyed and are skipped
# by the caller, not here.
normalise_ref() {
  printf '%s\n' "$1" | awk -F'@' '{
    n = split($1, p, "/")
    owner_repo = (n >= 2) ? p[1] "/" p[2] : $1
    print owner_repo "@" $2
  }'
}

overall_rc=0

for SHA in "$@"; do
  echo "=============================================================="
  if ! git -C "$STANDARDS_DIR" cat-file -e "${SHA}^{commit}" 2>/dev/null; then
    echo "SHA ${SHA}: ABSENT from local object store — cannot check"
    overall_rc=1
    continue
  fi
  SHORT=$(git -C "$STANDARDS_DIR" rev-parse --short=12 "$SHA")
  WHEN=$(git -C "$STANDARDS_DIR" show -s --format='%ci' "$SHA")
  SUBJ=$(git -C "$STANDARDS_DIR" show -s --format='%s' "$SHA")
  echo "SHA ${SHORT}  ${WHEN}"
  echo "     ${SUBJ}"

  if ! git -C "$STANDARDS_DIR" show "${SHA}:${LOCK_PATH}" > "$TMP/lock" 2>/dev/null; then
    # THIRD STATE, and not the same as POISON. With no lockfile there is
    # nothing for GitHub to validate the callee against, so a caller pinned
    # here does NOT die at startup -- it simply gets no immutable-action
    # guarantee. Predates the lock regime (which begins ~2026-08-03).
    # Reporting this as unsafe-to-pin would be a false alarm, so it does not
    # set the failure exit code.
    echo "  VERDICT: NO-LOCK — ${LOCK_PATH} does not exist at this SHA."
    echo "           NOT startup-fatal: with no lock there is nothing to"
    echo "           validate against. But callers pinned here get no"
    echo "           action-pinning guarantee either. Pre-dates the lock regime."
    continue
  fi

  # Parse the lock into "<workflow-path>\t<owner/repo@sha>" pairs.
  awk '
    /^workflows:/ { in_wf = 1; next }
    !in_wf { next }
    # A workflow key line:  \x27.github/workflows/foo.yml\x27:
    /^[[:space:]]+\x27[^\x27]+\x27:/ {
      line = $0
      sub(/^[[:space:]]+\x27/, "", line)
      sub(/\x27:.*$/, "", line)
      cur = line
      next
    }
    # An entry line:  - \x27owner/repo@sha\x27
    /^[[:space:]]*-[[:space:]]*\x27[^\x27]+\x27/ {
      line = $0
      sub(/^[[:space:]]*-[[:space:]]*\x27/, "", line)
      sub(/\x27.*$/, "", line)
      if (cur != "") print cur "\t" line
    }
  ' "$TMP/lock" | sort -u > "$TMP/locked"

  locked_pairs=$(wc -l < "$TMP/locked")
  locked_wfs=$(cut -f1 "$TMP/locked" | sort -u | wc -l)

  # Enumerate the workflow files present at this SHA.
  git -C "$STANDARDS_DIR" ls-tree -r --name-only "$SHA" -- .github/workflows \
    | /usr/bin/grep -E '\.ya?ml$' | sort > "$TMP/wfs"
  n_wfs=$(wc -l < "$TMP/wfs")

  # The lockfile LOWERCASES the owner/repo it keys ('swatinem/rust-cache')
  # while the workflow writes the upstream casing ('Swatinem/rust-cache').
  # Same action, same SHA. Comparing case-sensitively invents defects, so
  # fold both sides before matching. (This cost one false 'POISON' on
  # rust-ci-reusable.yml before it was caught.)
  tr 'A-Z' 'a-z' < "$TMP/locked" > "$TMP/locked.lc"

  : > "$TMP/missing"
  : > "$TMP/unkeyed_wf"
  : > "$TMP/scanned"

  while IFS= read -r wf; do
    [ -n "$wf" ] || continue
    git -C "$STANDARDS_DIR" show "${SHA}:${wf}" 2>/dev/null > "$TMP/wfbody" || continue
    # Extract every `uses:` value, strip inline comments and quotes.
    /usr/bin/grep -hoE '^[[:space:]]*(-[[:space:]]*)?uses:[[:space:]]*[^[:space:]#]+' "$TMP/wfbody" \
      | sed -E 's/.*uses:[[:space:]]*//; s/^["\x27]//; s/["\x27]$//' \
      | while IFS= read -r ref; do
          [ -n "$ref" ] || continue
          case "$ref" in
            ./*|docker://*) continue ;;   # local / docker: not lockfile-keyed
          esac
          case "$ref" in
            *@*) : ;;
            *) continue ;;                # unpinned (no @) — a different defect
          esac
          # A reusable-workflow call carries a .yml before the @; the lockfile
          # keys ACTIONS, not reusable calls. Report those separately.
          before="${ref%@*}"
          case "$before" in
            *.yml|*.yaml) printf '%s\t%s\tREUSABLE-CALL\n' "$wf" "$ref" >> "$TMP/scanned"; continue ;;
          esac
          key=$(normalise_ref "$ref")
          printf '%s\t%s\tACTION\n' "$wf" "$key" >> "$TMP/scanned"
          key_lc=$(printf '%s' "$key" | tr 'A-Z' 'a-z')
          wf_lc=$(printf '%s' "$wf" | tr 'A-Z' 'a-z')
          # Two DIFFERENT questions, deliberately not conflated:
          #   (1) this workflow has NO entry in the lock at all  -> UNKEYED
          #   (2) this workflow IS keyed, but not for this ref   -> MISSING
          # Only (2) is proven to kill a caller at startup. (1) is reported
          # separately because the lock also carries explicit ': []' entries,
          # which proves absence and "uses nothing" are distinct states the
          # generator can express -- so absence means the generator never saw
          # the file, not that the file is forbidden actions.
          if ! /usr/bin/grep -qF "$(printf '%s\t' "$wf_lc")" "$TMP/locked.lc"; then
            printf '%s\t%s\n' "$wf" "$key" >> "$TMP/unkeyed_wf"
          elif ! /usr/bin/grep -qF "$(printf '%s\t%s' "$wf_lc" "$key_lc")" "$TMP/locked.lc"; then
            printf '%s\t%s\n' "$wf" "$key" >> "$TMP/missing"
          fi
        done
  done < "$TMP/wfs"

  n_actions=$(/usr/bin/grep -c 'ACTION$' "$TMP/scanned" 2>/dev/null || echo 0)
  n_reusable=$(/usr/bin/grep -c 'REUSABLE-CALL$' "$TMP/scanned" 2>/dev/null || echo 0)
  n_missing=$(sort -u "$TMP/missing" 2>/dev/null | wc -l)
  n_unkeyed=$(sort -u "$TMP/unkeyed_wf" 2>/dev/null | wc -l)
  n_unkeyed_wf=$(cut -f1 "$TMP/unkeyed_wf" 2>/dev/null | sort -u | wc -l)

  echo "  workflows at this SHA: ${n_wfs}   lockfile keys: ${locked_wfs} workflow(s), ${locked_pairs} pair(s)"
  echo "  action refs used: ${n_actions}   reusable-workflow calls (not lock-keyed): ${n_reusable}"
  if [ "$n_unkeyed" -gt 0 ]; then
    echo "  note: ${n_unkeyed_wf} workflow(s) have NO lock entry at all (${n_unkeyed} ref(s)) —"
    echo "        a separate question from the verdict below, and not counted in it:"
    cut -f1 "$TMP/unkeyed_wf" | sort -u | sed 's/^/          /'
  fi

  if [ "$n_missing" -eq 0 ]; then
    echo "  VERDICT: SELF-CONSISTENT — every action ref used is keyed in this SHA's own lock."
    echo "           (Reachability at the remote is NOT proven by this check.)"
  else
    echo "  VERDICT: POISON — ${n_missing} action ref(s) used but NOT keyed in this SHA's own lock."
    echo "           Any caller pinned here dies at startup with 0 jobs and no stated reason."
    sort -u "$TMP/missing" | while IFS=$'\t' read -r wf key; do
      echo "             ${wf}"
      echo "               uses  ${key}"
      locked_for=$(/usr/bin/grep -F "$(printf '%s\t' "$wf")" "$TMP/locked" | cut -f2 | sort -u | tr '\n' ' ')
      echo "               lock keys for this workflow: ${locked_for:-<none>}"
    done
    overall_rc=1
  fi
done

echo "=============================================================="
if [ "$overall_rc" -eq 0 ]; then
  echo "ALL EXAMINED SHAs SELF-VALIDATE (examined: $#)"
else
  echo "AT LEAST ONE SHA IS UNSAFE TO PIN TO (examined: $#)"
fi
exit "$overall_rc"
