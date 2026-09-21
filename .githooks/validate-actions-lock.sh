#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Actions lockfile coverage: every SHA-pinned `uses:` ref in .github/workflows/
# AND in .github/actions/*/action.yml must have a matching entry in
# .github/workflows/actions.lock.
#
# WHY THIS EXISTS
#
#   GitHub validates a reusable workflow against the CALLEE repo's own
#   actions.lock. A lockfile that has drifted from its workflows therefore
#   does not break this repo -- it breaks every repo that calls into it, with
#   `failure` (not `startup_failure`), 0 jobs, and no reason in either the
#   REST or the GraphQL payload; only the run page says why. actionlint
#   passes either way. Dependabot bumps a `uses:` SHA without regenerating
#   the lock, and Dependabot never runs pre-commit -- so CI is the only place
#   this is caught before it reaches the callers.
#
# WHY NOT `gh actions-lock --verify-local`, measured 2026-09-15 on v0.1.6:
#
#   * It WRITES. It rewrote `uses: ./.github/actions/signed-push` to the
#     invalid `uses: $/.github/actions/signed-push` -- a ref that kills the
#     workflow at startup -- while running in a mode whose own help text
#     calls it read-only and "ideal for pre-commit hooks".
#     --no-migrate-local-actions suppresses that, but then the tool stops
#     descending into local composite actions and misreports their
#     dependencies as stale. Safety and accuracy are in conflict there.
#   * Its coverage is REPO-scoped, not SHA-exact. Bumping ONE of a workflow's
#     two refs to the same action leaves the old lock key still referenced by
#     the other: nothing reported stale, nothing reported missing, check
#     green, workflow broken. That mutant survives it and dies here.
#
# SCOPE -- stated, not implied:
#   * Only 40-hex SHA-pinned refs are checked. Tag refs (@v2, @main) are a
#     different question, gated by validate-sha-pins.sh.
#   * Reusable WORKFLOW refs (owner/repo/.github/workflows/x.yml@sha) are
#     skipped: the lockfile models actions and keys no entry for them.
#   * Local refs (./...) are skipped; they carry no SHA.
#   * Sub-path actions normalise to their repo -- codeql-action/init@X is
#     keyed once as codeql-action@X, never once per sub-path.
#   * Comparison is case-insensitive: workflows say Swatinem/rust-cache while
#     the lockfile stores swatinem/rust-cache.
#   * Local COMPOSITE actions (.github/actions/*/action.yml) are scanned too.
#     Their deps are keyed in the lock under the workflow that uses them, and
#     Dependabot bumps them like any other ref; scanning only the workflow
#     directory leaves that drift invisible.
#   * Membership is GLOBAL, not per-workflow-section. A ref present in some
#     other workflow's lock section satisfies this check. A workflow with
#     SHA-pinned refs but no lock section of its own is therefore NOT
#     detected here -- see the PR notes.

set -euo pipefail

REPO_ROOT="${INPUT_PATH:-.}"
LOCKFILE="$REPO_ROOT/.github/workflows/actions.lock"
WORKFLOW_DIR="$REPO_ROOT/.github/workflows"
ACTIONS_DIR="$REPO_ROOT/.github/actions"

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; NC='\033[0m'

# Refs deliberately absent from the lockfile. Each entry is an exact
# normalised owner/repo@sha plus the reason it is absent. This is an explicit
# list, never a pattern: anything not named here fails, so the list cannot
# quietly widen to cover an accident. An entry that stops being used is
# reported as stale, so it cannot rot either.
EXPECTED_ABSENT=(
  # Estate doctrine is bun-only; deno is banned. Keying it would make a
  # BANNED runtime a required lockfile key for every caller of
  # governance-reusable.yml. The cure is to remove the consumer -- port the
  # governance scripts to bun -- not to satisfy it.
  "denoland/setup-deno@22d081ff2d3a40755e97629de92e3bcbfa7cf2ed"
)

if [ ! -f "$LOCKFILE" ]; then
  echo -e "${RED}[validate-actions-lock] ERROR: $LOCKFILE not found${NC}" >&2
  exit 1
fi

# An unreadable lockfile must abort, never silently pass.
if [ ! -r "$LOCKFILE" ]; then
  echo -e "${RED}[validate-actions-lock] ERROR: $LOCKFILE unreadable${NC}" >&2
  exit 1
fi

# Parse the lockfile's keys into an explicit set ONCE, rather than substring
# -matching against the whole file. A substring test over a blob fails open in
# ways that are hard to see: if the blob is empty or truncated, every ref is
# reported "missing" and the operator is told to regenerate a lockfile that
# was actually fine. An explicit set can be counted, and is counted below.
mapfile -t LOCK_KEYS < <(
  grep -oE "'[A-Za-z0-9._-]+/[A-Za-z0-9._-]+@[0-9a-fA-F]{40}'" "$LOCKFILE" \
  | tr -d "'" | tr '[:upper:]' '[:lower:]' | sort -u
)

# Positive control: the lockfile is known to key at least one action. If the
# parse yields nothing, the FILE FORMAT changed or the read failed -- say so,
# rather than reporting every ref in the repo as missing.
if [ "${#LOCK_KEYS[@]}" -eq 0 ]; then
  echo -e "${RED}[validate-actions-lock] ERROR: parsed 0 keys from $LOCKFILE${NC}" >&2
  echo "    The lockfile exists but no 'owner/repo@sha' keys were found." >&2
  echo "    This is a parser/format failure, NOT a coverage failure." >&2
  exit 1
fi

lock_has() {
  local needle="$1" k
  for k in "${LOCK_KEYS[@]}"; do
    [ "$k" = "$needle" ] && return 0
  done
  return 1
}

ERRORS=0
CHECKED=0
declare -a SEEN_ABSENT=()

# Collect every SHA-pinned uses: ref across all workflow files.
mapfile -t RAW < <(
  grep -rhoE '^[[:space:]]*(-[[:space:]]+)?uses:[[:space:]]*[^[:space:]#]+@[0-9a-fA-F]{40}' \
    "$WORKFLOW_DIR"/*.yml "$WORKFLOW_DIR"/*.yaml \
    "$ACTIONS_DIR"/*/action.yml "$ACTIONS_DIR"/*/action.yaml 2>/dev/null \
  | sed -E 's/^[[:space:]]*(-[[:space:]]+)?uses:[[:space:]]*//' \
  | sort -u
)

# A zero-input pass is the classic fake green: if ref extraction ever breaks,
# this script would report success having checked nothing. If the lockfile
# already names workflows, finding no refs is a contradiction, not an empty
# repo -- fail instead of passing vacuously. The success line below also
# prints the number checked, so a silent collapse to near-zero is visible.
if [ "${#RAW[@]}" -eq 0 ]; then
  if grep -qE "^    '\\.github/workflows/" "$LOCKFILE"; then
    echo -e "${RED}[validate-actions-lock] ERROR: no SHA-pinned refs extracted, yet the lockfile names workflows -- extraction is broken${NC}" >&2
    exit 1
  fi
  echo -e "${YELLOW}[validate-actions-lock] no SHA-pinned refs found -- nothing to check${NC}"
  exit 0
fi

for ref in "${RAW[@]}"; do
  case "$ref" in
    ./*|.\\*) continue ;;                      # local action, carries no SHA
    */.github/workflows/*) continue ;;          # reusable workflow, not keyed
  esac
  case "$ref" in
    *.yml@*|*.yaml@*) continue ;;               # reusable workflow, any path
  esac

  sha="${ref##*@}"
  path="${ref%@*}"
  owner="${path%%/*}"
  rest="${path#*/}"
  repo="${rest%%/*}"
  [ -n "$owner" ] && [ -n "$repo" ] || continue
  norm="$(printf '%s/%s@%s' "$owner" "$repo" "$sha" | tr '[:upper:]' '[:lower:]')"

  skip=0
  for ex in "${EXPECTED_ABSENT[@]}"; do
    ex_lc="$(printf '%s' "$ex" | tr '[:upper:]' '[:lower:]')"
    if [ "$norm" = "$ex_lc" ]; then
      SEEN_ABSENT+=("$ex_lc")
      skip=1
      break
    fi
  done
  [ "$skip" -eq 1 ] && continue

  CHECKED=$((CHECKED + 1))
  if ! lock_has "$norm"; then
    echo -e "${RED}[validate-actions-lock] ERROR: not in actions.lock: ${ref}${NC}" >&2
    echo "    normalised to: $norm" >&2
    ERRORS=$((ERRORS + 1))
  fi
done

# A doctrine exception that is no longer used must be removed, or the list
# becomes a place where real coverage gaps can hide.
for ex in "${EXPECTED_ABSENT[@]}"; do
  ex_lc="$(printf '%s' "$ex" | tr '[:upper:]' '[:lower:]')"
  found=0
  for s in ${SEEN_ABSENT[@]+"${SEEN_ABSENT[@]}"}; do
    [ "$s" = "$ex_lc" ] && found=1 && break
  done
  if [ "$found" -eq 0 ]; then
    echo -e "${YELLOW}[validate-actions-lock] WARNING: stale exception, no workflow uses ${ex} -- remove it from EXPECTED_ABSENT${NC}" >&2
  fi
done

if [ "$ERRORS" -gt 0 ]; then
  echo -e "${RED}[validate-actions-lock] ${ERRORS} ref(s) missing from the lockfile${NC}" >&2
  echo "    Regenerate with the LOCKFILE ONLY, and verify the *.yml diff is empty:" >&2
  echo "      gh actions-lock <workflow paths> --no-migrate-local-actions --no-narrow" >&2
  echo "      git diff --stat -- '.github/workflows/*.yml'   # MUST be empty" >&2
  exit 1
fi

echo -e "${GREEN}[validate-actions-lock] ✅ ${CHECKED} SHA-pinned ref(s) found among ${#LOCK_KEYS[@]} lockfile keys, ${#EXPECTED_ABSENT[@]} doctrine exception(s)${NC}"
exit 0
