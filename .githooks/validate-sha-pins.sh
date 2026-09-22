#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# SHA-Pinning Validation -- canon rule 10: every third-party `uses:` ref names a
# full commit SHA, never a moving tag or a branch.
#
# TWO BUGS IN THE VERSION THIS REPLACES, both measured 2026-09-21 on main:
#
#   1. The fallback scan was
#          find "$SCAN_PATH" -path '*/.git/*' -prune -o \
#              -path '*/.github/workflows/*.yml' -o -path '*/.github/workflows/*.yaml' -print
#      `find` applies a bare action to the last test only, so -print bound to the
#      *.yaml clause alone. This repo has 53 root .yml workflows and no .yaml ones,
#      so find emitted 0 paths, the loop validated nothing, and the hook announced
#      "All workflow actions are SHA-pinned" over 26 unpinned refs. Parenthesising
#      the -o chain would have emitted 166. A scan that finds nothing must say so,
#      hence the empty-tree refusal below -- that guard is the part that keeps this
#      class of silent all-clear from coming back.
#
#   2. validate_file() whitened whole files: it complained only when a file had an
#      unpinned ref AND no pinned ref anywhere, so one `actions/checkout@<sha>`
#      excused every other action in the same workflow. Validation is per line now.
#
# SCOPE. Root .github/{workflows,actions} plus those directories in nested trees,
# minus rhodium-standard-repositories/**: that subtree is a vendored mirror of the
# RSR canon (gitlab.com/hyperpolymath/rhodium-standard-repositories), other
# repositories' workflows reproduced here as templates. Re-pinning it here fixes no
# live runner and desynchronises the mirror, so its unpinned refs are measured and
# tolerated in .machine_readable/Debtfile.a2ml (vendored-mirror-unpinned-actions)
# rather than skipped silently. The scope matches .githooks/validate-actions-lock.sh,
# the sibling hook this repo also runs in CI (.github/workflows/actions-lock-gate.yml).
set -euo pipefail
SCAN_PATH="${INPUT_PATH:-.}"
STAGED_FILES="${INPUT_STAGED_FILES:-}"
VENDORED_MARK="rhodium-standard-repositories/"
ERRORS=0
SCANNED=0
SKIPPED=0

is_vendored() { case "$1" in *"${VENDORED_MARK}"*) return 0 ;; *) return 1 ;; esac; }

# One line of a workflow, emitted per unpinned ref. Local paths (`./`, `../`) are
# this repo's own composite actions and `docker://` refs are container images, not
# actions -- neither is modelled by actions.lock, which keys actions only.
UNPINNED_FILTER() {
  # `$/...` is NOT valid `uses:` syntax (GitHub Actions has no such thing) —
  # `gh actions-lock` REWRITE MODE once invented `uses: $/.github/actions/...`
  # and every workflow carrying it died at startup. The alnum-first selector
  # below would silently skip such lines, so they are flagged explicitly:
  # waving that corruption through is the exact failure this gate exists to
  # catch. (Zero matches tree-wide today; this arm is purely prospective.)
  { grep -nE '^[[:space:]]*(-[[:space:]]*)?uses:[[:space:]]+[A-Za-z0-9]' \
      | grep -vE 'uses:[[:space:]]+[./]' \
      | grep -vE 'uses:[[:space:]]+docker://' \
      | grep -vE 'uses:[[:space:]]+[^[:space:]@]+@[0-9a-f]{40}([^0-9a-f]|$)' \
      || true;
    grep -nE 'uses:[[:space:]]+\$/' || true; }
}

validate_file() {
  local file="$1" rec lineno body
  while IFS= read -r rec; do
    [ -n "$rec" ] || continue
    lineno="${rec%%:*}"
    body="${rec#*:}"
    body="${body#"${body%%[![:space:]]*}"}"
    echo "[validate-sha-pins] ERROR: $file:$lineno: ${body}" >&2
    echo "    unpinned ref: canon rule 10 wants owner/repo@<40-hex> plus a '# <version>' comment," >&2
    echo "    and a matching key in .github/workflows/actions.lock" >&2
    ERRORS=$((ERRORS + 1))
  done < <(UNPINNED_FILTER < "$file")
}

if [ -n "$STAGED_FILES" ]; then
  while IFS=$'\n' read -r file; do
    [ -n "$file" ] || continue
    case "$file" in *.yml|*.yaml) ;; *) continue ;; esac
    case "$file" in *".github/workflows/"*|*".github/actions/"*) ;; *) continue ;; esac
    is_vendored "$file" && { SKIPPED=$((SKIPPED + 1)); continue; }
    [ -f "$file" ] || continue
    SCANNED=$((SCANNED + 1))
    validate_file "$file"
  done <<< "$STAGED_FILES"
else
  ALL=()
  while IFS= read -r f; do
    [ -n "$f" ] && ALL+=("$f")
  done < <(find "$SCAN_PATH" \
              -path '*/.git/*' -prune -o \
              \( -path '*/.github/workflows/*.yml' -o -path '*/.github/workflows/*.yaml' \
                 -o -path '*/.github/actions/*.yml'   -o -path '*/.github/actions/*.yaml' \) \
              -print 2>/dev/null | LC_ALL=C sort)
  # An empty scan is not a pass -- see bug 1 above.
  if [ "${#ALL[@]}" -eq 0 ]; then
    echo "[validate-sha-pins] ERROR: found 0 workflow files under '$SCAN_PATH'; refusing to certify a tree it never scanned" >&2
    exit 1
  fi
  for f in "${ALL[@]}"; do
    if is_vendored "$f"; then SKIPPED=$((SKIPPED + 1)); continue; fi
    SCANNED=$((SCANNED + 1))
    validate_file "$f"
  done
fi

echo "[validate-sha-pins] ${SCANNED} workflow file(s) scanned, ${SKIPPED} vendored mirror file(s) excluded, ${ERRORS} unpinned ref(s)"
if [ "$ERRORS" -gt 0 ]; then
  echo "[validate-sha-pins] FAIL: $ERRORS unpinned action ref(s)" >&2
  exit 1
fi
echo "[validate-sha-pins] OK: every third-party uses: ref in scope is pinned to a full SHA"
exit 0
