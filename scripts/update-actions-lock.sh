#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Refresh actions.lock without letting gh actions-lock rewrite the canonical
# workflow source from inline SHA pins to tag references.
set -euo pipefail

MODE=update
if [ "${1:-}" = "--verify-local" ]; then
  MODE=verify
  shift
fi

WF_DIR="${1:-.github/workflows}"
GH_BIN="${GH_BIN:-gh}"

[ -d "$WF_DIR" ] || { echo "update-actions-lock: no workflow directory: $WF_DIR" >&2; exit 1; }

SNAPSHOT="$(mktemp -d)"
COMPLETE=false

restore_workflows() {
  find "$SNAPSHOT/workflows" -maxdepth 1 -type f -print0 2>/dev/null |
    while IFS= read -r -d '' file; do
      cp "$file" "$WF_DIR/$(basename "$file")"
    done
}

workflow_references_reusable_dependency() {
  workflow=$1
  dependency=$2
  repo=${dependency%@*}
  ref=${dependency#*@}

  [[ -f "$workflow" ]] || return 1
  awk -v prefix="$repo/.github/workflows/" -v suffix="@$ref" '
    /^[[:space:]]*uses:[[:space:]]*/ {
      value = $0
      sub(/^[[:space:]]*uses:[[:space:]]*/, "", value)
      sub(/[[:space:]]*#.*/, "", value)
      sub(/[[:space:]]*$/, "", value)
      if (index(value, prefix) == 1 &&
          length(value) >= length(suffix) &&
          substr(value, length(value) - length(suffix) + 1) == suffix) {
        found = 1
      }
    }
    END { exit(found ? 0 : 1) }
  ' "$workflow"
}

verify_lock_coverage() {
  # gh-actions-lock v0.1.6 does not recognise reusable-workflow `uses:`
  # paths. GitHub's startup enforcement nevertheless requires callers to
  # carry the reusable repository and its transitive actions in actions.lock.
  # Accept only the tool's `stale` false positive when the named workflow
  # contains the exact owner/repo/.github/workflows/file@ref dependency.
  # Every other finding, including a wrong ref, remains blocking.
  command -v jq >/dev/null 2>&1 || {
    "$GH_BIN" actions-lock --verify-local
    return
  }

  set +e
  result=$("$GH_BIN" actions-lock --verify-local --json=valid,findings)
  status=$?
  set -e

  if ! printf '%s' "$result" | jq -e '.valid != null and (.findings | type == "array")' >/dev/null 2>&1; then
    printf '%s\n' "$result"
    [[ "$status" -ne 0 ]] && return "$status"
    return 1
  fi
  # The authoritative validity bit and the process exit are not equivalent.
  # gh-actions-lock v0.1.6 exits 1 for advisory findings such as `sha-as-ref`
  # even while reporting `"valid": true`. Treating that advisory exit as an
  # invalid lock makes every inline-SHA repository permanently red. Preserve
  # the findings in the log, but accept the lock exactly when the tool says it
  # is valid.
  if printf '%s' "$result" | jq -e '.valid == true' >/dev/null; then
    if printf '%s' "$result" | jq -e '.findings | length > 0' >/dev/null; then
      printf '%s\n' "$result"
      echo "actions-lock: valid with advisory finding(s)"
    fi
    return 0
  fi

  remaining=0
  accepted=0
  while IFS=$'\t' read -r category workflow dependency; do
    if [[ "$category" = stale ]] &&
       workflow_references_reusable_dependency "$workflow" "$dependency"; then
      echo "Accepted reusable-workflow lock coverage: $workflow -> $dependency"
      accepted=$((accepted + 1))
    else
      remaining=$((remaining + 1))
    fi
  done < <(printf '%s' "$result" | jq -r '.findings[] | [.category, .workflow, .dependency] | @tsv')

  # `valid:false` with no findings is contradictory and cannot be explained by
  # the one known reusable-workflow false positive. Fail closed rather than
  # turning an empty/malformed diagnostic into approval.
  if [[ "$remaining" -ne 0 || "$accepted" -eq 0 ]]; then
    printf '%s\n' "$result"
    return 1
  fi
}

cleanup() {
  status=$?
  if [ "$COMPLETE" != true ]; then
    restore_workflows
    if [ -f "$SNAPSHOT/actions.lock" ]; then
      cp "$SNAPSHOT/actions.lock" "$WF_DIR/actions.lock"
    else
      rm -f "$WF_DIR/actions.lock"
    fi
  fi
  rm -rf "$SNAPSHOT"
  exit "$status"
}
trap cleanup EXIT

mkdir -p "$SNAPSHOT/workflows"
[ ! -f "$WF_DIR/actions.lock" ] || cp "$WF_DIR/actions.lock" "$SNAPSHOT/actions.lock"

# gh actions-lock currently inserts its managed marker and rewrites direct SHA
# refs to their release tags. Those edits conflict with the estate's inline-SHA
# gate. Preserve every authored workflow while allowing the generated lockfile
# to change.
find "$WF_DIR" -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yaml' \) -print0 |
  while IFS= read -r -d '' file; do
    cp "$file" "$SNAPSHOT/workflows/$(basename "$file")"
  done

if [ "$MODE" = update ]; then
  "$GH_BIN" actions-lock
  restore_workflows
  bash "$(dirname "$0")/relock-sha-keys.sh" "$WF_DIR"
fi

# Despite its name, --verify-local can migrate local `./` action paths to an
# invalid `$/` spelling. Treat verification as mutating and restore authored
# workflow bytes afterward too.
verify_lock_coverage
restore_workflows
COMPLETE=true
