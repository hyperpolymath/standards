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
"$GH_BIN" actions-lock --verify-local
restore_workflows
COMPLETE=true
