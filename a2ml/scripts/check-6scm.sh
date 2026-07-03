#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# check-6scm.sh — verify the .machine_readable/6scm/ mirror is honest.
#
# The 6scm mirror duplicated the legacy Scheme (.scm) descriptor set into
# .machine_readable/6scm/ so tools that only read Scheme had a copy. That
# metadata has migrated to .machine_readable/6a2/ (A2ML / descriptiles), so
# for most repos there are no .scm sources left to mirror.
#
# A validator that validates nothing must not report success. This script
# therefore distinguishes three states LOUDLY (never a silent green):
#
#   1. Sources present  -> real diff check; fail on missing/out-of-sync mirror.
#   2. No sources, no mirror -> OBSOLETE no-op: exit 0 with an explicit
#      "retired" message (it does NOT claim "in sync").
#   3. No sources, but an orphaned mirror with files -> DRIFT: fail loudly.
#
# See CANONICAL-NAMES.adoc (6a2/descriptiles supersedes the legacy set) and
# standards Wave-0 "kill the false green" remediation.

set -euo pipefail

SCM_NAMES=(AGENTIC.scm ECOSYSTEM.scm META.scm NEUROSYM.scm PLAYBOOK.scm STATE.scm)
MIRROR_DIR=".machine_readable/6scm"

# Collect the sources that actually exist.
sources=()
for f in "${SCM_NAMES[@]}"; do
  [ -f ".machine_readable/$f" ] && sources+=("$f")
done

# State 2/3: no sources to mirror — the mechanism is obsolete for this repo.
if [ "${#sources[@]}" -eq 0 ]; then
  orphans=0
  if [ -d "$MIRROR_DIR" ]; then
    while IFS= read -r _; do orphans=$((orphans + 1)); done \
      < <(find "$MIRROR_DIR" -type f 2>/dev/null)
  fi
  if [ "$orphans" -gt 0 ]; then
    echo "DRIFT: $MIRROR_DIR holds $orphans mirror file(s) but no .scm sources exist." >&2
    echo "       The 6scm mirror is obsolete (metadata migrated to .machine_readable/6a2/)." >&2
    echo "       Remove $MIRROR_DIR and retire the sync-6scm/check-6scm recipes." >&2
    exit 1
  fi
  echo "OBSOLETE (no-op): no .scm sources; 6scm mirror retired (superseded by 6a2/descriptiles). Nothing to mirror."
  exit 0
fi

# State 1: sources exist — the mirror must be present and byte-identical.
missing=0
for f in "${sources[@]}"; do
  src=".machine_readable/$f"
  dst="$MIRROR_DIR/$f"
  if [ ! -f "$dst" ]; then
    echo "Missing mirror: $dst" >&2
    missing=1
    continue
  fi
  if ! diff -u "$src" "$dst" >/dev/null; then
    echo "Out of sync: $src -> $dst" >&2
    missing=1
  fi
done

if [ "$missing" -eq 0 ]; then
  echo "6scm mirror in sync (${#sources[@]} source(s) verified)."
fi
exit $missing
