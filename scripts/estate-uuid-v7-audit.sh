#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Audit a set of already-checked-out estate repositories without hiding gaps.
set -uo pipefail

ROOT=${1:-.}
found=0
failed=0

printf 'repository\tstatus\n'
while IFS= read -r -d '' repo; do
  found=$((found + 1))
  name=${repo#"$ROOT"/}
  if scripts/check-uuid-v7.sh "$repo" >/dev/null 2>&1; then
    printf '%s\tCLEAN\n' "$name"
  else
    printf '%s\tNON-COMPLIANT\n' "$name"
    failed=$((failed + 1))
  fi
done < <(find "$ROOT" -mindepth 1 -maxdepth 2 -type d -name .git -print0 | sed -z 's#/.git$##')

if [ "$found" -eq 0 ]; then
  printf '%s\n' 'UNMEASURED: no repository checkouts found' >&2
  exit 2
fi
if [ "$failed" -ne 0 ]; then
  printf '%s\n' "$failed repository checkout(s) failed UUID v7 conformance" >&2
  exit 1
fi
printf '%s\n' "All $found checked-out repositories passed UUID v7 literal conformance."
