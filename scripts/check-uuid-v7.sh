#!/bin/sh
# SPDX-License-Identifier: MPL-2.0
# Check UUID literals in files. Runtime generators still require type-aware tests.
set -eu

if [ "$#" -eq 0 ]; then
  set -- .
fi

# A UUID literal is v7 only when the version nibble is 7 and the variant nibble
# is 8, 9, a, or b. Keep this POSIX so it can run in every estate checkout.
status=0
while IFS= read -r file; do
  [ -f "$file" ] || continue
  # Ignore this checker and documentation examples of non-v7 UUIDs; scan source
  # and data files, not binary files.
  case "$file" in
    */.git/*|*/check-uuid-v7.sh) continue ;;
  esac
  if grep -IEni -- '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' "$file" >/dev/null 2>&1; then
    while IFS= read -r match; do
      uuid=$(printf '%s\n' "$match" | sed -nE 's/.*([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}).*/\1/p' | head -n 1)
      [ -n "$uuid" ] || continue
      version=$(printf '%s' "$uuid" | cut -d- -f3 | cut -c1)
      variant=$(printf '%s' "$uuid" | cut -d- -f4 | cut -c1 | tr 'A-F' 'a-f')
      case "$version:$variant" in
        7:8|7:9|7:a|7:b) : ;;
        *) printf '%s: non-v7 UUID literal (%s)\n' "$file" "$uuid" >&2; status=1 ;;
      esac
    done <<EOF
$(grep -IEni -- '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' "$file" || true)
EOF
  fi
done <<EOF
$(find "$@" -type f -not -path '*/.git/*' -print 2>/dev/null)
EOF

if [ "$status" -ne 0 ]; then
  printf '%s\n' 'UUID v7 check failed. Use the estate UUID v7 standard and type external/legacy IDs explicitly.' >&2
fi
exit "$status"
