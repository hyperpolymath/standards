#!/bin/sh
# SPDX-License-Identifier: MPL-2.0
# Check UUID literals in files. Runtime generators still require type-aware tests.
set -eu

if [ "$#" -eq 0 ]; then
  set -- .
fi

# Print the text of a file that is subject to the v7 rule.
#
# Julia project files name each dependency by the UUID the General registry
# assigned it. Those are external identifiers (the standard: preserve and type
# explicitly), so the [deps], [weakdeps] and [extras] tables of a
# Project.toml / JuliaProject.toml are not scanned, and neither is a Manifest
# (every entry is a resolved dependency). Everything else in a project file,
# including the package's own top-level `uuid =`, is still checked.
scannable_text() {
  case "${1##*/}" in
    Manifest.toml|Manifest-v*.toml|JuliaManifest.toml|JuliaManifest-v*.toml) ;;
    Project.toml|JuliaProject.toml)
      awk '/^[[:space:]]*\[/ { t = $0; gsub(/[[:space:]]/, "", t); sub(/#.*/, "", t)
             skip = (t == "[deps]" || t == "[weakdeps]" || t == "[extras]") }
           !skip' "$1" ;;
    *) cat "$1" ;;
  esac
}

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
$(scannable_text "$file" | grep -Ei -- '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' || true)
EOF
  fi
done <<EOF
$(find "$@" -type f -not -path '*/.git/*' -print 2>/dev/null)
EOF

if [ "$status" -ne 0 ]; then
  printf '%s\n' 'UUID v7 check failed. Use the estate UUID v7 standard and type external/legacy IDs explicitly.' >&2
fi
exit "$status"
