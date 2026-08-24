#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# check-implementation-inside-canon.sh — executable reference for HYP-S009.
#
# A directory is canonical only when .machine_readable/REGISTRY.a2ml names it
# as a local spec home. Local entries omit `kind`; external pointers explicitly
# set `kind = "external"`. Only tracked files are inspected, matching what a
# clean CI checkout and a pull-request scanner can observe.
#
# Usage: check-implementation-inside-canon.sh [REPOSITORY]
# Exit 0: no findings; 1: implementation manifests found; 2: bad input/schema.

set -euo pipefail

repo=${1:-.}
registry="$repo/.machine_readable/REGISTRY.a2ml"

if ! git -C "$repo" rev-parse --git-dir >/dev/null 2>&1; then
  echo "implementation-inside-canon: not a git repository: $repo" >&2
  exit 2
fi

if [ ! -f "$registry" ]; then
  echo "implementation-inside-canon: missing registry: $registry" >&2
  exit 2
fi

is_product_manifest() {
  case "$1" in
    Cargo.toml|deno.json|package.json|Containerfile|compose.yaml|CNAME|ads.txt)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

is_template_or_fixture() {
  case "/$1/" in
    */examples/*|*/templates/*|*/test/fixtures/*|*/tests/fixtures/*)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

guix_is_non_stub() {
  # A comment/whitespace-only guix.scm is a documented placeholder, not a
  # build surface. Any executable Scheme form makes it a product manifest.
  awk '
    /^[[:space:]]*($|;)/ { next }
    { substantive = 1 }
    END { exit(substantive ? 0 : 1) }
  ' "$1"
}

findings=0

while IFS=$'\t' read -r spec_id home; do
  [ -n "$home" ] || continue

  case "/$home/" in
    //*|*/../*|*/./*)
      echo "implementation-inside-canon: unsafe registry home for $spec_id: $home" >&2
      exit 2
      ;;
  esac

  while IFS= read -r -d '' tracked; do
    base=${tracked##*/}
    report=false

    if is_template_or_fixture "$tracked"; then
      continue
    fi

    if is_product_manifest "$base"; then
      report=true
    elif [ "$base" = guix.scm ] && guix_is_non_stub "$repo/$tracked"; then
      report=true
    fi

    if [ "$report" = true ]; then
      printf 'HYP-S009\t%s\t%s\n' "$spec_id" "$tracked"
      findings=$((findings + 1))
    fi
  done < <(git -C "$repo" ls-files -z -- "$home")
done < <(
  awk '
    function unquote(value) {
      sub(/^[[:space:]]*"/, "", value)
      sub(/"[[:space:]]*$/, "", value)
      return value
    }
    function emit() {
      if (in_spec && id != "" && home != "" && tolower(kind) != "external") {
        print id "\t" home
      }
    }
    /^\[\[spec\]\][[:space:]]*$/ {
      emit()
      in_spec = 1
      id = home = kind = ""
      next
    }
    in_spec && /^[[:space:]]*id[[:space:]]*=/ {
      value = $0
      sub(/^[^=]*=[[:space:]]*/, "", value)
      id = unquote(value)
      next
    }
    in_spec && /^[[:space:]]*home[[:space:]]*=/ {
      value = $0
      sub(/^[^=]*=[[:space:]]*/, "", value)
      home = unquote(value)
      next
    }
    in_spec && /^[[:space:]]*kind[[:space:]]*=/ {
      value = $0
      sub(/^[^=]*=[[:space:]]*/, "", value)
      kind = unquote(value)
      next
    }
    END { emit() }
  ' "$registry"
)

if [ "$findings" -gt 0 ]; then
  echo "implementation-inside-canon: $findings tracked product/build manifest(s) in local canonical homes" >&2
  exit 1
fi

echo "implementation-inside-canon: clean"
