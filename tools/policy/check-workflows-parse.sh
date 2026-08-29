#!/usr/bin/env bash
# Fail if any tracked GitHub Actions workflow does not parse as YAML.
set -uo pipefail

if ! command -v git >/dev/null 2>&1; then
  echo '::error::git is required to enumerate tracked workflows'
  exit 1
fi

declare -a workflows=()
mapfile -d '' -t workflows < <(
  git ls-files -z -- '.github/workflows/*.yml' '.github/workflows/*.yaml' \
    '**/.github/workflows/*.yml' '**/.github/workflows/*.yaml'
)

if [ "${#workflows[@]}" -eq 0 ]; then
  echo "no workflows tracked - nothing to check"
  exit 0
fi

parser=''
if command -v yq >/dev/null 2>&1; then
  parser=yq
elif command -v python3 >/dev/null 2>&1 && python3 -c 'import yaml' >/dev/null 2>&1; then
  parser=python
elif command -v ruby >/dev/null 2>&1; then
  parser=ruby
else
  echo "::error::no YAML parser available (yq, python3+pyyaml, or ruby)"
  exit 1
fi

parse_ok() {
  case "$parser" in
    yq) yq '.' "$1" >/dev/null 2>&1 ;;
    python) python3 -c 'import sys,yaml; yaml.safe_load(open(sys.argv[1], encoding="utf-8"))' "$1" >/dev/null 2>&1 ;;
    ruby) ruby -ryaml -e 'YAML.safe_load(File.read(ARGV[0]), aliases: true)' "$1" >/dev/null 2>&1 ;;
  esac
}

has_forbidden_control() {
  od -An -v -tu1 "$1" | awk '
    { for (i=1; i<=NF; i++) if (($i < 9) || ($i > 10 && $i < 13) || ($i > 13 && $i < 32)) found=1 }
    END { exit !found }
  '
}

status=0
for file in "${workflows[@]}"; do
  [ -f "$file" ] || continue
  if ! parse_ok "$file"; then
    status=1
    printf '::error file=%s::workflow does not parse; an unloaded workflow produces no check run\n' "$file"
    if has_forbidden_control "$file"; then
      echo '    contains a YAML-forbidden control character'
    fi
  fi
done

if [ "$status" -eq 0 ]; then
  echo "all ${#workflows[@]} workflow(s) parse"
else
  echo 'At least one workflow cannot load. Fix the YAML; do not delete the check.'
fi
exit "$status"
