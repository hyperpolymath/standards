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

# GitHub rejects a reusable-workflow call job before creating any jobs when it
# contains step-job-only keys such as timeout-minutes. The file remains valid
# YAML, so the parser gate alone cannot see this zero-check failure mode.
has_reusable_timeout() {
  case "$parser" in
    yq)
      yq -e '[.jobs[] | select(has("uses") and has("timeout-minutes"))] | length > 0' "$1" >/dev/null 2>&1
      ;;
    python)
      python3 -c 'import sys,yaml; d=yaml.safe_load(open(sys.argv[1], encoding="utf-8")) or {}; sys.exit(not any(isinstance(j,dict) and "uses" in j and "timeout-minutes" in j for j in (d.get("jobs") or {}).values()))' "$1"
      ;;
    ruby)
      ruby -ryaml -e 'd=YAML.safe_load(File.read(ARGV[0]), aliases: true) || {}; jobs=d["jobs"] || {}; exit(jobs.values.any? { |j| j.is_a?(Hash) && j.key?("uses") && j.key?("timeout-minutes") } ? 0 : 1)' "$1"
      ;;
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
  elif has_reusable_timeout "$file"; then
    status=1
    printf '%s\n' "::error file=$file::a reusable-workflow call job cannot declare timeout-minutes; GitHub rejects it before creating any jobs"
  fi
done

if [ "$status" -eq 0 ]; then
  echo "all ${#workflows[@]} workflow(s) parse"
else
  echo 'At least one workflow cannot load. Fix the YAML; do not delete the check.'
fi
exit "$status"
