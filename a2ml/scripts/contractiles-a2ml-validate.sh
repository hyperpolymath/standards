#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

tool="$root/scripts/contractiles-a2ml-tool.py"

if ! command -v a2ml >/dev/null 2>&1; then
  echo "a2ml CLI not found. Install it or add bin/a2ml to PATH." >&2
  exit 1
fi

files=(
  "$root/contractiles/must/Mustfile.a2ml"
  "$root/contractiles/trust/Trustfile.a2ml"
  "$root/contractiles/dust/Dustfile.a2ml"
  "$root/contractiles/lust/Intentfile.a2ml"
)

for file in "${files[@]}"; do
  a2ml validate "$file"
done

python3 "$tool" validate "${files[@]}"
