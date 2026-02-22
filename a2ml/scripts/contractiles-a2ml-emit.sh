#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

out_dir="${1:-$root/build/contractiles}"
mkdir -p "$out_dir"

tool="$root/scripts/contractiles-a2ml-tool.py"

python3 "$tool" emit "$root/contractiles/must/Mustfile.a2ml" "$out_dir/mustfile.json"
python3 "$tool" emit "$root/contractiles/trust/Trustfile.a2ml" "$out_dir/trustfile.json"
python3 "$tool" emit "$root/contractiles/dust/Dustfile.a2ml" "$out_dir/dustfile.json"
python3 "$tool" emit "$root/contractiles/lust/Intentfile.a2ml" "$out_dir/intentfile.json"

echo "Wrote: $out_dir/mustfile.json"
echo "Wrote: $out_dir/trustfile.json"
echo "Wrote: $out_dir/dustfile.json"
echo "Wrote: $out_dir/intentfile.json"
