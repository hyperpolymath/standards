#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
component="$root/contractiles/k9/a2ml-contractiles.k9.ncl"

if ! command -v nickel >/dev/null 2>&1; then
  echo "nickel not found. Install Nickel to typecheck the K9 component." >&2
  exit 1
fi

if ! command -v a2ml >/dev/null 2>&1; then
  echo "a2ml CLI not found. Install it or add bin/a2ml to PATH." >&2
  exit 1
fi

nickel typecheck "$component"

a2ml validate \
  "$root/contractiles/must/Mustfile.a2ml" \
  "$root/contractiles/trust/Trustfile.a2ml" \
  "$root/contractiles/dust/Dustfile.a2ml" \
  "$root/contractiles/lust/Intentfile.a2ml"
