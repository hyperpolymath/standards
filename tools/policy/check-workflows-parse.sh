#!/usr/bin/env bash
# Fail if any GitHub Actions workflow does not parse.
#
# WHY THIS EXISTS. Measured across the estate on 2026-08-27: **481 workflow files in
# 134 repos do not parse at all**. A workflow that cannot be loaded produces NO check
# run, so it is invisible to `?status=failure` sweeps and to `gh pr checks` — the gate
# simply never runs, and its absence looks exactly like success.
#
# One was root-caused to a literal BACKSPACE byte (0x08) committed inside a regex.
# The other 480 are structural YAML: 245 "mapping values are not allowed in this
# context", 137 "could not find expected ':'", 73 block-mapping errors, 6 unterminated
# quotes.
#
# Exit 0 = every workflow parses. Exit 1 = at least one does not.
set -uo pipefail

parser=""
if command -v yq        >/dev/null 2>&1; then parser=yq
elif command -v python3  >/dev/null 2>&1 && python3 -c 'import yaml' 2>/dev/null; then parser=python
elif command -v ruby     >/dev/null 2>&1; then parser=ruby
else
  echo "::error::no YAML parser available (yq, python3+pyyaml, or ruby) — cannot verify workflows"
  exit 1
fi

parse_ok() {
  case "$parser" in
    yq)     yq '.' "$1" >/dev/null 2>&1 ;;
    python) python3 -c 'import sys,yaml; yaml.safe_load(open(sys.argv[1]))' "$1" >/dev/null 2>&1 ;;
    ruby)   ruby -ryaml -e 'YAML.safe_load(File.read(ARGV[0]), aliases: true)' "$1" >/dev/null 2>&1 ;;
  esac
}

status=0; checked=0
while IFS= read -r f; do
  [ -f "$f" ] || continue
  checked=$((checked + 1))
  if ! parse_ok "$f"; then
    status=1
    printf '::error file=%s::workflow does not parse — it produces NO check run, so this gate never executes\n' "$f"
    case "$parser" in
      yq)     yq '.' "$f" 2>&1 | head -2 | sed 's/^/    /' ;;
      python) python3 -c 'import sys,yaml; yaml.safe_load(open(sys.argv[1]))' "$f" 2>&1 | tail -2 | sed 's/^/    /' ;;
      ruby)   ruby -ryaml -e 'YAML.safe_load(File.read(ARGV[0]), aliases: true)' "$f" 2>&1 | head -2 | sed 's/^/    /' ;;
    esac
    # control characters are a common, easily-missed cause
    if grep -qP '[\x00-\x08\x0B\x0C\x0E-\x1F]' "$f" 2>/dev/null; then
      echo "    ⚠ contains CONTROL CHARACTERS — YAML forbids them; see empty-linter"
      grep -nP '[\x00-\x08\x0B\x0C\x0E-\x1F]' "$f" | head -3 | cat -v | sed 's/^/      /'
    fi
  fi
done < <(git ls-files '.github/workflows/*.yml' '.github/workflows/*.yaml' '**/.github/workflows/*.yml' '**/.github/workflows/*.yaml' 2>/dev/null | sort -u)

if [ "$checked" -eq 0 ]; then echo "no workflows tracked — nothing to check"; exit 0; fi
if [ "$status" -eq 0 ]; then echo "✅ all $checked workflow(s) parse"; else
  echo
  echo "A workflow that does not parse produces no check run. Its gate has never run,"
  echo "and its silence is indistinguishable from success. Fix the YAML; do not delete"
  echo "the check."
fi
exit $status
