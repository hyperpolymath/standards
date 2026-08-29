#!/usr/bin/env bash
# Assert the invariant parts of the estate language policy in tracked CLAUDE.md files.
set -uo pipefail

status=0
declare -a files=()
mapfile -d '' -t files < <(git ls-files -z -- '*CLAUDE.md')

if [ "${#files[@]}" -eq 0 ]; then
  echo "no CLAUDE.md tracked - nothing to check"
  exit 0
fi

fail() {
  printf '  FAIL %s\n       %s\n' "$1" "$2"
  status=1
}

# Historical policy text is often retained in Markdown quotes. Remove blockquotes and
# quoted substrings while preserving the rest of each line, so a live violation after a
# historical quotation is still visible.
live_lines() {
  sed -E '/^[[:space:]]*>/d; s/"[^"]*"//g; s/“[^”]*”//g' "$1"
}

for file in "${files[@]}"; do
  case "$file" in
    node_modules/*|*/node_modules/*) continue ;;
  esac

  echo "checking $file"
  live=$(live_lines "$file")

  if grep -nF -- '| Bun | Deno |' "$file" >/dev/null; then
    fail "$file" 'Bun is listed as banned with Deno as its replacement.'
  fi
  if grep -F 'No package.json for runtime deps' <<<"$live" >/dev/null; then
    fail "$file" 'Policy forbids the dependency manifest that Bun requires.'
  fi
  if grep -F 'deno.json imports' <<<"$live" >/dev/null; then
    fail "$file" 'Policy directs runtime dependencies into deno.json.'
  fi

  typescript_runtime='Executes .\.ts. directly|JS/TS runtime|[Ss]upports? TypeScript|[Rr]uns? [^[:alnum:][:space:]]*\.ts[^[:alnum:][:space:]]* files?'
  if grep -E "$typescript_runtime" <<<"$live" >/dev/null; then
    fail "$file" 'Policy advertises TypeScript execution.'
  fi

  if awk -F'|' 'NF >= 4 && $2 ~ /^[[:space:]]*$/ { found=1; exit } END { exit !found }' "$file"; then
    fail "$file" 'Policy table contains an empty first cell (a blanking scar).'
  fi
  if grep -F '| **** |' "$file" >/dev/null; then
    fail "$file" 'Policy table contains an empty bold cell.'
  fi
  if grep -E '\*\*No new +files\*\*|Only where +cannot' "$file" >/dev/null; then
    fail "$file" 'Enforcement text contains a blanked language name.'
  fi
  if grep -E '^\|[[:space:]]*AffineScript[[:space:]]*\|[[:space:]]*AffineScript[[:space:]]*\|' "$file" >/dev/null; then
    fail "$file" 'The banned table maps AffineScript to itself.'
  fi

  if grep -qE '^### (ALLOWED|BANNED)' "$file"; then
    if ! grep -qE '^\|[[:space:]]*\*\*Bun\*\*[[:space:]]*\|' "$file" &&
       ! grep -qiE '^[-*][[:space:]]+\*{0,2}Bun\*{0,2}([[:space:]]|$)' "$file"; then
      fail "$file" 'No Bun entry appears in the allowed policy.'
    fi
    if ! grep -qE '^\|[[:space:]]*\*{0,2}Deno\*{0,2}[[:space:]]*\|[[:space:]]*\*{0,2}Bun\*{0,2}[[:space:]]*\|' "$file" &&
       ! grep -qiE '^[-*][[:space:]]+Deno[[:space:]]*\(use Bun\)' "$file"; then
      fail "$file" 'Deno is not listed as banned with Bun as its replacement.'
    fi
  fi
done

if [ "$status" -eq 0 ]; then
  echo "language policy OK"
else
  echo "Language-policy drift detected. Fix the local copy; do not weaken this gate."
fi
exit "$status"
