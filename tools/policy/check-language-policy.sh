#!/usr/bin/env bash
# Language-policy drift gate.
#
# WHY THIS EXISTS. The estate's language policy is duplicated into ~372 per-repo
# `.claude/CLAUDE.md` files across 131 repos. On 2026-08-26 a census found 868 of them
# still listed **Bun as BANNED** with Deno as its replacement - the exact inverse of the
# standing ruling - and nothing had ever detected it. Correcting `standards` fixes one copy;
# agents read the local one.
#
# WHY ASSERTIONS, NOT A DIFF. The copies are legitimately not identical: repos carry their
# own exemption tables, architecture notes and carve-outs. A byte-for-byte generator would
# be permanently red. So this gate asserts the INVARIANTS the policy must satisfy, whatever
# the surrounding wording.
#
# Exit 0 = compliant. Exit 1 = drift. Every failure prints file:line.
set -uo pipefail
status=0
files=$(git ls-files '*CLAUDE.md' 2>/dev/null | grep -v node_modules)
[ -z "$files" ] && { echo "no CLAUDE.md tracked - nothing to check"; exit 0; }

fail(){ printf '  \033[31mFAIL\033[0m %s\n        %s\n' "$1" "$2"; status=1; }

for f in $files; do
  echo "checking $f"

  # --- must NOT appear -------------------------------------------------------
  # 1. Bun banned. This is the inversion that went undetected across 868 files.
  if grep -nF -- '| Bun | Deno |' "$f" >/dev/null; then
    fail "$f:$(grep -nF -- '| Bun | Deno |' "$f" | head -1 | cut -d: -f1)" \
         'Bun is listed as BANNED with Deno as replacement - inverted. Bun is tier 1.'
  fi
  # 2. The rule that told repos not to declare dependencies at all. hyperpolymath/ubicity
  #    imported zod and glob, shipped no manifest, and could not build under ANY toolchain.
  if grep -nF 'No package.json for runtime deps' "$f" >/dev/null; then
    fail "$f:$(grep -nF 'No package.json for runtime deps' "$f" | head -1 | cut -d: -f1)" \
         'Forbids declaring dependencies. Bun is npm-compatible; a manifest is REQUIRED.'
  fi
  if grep -nF 'deno.json imports' "$f" >/dev/null; then
    fail "$f:$(grep -nF 'deno.json imports' "$f" | head -1 | cut -d: -f1)" \
         'Directs dependency declaration into deno.json. Use package.json + bun.lock.'
  fi
  # 3. No tool description may advertise TypeScript. Owner ruling 2026-08-27:
  #    "no typescript ... that should not exist at all."
  if grep -nE 'Executes .\.ts. directly|JS/TS runtime' "$f" >/dev/null; then
    fail "$f:$(grep -nE 'Executes .\.ts. directly|JS/TS runtime' "$f" | head -1 | cut -d: -f1)" \
         'Advertises TypeScript execution. TypeScript is banned; do not describe tools as TS runtimes.'
  fi
  # 4. Blanking scars. A bulk purge substituted a token with an EMPTY STRING, which also
  #    produced `rm -rf /lib` in wordpress-tools (the lethal shape is <token>/path -> /path).
  if awk -F'|' 'NF==4 && $2 ~ /^[[:space:]]*$/{exit 0} END{exit 1}' "$f"; then
    fail "$f" 'Policy table row with an EMPTY first cell - blanking scar from a bulk substitution.'
  fi
  if grep -nF '| **** |' "$f" >/dev/null; then
    fail "$f:$(grep -nF '| **** |' "$f" | head -1 | cut -d: -f1)" \
         'Empty bold cell (****) - the language name was blanked out.'
  fi
  if grep -nE '\*\*No new +files\*\*|Only where +cannot' "$f" >/dev/null; then
    fail "$f" 'Enforcement rule with a blanked language name.'
  fi
  # 5. A rule may not ban the language it mandates.
  if grep -nE '^\| AffineScript \| AffineScript \|' "$f" >/dev/null; then
    fail "$f" 'BANNED table maps AffineScript to itself - it bans the mandated language.'
  fi

  # --- must appear, if the file carries a language-policy table ---------------
  if grep -qE '^### (ALLOWED|BANNED)' "$f"; then
    grep -qE '^\| \*\*Bun\*\* \|' "$f" || \
      fail "$f" 'No Bun row in ALLOWED. Bun is the tier-1 JS runtime and package manager.'
    grep -qE '^\| \*?\*?Deno\*?\*? \| Bun \|' "$f" || \
      fail "$f" 'Deno is not listed in BANNED with Bun as its replacement (ruling 2026-08-26).'
  fi
done

if [ $status -eq 0 ]; then echo "language policy OK"; else
  echo
  echo "Language-policy drift detected. Canonical source: hyperpolymath/standards .claude/CLAUDE.md"
  echo "Fix the local copy; do not weaken this gate."
fi
exit $status
