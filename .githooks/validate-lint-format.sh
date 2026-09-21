#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Hyperpolymath Estate — ecosystem-scoped lint + format check (pre-commit)
# Source: https://github.com/hyperpolymath/standards
#
# CONFINEMENT IS THE POINT. Each check runs only when THIS repo has staged
# files of that kind. A Rust check must not fire in a ReScript repo, and a V
# toolchain must never be summoned to lint a Coq development.
#
# READ-ONLY. Every command here reports; none rewrites a tracked file.
# `cargo fmt --check`, `nickel format --check`, `v fmt -verify`. The
# bare/`-w`/`--fix` forms are banned in this file.
#
# TWO FAILURE POLICIES, and the difference is deliberate:
#
#   * TOOLCHAIN MISSING, ecosystem toolchain (cargo, nickel, v, bun) →
#     FAIL CLOSED. You cannot have authored a .rs without cargo, so "cargo not
#     found" means a broken environment, not an exempt one. Skipping there
#     would report success having examined nothing.
#
#   * DENO is neither: it is BANNED, so no Deno toolchain is ever summoned.
#     Its section refuses rather than checks — see it below.
#
#   * OPTIONAL LINTER MISSING (hlint, fourmolu) → reported as an explicit
#     SKIP line, not as a pass, and CI remains the authority. These are add-ons
#     rather than the toolchain, so their absence is an ordinary state of a
#     working machine and must not block every commit on every machine.
#
# Override for one commit: git commit --no-verify
# Skip the slow Rust clippy pass: ESTATE_HOOK_SKIP_SLOW=1 (prints a loud
# warning — the gate did NOT run, and that is not the same as passing).

set -euo pipefail

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; BLUE='\033[0;34m'; NC='\033[0m'

REPO_ROOT="${INPUT_PATH:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
cd "$REPO_ROOT"

STAGED="${INPUT_STAGED_FILES:-$(git diff --cached --name-only --diff-filter=ACM 2>/dev/null || true)}"

ERRORS=0
LEDGER=()

note()  { echo -e "${BLUE}[lint]${NC} $*"; }
warn()  { echo -e "${YELLOW}[lint] $*${NC}" >&2; }
fail()  { echo -e "${RED}[lint] $*${NC}" >&2; ERRORS=$((ERRORS + 1)); }

# Staged files matching an extended regex, one per line.
staged_matching() {
  [ -z "$STAGED" ] && return 0
  echo "$STAGED" | grep -E "$1" || true
}

# Does this repo TRACK a marker file? Distinct from "is a file staged":
# `v.mod` is what makes a `.v` a V source rather than a Coq proof script.
tracks() {
  git ls-files -- "$@" | grep -q . 2>/dev/null
}

require_tool() {
  local tool="$1" eco="$2" hint="$3"
  if ! command -v "$tool" >/dev/null 2>&1; then
    fail "${eco}: '${tool}' is not installed, so the ${eco} gate could not run. Refusing to pass a check that examined nothing. Install: ${hint}"
    return 1
  fi
  return 0
}

# ── Rust ────────────────────────────────────────────────────────────────
RS="$(staged_matching '\.rs$|(^|/)Cargo\.toml$')"
# The ecosystem gate is the MANIFEST, not the extension. A stray `.rs` staged in
# a ReScript tree with no Cargo.toml would otherwise run `cargo fmt --all`, which
# exits non-zero with "error: could not find `Cargo.toml`" — a RED gate for a
# reason that has nothing to do with the code. Confinement means the Rust gate
# does not fire in a non-Rust repo at all.
# `tracks` reads the INDEX, so a Cargo.toml added in THIS commit does count.
if [ -n "$RS" ] && tracks 'Cargo.toml' '*/Cargo.toml'; then
  note "Rust: $(echo "$RS" | grep -c .) staged file(s)"
  if require_tool cargo Rust "https://rustup.rs"; then
    cargo fmt --all --check || fail "Rust: sources are not formatted (cargo fmt --check)"
    if [ "${ESTATE_HOOK_SKIP_SLOW:-0}" = "1" ]; then
      warn "Rust: clippy SKIPPED via ESTATE_HOOK_SKIP_SLOW=1. A skip is not a pass — CI will still run it."
      LEDGER+=("rust-clippy: SKIPPED (ESTATE_HOOK_SKIP_SLOW)")
    else
      cargo clippy --all-targets -- -D warnings || fail "Rust: clippy reported warnings (treated as errors)"
    fi
  fi
else
  LEDGER+=("rust: not staged, or repo tracks no Cargo.toml")
fi

# ── Nickel ──────────────────────────────────────────────────────────────
NCL="$(staged_matching '\.ncl$')"
if [ -n "$NCL" ]; then
  note "Nickel: $(echo "$NCL" | grep -c .) staged file(s)"
  if require_tool nickel Nickel "https://github.com/tweag/nickel/releases"; then
    while IFS= read -r f; do
      [ -z "$f" ] && continue
      nickel format --check "$f" || fail "Nickel: $f is not formatted"
      nickel typecheck "$f"      || fail "Nickel: $f failed typecheck"
    done <<< "$NCL"
  fi
else
  LEDGER+=("nickel: not staged")
fi

# ── Deno (BANNED — a REFUSAL, not a lint) ───────────────────────────────
#
# Owner ruling: "deno is over, we're prioritising bun, and using bunx."
#
# This section used to install nothing and run `deno fmt --check` / `deno lint`
# over staged JS in a tree that tracked a deno.json, behind a `warn`. That
# lint-checked — and thereby blessed — the very thing the warning called
# banned, and it summoned a banned toolchain to do it.
#
# It now refuses. The question it asks is deliberately NARROWER than CI's:
#
#   * CI asks "does this repository still carry Deno debt?", and answers it
#     against the central shrink-only ledger
#     `.machine_readable/deno-allow.txt` in `standards`.
#   * This hook CANNOT ask that — it runs inside a caller's checkout and has
#     no access to that ledger — and must not fake an answer. It asks instead
#     "is THIS COMMIT ADDING Deno debt?", which is answerable right here.
#
# STAGED is `--diff-filter=ACM`, so a DELETED deno.json never appears: paying
# the debt down is never blocked, only growing it. That is the same
# shrink-only direction the central ledger enforces, reached without one.
DENO_CFG="$(staged_matching '(^|/)deno\.jsonc?$')"
DENO_TRACKED="$(git ls-files -- 'deno.json' '*/deno.json' 'deno.jsonc' '*/deno.jsonc' | grep -c . || true)"
if [ -n "$DENO_CFG" ]; then
  fail "Deno is BANNED (owner ruling 2026-08-26 — Bun is the estate runtime). This commit ADDS or MODIFIES $(echo "$DENO_CFG" | grep -c .) Deno config file(s):"
  while IFS= read -r cfg; do [ -n "$cfg" ] && echo "          - $cfg" >&2; done <<< "$DENO_CFG"
  echo "        Port the deno.json 'tasks' map to package.json scripts run by" >&2
  echo "        'bun run' — 'bun run' does NOT read a deno.json tasks map — and" >&2
  echo "        delete the config. Deleting a deno.json is never blocked here." >&2
  echo "        A genuinely grandfathered repository is exempted CENTRALLY, in" >&2
  echo "        .machine_readable/deno-allow.txt in hyperpolymath/standards, not" >&2
  echo "        by a local bypass." >&2
else
  LEDGER+=("deno: no deno.json(c) added or modified (repo tracks ${DENO_TRACKED}; the ledgered CI gate is the authority on existing debt)")
fi

# ⚠ Dropping the Deno lint leaves staged `.js`/`.jsx`/`.mjs`/`.cjs` with NO
# pre-commit gate: the estate has no pinned bun lint/format gate to fall
# through to. Declared here rather than left silent — an unchecked file type
# that nobody names reads as a checked one.
JS_SRC="$(staged_matching '\.(js|jsx|mjs|cjs)$')"
if [ -n "$JS_SRC" ]; then
  LEDGER+=("javascript: $(echo "$JS_SRC" | grep -c .) staged file(s) NOT checked — no pinned bun lint/format gate exists. A skip, not a pass.")
fi

# ── ReScript (BANNED for new code; migrates to AffineScript) ────────────
RES="$(staged_matching '\.resi?$')"
# Same confinement as Rust: `bunx rescript` needs a project manifest to resolve
# sources. Without one it fails for a reason unrelated to the staged file.
if [ -n "$RES" ] && tracks 'rescript.json' '*/rescript.json' 'bsconfig.json' '*/bsconfig.json'; then
  warn "ReScript is BANNED for new code (owner ruling 2026-04-30). Existing .res migrates to .affine — AffineScript, not TypeScript."
  note "ReScript: $(echo "$RES" | grep -c .) staged file(s)"
  if require_tool bunx ReScript "https://bun.sh"; then
    if bunx rescript format --help 2>&1 | grep -q -- '-check'; then
      bunx rescript format -all -check || fail "ReScript: sources are not formatted"
    else
      warn "ReScript: the installed CLI has no 'format -check'; the format gate did NOT run. This is a skip, not a pass."
      LEDGER+=("rescript-format: SKIPPED (CLI lacks -check)")
    fi
  fi
else
  LEDGER+=("rescript: not staged, or repo tracks no rescript.json/bsconfig.json")
fi

# ── V (BANNED; Zig migration completed 2026-05-28) ──────────────────────
V_SRC="$(staged_matching '\.v$')"
if [ -n "$V_SRC" ]; then
  if tracks 'v.mod' '*/v.mod'; then
    warn "V-lang is BANNED (owner ruling 2026-04-10; estate migration to Zig COMPLETED 2026-05-28). A v.mod here means a carve-out or a regression — confirm which."
    note "V: $(echo "$V_SRC" | grep -c .) staged file(s)"
    if require_tool v V "https://github.com/vlang/v/releases"; then
      # ⚠ MEASURED on v 0.5.2: `v fmt -verify <FILE>` prints its findings and
      # then EXITS 0 — a fake green. Only the DIRECTORY form exits 1. Do not
      # rewrite this as a per-file loop.
      v fmt -verify . || fail "V: sources are not formatted (v fmt -verify)"
      v vet .         || fail "V: v vet reported problems"
    fi
  else
    # `.v` is shared with Coq proof scripts and Verilog. Without a v.mod this
    # is NOT V source, and summoning a V toolchain for it would be wrong.
    note "V: .v staged but no v.mod tracked — treating as Coq/Verilog, not V. No V check run."
    LEDGER+=("v: .v staged without v.mod (Coq/Verilog — correctly not checked)")
  fi
else
  LEDGER+=("v: not staged")
fi

# ── Haskell ─────────────────────────────────────────────────────────────
HS="$(staged_matching '\.hs$')"
if [ -n "$HS" ] && tracks '*.cabal' 'stack.yaml' '*/stack.yaml'; then
  note "Haskell: $(echo "$HS" | grep -c .) staged file(s)"
  RAN_ANY=0
  if command -v fourmolu >/dev/null 2>&1; then
    # shellcheck disable=SC2086
    fourmolu --mode check $HS || fail "Haskell: sources are not formatted (fourmolu --mode check)"
    RAN_ANY=1
  fi
  if command -v hlint >/dev/null 2>&1; then
    # shellcheck disable=SC2086
    hlint $HS || fail "Haskell: hlint reported problems"
    RAN_ANY=1
  fi
  if [ "$RAN_ANY" -eq 0 ]; then
    # Optional linters, not the toolchain — see the header. Reported as a
    # skip so it cannot be mistaken for a clean result.
    warn "Haskell: neither fourmolu nor hlint is installed, so NO Haskell gate ran. This is a skip, not a pass — CI still checks with -Wall -Werror."
    LEDGER+=("haskell: SKIPPED (no fourmolu/hlint installed)")
  fi
else
  LEDGER+=("haskell: not staged or repo has no cabal/stack manifest")
fi

# ── Ledger ──────────────────────────────────────────────────────────────
# Make every non-run legible. An absent check must never read as a satisfied
# one just because the terminal stayed quiet.
if [ "${#LEDGER[@]}" -gt 0 ]; then
  echo ""
  echo -e "${BLUE}[lint] gates that did not run:${NC}"
  for l in "${LEDGER[@]}"; do echo "  - $l"; done
fi

if [ "$ERRORS" -gt 0 ]; then
  echo -e "${RED}[lint] FAILED with ${ERRORS} error(s).${NC}" >&2
  echo "  Fix locally with the ecosystem's own formatter, then re-stage." >&2
  echo "  Deliberate override for this one commit: git commit --no-verify" >&2
  exit 1
fi

echo -e "${GREEN}[lint] ecosystem lint + format checks passed.${NC}"
exit 0
