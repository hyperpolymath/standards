#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# js-runtimes.sh — one package.json, three runtimes (LANGUAGE-POLICY §1.4).
#
#   js-runtimes.sh regen [DIR]           the only writer: canonicalise
#                                        DIR/package.json (ijson-jcs fix) and
#                                        regenerate bun.lock, pnpm-lock.yaml
#                                        and deno.lock, each with its own tool
#   js-runtimes.sh check RUNTIME [DIR]   the gate, for one of bun|pnpm|deno:
#                                        package.json is I-JSON + JCS (D306),
#                                        RUNTIME's lockfile exists and installs
#                                        FROZEN (a stale lock fails), and the
#                                        `test` script, if any, passes under
#                                        RUNTIME's own runner
#
# Bun, pnpm and Deno 2 all read package.json natively, so there is no second
# manifest to keep in step: no deno.json (Deno stays banned as an authoring
# target, §1.3) and no translation layer. What differs per runtime is the
# lockfile, and each runtime writes its own. Bun is tier 1 and its lock is
# the one governance requires; pnpm-lock.yaml and deno.lock are generated
# compatibility locks, never hand-edited.
#
# Exit: 0 pass; 1 a finding (not canonical, missing or stale lock, failing
# test); 2 cannot run (missing tool or manifest). A gate that cannot look
# does not report a pass.
#
# Tools: ijson-jcs (on PATH or $IJSON_JCS), jq, and the runtime(s) concerned.
set -uo pipefail

IJSON_JCS="${IJSON_JCS:-ijson-jcs}"
RUNTIMES=(bun pnpm deno)

# die <message> [status] — write a diagnostic to stderr and exit the current
# shell with status (default: 2).
die()  { printf 'js-runtimes: %s\n' "$1" >&2; exit "${2:-2}"; }
# note <message> — write a prefixed message and newline to stdout.
note() { printf 'js-runtimes: %s\n' "$1"; }

# lockfile_of <runtime> — print the lockfile name for bun, pnpm or deno.
# An unknown runtime exits the current shell with status 2.
lockfile_of() {
  case "$1" in
    bun)  echo bun.lock ;;
    pnpm) echo pnpm-lock.yaml ;;
    deno) echo deno.lock ;;
    *)    die "unknown runtime '$1' (want one of: ${RUNTIMES[*]})" ;;
  esac
}

# need <command> — return 0 if the command resolves, otherwise exit with 2.
# Accepts a command name or an executable path, as used by IJSON_JCS.
need() { command -v "$1" >/dev/null 2>&1 || die "$1 is not on PATH (cannot run, not a pass)"; }

# Return 0 for a string scripts.test in the current directory's package.json,
# including an empty string. Other values return non-zero, as do jq errors;
# check treats either as no test script.
has_test_script() { # does package.json declare a string `scripts.test`?
  jq -e '(.scripts.test | type) == "string"' package.json >/dev/null 2>&1
}

# regen [DIR] — canonicalise package.json and regenerate all three locks in
# DIR (default: the current directory). Changes the shell's working directory;
# Deno also populates node_modules. Earlier writes remain if a later step fails.
# Exits with 2 for a missing directory, manifest or required tool; with 1 for
# bun.lockb or a failed canonicalisation or dependency resolution command.
regen() {
  local dir="${1:-.}"
  cd -- "$dir" || die "no directory $dir"
  [ -f package.json ] || die "no package.json in $dir"
  need "$IJSON_JCS"; need bun; need pnpm; need deno
  [ ! -e bun.lockb ] || die "bun.lockb (binary) present: remove it; bun.lock is the text lock" 1

  "$IJSON_JCS" fix package.json >/dev/null || die "package.json is not I-JSON (ijson-jcs fix refused it)" 1
  bun install --lockfile-only >/dev/null    || die "bun could not resolve package.json" 1
  pnpm install --lockfile-only >/dev/null   || die "pnpm could not resolve package.json" 1
  # deno has no lockfile-only mode; it also populates node_modules.
  deno install >/dev/null                   || die "deno could not resolve package.json" 1
  # A tool that rewrote package.json while resolving is put back in canon.
  "$IJSON_JCS" fix package.json >/dev/null  || die "package.json stopped being I-JSON during regen" 1
  note "regenerated in $(pwd): package.json (JCS), bun.lock, pnpm-lock.yaml, deno.lock"
}

# check <runtime> [DIR] — require I-JSON + JCS package.json and the runtime's
# lockfile, install frozen dependencies, then run a string test script, if any.
# Runtime is bun, pnpm or deno; DIR defaults to the current directory. Changes
# the shell's working directory and installs dependencies; test side effects
# depend on the package. Returns 0 on success. Exits with 2 for an unknown
# runtime or missing directory, manifest or tool; with 1 for any canonicality
# check, lockfile, frozen install or test failure, replacing tool exit statuses.
check() {
  local rt="$1" dir="${2:-.}" lock rc
  # Validate here: a die inside $(...) would only end the subshell.
  case " ${RUNTIMES[*]} " in *" $rt "*) ;; *) die "unknown runtime '$rt' (want one of: ${RUNTIMES[*]})" ;; esac
  lock="$(lockfile_of "$rt")"
  cd -- "$dir" || die "no directory $dir"
  [ -f package.json ] || die "no package.json in $dir"
  need "$IJSON_JCS"; need jq; need "$rt"

  rc=0; "$IJSON_JCS" check package.json >/dev/null 2>&1 || rc=$?
  case "$rc" in
    0) note "package.json is I-JSON + JCS" ;;
    1) die "package.json is not JCS-canonical: run scripts/js-runtimes.sh regen" 1 ;;
    *) die "package.json is not I-JSON (ijson-jcs exit $rc)" 1 ;;
  esac

  [ -f "$lock" ] || die "$lock is missing: run scripts/js-runtimes.sh regen" 1
  case "$rt" in
    bun)  bun install --frozen-lockfile ;;
    pnpm) pnpm install --frozen-lockfile ;;
    deno) deno install --frozen ;;
  esac >/dev/null 2>&1 || die "$rt: $lock is stale against package.json (frozen install refused): run scripts/js-runtimes.sh regen" 1
  note "$rt installs frozen from $lock"

  if has_test_script; then
    case "$rt" in
      bun)  bun run test ;;
      pnpm) pnpm run test ;;
      deno) deno task test ;;
    esac || die "$rt: the test script failed" 1
    note "$rt ran the test script"
  else
    note "$rt: no test script (install-only check)"
  fi
}

case "${1:-}" in
  regen) shift; regen "$@" ;;
  check) shift; [ $# -ge 1 ] || die "usage: js-runtimes.sh check bun|pnpm|deno [DIR]"; check "$@" ;;
  *)     die "usage: js-runtimes.sh regen [DIR] | check bun|pnpm|deno [DIR]" ;;
esac
