#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# check-debtfile-structure.sh — VALIDATE the shape of a Debtfile.a2ml.
#
# This is the structural half of Debtfile checking, exactly as
# scripts/check-mustfile-structure.sh is the structural half for Mustfiles.
# scripts/run-debtfile.sh is the executable half.
#
# A Debtfile entry is a claim that can be re-checked. The claim is only
# re-checkable if it carries the means to re-check it, so a HOLLOW ENTRY —
# one with no `probe`, or no `ceiling` — is rejected here. An entry with a
# number and no probe is exactly the failure this whole mechanism exists to
# prevent: a count that nothing re-measures, believed because it is written
# down.
#
# ⚠ `count` MUST NOT exceed `ceiling`. If it does, the file is internally
#   inconsistent: it simultaneously records debt above the tolerated maximum
#   and claims to be a passing state. The ratchet compares ceilings between
#   revisions and would not catch this, so it is caught here.
#
# ⚠ NO FALLBACK. Every failure is a hard failure. A structural validator that
#   skips what it cannot parse reports "valid" for a file it never read, and
#   "valid" is the state that passes.
#
# Usage: check-debtfile-structure.sh [path/to/Debtfile.a2ml]
#   Default: .machine_readable/Debtfile.a2ml
# Exit: 0 valid · 1 structural defect · 2 file missing / bad invocation

set -uo pipefail

DEBT="${1:-.machine_readable/Debtfile.a2ml}"

if [ "$#" -gt 1 ]; then
  echo "usage: check-debtfile-structure.sh [path/to/Debtfile.a2ml]" >&2
  exit 2
fi
if [ ! -f "$DEBT" ]; then
  echo "error: Debtfile not found: $DEBT" >&2
  exit 2
fi

fail=0
entries=0
seen_ids=" "

name="" probe="" count="" ceiling="" severity="" policy="" accepted=""

note() { printf '  %s\n' "$*"; }
bad()  { printf '  ❌ %s\n' "$*"; fail=1; }

is_uint() { case "${1:-}" in ''|*[!0-9]*) return 1;; *) return 0;; esac; }

validate() {
  [ -n "$name" ] || return 0
  entries=$((entries + 1))

  case "$seen_ids" in
    *" $name "*) bad "duplicate id: '$name'" ;;
    *) seen_ids="${seen_ids}${name} " ;;
  esac

  # Hollow-entry rejection — the core rule.
  if [ -z "$probe" ]; then
    bad "'$name' has no '- probe:' — a number nothing re-measures is not debt, it is folklore"
  fi
  if [ -z "$ceiling" ]; then
    bad "'$name' has no '- ceiling:' — without a ceiling the ratchet has nothing to hold"
  fi
  if [ -z "$count" ]; then
    bad "'$name' has no '- count:'"
  fi

  is_uint "$count"   || { [ -n "$count" ]   && bad "'$name' count '$count' is not a non-negative integer"; }
  is_uint "$ceiling" || { [ -n "$ceiling" ] && bad "'$name' ceiling '$ceiling' is not a non-negative integer"; }

  if is_uint "$count" && is_uint "$ceiling" && [ "$count" -gt "$ceiling" ]; then
    bad "'$name' count ($count) exceeds ceiling ($ceiling) — the file records more debt than it tolerates"
  fi

  case "$severity" in
    critical|high|medium|low) ;;
    '') bad "'$name' has no '- severity:'" ;;
    *)  bad "'$name' severity '$severity' is not one of critical|high|medium|low" ;;
  esac

  case "$policy" in
    remediable|flag-only) ;;
    '') bad "'$name' has no '- policy:' — say whether this may be auto-remediated or is flag-only" ;;
    *)  bad "'$name' policy '$policy' is not one of remediable|flag-only" ;;
  esac

  if [ -n "$accepted" ]; then
    case "$accepted" in
      [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]) ;;
      *) bad "'$name' accepted-until '$accepted' is not an ISO date (YYYY-MM-DD)" ;;
    esac
  else
    bad "'$name' has no '- accepted-until:' — debt without an expiry is debt nobody ever revisits"
  fi
}

reset_block() { name="$1"; probe=""; count=""; ceiling=""; severity=""; policy=""; accepted=""; }

while IFS= read -r raw || [ -n "$raw" ]; do
  line="${raw#"${raw%%[![:space:]]*}"}"
  case "$line" in
    '### '*)              validate; reset_block "${line:4}" ;;
    '- probe: '*)         probe="${line#- probe: }" ;;
    '- count: '*)         count="${line#- count: }" ;;
    '- ceiling: '*)       ceiling="${line#- ceiling: }" ;;
    '- severity: '*)      severity="${line#- severity: }" ;;
    '- policy: '*)        policy="${line#- policy: }" ;;
    '- accepted-until: '*) accepted="${line#- accepted-until: }" ;;
  esac
done < "$DEBT"
validate  # flush last block

echo
if [ "$entries" -eq 0 ]; then
  echo "❌ Debtfile structure FAILED: $DEBT declares no entries." >&2
  echo "   An empty Debtfile and an unparsed Debtfile look identical from here," >&2
  echo "   so this is treated as a defect rather than as zero debt." >&2
  exit 1
fi

if [ "$fail" -ne 0 ]; then
  echo "❌ Debtfile structure FAILED ($entries entr(y|ies) examined)." >&2
  exit 1
fi
echo "✅ Debtfile structure OK ($entries entries)."
