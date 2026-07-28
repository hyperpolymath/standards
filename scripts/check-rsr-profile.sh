#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Reference checker for the RSR template-applicability model.
# Policy: ../TEMPLATE-APPLICABILITY-POLICY.adoc
# Data:   ../.machine_readable/template-capability-gates.toml
#
# Reads a target repo's .machine_readable/rsr-profile.a2ml, resolves its
# effective capability set (preset capabilities + add - remove) against the
# gate table, and reports gated modules that are:
#   VESTIGIAL — present but the repo does not declare the gating capability.
#   MISSING   — the capability is declared but the module is absent.
#
# Usage: check-rsr-profile.sh [REPO_DIR]   (default: current directory)
# Exit:  0 OK | 1 violations | 2 setup error
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GATES="${RSR_GATES:-$SCRIPT_DIR/../.machine_readable/template-capability-gates.toml}"
REPO="${1:-.}"
PROFILE="$REPO/.machine_readable/rsr-profile.a2ml"

[ -f "$GATES" ] || { echo "ERROR: gates file not found: $GATES" >&2; exit 2; }
[ -f "$PROFILE" ] || { echo "ERROR: no profile at $PROFILE" >&2; exit 2; }

# Body lines of a [section] (comments and blanks stripped).
section() { awk -v s="[$1]" '$0==s{f=1;next} /^\[/{f=0} f && !/^[[:space:]]*#/ && NF' "$2"; }
# "quoted" tokens on the line whose key is $1 (reads section body from stdin).
quoted_on_key() { grep -E "^[[:space:]]*$1[[:space:]]*=" | grep -oE '"[^"]+"' | tr -d '"' || true; }
# Same, but the array may span lines (`key = [` … `]`) with trailing comments —
# the shape rsr-template-repo ships in its rsr-profile.a2ml, and one the
# oracle's record-dialect parser accepts, so this checker must too. The gates
# file itself stays single-line (its own header mandates that discipline).
array_on_key() { # $1 = key; section body on stdin
  awk -v k="$1" '
    on        { sub(/#.*/, ""); print; if (/\]/) on=0; next }
    $0 ~ "^[[:space:]]*"k"[[:space:]]*=" { sub(/#.*/, ""); print; if (!/\]/) on=1 }
  ' | grep -oE '"[^"]+"' | tr -d '"' || true
}

# --- target profile ---
# Capability declaration is primary: a profile may list `capabilities = [...]`
# directly. `preset` is optional sugar — a named bundle expanded from the gate
# data. A profile must provide at least one of the two.
# Section may be [rsr-profile] (what rsr-template-repo ships) or [profile]
# (the original spelling here) — the normative oracle
# (hypatia Hypatia.Rules.RsrConformance.load_capabilities/1) accepts both,
# and this reference checker must not be stricter than the oracle.
PBODY="$(section rsr-profile "$PROFILE")"
[ -n "$PBODY" ] || PBODY="$(section profile "$PROFILE")"
PRESET="$(printf '%s\n' "$PBODY" | quoted_on_key preset | sed -n 1p)"
DIRECT="$(printf '%s\n' "$PBODY" | array_on_key capabilities)"
ADD="$(printf '%s\n' "$PBODY" | array_on_key add)"
REMOVE="$(printf '%s\n' "$PBODY" | array_on_key remove)"
[ -n "$PRESET$DIRECT" ] || { echo "ERROR: profile declares neither 'capabilities' nor 'preset'" >&2; exit 2; }

# --- preset's base capabilities, if a preset is named ---
PRESETCAPS=""
if [ -n "$PRESET" ]; then
  PRESETCAPS="$(section presets "$GATES" | quoted_on_key "$PRESET")"
  [ -n "$PRESETCAPS" ] || { echo "ERROR: unknown preset '$PRESET' (not in $GATES [presets])" >&2; exit 2; }
fi

# --- effective capabilities = capabilities + presetcaps + add - remove ---
EFFECTIVE="$(printf '%s\n%s\n%s\n' "$DIRECT" "$PRESETCAPS" "$ADD" | sort -u | sed '/^$/d')"
if [ -n "$REMOVE" ]; then
  EFFECTIVE="$(comm -23 <(printf '%s\n' "$EFFECTIVE") <(printf '%s\n' "$REMOVE" | sort -u))"
fi
has_cap() { printf '%s\n' "$EFFECTIVE" | grep -qx "$1"; }

# Presence of a module path: file, dir/ (trailing slash), or glob (contains *).
present() {
  local key="$1"
  case "$key" in
    */) [ -d "$REPO/${key%/}" ] ;;
    *'*'*) ( shopt -s globstar nullglob; local m=("$REPO"/$key); [ ${#m[@]} -gt 0 ] ) ;;
    *) [ -e "$REPO/$key" ] ;;
  esac
}

echo "repo:    $REPO"
echo "profile: ${PRESET:+preset=$PRESET }${DIRECT:+direct-capabilities}"
echo "effective capabilities: $(printf '%s ' $EFFECTIVE)"
echo

fail=0
while IFS= read -r line; do
  path="$(printf '%s' "$line" | grep -oE '"[^"]+"' | sed -n 1p | tr -d '"')"
  cap="$(printf '%s' "$line" | grep -oE '"[^"]+"' | sed -n 2p | tr -d '"')"
  [ -n "$path" ] && [ -n "$cap" ] || continue
  if has_cap "$cap"; then
    present "$path" || { echo "  MISSING ($cap): $path"; fail=1; }
  else
    present "$path" && { echo "  VESTIGIAL (no '$cap' capability): $path"; fail=1; }
  fi
done < <(section gates "$GATES")

if [ "$fail" -ne 0 ]; then
  cat >&2 <<'MSG'

rsr-profile check: FAIL — scaffold does not match declared capabilities.
Fix one of:
  * remove the vestigial module, OR
  * declare the capability in .machine_readable/rsr-profile.a2ml (with a
    [rationale] line), OR
  * add the missing module.
MSG
  exit 1
fi
echo "rsr-profile check: OK — scaffold matches declared capabilities."
