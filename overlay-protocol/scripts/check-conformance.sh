#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Overlay Protocol Conformance Checker
#
# Validates that a directory claiming to be an overlay conforms to the
# Overlay Protocol specification (v1.0.0-draft).
#
# Usage:
#   check-conformance.sh <overlay-dir> [base-dir]
#
# If base-dir is not provided, it is read from the overlay-protocol.base
# field in ECOSYSTEM.scm.
#
# Exit codes:
#   0 — all checks passed
#   1 — one or more checks failed
#   2 — usage error

set -euo pipefail

# --- Colours (disabled if not a terminal) ---
if [ -t 1 ]; then
  RED='\033[0;31m'
  GREEN='\033[0;32m'
  YELLOW='\033[0;33m'
  BOLD='\033[1m'
  RESET='\033[0m'
else
  RED='' GREEN='' YELLOW='' BOLD='' RESET=''
fi

pass_count=0
fail_count=0
warn_count=0

pass()  { echo -e "  ${GREEN}PASS${RESET}  $1"; pass_count=$((pass_count + 1)); }
fail()  { echo -e "  ${RED}FAIL${RESET}  $1"; fail_count=$((fail_count + 1)); }
warn()  { echo -e "  ${YELLOW}WARN${RESET}  $1"; warn_count=$((warn_count + 1)); }
info()  { echo -e "  ${BOLD}INFO${RESET}  $1"; }

# --- Usage ---
if [ $# -lt 1 ]; then
  echo "Usage: check-conformance.sh <overlay-dir> [base-dir]"
  echo ""
  echo "Validates an overlay directory against the Overlay Protocol spec."
  exit 2
fi

OVERLAY_DIR="$(cd "$1" && pwd)"
BASE_DIR_ARG="${2:-}"

echo -e "${BOLD}Overlay Protocol Conformance Check${RESET}"
echo "  Overlay: $OVERLAY_DIR"
echo ""

# --- Locate ECOSYSTEM.scm ---
ECOSYSTEM_FILE=""
for candidate in \
  "$OVERLAY_DIR/.machine_readable/ECOSYSTEM.scm" \
  "$OVERLAY_DIR/.machine_readable/6scm/ECOSYSTEM.scm"; do
  if [ -f "$candidate" ]; then
    ECOSYSTEM_FILE="$candidate"
    break
  fi
done

echo -e "${BOLD}1. Declaration (ECOSYSTEM.scm)${RESET}"

if [ -z "$ECOSYSTEM_FILE" ]; then
  fail "ECOSYSTEM.scm not found in .machine_readable/ or .machine_readable/6scm/"
else
  pass "ECOSYSTEM.scm found: ${ECOSYSTEM_FILE#"$OVERLAY_DIR"/}"
fi

# --- Helper: extract field value from overlay-protocol section ---
# Simple grep-based extraction for (field . "value") patterns.
extract_field() {
  local field="$1"
  if [ -z "$ECOSYSTEM_FILE" ]; then
    echo ""
    return
  fi
  # Match (field . "value") or (field . #t) / (field . #f)
  grep -oP "\($field\s+\.\s+\K[^\)]+(?=\))" "$ECOSYSTEM_FILE" 2>/dev/null \
    | head -1 \
    | sed 's/^"//; s/"$//' \
    || true
}

# --- Check overlay-protocol section exists ---
if [ -n "$ECOSYSTEM_FILE" ]; then
  if grep -q 'overlay-protocol' "$ECOSYSTEM_FILE"; then
    pass "overlay-protocol section present"
  else
    fail "overlay-protocol section missing from ECOSYSTEM.scm"
  fi
fi

# --- Required fields ---
echo ""
echo -e "${BOLD}2. Required Fields${RESET}"

FIELD_BASE="$(extract_field 'base')"
FIELD_UPSTREAM="$(extract_field 'upstream')"
FIELD_PEER_TYPE="$(extract_field 'peer-type')"
FIELD_ACTIVATION="$(extract_field 'activation')"
FIELD_DEACTIVATION="$(extract_field 'deactivation')"
FIELD_SWITCHABLE="$(extract_field 'switchable')"
FIELD_MODIFIES_BASE="$(extract_field 'modifies-base')"
FIELD_DESCRIPTION="$(extract_field 'description')"

check_field() {
  local name="$1" value="$2"
  if [ -n "$value" ]; then
    pass "$name = $value"
  else
    fail "$name not found or empty"
  fi
}

check_field "base" "$FIELD_BASE"
check_field "upstream" "$FIELD_UPSTREAM"
check_field "peer-type" "$FIELD_PEER_TYPE"
check_field "activation" "$FIELD_ACTIVATION"
check_field "deactivation" "$FIELD_DEACTIVATION"

# switchable and modifies-base need special handling (boolean, not quoted)
if [ -n "$ECOSYSTEM_FILE" ]; then
  if grep -qP 'switchable\s+\.\s+#t' "$ECOSYSTEM_FILE" 2>/dev/null; then
    pass "switchable = #t"
  elif grep -qP 'switchable\s+\.\s+#f' "$ECOSYSTEM_FILE" 2>/dev/null; then
    fail "switchable = #f (MUST be #t)"
  else
    fail "switchable field not found"
  fi

  if grep -qP 'modifies-base\s+\.\s+#f' "$ECOSYSTEM_FILE" 2>/dev/null; then
    pass "modifies-base = #f"
  elif grep -qP 'modifies-base\s+\.\s+#t' "$ECOSYSTEM_FILE" 2>/dev/null; then
    fail "modifies-base = #t (MUST be #f)"
  else
    fail "modifies-base field not found"
  fi
fi

check_field "description" "$FIELD_DESCRIPTION"

# --- Validate peer-type value ---
if [ -n "$FIELD_PEER_TYPE" ]; then
  case "$FIELD_PEER_TYPE" in
    o-extension|aggregate-library)
      pass "peer-type is valid ($FIELD_PEER_TYPE)"
      ;;
    *)
      fail "peer-type '$FIELD_PEER_TYPE' not recognised (must be o-extension or aggregate-library)"
      ;;
  esac
fi

# --- Resolve base directory ---
echo ""
echo -e "${BOLD}3. Base Project Integrity${RESET}"

if [ -n "$BASE_DIR_ARG" ]; then
  BASE_DIR="$(cd "$BASE_DIR_ARG" && pwd)"
elif [ -n "$FIELD_BASE" ]; then
  CANDIDATE="$OVERLAY_DIR/$FIELD_BASE"
  if [ -d "$CANDIDATE" ]; then
    BASE_DIR="$(cd "$CANDIDATE" && pwd)"
  else
    BASE_DIR=""
  fi
else
  BASE_DIR=""
fi

if [ -n "$BASE_DIR" ]; then
  info "Base project: $BASE_DIR"

  # Check that the overlay has not placed files inside the base directory.
  # We check if the overlay's VCS tracks any files under the base path.
  if [ -d "$OVERLAY_DIR/.git" ] || git -C "$OVERLAY_DIR" rev-parse --git-dir >/dev/null 2>&1; then
    overlay_files_in_base="$(git -C "$OVERLAY_DIR" ls-files -- "$BASE_DIR" 2>/dev/null | head -5)"
    if [ -n "$overlay_files_in_base" ]; then
      fail "Overlay VCS tracks files inside base project: $(echo "$overlay_files_in_base" | head -3)"
    else
      pass "No overlay-tracked files inside base project"
    fi
  else
    # No git — do a simpler check: look for files owned by the overlay
    # that are symlinked or placed in the base dir
    warn "Not a git repository — cannot verify base integrity via VCS (manual check recommended)"
  fi
else
  warn "Base directory not found or not provided — skipping base integrity checks"
fi

# --- Peer-type specific checks ---
echo ""
echo -e "${BOLD}4. Peer-Type Specific Checks (${FIELD_PEER_TYPE:-unknown})${RESET}"

if [ "$FIELD_PEER_TYPE" = "o-extension" ]; then
  # activate.sh must exist and be executable
  if [ -f "$OVERLAY_DIR/activate.sh" ]; then
    pass "activate.sh exists"
    if [ -x "$OVERLAY_DIR/activate.sh" ]; then
      pass "activate.sh is executable"
    else
      fail "activate.sh is not executable (chmod +x needed)"
    fi

    # Check activate.sh sets at least one environment variable
    if grep -qP '^\s*export\s+\w+=' "$OVERLAY_DIR/activate.sh" 2>/dev/null; then
      pass "activate.sh exports environment variables"
    else
      warn "activate.sh does not appear to export any variables"
    fi
  else
    fail "activate.sh not found (required for o-extension)"
  fi

elif [ "$FIELD_PEER_TYPE" = "aggregate-library" ]; then
  # specs directory should exist with test cases
  if [ -d "$OVERLAY_DIR/specs" ]; then
    spec_count="$(find "$OVERLAY_DIR/specs" -name '*.md' -o -name '*.yaml' -o -name '*.yml' 2>/dev/null | wc -l)"
    if [ "$spec_count" -gt 0 ]; then
      pass "specs/ directory with $spec_count spec files"
    else
      fail "specs/ directory is empty (needs specification files)"
    fi
  else
    fail "specs/ directory not found (required for aggregate-library)"
  fi

  # Check for test cases
  if [ -d "$OVERLAY_DIR/test" ] || [ -d "$OVERLAY_DIR/tests" ]; then
    pass "Test directory found"
  else
    warn "No test/ or tests/ directory — conformance tests recommended"
  fi

else
  warn "Unknown peer-type — skipping peer-type specific checks"
fi

# --- AI Manifest ---
echo ""
echo -e "${BOLD}5. AI Manifest${RESET}"

MANIFEST_FILE=""
for candidate in \
  "$OVERLAY_DIR/0-AI-MANIFEST.a2ml" \
  "$OVERLAY_DIR/AI.a2ml"; do
  if [ -f "$candidate" ]; then
    MANIFEST_FILE="$candidate"
    break
  fi
done

if [ -n "$MANIFEST_FILE" ]; then
  pass "AI manifest found: ${MANIFEST_FILE#"$OVERLAY_DIR"/}"

  # Check for non-modification invariant declaration
  if grep -qiP 'never\s*modif|non.modification|modifies.base|untouched|never\s*touch|never\s*change' "$MANIFEST_FILE" 2>/dev/null; then
    pass "AI manifest declares non-modification invariant"
  else
    warn "AI manifest does not explicitly declare non-modification invariant"
  fi
else
  warn "No AI manifest (0-AI-MANIFEST.a2ml or AI.a2ml) — recommended for overlay protocol"
fi

# --- META.scm ADR check ---
echo ""
echo -e "${BOLD}6. Architecture Decision Record${RESET}"

META_FILE=""
for candidate in \
  "$OVERLAY_DIR/.machine_readable/META.scm" \
  "$OVERLAY_DIR/.machine_readable/6scm/META.scm"; do
  if [ -f "$candidate" ]; then
    META_FILE="$candidate"
    break
  fi
done

if [ -n "$META_FILE" ]; then
  if grep -qi 'overlay\|o-extension\|aggregate.library' "$META_FILE" 2>/dev/null; then
    pass "META.scm contains ADR explaining overlay choice"
  else
    warn "META.scm found but no ADR mentioning overlay/o-extension/aggregate-library"
  fi
else
  warn "META.scm not found — ADR documenting overlay choice recommended"
fi

# --- SPDX License Header ---
echo ""
echo -e "${BOLD}7. License${RESET}"

if [ -n "$ECOSYSTEM_FILE" ] && head -5 "$ECOSYSTEM_FILE" | grep -q 'SPDX-License-Identifier' 2>/dev/null; then
  pass "ECOSYSTEM.scm has SPDX license header"
else
  warn "ECOSYSTEM.scm missing SPDX license header"
fi

# --- Summary ---
echo ""
echo -e "${BOLD}Summary${RESET}"
echo -e "  ${GREEN}$pass_count passed${RESET}  ${RED}$fail_count failed${RESET}  ${YELLOW}$warn_count warnings${RESET}"
echo ""

if [ "$fail_count" -gt 0 ]; then
  echo -e "${RED}RESULT: NOT CONFORMANT${RESET} — $fail_count check(s) failed"
  exit 1
else
  if [ "$warn_count" -gt 0 ]; then
    echo -e "${GREEN}RESULT: CONFORMANT${RESET} (with $warn_count warning(s))"
  else
    echo -e "${GREEN}RESULT: FULLY CONFORMANT${RESET}"
  fi
  exit 0
fi
