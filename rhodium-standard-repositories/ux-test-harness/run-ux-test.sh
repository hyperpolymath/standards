#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# UX Test Runner — executed inside each test container
# Validates that a repo's setup/doctor/build flow works on this platform.
#
# Usage: Mounted repo at /repo, this script runs the UX checks.
# Output: JSON report to stdout (pipe to file from host).

set -uo pipefail

REPO_DIR="/repo"
REPORT_FILE="/tmp/ux-test-report.json"
OS_ID="$(cat /etc/os-release 2>/dev/null | grep '^ID=' | cut -d= -f2 | tr -d '"')"
OS_VERSION="$(cat /etc/os-release 2>/dev/null | grep '^VERSION_ID=' | cut -d= -f2 | tr -d '"')"
ARCH="$(uname -m)"
LIBC="$(ldd --version 2>&1 | head -1 || echo "unknown")"
TIMESTAMP="$(date -Iseconds)"

echo "=== UX Test: $OS_ID $OS_VERSION ($ARCH) ===" >&2
echo "Repo: $REPO_DIR" >&2
echo "" >&2

RESULTS=()
PASS=0
FAIL=0
WARN=0

record() {
    local check="$1" status="$2" detail="$3"
    RESULTS+=("{\"check\":\"$check\",\"status\":\"$status\",\"detail\":\"$detail\"}")
    case "$status" in
        pass) PASS=$((PASS + 1)) ;;
        fail) FAIL=$((FAIL + 1)) ;;
        warn) WARN=$((WARN + 1)) ;;
    esac
    echo "  [$status] $check: $detail" >&2
}

# --- Phase 1: Repo structure checks ---
echo "Phase 1: Structure" >&2

[ -f "$REPO_DIR/README.adoc" ] && record "readme" "pass" "README.adoc present" \
    || record "readme" "fail" "README.adoc missing"

[ -f "$REPO_DIR/QUICKSTART-USER.adoc" ] && record "quickstart" "pass" "QUICKSTART-USER.adoc present" \
    || record "quickstart" "fail" "QUICKSTART-USER.adoc missing"

[ -f "$REPO_DIR/EXPLAINME.adoc" ] && record "explainme" "pass" "EXPLAINME.adoc present" \
    || record "explainme" "warn" "EXPLAINME.adoc missing"

[ -f "$REPO_DIR/Justfile" ] || [ -f "$REPO_DIR/justfile" ] && record "justfile" "pass" "Justfile present" \
    || record "justfile" "fail" "Justfile missing"

[ -f "$REPO_DIR/0-AI-MANIFEST.a2ml" ] && record "manifest" "pass" "AI manifest present" \
    || record "manifest" "warn" "0-AI-MANIFEST.a2ml missing"

[ -f "$REPO_DIR/LICENSE" ] && record "license" "pass" "LICENSE present" \
    || record "license" "fail" "LICENSE missing"

[ -d "$REPO_DIR/.machine_readable" ] && record "machine_readable" "pass" ".machine_readable/ present" \
    || record "machine_readable" "warn" ".machine_readable/ missing"

# --- Phase 2: Hardcoded path scan ---
echo "" >&2
echo "Phase 2: Hardcoded paths" >&2

HARDCODED=$(grep -r --include='*.sh' --include='*.rs' --include='*.ex' --include='*.exs' \
    --include='*.zig' --include='*.res' --include='*.ncl' --include='*.v' \
    -l '/home/hyper\|/mnt/eclipse\|/var/mnt/eclipse' "$REPO_DIR/src" "$REPO_DIR/lib" \
    "$REPO_DIR/scripts" "$REPO_DIR/ffi" 2>/dev/null | head -20)

if [ -z "$HARDCODED" ]; then
    record "hardcoded_paths" "pass" "No hardcoded absolute paths in source"
else
    COUNT=$(echo "$HARDCODED" | wc -l)
    record "hardcoded_paths" "fail" "$COUNT files with hardcoded paths"
fi

# --- Phase 3: just doctor ---
echo "" >&2
echo "Phase 3: just doctor" >&2

if [ -f "$REPO_DIR/Justfile" ] || [ -f "$REPO_DIR/justfile" ]; then
    cd "$REPO_DIR"
    if just --list 2>/dev/null | grep -q "doctor"; then
        DOCTOR_OUT=$(just doctor 2>&1) || true
        DOCTOR_FAILS=$(echo "$DOCTOR_OUT" | grep -c '\[FAIL\]' || true)
        DOCTOR_WARNS=$(echo "$DOCTOR_OUT" | grep -c '\[WARN\]' || true)
        DOCTOR_PASS=$(echo "$DOCTOR_OUT" | grep -c '\[OK\]' || true)
        record "just_doctor" "pass" "doctor recipe exists: $DOCTOR_PASS OK, $DOCTOR_FAILS FAIL, $DOCTOR_WARNS WARN"

        # Each FAIL from doctor is a platform issue
        while IFS= read -r line; do
            tool=$(echo "$line" | sed "s/.*\[FAIL\] //; s/:.*//")
            record "doctor_$tool" "fail" "$(echo "$line" | sed 's/.*\[FAIL\] //')"
        done < <(echo "$DOCTOR_OUT" | grep '\[FAIL\]')
    else
        record "just_doctor" "fail" "No doctor recipe in Justfile"
    fi

    # Check for heal recipe
    if just --list 2>/dev/null | grep -q "heal"; then
        record "just_heal" "pass" "heal recipe exists"
    else
        record "just_heal" "warn" "No heal recipe"
    fi

    # Check for tour recipe
    if just --list 2>/dev/null | grep -q "tour"; then
        record "just_tour" "pass" "tour recipe exists"
    else
        record "just_tour" "warn" "No tour recipe"
    fi

    # Check for help-me recipe
    if just --list 2>/dev/null | grep -q "help-me"; then
        record "just_help_me" "pass" "help-me recipe exists"
    else
        record "just_help_me" "warn" "No help-me recipe"
    fi
else
    record "just_doctor" "fail" "No Justfile"
fi

# --- Phase 4: Contractile checks ---
echo "" >&2
echo "Phase 4: Contractiles" >&2

MR="$REPO_DIR/.machine_readable"
[ -f "$MR/MUST.contractile" ] && record "must_contractile" "pass" "MUST.contractile present" \
    || record "must_contractile" "warn" "MUST.contractile missing"

[ -f "$MR/TRUST.contractile" ] && record "trust_contractile" "pass" "TRUST.contractile present" \
    || record "trust_contractile" "warn" "TRUST.contractile missing"

[ -f "$MR/INTENT.contractile" ] && record "intent_contractile" "pass" "INTENT.contractile present" \
    || record "intent_contractile" "warn" "INTENT.contractile missing"

[ -f "$MR/ADJUST.contractile" ] && record "adjust_contractile" "pass" "ADJUST.contractile present" \
    || record "adjust_contractile" "warn" "ADJUST.contractile missing"

# --- Phase 5: Guix/Nix environment ---
echo "" >&2
echo "Phase 5: Reproducible environment" >&2

[ -f "$REPO_DIR/guix.scm" ] && record "guix" "pass" "guix.scm present" \
    || record "guix" "warn" "guix.scm missing"

[ -f "$REPO_DIR/flake.nix" ] && record "nix" "pass" "flake.nix present" \
    || record "nix" "warn" "flake.nix missing"

# --- Phase 6: LLM warmup ---
echo "" >&2
echo "Phase 6: LLM warmup" >&2

[ -f "$REPO_DIR/llm-warmup-user.md" ] && record "llm_warmup_user" "pass" "User warmup present" \
    || record "llm_warmup_user" "warn" "llm-warmup-user.md missing"

[ -f "$REPO_DIR/llm-warmup-dev.md" ] && record "llm_warmup_dev" "pass" "Dev warmup present" \
    || record "llm_warmup_dev" "warn" "llm-warmup-dev.md missing"

# --- Phase 7: Platform security snapshot ---
echo "" >&2
echo "Phase 7: Platform security" >&2

if command -v firewall-cmd >/dev/null 2>&1; then
    record "firewall" "pass" "firewalld available"
elif command -v ufw >/dev/null 2>&1; then
    record "firewall" "pass" "ufw available"
else
    record "firewall" "warn" "No firewall detected (container environment)"
fi

if command -v getenforce >/dev/null 2>&1; then
    SE_STATUS=$(getenforce 2>/dev/null || echo "unknown")
    record "selinux" "pass" "SELinux: $SE_STATUS"
else
    record "selinux" "warn" "SELinux not available"
fi

# --- Emit JSON report ---
echo "" >&2
echo "=== Results: $PASS pass, $FAIL fail, $WARN warn ===" >&2

RESULTS_JSON=$(printf '%s\n' "${RESULTS[@]}" | paste -sd, -)

cat <<ENDJSON
{
  "timestamp": "$TIMESTAMP",
  "platform": {
    "os": "$OS_ID",
    "version": "$OS_VERSION",
    "arch": "$ARCH",
    "libc": "$LIBC"
  },
  "repo": "$(basename "$REPO_DIR")",
  "summary": {
    "pass": $PASS,
    "fail": $FAIL,
    "warn": $WARN
  },
  "results": [$RESULTS_JSON]
}
ENDJSON
