#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# test.sh - K9 SVC Test Suite
#
# Runs all tests for the K9 Self-Validating Component system.
# Exit codes: 0 = all pass, 1 = failures

set -eu

PASS=0
FAIL=0
SKIP=0

# ─────────────────────────────────────────────────────────────
# Test Utilities
# ─────────────────────────────────────────────────────────────

green() { printf '\033[32m%s\033[0m\n' "$1"; }
red() { printf '\033[31m%s\033[0m\n' "$1"; }
yellow() { printf '\033[33m%s\033[0m\n' "$1"; }

pass() {
    green "  ✓ $1"
    PASS=$((PASS + 1))
}

fail() {
    red "  ✗ $1"
    FAIL=$((FAIL + 1))
}

skip() {
    yellow "  ○ $1 (skipped)"
    SKIP=$((SKIP + 1))
}

section() {
    echo ""
    echo "━━━ $1 ━━━"
}

# ─────────────────────────────────────────────────────────────
# Environment Tests
# ─────────────────────────────────────────────────────────────

test_environment() {
    section "Environment"

    # Test must shim
    if ./must status >/dev/null 2>&1; then
        pass "must shim executes"
    else
        fail "must shim failed"
    fi

    # Test OS detection
    os=$(./must status | grep "OS:" | awk '{print $2}')
    if [ -n "$os" ]; then
        pass "OS detected: $os"
    else
        fail "OS detection failed"
    fi

    # Test Nickel
    if command -v nickel >/dev/null 2>&1; then
        pass "Nickel installed"
    else
        fail "Nickel not found"
    fi

    # Test Just
    if command -v just >/dev/null 2>&1; then
        pass "Just installed"
    else
        fail "Just not found"
    fi

    # Test OpenSSL (for signing)
    if command -v openssl >/dev/null 2>&1; then
        if openssl genpkey -algorithm Ed25519 -out /dev/null 2>/dev/null; then
            pass "OpenSSL with Ed25519 support"
        else
            skip "OpenSSL without Ed25519 (older version)"
        fi
    else
        skip "OpenSSL not installed"
    fi
}

# ─────────────────────────────────────────────────────────────
# Schema Tests
# ─────────────────────────────────────────────────────────────

test_schemas() {
    section "Schema Validation"

    # Test pedigree.ncl
    if nickel typecheck pedigree.ncl 2>/dev/null; then
        pass "pedigree.ncl typechecks"
    else
        fail "pedigree.ncl typecheck failed"
    fi

    # Test register.ncl
    if nickel typecheck register.ncl 2>/dev/null; then
        pass "register.ncl typechecks"
    else
        fail "register.ncl typecheck failed"
    fi

    # Test leash.ncl
    if nickel typecheck leash.ncl 2>/dev/null; then
        pass "leash.ncl typechecks"
    else
        fail "leash.ncl typecheck failed"
    fi
}

# ─────────────────────────────────────────────────────────────
# Example Tests
# ─────────────────────────────────────────────────────────────

test_examples() {
    section "Example Components"

    # Test hello.k9 (Kennel level)
    if [ -f "examples/hello.k9" ]; then
        if head -1 examples/hello.k9 | grep -q "K9!"; then
            pass "hello.k9 has magic number"
        else
            fail "hello.k9 missing magic number"
        fi
    else
        fail "hello.k9 not found"
    fi

    # Test config.k9.ncl (Yard level)
    if [ -f "examples/config.k9.ncl" ]; then
        if nickel typecheck examples/config.k9.ncl 2>/dev/null; then
            pass "config.k9.ncl typechecks"
        else
            fail "config.k9.ncl typecheck failed"
        fi
    else
        fail "config.k9.ncl not found"
    fi

    # Test deploy.k9.ncl (Hunt level)
    if [ -f "examples/deploy.k9.ncl" ]; then
        if nickel typecheck examples/deploy.k9.ncl 2>/dev/null; then
            pass "deploy.k9.ncl typechecks"
        else
            fail "deploy.k9.ncl typecheck failed"
        fi

        # Check it declares Hunt level
        if grep -q "trust_level.*'Hunt" examples/deploy.k9.ncl; then
            pass "deploy.k9.ncl declares Hunt level"
        else
            fail "deploy.k9.ncl should declare Hunt level"
        fi
    else
        fail "deploy.k9.ncl not found"
    fi
}

# ─────────────────────────────────────────────────────────────
# MIME Tests
# ─────────────────────────────────────────────────────────────

test_mime() {
    section "MIME Files"

    # Test k9.xml
    if [ -f "mime/k9.xml" ]; then
        if command -v xmllint >/dev/null 2>&1; then
            if xmllint --noout mime/k9.xml 2>/dev/null; then
                pass "k9.xml is valid XML"
            else
                fail "k9.xml is invalid XML"
            fi
        else
            skip "xmllint not installed"
        fi

        # Check for magic number definition
        if grep -q 'value="K9!"' mime/k9.xml; then
            pass "k9.xml defines magic number"
        else
            fail "k9.xml missing magic number"
        fi
    else
        fail "mime/k9.xml not found"
    fi

    # Test k9.uti.plist (macOS)
    if [ -f "mime/k9.uti.plist" ]; then
        if command -v xmllint >/dev/null 2>&1; then
            if xmllint --noout mime/k9.uti.plist 2>/dev/null; then
                pass "k9.uti.plist is valid XML"
            else
                fail "k9.uti.plist is invalid XML"
            fi
        else
            skip "xmllint not installed"
        fi
    else
        fail "mime/k9.uti.plist not found"
    fi

    # Test k9.magic
    if [ -f "mime/k9.magic" ]; then
        if grep -q "K9!" mime/k9.magic; then
            pass "k9.magic defines magic pattern"
        else
            fail "k9.magic missing magic pattern"
        fi
    else
        fail "mime/k9.magic not found"
    fi
}

# ─────────────────────────────────────────────────────────────
# Signing Tests
# ─────────────────────────────────────────────────────────────

test_signing() {
    section "Signing System"

    if [ ! -f "sign.sh" ]; then
        fail "sign.sh not found"
        return
    fi

    if [ ! -x "sign.sh" ]; then
        fail "sign.sh not executable"
        return
    fi

    # Check OpenSSL Ed25519 support
    if ! command -v openssl >/dev/null 2>&1; then
        skip "OpenSSL not installed"
        return
    fi

    if ! openssl genpkey -algorithm Ed25519 -out /dev/null 2>/dev/null; then
        skip "OpenSSL doesn't support Ed25519"
        return
    fi

    # Test key generation (in temp dir)
    tmpdir=$(mktemp -d)
    export XDG_CONFIG_HOME="$tmpdir"

    if ./sign.sh keygen test 2>/dev/null; then
        pass "Key generation works"

        # Test key listing
        if ./sign.sh list 2>/dev/null | grep -q "test"; then
            pass "Key listing works"
        else
            fail "Key listing failed"
        fi

        # Test signing
        echo "test content" > "$tmpdir/testfile"
        if ./sign.sh sign "$tmpdir/testfile" test 2>/dev/null; then
            pass "Signing works"

            # Trust the key (XDG_CONFIG_HOME is $tmpdir, so keys are in $tmpdir/k9/keys/)
            ./sign.sh trust "$tmpdir/k9/keys/test.pub" 2>/dev/null

            # Test verification
            if ./sign.sh verify "$tmpdir/testfile" 2>/dev/null; then
                pass "Verification works"
            else
                fail "Verification failed"
            fi
        else
            fail "Signing failed"
        fi
    else
        fail "Key generation failed"
    fi

    rm -rf "$tmpdir"
    unset XDG_CONFIG_HOME
}

# ─────────────────────────────────────────────────────────────
# Container Tests
# ─────────────────────────────────────────────────────────────

test_container() {
    section "Container"

    if [ -f "Containerfile" ]; then
        pass "Containerfile exists"

        # Check for multi-stage build
        if grep -q "FROM.*AS builder" Containerfile; then
            pass "Multi-stage build configured"
        else
            fail "Multi-stage build not configured"
        fi

        # Check for non-root user
        if grep -q "USER" Containerfile; then
            pass "Non-root user configured"
        else
            fail "Running as root (security issue)"
        fi
    else
        fail "Containerfile not found"
    fi

    if [ -f "compose.yaml" ]; then
        pass "compose.yaml exists"
    else
        skip "compose.yaml not found"
    fi
}

# ─────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────

echo "K9 SVC Test Suite"
echo "═════════════════════════════════════"

test_environment
test_schemas
test_examples
test_mime
test_signing
test_container

echo ""
echo "═════════════════════════════════════"
echo "Results: $(green "$PASS passed"), $(red "$FAIL failed"), $(yellow "$SKIP skipped")"
echo ""

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi

exit 0
