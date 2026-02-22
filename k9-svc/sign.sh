#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# sign.sh - Ed25519 signing and verification for K9 Hunt-level components
#
# Uses OpenSSL for cryptographic operations.
# Keys stored in ~/.config/k9/keys/
#
# Usage:
#   ./sign.sh keygen [name]        Generate new keypair
#   ./sign.sh sign <file>          Sign a component
#   ./sign.sh verify <file>        Verify a component's signature
#   ./sign.sh trust <pubkey>       Add public key to trusted keys
#   ./sign.sh list                 List trusted keys

set -eu

K9_CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/k9"
K9_KEYS_DIR="$K9_CONFIG_DIR/keys"
K9_TRUSTED_DIR="$K9_KEYS_DIR/trusted"

# ─────────────────────────────────────────────────────────────
# Setup
# ─────────────────────────────────────────────────────────────

ensure_dirs() {
    mkdir -p "$K9_KEYS_DIR"
    mkdir -p "$K9_TRUSTED_DIR"
    chmod 700 "$K9_KEYS_DIR"
}

check_openssl() {
    if ! command -v openssl >/dev/null 2>&1; then
        echo "K9: Error: OpenSSL not found. Install it first." >&2
        exit 1
    fi
    # Check for Ed25519 support (OpenSSL 1.1.1+)
    if ! openssl genpkey -algorithm Ed25519 -out /dev/null 2>/dev/null; then
        echo "K9: Error: OpenSSL version doesn't support Ed25519." >&2
        echo "K9: Requires OpenSSL 1.1.1 or later." >&2
        exit 1
    fi
}

# ─────────────────────────────────────────────────────────────
# Key Generation
# ─────────────────────────────────────────────────────────────

cmd_keygen() {
    ensure_dirs
    check_openssl

    name="${1:-primary}"
    privkey="$K9_KEYS_DIR/${name}.key"
    pubkey="$K9_KEYS_DIR/${name}.pub"

    if [ -f "$privkey" ]; then
        echo "K9: Key '$name' already exists at $privkey" >&2
        echo "K9: Delete it first if you want to regenerate." >&2
        exit 1
    fi

    echo "K9: Generating Ed25519 keypair '$name'..."

    # Generate private key
    openssl genpkey -algorithm Ed25519 -out "$privkey"
    chmod 600 "$privkey"

    # Extract public key
    openssl pkey -in "$privkey" -pubout -out "$pubkey"

    echo "K9: Keypair generated:"
    echo "  Private: $privkey (keep secret!)"
    echo "  Public:  $pubkey (share this)"
    echo ""
    echo "K9: To trust this key for verification:"
    echo "  ./sign.sh trust $pubkey"
}

# ─────────────────────────────────────────────────────────────
# Signing
# ─────────────────────────────────────────────────────────────

cmd_sign() {
    ensure_dirs
    check_openssl

    file="$1"
    name="${2:-primary}"
    privkey="$K9_KEYS_DIR/${name}.key"
    sigfile="${file}.sig"

    if [ ! -f "$file" ]; then
        echo "K9: Error: File not found: $file" >&2
        exit 1
    fi

    if [ ! -f "$privkey" ]; then
        echo "K9: Error: Private key not found: $privkey" >&2
        echo "K9: Generate one with: ./sign.sh keygen $name" >&2
        exit 1
    fi

    echo "K9: Signing $file with key '$name'..."

    # Create signature
    openssl pkeyutl -sign \
        -inkey "$privkey" \
        -rawin \
        -in "$file" \
        -out "$sigfile"

    # Also create base64 version for embedding
    sig_b64=$(openssl base64 -in "$sigfile" | tr -d '\n')

    echo "K9: Signature created: $sigfile"
    echo ""
    echo "K9: To embed in component, add to security section:"
    echo "  signature = \"$sig_b64\","
    echo ""
    echo "K9: To verify:"
    echo "  ./sign.sh verify $file"
}

# ─────────────────────────────────────────────────────────────
# Verification
# ─────────────────────────────────────────────────────────────

cmd_verify() {
    ensure_dirs
    check_openssl

    file="$1"
    sigfile="${file}.sig"

    if [ ! -f "$file" ]; then
        echo "K9: Error: File not found: $file" >&2
        exit 1
    fi

    if [ ! -f "$sigfile" ]; then
        echo "K9: Error: Signature not found: $sigfile" >&2
        echo "K9: Sign the file first with: ./sign.sh sign $file" >&2
        exit 1
    fi

    # Try each trusted key
    verified=false
    for pubkey in "$K9_TRUSTED_DIR"/*.pub; do
        if [ ! -f "$pubkey" ]; then
            continue
        fi

        keyname=$(basename "$pubkey" .pub)

        if openssl pkeyutl -verify \
            -pubin -inkey "$pubkey" \
            -rawin \
            -in "$file" \
            -sigfile "$sigfile" 2>/dev/null; then
            echo "K9: ✓ Signature VALID (key: $keyname)"
            verified=true
            break
        fi
    done

    if [ "$verified" = false ]; then
        echo "K9: ✗ Signature INVALID or key not trusted" >&2
        echo ""
        echo "K9: Trusted keys in $K9_TRUSTED_DIR:"
        ls -1 "$K9_TRUSTED_DIR"/*.pub 2>/dev/null || echo "  (none)"
        exit 1
    fi
}

# ─────────────────────────────────────────────────────────────
# Trust Management
# ─────────────────────────────────────────────────────────────

cmd_trust() {
    ensure_dirs

    pubkey="$1"

    if [ ! -f "$pubkey" ]; then
        echo "K9: Error: Public key not found: $pubkey" >&2
        exit 1
    fi

    keyname=$(basename "$pubkey" .pub)
    dest="$K9_TRUSTED_DIR/${keyname}.pub"

    cp "$pubkey" "$dest"
    echo "K9: Trusted key added: $keyname"
    echo "K9: Location: $dest"
}

cmd_untrust() {
    ensure_dirs

    keyname="$1"
    keyfile="$K9_TRUSTED_DIR/${keyname}.pub"

    if [ ! -f "$keyfile" ]; then
        echo "K9: Error: Trusted key not found: $keyname" >&2
        exit 1
    fi

    rm "$keyfile"
    echo "K9: Removed trusted key: $keyname"
}

cmd_list() {
    ensure_dirs

    echo "K9: Key Management"
    echo "─────────────────────────────────────"
    echo ""
    echo "Your keys ($K9_KEYS_DIR):"
    if ls "$K9_KEYS_DIR"/*.key 2>/dev/null | head -1 >/dev/null; then
        for key in "$K9_KEYS_DIR"/*.key; do
            name=$(basename "$key" .key)
            echo "  - $name"
        done
    else
        echo "  (none - run './sign.sh keygen' to create)"
    fi
    echo ""
    echo "Trusted keys ($K9_TRUSTED_DIR):"
    if ls "$K9_TRUSTED_DIR"/*.pub 2>/dev/null | head -1 >/dev/null; then
        for key in "$K9_TRUSTED_DIR"/*.pub; do
            name=$(basename "$key" .pub)
            # Show key fingerprint
            fp=$(openssl pkey -pubin -in "$key" -outform DER 2>/dev/null | openssl dgst -sha256 | cut -d' ' -f2 | head -c 16)
            echo "  - $name (sha256:$fp...)"
        done
    else
        echo "  (none - run './sign.sh trust <pubkey>' to add)"
    fi
}

# ─────────────────────────────────────────────────────────────
# Hunt Authorization
# ─────────────────────────────────────────────────────────────

cmd_authorize() {
    file="$1"

    echo "K9: Hunt Authorization for $file"
    echo "─────────────────────────────────────"
    echo ""

    # Check if file exists
    if [ ! -f "$file" ]; then
        echo "K9: Error: File not found: $file" >&2
        exit 1
    fi

    # Check security level
    if grep -q "trust_level.*'Hunt" "$file" 2>/dev/null; then
        echo "K9: Security level: 'Hunt (full execution)"
    else
        echo "K9: This file is not Hunt-level. No authorization needed."
        exit 0
    fi

    # Check for signature
    sigfile="${file}.sig"
    if [ ! -f "$sigfile" ]; then
        echo ""
        echo "K9: ⚠️  No signature found."
        echo "K9: Sign the file first: ./sign.sh sign $file"
        exit 1
    fi

    # Verify signature
    echo ""
    cmd_verify "$file"

    echo ""
    echo "K9: ✓ Component authorized for Hunt-level execution."
    echo "K9: You may now run its recipes."
}

# ─────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────

usage() {
    echo "K9 Signing Tool - Ed25519 signatures for Hunt-level components"
    echo ""
    echo "Usage: ./sign.sh <command> [args]"
    echo ""
    echo "Commands:"
    echo "  keygen [name]     Generate new Ed25519 keypair (default: primary)"
    echo "  sign <file>       Sign a component file"
    echo "  verify <file>     Verify a component's signature"
    echo "  authorize <file>  Full Hunt authorization check"
    echo "  trust <pubkey>    Add public key to trusted keys"
    echo "  untrust <name>    Remove key from trusted keys"
    echo "  list              List all keys"
    echo ""
    echo "Key storage: $K9_KEYS_DIR"
    echo ""
    echo "Example workflow:"
    echo "  ./sign.sh keygen                    # Generate keypair"
    echo "  ./sign.sh trust ~/.config/k9/keys/primary.pub"
    echo "  ./sign.sh sign examples/deploy.k9.ncl"
    echo "  ./sign.sh authorize examples/deploy.k9.ncl"
}

case "${1:-help}" in
    keygen)
        shift
        cmd_keygen "${1:-primary}"
        ;;
    sign)
        shift
        if [ -z "${1:-}" ]; then
            echo "K9: Error: Missing file argument" >&2
            exit 1
        fi
        cmd_sign "$1" "${2:-primary}"
        ;;
    verify)
        shift
        if [ -z "${1:-}" ]; then
            echo "K9: Error: Missing file argument" >&2
            exit 1
        fi
        cmd_verify "$1"
        ;;
    authorize)
        shift
        if [ -z "${1:-}" ]; then
            echo "K9: Error: Missing file argument" >&2
            exit 1
        fi
        cmd_authorize "$1"
        ;;
    trust)
        shift
        if [ -z "${1:-}" ]; then
            echo "K9: Error: Missing pubkey argument" >&2
            exit 1
        fi
        cmd_trust "$1"
        ;;
    untrust)
        shift
        if [ -z "${1:-}" ]; then
            echo "K9: Error: Missing key name argument" >&2
            exit 1
        fi
        cmd_untrust "$1"
        ;;
    list)
        cmd_list
        ;;
    help|--help|-h)
        usage
        ;;
    *)
        echo "K9: Unknown command: $1" >&2
        usage
        exit 1
        ;;
esac
