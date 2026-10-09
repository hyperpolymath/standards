#!/usr/bin/env bash
# ==============================================================================
# RHODIUM STANDARD BOOTSTRAP (v2.0)
# Authority: github.com/hyperpolymath/must-spec
# Targets: Linux, Minix, macOS, iOS, Android, PC (ASIC/Edge compatible)
# Shells: bash, cmd, oil, ash, csh, dash, elvish, fish, ion, ksh, murex, 
#         ngs, nushell, powershell-core, tcsh, tsh, zsh, minix shell
# ==============================================================================

set -euo pipefail

# 1. CONSTANTS & PATHS
BIN_DIR="$HOME/.local/bin"
mkdir -p "$BIN_DIR"
export PATH="$BIN_DIR:$PATH"

# 2. TOOL MANIFEST
# Satellite Repos: must-spec, nickel-augmented (nicaug), tnav (tree-navigator)
TOOLS=("just" "must" "nicaug")

# 3. HELPER: INSTALLER
install_tool() {
    local tool=$1
    echo "--- [Securing $tool] ---"
    
    # Priority Route: Check for local binary first (Offline-First)
    if command -v "$tool" &> /dev/null; then
        echo "$tool is already locked. skipping."
        return
    fi

    # Deployment Route: fetch a PINNED, VERIFIED release.
    #
    # ⚠ WHAT THIS REPLACED SILENTLY INSTALLED A BROKEN STUB. The previous form
    # was `curl -L <url> -o "$BIN_DIR/$tool"` with no `-f`, so an HTTP error
    # was written to the output file as if it were the program. Measured
    # 2026-08-07: BOTH release URLs 404 — `hyperpolymath/must-spec` does not
    # exist at all, and `nickel-augmented` is misnamed (`nickel-augmentation`)
    # and has zero releases. So the script wrote a 9-byte file containing the
    # text `Not Found`, ran `chmod +x` on it, and reported success. Every
    # bootstrap since has produced an environment whose tools are error pages.
    #
    # `-f` makes curl fail on HTTP errors, and the artefact is checked before
    # it is made executable. A bootstrap that cannot install a tool must say so
    # — installing something unusable and exiting 0 is worse than not running.
    case $tool in
        "just")
            install_just_verified || {
                echo "bootstrap: could not install just" >&2
                return 1
            }
            return
            ;;
        "must"|"nicaug")
            echo "bootstrap: no published release for '$tool'." >&2
            echo "  There is currently no release artefact to install:" >&2
            echo "    must   -> hyperpolymath/must-spec does not exist" >&2
            echo "    nicaug -> hyperpolymath/nickel-augmentation has no releases" >&2
            echo "  Install it from source, or set PATH to a local build." >&2
            return 1
            ;;
        *)
            echo "bootstrap: unknown tool '$tool'" >&2
            return 1
            ;;
    esac
}

# Fetch a PINNED just release and verify it before use, resolving the platform
# rather than assuming one. Digests computed from the artifacts on 2026-08-07;
# casey/just publishes none, so this is trust-on-first-use — it does not prove
# they were authentic then, but any later substitution fails loudly.
JUST_VERSION="1.58.0"

just_target() {
    local os arch
    os="$(uname -s 2>/dev/null)"; arch="$(uname -m 2>/dev/null)"
    case "${os}:${arch}" in
        Linux:x86_64|Linux:amd64)   echo "x86_64-unknown-linux-musl" ;;
        Linux:aarch64|Linux:arm64)  echo "aarch64-unknown-linux-musl" ;;
        Darwin:x86_64)              echo "x86_64-apple-darwin" ;;
        Darwin:arm64|Darwin:aarch64) echo "aarch64-apple-darwin" ;;
        *)                          echo "" ;;
    esac
}

just_sha256() {
    case "$1" in
        x86_64-unknown-linux-musl)  echo "4a5cc2f53e6f0f8c59092a6cc38291eb729d46a7dd95d3ae582008881b84931d" ;;
        aarch64-unknown-linux-musl) echo "748237128c4c40cbdabc65e841d05ceba13cc23a91eaba395495894c1d9764df" ;;
        x86_64-apple-darwin)        echo "9a09cfef66aaa79da58203970103a0684307716caaabd3e9844cacc4dc0f4023" ;;
        aarch64-apple-darwin)       echo "50ae3e996c974a0bf32ea7d10f495070df33f1b43e0616b2769e3d4821ed8f48" ;;
        *)                          echo "" ;;
    esac
}

install_just_verified() {
    local target want tmp url
    target="$(just_target)"
    [ -z "$target" ] && {
        echo "just: no verified build for $(uname -s)/$(uname -m); use your package manager" >&2
        return 1
    }
    want="$(just_sha256 "$target")"
    [ -z "$want" ] && { echo "just: no pinned digest for $target" >&2; return 1; }
    tmp="$(mktemp -d)"
    url="https://github.com/casey/just/releases/download/${JUST_VERSION}/just-${JUST_VERSION}-${target}.tar.gz"
    curl -fsSL --proto '=https' --tlsv1.2 -o "$tmp/just.tar.gz" "$url" || { rm -rf "$tmp"; return 1; }
    if ! printf '%s  %s\n' "$want" "$tmp/just.tar.gz" | sha256sum -c - >/dev/null 2>&1; then
        echo "just: CHECKSUM MISMATCH for $url" >&2
        echo "  expected $want" >&2
        echo "  actual   $(sha256sum "$tmp/just.tar.gz" | cut -d" " -f1)" >&2
        rm -rf "$tmp"
        return 1
    fi
    tar -xzf "$tmp/just.tar.gz" -C "$tmp" just
    mkdir -p "$BIN_DIR"
    install -m 0755 "$tmp/just" "$BIN_DIR/just"
    rm -rf "$tmp"
}

# 4. EXECUTION
echo "Initializing Rhodium Environment..."

for tool in "${TOOLS[@]}"; do
    install_tool "$tool"
done

# 5. ALIAS ENFORCEMENT
# Ensures tree-navigator is always invoked as tnav
if [[ ! -L "$BIN_DIR/tnav" ]] && [[ -f "$BIN_DIR/tree-navigator" ]]; then
    ln -s "$BIN_DIR/tree-navigator" "$BIN_DIR/tnav"
    echo "Alias tnav -> tree-navigator secured."
fi

echo "--- Rhodium Standard Environment Secured ---"
echo "Use 'just' for local tasks and 'must' for global deployment."
