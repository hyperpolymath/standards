# k9-sign

**Memory-safe Ed25519 signing and verification for K9 Hunt-level components**

This is a Rust rewrite of `sign.sh`, eliminating:
- Shell injection vulnerabilities
- Buffer overflow risks
- Path traversal attacks
- Race conditions in file operations

## Security Improvements Over sign.sh

| Issue | sign.sh (Shell) | k9-sign (Rust) |
|-------|-----------------|----------------|
| **Memory safety** | ❌ Shell scripts have no memory safety | ✅ Rust guarantees memory safety |
| **Injection attacks** | ❌ Command injection via unsanitized input | ✅ No shell execution, all operations in-process |
| **Buffer overflows** | ❌ Possible with OpenSSL piping | ✅ Impossible (Rust's borrow checker) |
| **Path traversal** | ❌ Shell globbing can be exploited | ✅ Safe path handling with std::path |
| **Race conditions** | ❌ TOCTOU bugs in file operations | ✅ Atomic file operations where possible |
| **Error handling** | ❌ Errors can be silently ignored | ✅ Result types force error handling |
| **Type safety** | ❌ Everything is strings | ✅ Strong typing throughout |
| **Binary size** | ~11KB (shell script) | 756KB (self-contained binary) |

## Installation

```bash
cd k9-svc/k9-sign
cargo build --release
sudo cp target/release/k9-sign /usr/local/bin/
```

## Usage

### Generate a keypair

```bash
k9-sign keygen [name]
```

Default name is "primary". Keys stored in `~/.config/k9/keys/`.

### Sign a component

```bash
k9-sign sign component.k9.ncl [key-name]
```

Creates `component.k9.ncl.sig` and displays base64 signature for embedding.

### Verify a signature

```bash
k9-sign verify component.k9.ncl
```

Checks signature against all trusted keys.

### Trust a public key

```bash
k9-sign trust ~/.config/k9/keys/primary.pub
```

Copies key to `~/.config/k9/keys/trusted/`.

### List keys

```bash
k9-sign list
```

Shows your keys and trusted keys with SHA-256 fingerprints.

### Full Hunt authorization

```bash
k9-sign authorize component.k9.ncl
```

Complete authorization check: verifies Hunt level, signature presence, and signature validity.

## Example Workflow

```bash
# 1. Generate your keypair
k9-sign keygen my-key

# 2. Trust your own key (for testing)
k9-sign trust ~/.config/k9/keys/my-key.pub

# 3. Sign a Hunt component
k9-sign sign examples/deploy.k9.ncl my-key

# 4. Verify the signature
k9-sign verify examples/deploy.k9.ncl

# 5. Full authorization (checks everything)
k9-sign authorize examples/deploy.k9.ncl
```

## Cryptography

- **Algorithm:** Ed25519 (EdDSA on Curve25519)
- **Library:** `ed25519-dalek` (pure Rust, audited)
- **Key size:** 32 bytes (256 bits)
- **Signature size:** 64 bytes (512 bits)
- **Security level:** ~128 bits (equivalent to RSA-3072)

## Advantages of Ed25519

- **Fast:** 10x faster than RSA for signing, 20x faster for verification
- **Small:** Keys and signatures are tiny (32 and 64 bytes)
- **Secure:** Immune to timing attacks, no RNG required for signing
- **Simple:** No parameter choices (unlike RSA/ECDSA)
- **Proven:** Used by SSH, Signal, Tor, and many others

## Comparison to OpenSSL (used by sign.sh)

| Feature | OpenSSL (sign.sh) | ed25519-dalek (k9-sign) |
|---------|-------------------|-------------------------|
| **Memory safety** | C (unsafe) | Rust (safe) |
| **Dependencies** | External OpenSSL binary | Self-contained library |
| **Attack surface** | Full OpenSSL (>400k LOC) | Minimal (crypto only) |
| **Audit status** | OpenSSL audited | ed25519-dalek audited |
| **Performance** | Shell overhead + fork/exec | Pure Rust (no syscalls) |

## Migration from sign.sh

`k9-sign` is a drop-in replacement for `sign.sh`:

```bash
# Old (sign.sh)
./sign.sh keygen my-key
./sign.sh sign component.k9.ncl my-key
./sign.sh verify component.k9.ncl

# New (k9-sign)
k9-sign keygen my-key
k9-sign sign component.k9.ncl my-key
k9-sign verify component.k9.ncl
```

**Key format compatibility:**

❌ **NOT compatible** - k9-sign uses raw Ed25519 keys (32 bytes), while sign.sh uses OpenSSL PEM format.

**Migration path:**

1. Generate new keys with `k9-sign keygen`
2. Re-sign all components with new keys
3. Distribute new public keys to users
4. Remove old sign.sh and OpenSSL dependency

## Testing

```bash
# Unit tests
cargo test

# Integration test
cargo build --release
./target/release/k9-sign keygen test-key
./target/release/k9-sign trust ~/.config/k9/keys/test-key.pub
echo "Test data" > test.txt
./target/release/k9-sign sign test.txt test-key
./target/release/k9-sign verify test.txt
```

## License

AGPL-3.0-or-later

## Author

Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

## See Also

- [K9-SVC Security Best Practices](../docs/SECURITY-BEST-PRACTICES.adoc)
- [K9-SVC Security Roadmap](../docs/SECURITY-ROADMAP.adoc)
- [sign.sh (original shell implementation)](../sign.sh)
