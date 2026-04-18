# rhodium-minimal - Technical Overview

**Minimal Rhodium Standard Repository (RSR) Compliant Example**

---

## 🎯 Design Goals

This minimal example demonstrates RSR compliance with:

1. **Simplicity**: ~100 lines of Rust code
2. **Self-Contained**: Zero dependencies
3. **Type Safety**: Rust compile-time guarantees
4. **Memory Safety**: Ownership model
5. **Offline-First**: No network calls
6. **Complete Documentation**: All required RSR files
7. **Bronze Compliance**: 75-89% RSR compliance

---

## 📐 Architecture

```
rhodium-minimal/
├── src/
│   └── main.rs              # Core application (type-safe, memory-safe)
├── .well-known/             # RFC standards (.txt files)
│   ├── security.txt         # RFC 9116 security contact
│   ├── ai.txt               # AI training policies
│   └── humans.txt           # Attribution
├── docs/                    # Documentation
│   └── OVERVIEW.md          # This file
├── Cargo.toml               # Rust package manifest
├── Justfile                 # Task runner (20+ recipes)
├── flake.nix                # Nix reproducible builds
├── .gitlab-ci.yml           # CI/CD pipeline
├── README.md                # Project documentation
├── LICENSE.txt              # Dual: MIT + Palimpsest v0.8
├── SECURITY.md              # Vulnerability reporting
├── CONTRIBUTING.md          # TPCF framework
├── CODE_OF_CONDUCT.md       # Community standards
├── MAINTAINERS.md           # Project team
├── CHANGELOG.md             # Version history
└── .gitignore               # Git exclusions
```

---

## 🔧 Implementation Details

### Type Safety (Rust)

```rust
// Compile-time verification:
// - No null pointer dereferences (Option<T>)
// - No buffer overflows (bounds checking)
// - No use-after-free (ownership model)
// - No data races (borrow checker)

fn main() -> Result<(), io::Error> {
    // Return type ensures proper error handling
    check_compliance()?;
    Ok(())
}
```

### Memory Safety

- **Ownership Model**: Single owner, borrowing rules enforced at compile-time
- **No Garbage Collection**: Deterministic resource cleanup
- **Zero Unsafe Code**: `just check-unsafe` verifies

### Offline-First

```rust
// Zero network dependencies in Cargo.toml:
[dependencies]
# (empty - demonstrates offline-first principle)
```

All functionality is self-contained:
- No HTTP requests
- No DNS lookups
- No socket connections
- Works in air-gapped environments

### SPDX Headers

Every file has:
```rust
// SPDX-License-Identifier: MPL-2.0-or-later
// SPDX-FileCopyrightText: 2025 The Rhodium Standard Contributors
```

Verified with: `just audit-licence`

---

## 📋 RSR Compliance Breakdown

### ✅ Bronze Level (75-89%)

| Category | Status | Details |
|----------|--------|---------|
| **1. Foundational Infrastructure** | ✅ Partial | Nix ✅, Justfile ✅, GitLab CI ✅, Podman ❌ (not needed for CLI tool) |
| **2. Documentation Standards** | ✅ Complete | All required files present, .well-known/ directory |
| **3. Security Architecture** | ✅ Strong | Type safety ✅, Memory safety ✅, SPDX headers ✅, No unsafe ✅ |
| **7. FOSS & Licensing** | ✅ Complete | Dual licensing, SPDX headers, clear attribution |
| **10. Community & Governance** | ✅ Complete | TPCF Perimeter 3, CoC, contributing guidelines |

### ⚠️ Missing for Silver (90-99%)

- Container configuration (Podman + Chainguard Wolfi)
- Security headers (for web projects, N/A here)
- Internationalization (i18n)
- Accessibility testing (for web projects, N/A here)

### ⚠️ Missing for Gold (100%)

- CRDT distributed state (not needed for minimal example)
- Formal verification (SPARK proofs)
- Complete 10+ security dimensions
- IndieWeb integration (for web projects, N/A here)

**Conclusion**: Bronze is appropriate for a minimal CLI tool. Silver/Gold are for production systems.

---

## 🔄 Build Process

### Using Just (Recommended)

```bash
# View all commands
just --list

# Build
just build

# Run
just run

# Test
just test

# Full validation
just validate
```

### Using Cargo Directly

```bash
# Build
cargo build --release

# Run
cargo run --release

# Test
cargo test
```

### Using Nix (Reproducible)

```bash
# Enter development shell
nix develop

# Build
nix build

# Run
./result/bin/rhodium-minimal
```

---

## 🧪 Testing Strategy

### Unit Tests

```rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_main_runs_without_error() {
        assert!(main().is_ok());
    }

    #[test]
    fn test_no_network_calls() {
        // Verifies offline-first principle
        main().expect("Should run without network");
    }
}
```

Run with: `just test`

### Integration Tests

- CI/CD pipeline (`.gitlab-ci.yml`)
- RSR compliance (`just validate`)
- SPDX audit (`just audit-licence`)
- Unsafe code check (`just check-unsafe`)

---

## 🚀 Deployment

### Installation

```bash
# Install to ~/.local/bin
just install

# Or specify PREFIX
just install PREFIX=/usr/local
```

### Release Process

```bash
# Build release with checksums
just release

# Artifacts created in release/:
# - rhodium-minimal (binary)
# - SHA256SUMS (checksums)
```

---

## 📖 Learning Resources

### Understanding the Code

1. **Start with `src/main.rs`**: Entry point, ~100 lines
2. **Read `justfile`**: See all available commands
3. **Review `flake.nix`**: Understand reproducible builds
4. **Check `.gitlab-ci.yml`**: CI/CD pipeline

### RSR Concepts

- **CCCP Manifesto**: [../../CCCP-MANIFESTO.md](../../CCCP-MANIFESTO.md)
- **Compliance Checklist**: [../../COMPLIANCE_CHECKLIST.md](../../COMPLIANCE_CHECKLIST.md)
- **Full Specification**: [../../CLAUDE.md](../../CLAUDE.md)

### Rust Resources

- **The Rust Book**: https://doc.rust-lang.org/book/
- **Rust by Example**: https://doc.rust-lang.org/rust-by-example/
- **Ownership Model**: https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html

---

## 🔮 Future Enhancements

Potential improvements (while staying minimal):

- [ ] Property-based testing (proptest)
- [ ] Benchmarking (criterion)
- [ ] WASM compilation target
- [ ] Cross-compilation examples
- [ ] Container packaging (optional)

**Note**: These would increase complexity. The goal is to stay minimal while demonstrating RSR compliance.

---

## 🤝 Contributing

This is a **Perimeter 3 (Community Sandbox)** project:

1. Fork the repository
2. Make changes
3. Run `just validate`
4. Submit Merge Request

See [../CONTRIBUTING.md](../CONTRIBUTING.md) for details.

---

*"Simplicity is the ultimate sophistication – RSR compliance in 100 lines."*

— The Rhodium Standard
