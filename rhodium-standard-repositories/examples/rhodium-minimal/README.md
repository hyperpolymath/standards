# rhodium-minimal

**Minimal Rhodium Standard Repository (RSR) Compliant Example**

image:https://img.shields.io/badge/License-MPL_2.0-blue.svg[MPL-2.0-or-later,link="https://opensource.org/licenses/MPL-2.0"]
image:https://img.shields.io/badge/License-MPL_2.0-blue.svg[MPL-2.0-or-later,link="https://opensource.org/licenses/MPL-2.0"]
[![RSR Compliance: Bronze](https://img.shields.io/badge/RSR-Bronze-cd7f32.svg)](../../COMPLIANCE_CHECKLIST.md)

---

## 🎯 Purpose

This is the **simplest possible** Rhodium Standard Repository (RSR) compliant project. It demonstrates:

- ✅ **Type Safety**: Rust's compile-time guarantees
- ✅ **Memory Safety**: Ownership model, no garbage collection
- ✅ **Offline-First**: No network dependencies, works anywhere
- ✅ **Reversibility**: Git-based workflow, safe experimentation
- ✅ **TPCF Perimeter 3**: Community Sandbox (open contribution)
- ✅ **Dual Licensing**: MIT (permissive) + Palimpsest v0.8 (ethical AI)

---

## 📦 Installation

### Using Nix (Recommended)

```bash
# Enter development environment
nix develop

# Build the project
just build

# Run the application
just run
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

---

## 🚀 Usage

```bash
# Run the minimal example
rhodium-minimal

# Output:
# 🎖️  Rhodium Standard Repository (RSR) - Minimal Example
#
# ✅ Type Safety: Rust compile-time guarantees
# ✅ Memory Safety: Ownership model, no garbage collection
# ✅ Offline-First: No network calls, works anywhere
# ✅ Reversibility: Every operation can be undone
# ✅ TPCF: Community Sandbox (Perimeter 3)
```

---

## 📋 RSR Compliance

This minimal example demonstrates **Bronze-level RSR compliance**:

### ✅ Category 1: Foundational Infrastructure
- Nix flake (`flake.nix`)
- Justfile with comprehensive recipes
- GitLab CI/CD (`.gitlab-ci.yml`)

### ✅ Category 2: Documentation Standards
- README.md (this file)
- LICENSE.txt (dual: MIT + Palimpsest)
- SECURITY.md (vulnerability reporting)
- CONTRIBUTING.md (TPCF framework)
- CODE_OF_CONDUCT.md
- `.well-known/` directory

### ✅ Category 3: Security Architecture
- **Type Safety**: Rust compile-time verification
- **Memory Safety**: Ownership model, no unsafe code
- **SPDX Headers**: On every source file
- **Supply Chain**: No dependencies (minimal attack surface)

### ✅ Category 7: FOSS & Licensing
- Dual licensing: MIT + Palimpsest v0.8
- SPDX headers on all files
- Clear attribution in MAINTAINERS.md

### ✅ Category 10: Community & Governance
- TPCF Perimeter 3 (Community Sandbox)
- Clear contribution guidelines
- Code of Conduct

---

## 🔍 What's Missing for Silver/Gold?

This minimal example achieves **Bronze** (75-89% compliance). To reach **Silver** (90-99%) or **Gold** (100%), you would need:

**For Silver:**
- Container configuration (Podman + Chainguard Wolfi)
- Security headers configuration
- Internationalization (i18n)
- Accessibility testing

**For Gold:**
- CRDT distributed state management
- Formal verification (SPARK proofs)
- Complete 10+ security dimensions
- IndieWeb integration

See [COMPLIANCE_CHECKLIST.md](../../COMPLIANCE_CHECKLIST.md) for the full 150+ checkpoint list.

---

## 🛠️ Development

### Prerequisites

- **Nix** with flakes enabled (recommended)
- **OR** Rust 1.75+ and Cargo

### Quick Start

```bash
# Clone the parent repository
git clone https://gitlab.com/hyperpolymath/rhodium-standard-repositories.git
cd rhodium-standard-repositories/examples/rhodium-minimal

# Enter Nix development shell
nix develop

# View available tasks
just --list

# Run full validation
just validate

# Build and run
just build
just run
```

### Available Commands

See `justfile` for all available commands:

```bash
just --list
```

Key commands:
- `just build` - Build the project
- `just run` - Run the application
- `just test` - Run tests
- `just validate` - Run RSR compliance checks
- `just audit-licence` - Verify SPDX headers
- `just clean` - Clean build artifacts

---

## 📝 License

This project is dual-licensed under:

- **MIT License** – For permissive open-source use
- **Palimpsest License v0.8** – For ethical AI training with attribution preservation

See [LICENSE.txt](LICENSE.txt) for full details.

### AI Training Notice

If using this code for AI training, you **MUST** comply with the Palimpsest License:
- Attribution preservation through derivative works
- Human authorship traceability
- Reciprocal transparency for AI systems

See `.well-known/ai.txt` for complete policies.

---

## 🤝 Contributing

This is a **Perimeter 3 (Community Sandbox)** project – **open to all contributors**.

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'feat: add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Merge Request

See [CONTRIBUTING.md](CONTRIBUTING.md) for complete guidelines.

---

## 🔒 Security

**Vulnerability Reporting**: See [SECURITY.md](SECURITY.md)

**Security Contact**: `.well-known/security.txt` (RFC 9116 compliant)

**Response SLA**: 24-hour acknowledgement, 72-hour assessment

---

## 🌐 Resources

- **Parent Repository**: [rhodium-standard-repositories](../../)
- **CCCP Manifesto**: [CCCP-MANIFESTO.md](../../CCCP-MANIFESTO.md)
- **Compliance Checklist**: [COMPLIANCE_CHECKLIST.md](../../COMPLIANCE_CHECKLIST.md)
- **Full Specification**: [CLAUDE.md](../../CLAUDE.md)

---

## 👥 Maintainers

See [MAINTAINERS.md](MAINTAINERS.md)

---

## 📜 Changelog

See [CHANGELOG.md](CHANGELOG.md) for version history.

---

*"The simplest path to excellence – RSR compliance in 100 lines of Rust."*

— The Rhodium Standard
