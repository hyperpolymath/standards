# RSR Examples

**Rhodium Standard Repository (RSR) - Reference Implementations**

This directory contains working examples of RSR-compliant projects at different compliance levels and for different use cases.

---

## 📊 Overview

The Rhodium Standard Repository defines three compliance levels based on the [COMPLIANCE_CHECKLIST.md](../COMPLIANCE_CHECKLIST.md):

| Level | Badge | Description | % Compliance | Example |
|-------|-------|-------------|--------------|---------|
| **Bronze** | ![Bronze](https://img.shields.io/badge/RSR-Bronze-cd7f32) | Basic RSR compliance | 75-89% | [rhodium-minimal](rhodium-minimal/) |
| **Silver** | ![Silver](https://img.shields.io/badge/RSR-Silver-c0c0c0) | Strong RSR compliance | 90-99% | *(planned)* |
| **Gold** | ![Gold](https://img.shields.io/badge/RSR-Gold-ffd700) | Full RSR compliance | 100% | *(planned)* |

---

## 📂 Available Examples

### [rhodium-minimal](./rhodium-minimal/) - Bronze Compliance (CLI Tool)

**Purpose**: Simplest possible RSR-compliant project

**Highlights**:
- ✅ ~100 lines of Rust code
- ✅ Zero dependencies (offline-first)
- ✅ Type safety + Memory safety
- ✅ Complete documentation
- ✅ Bronze-level compliance (75-89%)
- ✅ TPCF Perimeter 3 (Community Sandbox)

**Best for**: Learning RSR basics, CLI tools, minimal projects

**Quick start**:
```bash
cd rhodium-minimal
cargo run
# Or with just:
just run
```

**Compliance**:
- **Level**: Bronze (75-89%)
- **Category 1**: Nix ✅, Justfile ✅, GitLab CI ✅
- **Category 2**: All docs ✅, .well-known/ ✅
- **Category 3**: Type safety ✅, Memory safety ✅, SPDX ✅
- **Category 7**: Dual licensing ✅
- **Category 10**: TPCF ✅, CoC ✅

**Learn More**: [rhodium-minimal/README.md](rhodium-minimal/README.md)

---

## 🎯 Planned Examples

### rhodium-web (Silver Compliance)

**Purpose**: Web application with HTTP/3, QUIC, security headers

**Tech Stack**: ReScript frontend, CADRE router, Elixir backend, CRDTs

**Status**: Planned

---

### rhodium-verified (Gold Compliance)

**Purpose**: Safety-critical system with formal verification

**Tech Stack**: Ada + SPARK, Rust FFI, complete 10+ security dimensions

**Status**: Planned

---

### rhodium-distributed (Gold Compliance)

**Purpose**: Distributed system with offline-first, CRDTs, blockchain audit trail

**Tech Stack**: Elixir OTP, CRDTs, Deno KV, SaltRover

**Status**: Planned

---

## 📊 Compliance Level Comparison

| Example | Level | % | Type Safety | Memory Safety | Offline-First | CRDT | Formal Verification | Container | IndieWeb |
|---------|-------|---|-------------|---------------|---------------|------|---------------------|-----------|----------|
| **rhodium-minimal** | Bronze | 75-89% | ✅ (Rust) | ✅ (Rust) | ✅ | ❌ | ❌ | ❌ | ❌ |
| rhodium-web | Silver | 90-99% | ✅ (ReScript) | ✅ (Rust) | ✅ | ✅ | ❌ | ✅ | ✅ |
| rhodium-verified | Gold | 100% | ✅ (Ada) | ✅ (SPARK) | ✅ | ✅ | ✅ | ✅ | ❌ |
| rhodium-distributed | Gold | 100% | ✅ (Elixir) | ✅ (OTP) | ✅ | ✅ | ⚠️ | ✅ | ⚠️ |

**Legend**:
- ✅ Fully implemented
- ⚠️ Partially implemented
- ❌ Not applicable / not implemented

---

## 🚀 Using These Examples

### As Learning Resources

1. **Start with rhodium-minimal**: Understand RSR basics
2. **Study the structure**: See how files are organised
3. **Read the documentation**: Each example is fully documented
4. **Run the validation**: `just validate` in each directory
5. **Modify and experiment**: Everything is reversible!

### As Project Templates

Each example can serve as a starting point:

```bash
# Copy an example
cp -r examples/rhodium-minimal my-new-project
cd my-new-project

# Customise
$EDITOR README.md Cargo.toml src/main.rs

# Validate
just validate

# Commit
git init
git add .
git commit -m "feat: initial commit from rhodium-minimal template"
```

**Better approach**: Use `rhodium-init` tool (when available):

```bash
# Interactive TUI wizard
rhodium-init

# Or with flags
rhodium-init --name my-project --language rust --template minimal
```

---

## 📋 RSR Compliance Checklist

All examples demonstrate:

### ✅ Required Files
- README.md (project overview)
- LICENSE.txt (dual: MIT + Palimpsest v0.8)
- SECURITY.md (vulnerability reporting)
- CONTRIBUTING.md (TPCF framework)
- CODE_OF_CONDUCT.md
- MAINTAINERS.md
- CHANGELOG.md
- .well-known/security.txt (RFC 9116)
- .well-known/ai.txt (AI training policies)
- .well-known/humans.txt (attribution)

### ✅ Build System
- justfile (task runner)
- flake.nix (Nix reproducible builds)
- .gitlab-ci.yml (CI/CD)

### ✅ Code Quality
- SPDX headers on all source files
- Type safety (language-dependent)
- Memory safety (language-dependent)
- Offline-first capability

See [../COMPLIANCE_CHECKLIST.md](../COMPLIANCE_CHECKLIST.md) for complete criteria.

---

## 🛠️ Development Workflow

### For Each Example

```bash
# Enter example directory
cd rhodium-minimal

# Enter Nix development shell (recommended)
nix develop

# View available commands
just --list

# Build
just build

# Run
just run

# Test
just test

# Validate RSR compliance
just validate
```

### Common Commands

All examples support these `just` recipes:

- `just build` - Build the project
- `just run` - Run the application
- `just test` - Run tests
- `just validate` - Full RSR compliance check
- `just audit-licence` - Verify SPDX headers
- `just format` - Format code
- `just lint` - Run linter
- `just clean` - Clean build artifacts

---

## 📚 Additional Resources

### RSR Specifications

- [CLAUDE.md](../CLAUDE.md) - Complete RSR specification
- [COMPLIANCE_CHECKLIST.md](../COMPLIANCE_CHECKLIST.md) - 150+ checkpoints
- [CCCP-MANIFESTO.md](../CCCP-MANIFESTO.md) - Philosophical foundation

### Tool Designs

- [docs/rhodium-init-design.md](../docs/rhodium-init-design.md) - Ada TUI tool
- [docs/elm-gui-design.md](../docs/elm-gui-design.md) - Web-based GUI

### Templates

- [templates/](../templates/) - Generic project templates

---

## 🤝 Contributing Examples

Want to contribute a new example?

1. **Propose**: Open an issue describing your example
2. **Design**: Outline the architecture and compliance level
3. **Implement**: Create the example following RSR patterns
4. **Document**: Include all required documentation
5. **Validate**: Run `just validate` to ensure compliance
6. **Submit**: Create a Merge Request

**Requirements**:
- Must achieve minimum Bronze compliance (75%)
- Must include all required documentation files
- Must have working build system (justfile + flake.nix)
- Must have .well-known/ directory
- Must have SPDX headers on all source files

See [../CONTRIBUTING.md](../CONTRIBUTING.md) for details.

---

## 📞 Contact

- **Questions**: Open an issue
- **Security**: See .well-known/security.txt
- **Discussion**: GitLab discussions

---

*"Learning by example, excellence through practice."*

— The Rhodium Standard
