# Contributing to rhodium-minimal

**Welcome to the Rhodium Standard Repository (RSR) minimal example!**

This project follows the **Tri-Perimeter Contribution Framework (TPCF)** – a graduated trust model that balances openness with architectural integrity.

---

## 🌱 TPCF: This is a Perimeter 3 (Community Sandbox) Project

**What does this mean?**

✅ **Open to ALL contributors** – No prior approval needed
✅ **Simple contribution process** – Fork, change, submit MR
✅ **Safe experimentation** – Reversibility built-in
✅ **Friendly review** – Helpful feedback, no gatekeeping

---

## 🚀 Quick Start

### 1. Fork and Clone

```bash
# Fork the repository on GitLab
# Then clone your fork:
git clone https://gitlab.com/YOUR_USERNAME/rhodium-standard-repositories.git
cd rhodium-standard-repositories/examples/rhodium-minimal
```

### 2. Set Up Development Environment

**Using Nix (Recommended):**
```bash
nix develop
```

**Using Cargo directly:**
```bash
# Requires Rust 1.75+
cargo build
```

### 3. Make Your Changes

```bash
# Create a feature branch
git checkout -b feature/amazing-contribution

# Make changes
$EDITOR src/main.rs

# Test your changes
just test
just validate

# Commit with conventional commit format
git commit -m "feat: add amazing feature"
```

### 4. Submit a Merge Request

```bash
# Push to your fork
git push origin feature/amazing-contribution

# Open a Merge Request on GitLab
# Our CI will run automated checks
```

---

## 📋 Contribution Guidelines

### Code Standards

1. **SPDX Headers**: Every file must have SPDX license identifier
   ```rust
   // SPDX-License-Identifier: MPL-2.0-or-later
   // SPDX-FileCopyrightText: 2025 The Rhodium Standard Contributors
   ```

2. **Formatting**: Use `cargo fmt`
   ```bash
   just format
   ```

3. **Linting**: Pass Clippy checks
   ```bash
   just lint
   ```

4. **Testing**: Add tests for new functionality
   ```rust
   #[test]
   fn test_my_feature() {
       assert!(my_feature_works());
   }
   ```

5. **Documentation**: Update README.md if needed

### Commit Message Format

We use **Conventional Commits**:

```
<type>: <description>

[optional body]

[optional footer]
```

**Types:**
- `feat:` New feature
- `fix:` Bug fix
- `docs:` Documentation changes
- `refactor:` Code refactoring (no behavior change)
- `test:` Adding or updating tests
- `security:` Security improvements
- `perf:` Performance optimizations
- `build:` Build system changes

**Examples:**
```bash
git commit -m "feat: add colour output support"
git commit -m "fix: correct off-by-one error in bounds check"
git commit -m "docs: clarify installation instructions"
```

---

## 🎯 What Can You Contribute?

### Easy (Good First Issues)

- 📝 **Documentation improvements**
- 🐛 **Bug fixes** (typos, small logic errors)
- ✨ **Code examples** in README
- 🧪 **Additional tests**

### Medium

- 🔧 **New features** (aligned with RSR principles)
- 🎨 **Output formatting** improvements
- 📊 **Performance optimizations**

### Advanced

- 🔒 **Security enhancements**
- 🧩 **Integration with other RSR tools**
- 📦 **Packaging** for different distributions

---

## ✅ Pre-Submission Checklist

Before submitting your MR, ensure:

- [ ] Code builds successfully (`just build`)
- [ ] All tests pass (`just test`)
- [ ] Linter passes (`just lint`)
- [ ] Code is formatted (`just format`)
- [ ] SPDX headers present (`just audit-licence`)
- [ ] RSR validation passes (`just validate`)
- [ ] Commit messages follow conventional format
- [ ] README updated if behavior changed

Quick check:
```bash
just validate
```

---

## 🔄 Review Process

### What to Expect

1. **Automated Checks**: CI runs within minutes
   - Build verification
   - Test execution
   - Linting and formatting
   - SPDX header validation

2. **Maintainer Review**: Usually within 1-3 days
   - Code quality assessment
   - RSR compliance check
   - Architectural fit evaluation

3. **Feedback**: Helpful, constructive comments
   - No hostile criticism
   - Suggestions for improvement
   - Appreciation for contribution

4. **Iteration**: Make requested changes
   - Push to same branch
   - CI re-runs automatically

5. **Merge**: Once approved
   - Squash or rebase as appropriate
   - Thank you message!

### Reversibility Guarantee

**Don't worry about making mistakes!** Everything is reversible:

- Git history preserves all changes
- Maintainers can revert if needed
- No shame in learning by doing

---

## 🤝 Code of Conduct

This project adheres to our [Code of Conduct](CODE_OF_CONDUCT.md).

**TL;DR:**
- ✅ Be respectful and inclusive
- ✅ Welcome diverse perspectives
- ✅ Focus on technical merit
- ✅ Assume good intentions
- ❌ No harassment, discrimination, or hostility

---

## 📚 Learning Resources

### Rust

- **The Rust Book**: https://doc.rust-lang.org/book/
- **Rust by Example**: https://doc.rust-lang.org/rust-by-example/
- **Clippy Lints**: https://rust-lang.github.io/rust-clippy/

### RSR Principles

- **CCCP Manifesto**: [../../CCCP-MANIFESTO.md](../../CCCP-MANIFESTO.md)
- **Compliance Checklist**: [../../COMPLIANCE_CHECKLIST.md](../../COMPLIANCE_CHECKLIST.md)
- **Full Specification**: [../../CLAUDE.md](../../CLAUDE.md)

### TPCF Framework

```
┌─────────────────────────────────────────────────────────┐
│ Perimeter 3: Community Sandbox (THIS PROJECT)          │
│ - Open to all contributors                             │
│ - Languages: Rust, Markdown, Shell                     │
│ - Scope: Features, docs, tests                         │
│ - Process: Fork → MR → Review → Merge                  │
└─────────────────────────────────────────────────────────┘
           ▲
           │ Graduation pathway (build trust over time)
           │
┌─────────────────────────────────────────────────────────┐
│ Perimeter 2: Expert Extensions                         │
│ - Trusted contributors                                  │
│ - Languages: Rust, Nickel, Bash                        │
│ - Scope: Protocol extensions, build system             │
│ - Process: Apply via issue → Review → Approved         │
└─────────────────────────────────────────────────────────┘
           ▲
           │ Rare graduation (deep architectural work)
           │
┌─────────────────────────────────────────────────────────┐
│ Perimeter 1: Core Systems                              │
│ - Maintainers only                                      │
│ - Languages: Rust, Nickel, Bash, C++                   │
│ - Scope: Critical infrastructure                       │
│ - Process: Direct commits                              │
└─────────────────────────────────────────────────────────┘
```

---

## 💬 Getting Help

### Questions?

- **Technical**: Open a discussion in GitLab
- **Security**: See [SECURITY.md](SECURITY.md)
- **General**: Create an issue with `question` label

### Stuck?

- Check existing issues and MRs
- Read the documentation in `docs/`
- Ask in the community chat (if available)

---

## 🏆 Recognition

All contributors are:

- Added to `MAINTAINERS.md` (with permission)
- Mentioned in release notes
- Part of the RSR community
- Building something meaningful together

---

## 📜 License

By contributing, you agree that your contributions will be licensed under:

- **MIT License** (permissive open source)
- **Palimpsest License v0.8** (ethical AI training)

See [LICENSE.txt](LICENSE.txt) for details.

---

*"Community over ego, collaboration over gatekeeping."*

— The Rhodium Standard, TPCF Principle
