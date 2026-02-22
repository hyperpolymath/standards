# Changelog

All notable changes to the rhodium-minimal project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [Unreleased]

### Planned
- Additional compliance features
- More comprehensive examples
- Enhanced documentation

---

## [0.1.0] - 2025-11-22

### Added - Initial Release

#### Core Functionality
- ✅ Rust-based minimal RSR-compliant application
- ✅ Type-safe implementation (Rust compile-time guarantees)
- ✅ Memory-safe implementation (ownership model)
- ✅ Offline-first design (zero network dependencies)
- ✅ Self-verification of RSR compliance

#### Documentation
- ✅ Comprehensive README.md
- ✅ SECURITY.md with vulnerability reporting
- ✅ CONTRIBUTING.md with TPCF framework
- ✅ CODE_OF_CONDUCT.md
- ✅ LICENSE.txt (dual: MIT + Palimpsest v0.8)
- ✅ MAINTAINERS.md
- ✅ This CHANGELOG.md

#### .well-known/ Directory
- ✅ security.txt (RFC 9116 compliant)
- ✅ ai.txt (AI training policies, Palimpsest License)
- ✅ humans.txt (attribution and credits)

#### Build System
- ✅ Justfile with comprehensive recipes (20+ commands)
- ✅ Nix flake for reproducible builds
- ✅ Cargo.toml with proper metadata

#### RSR Compliance
- ✅ SPDX headers on all source files
- ✅ Bronze-level compliance (75-89%)
- ✅ `just validate` command for compliance checking
- ✅ Zero unsafe code blocks

#### Testing
- ✅ Unit tests for core functionality
- ✅ Compliance verification tests
- ✅ Offline-first verification

---

## Versioning Strategy

This project follows **Semantic Versioning 2.0**:

- **MAJOR** version (X.0.0): Incompatible API changes
- **MINOR** version (0.X.0): New functionality, backwards-compatible
- **PATCH** version (0.0.X): Bug fixes, backwards-compatible

---

## Release Process

1. Update CHANGELOG.md with all changes
2. Update version in Cargo.toml
3. Run `just validate` to ensure compliance
4. Create git tag: `git tag -a v0.X.0 -m "Release v0.X.0"`
5. Build release: `just release`
6. Create GitLab release with artifacts

---

## Migration Guides

### From 0.0.x to 0.1.0
*(No previous versions - initial release)*

---

## Deprecation Warnings

*(None yet)*

When features are deprecated, they will be:
1. Marked deprecated in code (with replacement guidance)
2. Listed here with migration path
3. Removed in next MAJOR version

---

## Links

- **Repository**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories
- **Issues**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories/-/issues
- **Releases**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories/-/releases

---

[Unreleased]: https://gitlab.com/hyperpolymath/rhodium-standard-repositories/-/compare/v0.1.0...HEAD
[0.1.0]: https://gitlab.com/hyperpolymath/rhodium-standard-repositories/-/tree/v0.1.0
