# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Complete rewrite from Python to ReScript for type safety
- RSR Gold compliance infrastructure
- Echidna integration for mathematical proofs
- OpenCyc integration for semantic grounding
- Nickel-based configuration system
- Podman containerization (replacing Docker)
- Comprehensive Justfile with 70+ recipes
- Nix flake for reproducible development
- Full test suite with Vitest
- AsciiDoc documentation

### Changed
- Migrated from Python to ReScript
- Replaced requirements.txt with package.json
- Switched from Apache 2.0 to MIT + Palimpsest dual license
- Documentation format from Markdown to AsciiDoc (except SECURITY.md)

### Removed
- All Python code and dependencies
- Docker support (replaced with Podman)
- Legacy crawler implementations

## [0.0.1] - 2018-XX-XX

### Added
- Initial Python implementation
- Support for bible.cloud, bible.com, pngscriptures.org
- ISO 639-3 language code support
- Basic corpus alignment functionality
- Apache 2.0 license

---

[Unreleased]: https://github.com/Hyperpolymath/1000Langs/compare/v0.0.1...HEAD
[0.0.1]: https://github.com/Hyperpolymath/1000Langs/releases/tag/v0.0.1
