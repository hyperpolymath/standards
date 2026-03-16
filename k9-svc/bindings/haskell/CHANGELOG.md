# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.0] - 2026-03-16

### Added
- Initial release.
- `Data.K9.Types` — Core AST types (Component, Pedigree, SecurityLevel, SecurityPolicy, Target, Recipes, Validation, Contract).
- `Data.K9.Parser` — Parse `.k9` (YAML-like) files into the typed AST. Nickel `.k9.ncl` support planned.
- `Data.K9.Renderer` — Render the AST back to K9 surface syntax.
- `Data.K9` — Convenience re-export module.
