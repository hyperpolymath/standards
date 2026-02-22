# Changelog

All notable changes to STATE will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.0.0] - 2025-12-06

### Added

- **Modular architecture**: Split into separate modules
  - `state-core`: Core accessors and predicates
  - `state-kanren`: minikanren integration with fallback
  - `state-graph`: GraphViz DOT and Mermaid visualization
  - `state-history`: Time tracking and velocity estimation
  - `state`: Main entry point re-exporting all modules

- **History tracking**: New `(history ...)` section in STATE.scm
  - Completion snapshots over time
  - Velocity calculation (% per day)
  - Estimated completion dates
  - Burndown chart data

- **Visualization**: Generate dependency graphs
  - GraphViz DOT output
  - Mermaid diagram output
  - Critical path highlighting
  - Status-based grouping

- **minikanren queries**: Relational logic programming
  - `projecto`, `statuso`, `dependso`, `blockso`, `categoryo`
  - Fallback implementation when minikanren not available
  - Full unification support

- **RSR compliance**: Rhodium Standard Repositories files
  - LICENSE.txt (MIT + Palimpsest v0.8)
  - SECURITY.md
  - CODE_OF_CONDUCT.adoc
  - CONTRIBUTING.adoc
  - GOVERNANCE.adoc
  - flake.nix for Nix reproducibility
  - justfile for task automation
  - Containerfile for Podman/Docker

### Changed

- STATE.scm format version bumped to 2.0
- Moved all functions from STATE.scm to lib/ modules
- STATE.scm now contains only data and quick reference

### Deprecated

- Inline function definitions in STATE.scm (use lib/ modules instead)

## [1.0.0] - 2025-12-06

### Added

- Initial implementation of STATE checkpoint system
- Basic STATE.scm structure with:
  - `metadata`: Format version and timestamps
  - `user`: Name, roles, preferences
  - `session`: Conversation tracking
  - `focus`: Current project and phase
  - `projects`: Full project catalog
  - `critical-next`: Priority actions
- Accessor functions for extracting state data
- Query helpers for filtering projects
- Documentation in djot format (later converted to AsciiDoc)

[Unreleased]: https://github.com/Hyperpolymath/STATE.scm/compare/v2.0.0...HEAD
[2.0.0]: https://github.com/Hyperpolymath/STATE.scm/compare/v1.0.0...v2.0.0
[1.0.0]: https://github.com/Hyperpolymath/STATE.scm/releases/tag/v1.0.0
