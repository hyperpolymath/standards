# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.0] - 2026-03-16

### Added
- Initial release.
- `Data.A2ML.Types` — Core AST types (Document, Block, Inline, Directive, Attestation, TrustLevel, Manifest).
- `Data.A2ML.Parser` — Parse `.a2ml` files into the typed AST.
- `Data.A2ML.Renderer` — Render the AST back to A2ML surface syntax.
- `Data.A2ML` — Convenience re-export module.
