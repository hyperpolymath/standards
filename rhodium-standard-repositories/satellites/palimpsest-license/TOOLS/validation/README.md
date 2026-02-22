# Palimpsest License Validation Tools

This directory contains the validation toolkit for the Palimpsest License project.

## What's Here

### Haskell Validator (`haskell/`)

A comprehensive, functional programming-based validation suite for:

- **License text format validation** (Markdown structure, clause numbering)
- **Metadata schema validation** (JSON-LD, XML, Dublin Core)
- **Bilingual consistency checking** (Dutch ↔ English alignment)
- **Cross-reference validation** (clause references, file links, internal anchors)

**Why Haskell?**
- Strong type safety for legal precision
- Pure functional programming ensures reliable, reproducible validation
- Excellent parsing ecosystem (Megaparsec, Aeson, xml-conduit)
- Composable validators
- Compiles to standalone binaries

See `haskell/README.md` for detailed information.

### Installation Script (`install.sh`)

Automated installation script that:
- Detects your Haskell toolchain (GHC/Cabal or Stack)
- Builds the validator
- Installs `palimpsest-validate` executable
- Verifies PATH configuration

```bash
./install.sh
```

## Quick Start

### Install

```bash
cd TOOLS/validation
./install.sh
```

### Validate Project

```bash
palimpsest-validate project --root /path/to/palimpsest-license --verbose
```

### Validate Specific Files

```bash
# Validate a licence file
palimpsest-validate license LICENSES/v0.4/palimpsest-v0.4.md

# Validate metadata
palimpsest-validate metadata METADATA_v0.4/spdx_example.jsonld

# Check bilingual consistency
palimpsest-validate bilingual \
  LICENSES/v0.3/palimpsest-license-v0.3.en.md \
  LICENSES/v0.3/palimpsest-license-v0.3.nl.md \
  docs/bilingual-map.md
```

## Documentation

**Primary Documentation:** `/docs/validation-tools.md`

This comprehensive guide covers:
- Architecture and design decisions
- Installation and usage
- Validation rules and requirements
- Development workflow
- Integration with CI/CD
- Troubleshooting

**Haskell-Specific:** `haskell/README.md`

## Requirements

- **GHC** 8.10+ and **Cabal** 3.0+, OR
- **Stack** (alternative Haskell build tool)

### Installing Haskell

**GHCup** (recommended):
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

**Stack**:
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

## Features

### ✅ License Validation

- Document structure (headers, sections)
- Clause numbering (sequential, no gaps)
- Required sections (Definitions, Attribution, Governing Law, etc.)
- Common pitfalls (Clause 1.2, 2.3 compliance)
- Language detection (English/Dutch)

### ✅ Metadata Validation

- JSON-LD syntax and schema
- SPDX licence entries
- XML lineage tags
- Dublin Core metadata
- Palimpsest-specific requirements

### ✅ Bilingual Consistency

- Structural parity (clause count, headers)
- Clause alignment (matching numbers and titles)
- Version consistency
- Translation accuracy (via bilingual-map.md)

### ✅ Cross-Reference Validation

- Clause references (format and existence)
- File references (broken link detection)
- Internal anchors (heading-based links)
- URL format validation

## Development

### Build from Source

```bash
cd haskell
make build        # Build with Cabal
make build-stack  # Build with Stack
```

### Run Tests

```bash
cd haskell
make test
```

### Development Workflow

```bash
cd haskell

# Quick development build
make dev

# Run on project
make run

# Format code
make format

# Generate docs
make docs
```

## Project Structure

```
validation/
├── haskell/                         # Haskell validation suite
│   ├── app/                         # CLI application
│   ├── src/                         # Source modules
│   ├── test/                        # Test suite
│   ├── palimpsest-validator.cabal   # Cabal config
│   ├── stack.yaml                   # Stack config
│   ├── Makefile                     # Build automation
│   └── README.md                    # Haskell-specific docs
├── install.sh                       # Installation script
└── README.md                        # This file
```

## Why Not Python?

Initially, Python scaffolding was created (`pyproject.toml`, `requirements.txt`), but after evaluation, **Haskell was chosen** for:

1. **Type safety** - Compile-time error checking
2. **Reliability** - Pure functions, no side effects
3. **Performance** - Native compilation
4. **Legal precision** - Type system enforces correctness
5. **Maintainability** - Types document behaviour

The Python files have been archived to `ARCHIVE/python-validation-replaced-2024-11/`.

## Integration

### CI/CD (GitHub Actions)

```yaml
- name: Setup Haskell
  uses: haskell/actions/setup@v2
  with:
    ghc-version: '9.2'

- name: Install validator
  run: cd TOOLS/validation && ./install.sh

- name: Validate
  run: palimpsest-validate project --strict
```

### Pre-commit Hook

```bash
#!/bin/bash
palimpsest-validate project --strict
```

## Exit Codes

- `0` - Validation passed
- `1` - Validation failed

## Getting Help

1. Read `/docs/validation-tools.md`
2. Check `haskell/README.md`
3. Run `palimpsest-validate --help`
4. Open a GitHub issue

## Contributing

See `CONTRIBUTING.md` in the project root. When contributing to the validator:

1. Follow Haskell style guidelines
2. Add tests for new features
3. Update documentation
4. Ensure all tests pass

## Licence

MIT Licence (see `haskell/LICENSE`)

---

**Version:** 0.1.0
**Maintainer:** Palimpsest Stewardship Council
