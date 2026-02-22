# Palimpsest Validator

A functional programming-based validation suite for the Palimpsest License project, written in Haskell.

## Features

- **License Text Validation**: Validates Markdown structure, clause numbering, and format compliance
- **Metadata Schema Validation**: Validates JSON-LD and XML metadata files against expected schemas
- **Bilingual Consistency Checking**: Ensures Dutch ↔ English clause alignment and structural parity
- **Cross-Reference Validation**: Checks internal links, clause references, and file references
- **Type-Safe**: Leverages Haskell's strong type system for legal precision
- **Comprehensive Error Reporting**: Clear, actionable error messages with context

## Why Haskell?

Haskell was chosen for this validation suite because:

1. **Type Safety**: Strong static typing prevents validation logic errors
2. **Legal Precision**: Pure functions ensure predictable, reproducible validation
3. **Pattern Matching**: Natural fit for validating structured legal documents
4. **Excellent Parsing**: Rich ecosystem for parsing Markdown, JSON-LD, and XML
5. **Composability**: Easy to combine multiple validators
6. **Reliability**: Functional purity means no hidden side effects

## Requirements

- GHC 8.10+ or Stack
- Cabal 3.0+

## Installation

### Using the Installation Script

```bash
cd TOOLS/validation
./install.sh
```

This will detect your Haskell toolchain (Cabal or Stack) and install the validator.

### Manual Installation with Cabal

```bash
cd TOOLS/validation/haskell
cabal update
cabal build
cabal install --installdir=$HOME/.local/bin
```

### Manual Installation with Stack

```bash
cd TOOLS/validation/haskell
stack build
stack install
```

## Usage

### Validate Entire Project

```bash
palimpsest-validate project --root /path/to/palimpsest-license
```

### Validate a Single License File

```bash
palimpsest-validate license LICENSES/v0.4/palimpsest-v0.4.md
```

### Validate Metadata

```bash
palimpsest-validate metadata METADATA_v0.4/spdx_example.jsonld
```

### Validate Bilingual Consistency

```bash
palimpsest-validate bilingual \
  LICENSES/v0.3/palimpsest-license-v0.3.en.md \
  LICENSES/v0.3/palimpsest-license-v0.3.nl.md \
  docs/bilingual-map.md
```

### Validate Cross-References

```bash
palimpsest-validate references \
  GUIDES_v0.4/*.md \
  TOOLKIT_v0.4/*.md
```

## Command-Line Options

- `--root PATH` - Root path of the project (default: current directory)
- `--strict` - Strict mode: fail on warnings
- `--verbose` - Verbose output
- `--no-bilingual` - Skip bilingual consistency checks
- `--no-references` - Skip cross-reference validation

## Development

### Build

```bash
make build          # Build with Cabal
make build-stack    # Build with Stack
```

### Run Tests

```bash
make test
```

### Run Validation

```bash
make run            # Full project validation
make run-license    # License validation only
make run-metadata   # Metadata validation only
make run-bilingual  # Bilingual validation only
```

### Development Workflow

```bash
make dev            # Quick development build
make format         # Format Haskell code
make docs           # Generate Haddock documentation
```

## Project Structure

```
haskell/
├── app/
│   └── Main.hs                      # CLI application
├── src/
│   └── Palimpsest/
│       └── Validator/
│           ├── Types.hs             # Core types and monad
│           ├── Utils.hs             # Utility functions
│           ├── License.hs           # License format validator
│           ├── Metadata.hs          # Metadata schema validator
│           ├── Bilingual.hs         # Bilingual consistency checker
│           └── Reference.hs         # Cross-reference validator
├── test/                            # Test suite
├── palimpsest-validator.cabal       # Cabal configuration
├── stack.yaml                       # Stack configuration
└── README.md                        # This file
```

## Validation Rules

### License Text

- Must have proper header with title
- Version must be detectable
- Clauses must be numbered sequentially
- Required sections: Definitions, Attribution, Governing Law, Termination
- Must include metadata preservation clause (Clause 2.3)
- Must include NI systems consent clause (Clause 1.2)

### Metadata

- JSON-LD must have valid `@context` and `@type`
- SPDX license entries must have `licenseId` and `name`
- XML lineage tags must have `creator`, `workId`, and `timestamp`

### Bilingual Consistency

- English and Dutch versions must have same number of clauses
- Clause numbers must align between versions
- Clause titles must match bilingual mapping
- Versions must match between languages

### Cross-References

- File references must point to existing files
- Clause references must use valid format
- Internal links must point to existing anchors
- URLs should have valid format

## Exit Codes

- `0` - Validation passed
- `1` - Validation failed (errors found or strict mode with warnings)

## Licence

MIT Licence (same as Palimpsest tooling)

## Contributing

See the main project's `CONTRIBUTING.md` for guidelines.
