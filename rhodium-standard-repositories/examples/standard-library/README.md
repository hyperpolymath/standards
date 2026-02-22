# Standard Library

**Silver Level Rhodium Compliance Example**

[![Rhodium Compliance](https://img.shields.io/badge/rhodium-silver-C0C0C0)](https://github.com/Hyperpolymath/rhodium-standard-repositories)
image:https://img.shields.io/badge/License-MPL_2.0-blue.svg[MPL-2.0-or-later,link="https://opensource.org/licenses/MPL-2.0"]

A collection of useful data structures demonstrating Silver-level compliance with the Rhodium Framework. This project builds upon the Bronze-level example and shows a more production-ready library structure.

## Overview

This library provides fundamental data structures:

- **Stack**: LIFO (Last-In-First-Out) data structure
- **Queue**: FIFO (First-In-First-Out) data structure
- Optional serialization support via Serde

## Features

- Well-documented API with examples
- Comprehensive test coverage (>70%)
- Optional serialization support
- Examples demonstrating usage
- Full Silver-level Rhodium compliance

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
standard-library = "0.2.1"

# Enable serialization support
standard-library = { version = "0.2.1", features = ["serialization"] }
```

## Usage

### Stack Example

```rust
use standard_library::Stack;

let mut stack = Stack::new();
stack.push(1);
stack.push(2);
stack.push(3);

assert_eq!(stack.pop(), Some(3));
assert_eq!(stack.pop(), Some(2));
assert_eq!(stack.pop(), Some(1));
```

### Queue Example

```rust
use standard_library::Queue;

let mut queue = Queue::new();
queue.enqueue("first");
queue.enqueue("second");
queue.enqueue("third");

assert_eq!(queue.dequeue(), Some("first"));
assert_eq!(queue.dequeue(), Some("second"));
```

### Running Examples

```bash
cargo run --example basic_usage
```

## Building

```bash
# Build the library
cargo build

# Build with all features
cargo build --all-features

# Build examples
cargo build --examples
```

## Testing

```bash
# Run all tests
cargo test

# Run tests with all features
cargo test --all-features

# Run with coverage (requires cargo-tarpaulin)
cargo tarpaulin --out Html
```

## Documentation

Generate and view the documentation:

```bash
cargo doc --no-deps --open
```

## Rhodium Compliance

This project demonstrates **Silver** level compliance. It was provisioned using:

```bash
rhodium provision --level silver
```

### Silver Level Requirements

All Bronze requirements plus:

- [x] CHANGELOG.md following Keep a Changelog format
- [x] CONTRIBUTING.md with contribution guidelines
- [x] CITATION.cff for academic citation
- [x] GitHub issue templates
- [x] Pull request template
- [x] Comprehensive test coverage (>70%)
- [x] API documentation generation
- [x] Semantic versioning

### Checking Compliance

To verify compliance:

```bash
rhodium check --level silver
```

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details on:

- Code of conduct
- Development setup
- Submitting pull requests
- Coding standards
- Testing requirements

## Changelog

See [CHANGELOG.md](CHANGELOG.md) for a detailed history of changes.

## Citation

If you use this library in academic work, please cite it using the information in [CITATION.cff](CITATION.cff).

## Next Steps

To see how this project could be enhanced to Gold level, see the `examples/enterprise-service/` project which adds:

- ARCHITECTURE.md documenting system design
- SECURITY.md with security policies
- ADRs (Architecture Decision Records)
- SBOM (Software Bill of Materials)
- Provenance tracking
- Performance benchmarks
- Multi-platform CI/CD

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Code of Conduct

This project adheres to the Contributor Covenant Code of Conduct. See [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) for details.
