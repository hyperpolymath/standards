# Minimal Rust Project

**Bronze Level Rhodium Compliance Example**

This is a minimal Rust library project that demonstrates Bronze-level compliance with the Rhodium Framework. It serves as a starting point for understanding the Rhodium standards and shows the minimum required artifacts for a compliant project.

## Overview

This project provides simple mathematical operations (addition and multiplication) and includes all Bronze-level compliance requirements:

- README.md (this file)
- LICENSE file
- CODE_OF_CONDUCT.md
- Basic test suite
- Version control (Git)

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
minimal-rust-project = "0.1.0"
```

## Usage

```rust
use minimal_rust_project::{add, multiply};

fn main() {
    let sum = add(2, 3);
    println!("2 + 3 = {}", sum);

    let product = multiply(4, 5);
    println!("4 * 5 = {}", product);
}
```

## Building

```bash
cargo build
```

## Testing

```bash
cargo test
```

## Rhodium Compliance

This project demonstrates **Bronze** level compliance. It was initialized using:

```bash
rhodium init --level bronze
```

### Bronze Level Requirements

- [x] README.md with project description
- [x] LICENSE file (dual MIT/Apache-2.0)
- [x] CODE_OF_CONDUCT.md
- [x] Basic test suite
- [x] Version control initialized

### Checking Compliance

To verify compliance:

```bash
rhodium check --level bronze
```

## Next Steps

To see how this project could be enhanced to Silver level, see the `examples/standard-library/` project which adds:

- CHANGELOG.md
- CONTRIBUTING.md
- CITATION.cff
- GitHub templates (issues, PRs)
- More comprehensive testing
- Documentation generation

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Code of Conduct

This project adheres to the Contributor Covenant Code of Conduct. See [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) for details.
