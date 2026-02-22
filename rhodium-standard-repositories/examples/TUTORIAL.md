# Rhodium Framework Tutorial

A step-by-step guide to implementing Rhodium Framework compliance levels in your projects.

## Table of Contents

1. [Introduction](#introduction)
2. [Level 1: Bronze](#level-1-bronze---basic-hygiene)
3. [Level 2: Silver](#level-2-silver---production-ready)
4. [Level 3: Gold](#level-3-gold---enterprise-grade)
5. [Level 4: Rhodium](#level-4-rhodium---aspirational)
6. [Best Practices](#best-practices)

## Introduction

This tutorial walks you through building a project from scratch, progressively adding compliance features at each Rhodium level.

### Prerequisites

- Rust 1.70+ installed
- Basic knowledge of Rust and Cargo
- Git installed
- Text editor or IDE

### What You'll Build

We'll create a simple library and progressively enhance it to demonstrate each compliance level.

## Level 1: Bronze - Basic Hygiene

**Time**: 15-30 minutes

Bronze level establishes the foundational requirements for any project.

### Step 1: Create the Project

```bash
cargo new my-rhodium-project --lib
cd my-rhodium-project
```

### Step 2: Write Basic Code

Edit `src/lib.rs`:

```rust
/// Adds two numbers together.
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(2, 2), 4);
    }
}
```

### Step 3: Create README.md

```markdown
# My Rhodium Project

A simple library demonstrating Rhodium compliance.

## Installation

\`\`\`toml
[dependencies]
my-rhodium-project = "0.1.0"
\`\`\`

## Usage

\`\`\`rust
use my_rhodium_project::add;

let result = add(2, 3);
\`\`\`

## License

MIT OR Apache-2.0
```

### Step 4: Add LICENSE

Choose a license (e.g., MIT or Apache-2.0) and add LICENSE file.

### Step 5: Add CODE_OF_CONDUCT.md

Use the Contributor Covenant template (see `examples/minimal-rust-project/CODE_OF_CONDUCT.md`).

### Step 6: Create Rhodium Configuration

Create `.rhodium/config.toml`:

```toml
[project]
name = "my-rhodium-project"
version = "0.1.0"
compliance_level = "bronze"

[compliance.bronze]
readme = true
license = true
code_of_conduct = true
basic_tests = true
version_control = true
```

### Step 7: Verify Bronze Compliance

```bash
cargo test
rhodium check --level bronze
```

**Checkpoint**: You now have a Bronze-compliant project!

---

## Level 2: Silver - Production Ready

**Time**: 30-60 minutes

Silver level adds community standards and comprehensive documentation.

### Step 1: Create CHANGELOG.md

Use Keep a Changelog format:

```markdown
# Changelog

## [Unreleased]

## [0.1.0] - 2025-11-22
### Added
- Initial release
- Basic functionality
```

### Step 2: Create CONTRIBUTING.md

Document how others can contribute:

```markdown
# Contributing

## Development Setup

1. Clone the repository
2. Run `cargo build`
3. Run `cargo test`

## Submitting Changes

1. Fork the repo
2. Create a feature branch
3. Make your changes
4. Run tests
5. Submit a pull request
```

### Step 3: Create CITATION.cff

```yaml
cff-version: 1.2.0
message: "If you use this software, please cite it as below."
title: "My Rhodium Project"
version: 0.1.0
date-released: 2025-11-22
authors:
  - family-names: "Your Name"
    given-names: "Your Given Name"
license: "MIT OR Apache-2.0"
```

### Step 4: Add GitHub Templates

Create `.github/ISSUE_TEMPLATE/bug_report.md`:

```markdown
---
name: Bug report
about: Create a report to help us improve
---

## Bug Description
[Description]

## To Reproduce
1. Step 1
2. Step 2

## Expected Behavior
[What you expected]

## Actual Behavior
[What happened]
```

Create `.github/PULL_REQUEST_TEMPLATE.md`:

```markdown
## Description
[Describe your changes]

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change

## Checklist
- [ ] Tests pass
- [ ] Documentation updated
```

### Step 5: Improve Test Coverage

Add more tests to achieve >70% coverage:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_positive() {
        assert_eq!(add(2, 2), 4);
    }

    #[test]
    fn test_add_negative() {
        assert_eq!(add(-1, -1), -2);
    }

    #[test]
    fn test_add_zero() {
        assert_eq!(add(0, 0), 0);
    }
}
```

### Step 6: Generate Documentation

Add doc comments and generate docs:

```rust
//! # My Rhodium Project
//!
//! A simple library for mathematical operations.

/// Adds two numbers together.
///
/// # Examples
///
/// ```
/// use my_rhodium_project::add;
/// assert_eq!(add(2, 3), 5);
/// ```
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

Generate docs:

```bash
cargo doc --no-deps --open
```

### Step 7: Update Rhodium Configuration

Update `.rhodium/config.toml`:

```toml
[project]
compliance_level = "silver"

[compliance.silver]
changelog = true
contributing_guide = true
citation_file = true
issue_templates = true
pr_template = true
comprehensive_tests = true
documentation_generation = true
```

### Step 8: Verify Silver Compliance

```bash
cargo test
cargo doc
rhodium check --level silver
```

**Checkpoint**: You now have a Silver-compliant project!

---

## Level 3: Gold - Enterprise Grade

**Time**: 1-2 hours

Gold level adds enterprise practices and comprehensive governance.

### Step 1: Document Architecture

Create `ARCHITECTURE.md`:

```markdown
# Architecture

## Overview
[System overview]

## Components
[Component descriptions]

## Design Decisions
See docs/adr/
```

### Step 2: Create Architecture Decision Records

Create `docs/adr/001-first-decision.md`:

```markdown
# ADR 001: [Title]

## Status
Accepted

## Context
[What led to this decision]

## Decision
[What was decided]

## Consequences
[Impact of the decision]
```

### Step 3: Add Security Policy

Create `SECURITY.md`:

```markdown
# Security Policy

## Reporting Vulnerabilities

Email security@example.com

## Supported Versions

| Version | Supported |
|---------|-----------|
| 0.1.x   | ✓         |
```

### Step 4: Add Performance Benchmarks

Create `benches/benchmarks.rs`:

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use my_rhodium_project::add;

fn bench_add(c: &mut Criterion) {
    c.bench_function("add", |b| {
        b.iter(|| add(black_box(2), black_box(3)))
    });
}

criterion_group!(benches, bench_add);
criterion_main!(benches);
```

Add to `Cargo.toml`:

```toml
[dev-dependencies]
criterion = "0.5"

[[bench]]
name = "benchmarks"
harness = false
```

### Step 5: Add CI/CD

Create `.github/workflows/ci.yml`:

```yaml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo test

  clippy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo clippy -- -D warnings
```

### Step 6: Add Security Scanning

Create `.github/workflows/security.yml`:

```yaml
name: Security

on:
  schedule:
    - cron: '0 0 * * *'

jobs:
  audit:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: cargo install cargo-audit
      - run: cargo audit
```

### Step 7: Update Rhodium Configuration

```toml
[compliance.gold]
architecture_documentation = true
security_policy = true
adr_records = true
performance_benchmarks = true
ci_cd_pipeline = true
code_coverage = true
```

### Step 8: Verify Gold Compliance

```bash
cargo test
cargo bench
rhodium check --level gold
```

**Checkpoint**: You now have a Gold-compliant project!

---

## Level 4: Rhodium - Aspirational

**Time**: 2-4 hours

Rhodium level demonstrates advanced features like multi-language support and ML governance.

### Key Additions for Rhodium

1. **Multi-Language Support**
   - Add Python bindings
   - Integrate with PyO3

2. **Advanced Automation**
   - Automated releases
   - Dependency updates
   - Compliance checking

3. **Complete Observability**
   - Metrics collection
   - Distributed tracing
   - Advanced logging

4. **ML-Specific Features** (if applicable)
   - Model cards
   - Data cards
   - Fairness assessment

For detailed implementation, see `examples/ai-ml-project/`.

---

## Best Practices

### Testing

1. **Write tests first**: TDD helps ensure compliance
2. **Aim for high coverage**: >80% for production code
3. **Test edge cases**: Not just happy paths
4. **Use integration tests**: Test real-world scenarios

### Documentation

1. **Keep README updated**: First thing users see
2. **Document decisions**: Use ADRs
3. **Example code**: Include working examples
4. **API docs**: Document all public APIs

### Automation

1. **CI/CD from day one**: Catch issues early
2. **Automate compliance**: Use Rhodium CLI
3. **Auto-format**: Use rustfmt
4. **Auto-lint**: Use clippy

### Security

1. **Regular audits**: Run cargo-audit
2. **Update dependencies**: Stay current
3. **Security policy**: Clear reporting process
4. **No secrets in code**: Use environment variables

## Progression Strategy

### Start Small

Begin with Bronze, then gradually add features:

1. **Week 1**: Bronze compliance
2. **Week 2-3**: Silver compliance
3. **Month 2**: Gold compliance
4. **Ongoing**: Maintain and improve

### Incremental Approach

Don't try to jump straight to Gold:
- Each level builds on the previous
- Learn best practices at each stage
- Adjust to your project's needs

### Team Adoption

1. **Training**: Teach team about Rhodium
2. **Templates**: Use examples as templates
3. **Reviews**: Check compliance in PRs
4. **Automation**: Integrate into CI/CD

## Common Pitfalls

1. **Skipping levels**: Don't jump ahead
2. **Documentation debt**: Keep docs updated
3. **Test neglect**: Maintain coverage
4. **Security ignorance**: Regular audits

## Next Steps

1. **Choose your starting level**: Typically Bronze
2. **Follow this tutorial**: Step by step
3. **Use examples**: Reference example projects
4. **Verify compliance**: Use Rhodium CLI
5. **Iterate**: Continuously improve

## Resources

- [Example Projects](README.md)
- [Rhodium CLI Documentation](https://github.com/Hyperpolymath/rhodium-cli)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)

## Getting Help

- Review example projects
- Check individual READMEs
- Open issues for questions

---

**Happy coding with Rhodium!**
