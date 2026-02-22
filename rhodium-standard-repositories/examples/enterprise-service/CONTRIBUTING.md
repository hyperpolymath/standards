# Contributing to Standard Library

First off, thank you for considering contributing to Standard Library! It's people like you that make this project such a great tool.

## Code of Conduct

This project and everyone participating in it is governed by our [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to the project maintainers.

## How Can I Contribute?

### Reporting Bugs

Before creating bug reports, please check the existing issues to avoid duplicates. When you create a bug report, include as many details as possible using our bug report template.

**Bug reports should include:**

- A clear and descriptive title
- Steps to reproduce the problem
- Expected behavior vs. actual behavior
- Your environment (OS, Rust version, etc.)
- Any relevant code samples or error messages

### Suggesting Enhancements

Enhancement suggestions are tracked as GitHub issues. When creating an enhancement suggestion, please include:

- A clear and descriptive title
- A detailed description of the proposed feature
- Explain why this enhancement would be useful
- List any similar features in other projects, if applicable

### Pull Requests

1. Fork the repo and create your branch from `main`
2. If you've added code that should be tested, add tests
3. If you've changed APIs, update the documentation
4. Ensure the test suite passes
5. Make sure your code follows the existing style
6. Write a clear commit message

## Development Setup

### Prerequisites

- Rust 1.70 or later
- Cargo
- Git

### Setting Up Your Environment

```bash
# Clone your fork
git clone https://github.com/YOUR-USERNAME/rhodium-standard-repositories.git
cd rhodium-standard-repositories/examples/standard-library

# Build the project
cargo build

# Run tests
cargo test

# Run linter
cargo clippy -- -D warnings

# Check formatting
cargo fmt -- --check
```

## Coding Standards

### Rust Style Guidelines

- Follow the [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)
- Use `cargo fmt` to format your code
- Use `cargo clippy` and address all warnings
- Write documentation for public APIs
- Include examples in doc comments

### Documentation

- All public functions must have documentation
- Include at least one example in doc comments
- Keep documentation up-to-date with code changes
- Use clear, concise language

### Testing

- Write unit tests for new functionality
- Maintain or improve test coverage (target: >70%)
- Include both positive and negative test cases
- Test edge cases and error conditions

Example test structure:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_feature_name() {
        // Arrange
        let mut stack = Stack::new();

        // Act
        stack.push(42);

        // Assert
        assert_eq!(stack.pop(), Some(42));
    }
}
```

### Commit Messages

Follow the [Conventional Commits](https://www.conventionalcommits.org/) specification:

```
type(scope): subject

body

footer
```

Types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `test`: Adding or updating tests
- `refactor`: Code refactoring
- `perf`: Performance improvements
- `chore`: Maintenance tasks

Example:

```
feat(queue): add peek_back method

Add a method to view the last item in the queue without removing it.
This is useful for inspecting the queue state.

Closes #123
```

## Pull Request Process

1. **Update documentation**: Ensure README, CHANGELOG, and API docs are updated
2. **Add tests**: New features must include tests
3. **Run the full test suite**: `cargo test --all-features`
4. **Check formatting**: `cargo fmt -- --check`
5. **Run linter**: `cargo clippy -- -D warnings`
6. **Update CHANGELOG.md**: Add your changes under "Unreleased"
7. **Create PR**: Use the PR template and fill in all sections

### PR Review Process

- At least one maintainer must review and approve
- All CI checks must pass
- Address all review comments
- Maintain a clean commit history (squash if requested)

## Testing Guidelines

### Running Tests

```bash
# Run all tests
cargo test

# Run specific test
cargo test test_name

# Run with all features
cargo test --all-features

# Run with coverage
cargo tarpaulin --out Html
```

### Test Coverage

- Maintain >70% code coverage
- Focus on critical paths and edge cases
- Don't write tests just to meet coverage targets
- Quality over quantity

## Documentation Guidelines

### API Documentation

```rust
/// Brief description of what the function does.
///
/// More detailed explanation if needed.
///
/// # Arguments
///
/// * `arg1` - Description of arg1
/// * `arg2` - Description of arg2
///
/// # Returns
///
/// Description of return value
///
/// # Examples
///
/// ```
/// use standard_library::Stack;
///
/// let mut stack = Stack::new();
/// stack.push(42);
/// ```
///
/// # Panics
///
/// Describe any panic conditions
///
/// # Errors
///
/// Describe any error conditions (for Result returns)
pub fn example_function(arg1: i32, arg2: i32) -> i32 {
    arg1 + arg2
}
```

### Building Documentation

```bash
# Generate docs
cargo doc --no-deps

# Generate and open docs
cargo doc --no-deps --open

# Generate with all features
cargo doc --all-features --no-deps
```

## Release Process

(For maintainers)

1. Update version in `Cargo.toml`
2. Update `CHANGELOG.md`
3. Update version references in README
4. Create release commit: `chore: release v0.x.0`
5. Tag the release: `git tag v0.x.0`
6. Push: `git push origin main --tags`
7. Publish to crates.io: `cargo publish`

## Getting Help

- Check existing [issues](https://github.com/Hyperpolymath/rhodium-standard-repositories/issues)
- Ask questions in [discussions](https://github.com/Hyperpolymath/rhodium-standard-repositories/discussions)
- Read the [documentation](https://docs.rs/standard-library)

## License

By contributing, you agree that your contributions will be licensed under the same MIT/Apache-2.0 dual license as the project.

## Recognition

Contributors will be recognized in:
- CHANGELOG.md for specific contributions
- GitHub contributors page
- Project documentation

Thank you for contributing!
