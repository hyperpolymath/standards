# Contributing to php-aegis

Thank you for your interest in contributing to php-aegis!

## Getting Started

### Prerequisites

- PHP 8.1 or higher
- Composer
- Git

### Development Setup

```bash
# Clone the repository
git clone https://github.com/hyperpolymath/php-aegis.git
cd php-aegis

# Install dependencies
composer install

# Run tests to verify setup
vendor/bin/phpunit
```

## How to Contribute

### Reporting Bugs

1. Check [existing issues](https://github.com/hyperpolymath/php-aegis/issues) to avoid duplicates
2. Create a new issue with:
   - Clear, descriptive title
   - Steps to reproduce
   - Expected vs actual behavior
   - PHP version and environment details

### Suggesting Features

1. Open a [new issue](https://github.com/hyperpolymath/php-aegis/issues/new) with the `enhancement` label
2. Describe the use case and security benefit
3. Include example API if proposing new methods

### Submitting Code

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/your-feature`
3. Write tests for new functionality
4. Ensure all tests pass: `vendor/bin/phpunit`
5. Run static analysis: `vendor/bin/phpstan analyse src`
6. Submit a pull request

## Code Standards

### PHP Standards

This project follows [PSR-12](https://www.php-fig.org/psr/psr-12/) coding standards.

```bash
# Check formatting
vendor/bin/php-cs-fixer fix --dry-run

# Auto-fix formatting
vendor/bin/php-cs-fixer fix
```

### Documentation

- All public methods must have PHPDoc comments
- Include `@param` and `@return` annotations
- Document security considerations where relevant

### Testing

- New features require tests
- Security-related code requires comprehensive test coverage
- Use meaningful test method names: `test_validator_rejects_invalid_email()`

## Security Contributions

Given the security-focused nature of this project:

- **Do not** submit PRs that fix security vulnerabilities publicly
- Instead, follow the process in [SECURITY.md](SECURITY.md)
- Security enhancements (new features) can be submitted normally

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
