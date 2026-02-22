# Enterprise Service

**Gold Level Rhodium Compliance Example**

[![Rhodium Compliance](https://img.shields.io/badge/rhodium-gold-FFD700)](https://github.com/Hyperpolymath/rhodium-standard-repositories)
image:https://img.shields.io/badge/License-MPL_2.0-blue.svg[MPL-2.0-or-later,link="https://opensource.org/licenses/MPL-2.0"]

A production-ready REST API microservice demonstrating Gold-level compliance with the Rhodium Framework. This example showcases enterprise-grade architecture, comprehensive documentation, security best practices, and complete observability.

## Features

- **REST API**: User management endpoints
- **Type Safety**: Leverages Rust's type system
- **Async**: Built on Tokio runtime
- **Logging**: Structured logging with tracing
- **Error Handling**: Type-safe error responses
- **Testing**: Unit + integration tests
- **Benchmarks**: Performance tracking
- **Documentation**: Comprehensive ADRs and architecture docs
- **Security**: Input validation, audit logging ready
- **Gold-Level Compliance**: Complete Rhodium Framework compliance

## Quick Start

```bash
# Clone and navigate
cd examples/enterprise-service

# Run tests
cargo test

# Run the service
cargo run

# Run benchmarks
cargo bench
```

## API Endpoints

| Method | Endpoint              | Description           |
|--------|----------------------|-----------------------|
| GET    | `/health`            | Health check          |
| GET    | `/build-info`        | Build metadata        |
| GET    | `/api/v1/users`      | List all users        |
| POST   | `/api/v1/users`      | Create a new user     |
| GET    | `/api/v1/users/:id`  | Get user by ID        |
| DELETE | `/api/v1/users/:id`  | Delete user           |

## Rhodium Compliance

This project demonstrates **Gold** level compliance:

```bash
rhodium check --level gold
```

### Gold Level Requirements

All Bronze and Silver requirements plus:

- [x] ARCHITECTURE.md with system design
- [x] SECURITY.md with security policies
- [x] ADRs (Architecture Decision Records)
- [x] SBOM generation support
- [x] Build provenance tracking
- [x] Performance benchmarks
- [x] CI/CD workflows
- [x] >80% test coverage
- [x] Dependency scanning
- [x] Release automation

## Documentation

- [ARCHITECTURE.md](ARCHITECTURE.md) - System architecture and design
- [SECURITY.md](SECURITY.md) - Security policies and practices
- [docs/adr/](docs/adr/) - Architecture Decision Records
- [CONTRIBUTING.md](CONTRIBUTING.md) - Contribution guidelines
- [CHANGELOG.md](CHANGELOG.md) - Version history

## Testing

```bash
# Unit tests
cargo test

# With coverage
cargo tarpaulin --out Html

# Integration tests
cargo test --test '*'

# Benchmarks
cargo bench
```

## Security

See [SECURITY.md](SECURITY.md) for:
- Vulnerability reporting
- Security best practices
- Known limitations
- Security roadmap

## Next Steps

To see the ultimate Rhodium compliance level, see `examples/ai-ml-project/` which demonstrates:
- Multi-language support
- Advanced automation
- Full compliance automation
- AI/ML specific standards

## License

Licensed under either of:
- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))
- MIT license ([LICENSE-MIT](LICENSE-MIT))

## Citation

See [CITATION.cff](CITATION.cff) for citation information.
