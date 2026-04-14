# Language Testing Standards

**Version:** 1.0.0
**Date:** 2024-04-14
**Status:** Active

This document establishes canonical testing standards for all programming languages used across our projects.

## Table of Contents

1. [Rust](#rust)
2. [Julia](#julia)
3. [Version Control](#version-control)

## Rust

### Core Tools

| Tool | Purpose | Integration | CI/CD Stage |
|------|---------|-------------|-------------|
| `rustfmt` | Code formatting | ✅ Integrated | Check |
| `clippy` | Linting | ✅ Integrated | Check |
| `cargo audit` | Security auditing | ✅ Integrated | Security |
| `cargo test` | Unit testing | ✅ Integrated | Test |
| `cargo bench` | Benchmarking | ✅ Integrated | Test |

### Current Implementation

**GitHub Actions:** `rust-ci.yml`
- Format checking: `cargo fmt --all -- --check`
- Clippy linting: `cargo clippy --all-targets --all-features -- -D warnings`
- Security audit: `cargo audit`
- Test coverage: `cargo tarpaulin`

**GitLab CI:** `.gitlab-ci.yml`
- Format checking: `cargo fmt --all -- --check`
- Clippy linting: `cargo clippy --all -- -D warnings`
- Security audit: `cargo audit`
- Additional security: `cargo-geiger`, `cargo-license`

### Best Practices

1. **Format on save**: Configure editors to run `rustfmt` on file save
2. **Warnings as errors**: Use `-D warnings` flag to treat warnings as errors
3. **Regular audits**: Run `cargo audit` weekly minimum
4. **Coverage targets**: Maintain >80% test coverage

## Julia

### Equivalent Tools

| Julia Tool | Rust Equivalent | Purpose | Integration Status |
|-----------|-----------------|---------|-------------------|
| `JuliaFormatter.jl` | `rustfmt` | Code formatting | ❌ Not yet integrated |
| `JET.jl` | `clippy` | Static analysis | ❌ Not yet integrated |
| `Aqua.jl` | `cargo audit` | Package security | ❌ Not yet integrated |
| `Pkg.test()` | `cargo test` | Unit testing | ✅ Integrated |
| `BenchmarkTools.jl` | `cargo bench` | Benchmarking | ❌ Not yet integrated |

### Recommended Julia CI/CD Integration

```yaml
# Julia GitHub Actions Example
name: Julia CI
on: [push, pull_request]

jobs:
  format:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: julia-actions/setup-julia@v2
      - name: Format check
        run: |
          julia --project=docs -e '
            using JuliaFormatter
            JuliaFormatter.format("."; verbose=true, overwrite=false)
          '
  
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: julia-actions/setup-julia@v2
      - name: Static analysis
        run: |
          julia --project=docs -e '
            using JET
            JET.test_package(path=".")
          '
  
  security:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: julia-actions/setup-julia@v2
      - name: Package security audit
        run: |
          julia --project=docs -e '
            using Aqua
            Aqua.test_all(deps=true)
          '
  
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: julia-actions/setup-julia@v2
      - name: Run tests
        run: julia --project=. -e 'using Pkg; Pkg.test()'
```

### Julia Best Practices

1. **Project.toml**: Always include proper dependency specification
2. **Test coverage**: Use `Coverage.jl` for coverage reports
3. **Documentation**: Use `Documenter.jl` for doc generation
4. **CI templates**: Use `julia-actions/setup-julia` GitHub action

## Version Control

### Git Standards

- **Commit messages**: Follow [Conventional Commits](https://www.conventionalcommits.org/)
- **Branch naming**: `feature/`, `fix/`, `docs/`, `refactor/` prefixes
- **Pull requests**: Require approval from 2 maintainers
- **Semantic versioning**: Follow [SemVer 2.0.0](https://semver.org/)

### Git Hooks

Recommended hooks for all repositories:

```bash
# pre-commit: Run formatters and linters
# pre-push: Run tests
# commit-msg: Validate commit message format
```

## Implementation Roadmap

### Phase 1: Documentation (✅ Complete)
- [x] Create canonical language standards document
- [x] Document current Rust implementation
- [x] Document recommended Julia implementation

### Phase 2: Julia Integration
- [ ] Add JuliaFormatter to JuliaPackage-Reuse-Audit.jl
- [ ] Add JET.jl static analysis
- [ ] Add Aqua.jl security checks
- [ ] Update CI/CD pipelines

### Phase 3: Monitoring
- [ ] Set up regular audit scheduling
- [ ] Create compliance dashboard
- [ ] Establish metrics tracking

## Maintenance

**Review cycle**: Quarterly
**Next review**: 2024-07-14
**Maintainers**: @hyperpolymath/core-team

## Changelog

**1.0.0 (2024-04-14)**: Initial release with Rust and Julia standards