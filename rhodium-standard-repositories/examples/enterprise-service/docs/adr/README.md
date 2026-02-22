# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records (ADRs) for the Enterprise Service project.

## What are ADRs?

Architecture Decision Records (ADRs) are documents that capture important architectural decisions made during the project, along with their context and consequences.

## Format

Each ADR follows this structure:

- **Title**: Numbered sequentially (e.g., "001-use-axum-web-framework")
- **Status**: Proposed, Accepted, Deprecated, Superseded
- **Context**: The issue or situation driving the decision
- **Decision**: The chosen solution
- **Rationale**: Why this decision was made
- **Consequences**: The positive, negative, and neutral impacts
- **Alternatives**: Other options considered and why they were rejected

## Index

| Number | Title | Status | Date |
|--------|-------|--------|------|
| [001](001-use-axum-web-framework.md) | Use Axum Web Framework | Accepted | 2025-11-01 |
| [002](002-in-memory-state-management.md) | In-Memory State Management | Accepted (Temporary) | 2025-11-01 |
| [003](003-security-roadmap.md) | Security Roadmap | Proposed | 2025-11-22 |

## Creating a New ADR

When making a significant architectural decision:

1. Copy the ADR template (if available) or follow the format above
2. Number it sequentially
3. Fill in all sections
4. Get it reviewed
5. Update this index
6. Commit to version control

## References

- [ADR GitHub Organization](https://adr.github.io/)
- [Documenting Architecture Decisions](https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions)
- [ADR Tools](https://github.com/npryce/adr-tools)
