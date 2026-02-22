# ADR 001: Use Axum Web Framework

## Status

Accepted

## Context

We need to choose a web framework for building our REST API service. The framework should be:
- Production-ready and stable
- High performance
- Type-safe
- Well-documented
- Compatible with Tokio async runtime
- Actively maintained

### Options Considered

1. **Axum** - Ergonomic web framework built on Tokio and Tower
2. **Actix-Web** - Mature, high-performance web framework
3. **Rocket** - Developer-friendly framework with macro-based routing
4. **Warp** - Filter-based web framework

## Decision

We will use **Axum** as our web framework.

## Rationale

### Axum Advantages

1. **Type Safety**: Leverages Rust's type system extensively
   - Compile-time route checking
   - Type-safe extractors
   - Strongly-typed error handling

2. **Performance**: Built on Tokio and Tower
   - Zero-cost abstractions
   - Efficient async I/O
   - Minimal overhead

3. **Ergonomics**: Clean, intuitive API
   - Handler functions are just async functions
   - Minimal boilerplate
   - Easy to understand and maintain

4. **Ecosystem**: Official Tokio project
   - First-class Tower middleware support
   - Good integration with tracing
   - Active development and community

5. **Documentation**: Well-documented with examples
   - Comprehensive guides
   - Active community support
   - Production use cases

### Comparison

| Feature         | Axum | Actix-Web | Rocket | Warp |
|----------------|------|-----------|--------|------|
| Type Safety    | ★★★★★ | ★★★★☆    | ★★★★★  | ★★★☆☆ |
| Performance    | ★★★★★ | ★★★★★    | ★★★★☆  | ★★★★☆ |
| Ergonomics     | ★★★★★ | ★★★★☆    | ★★★★★  | ★★☆☆☆ |
| Maturity       | ★★★★☆ | ★★★★★    | ★★★★☆  | ★★★☆☆ |
| Documentation  | ★★★★★ | ★★★★☆    | ★★★★☆  | ★★★☆☆ |

### Trade-offs

**Cons of Axum**:
- Relatively newer (less battle-tested than Actix-Web)
- Smaller ecosystem compared to Actix-Web
- Learning curve for Tower middleware

**Why Not Actix-Web**:
- More complex API
- Less type-safe extractors
- Historical concerns about actor model overhead (though largely addressed)

**Why Not Rocket**:
- Requires nightly Rust (for some features)
- Different async runtime (not Tokio)
- Less flexible middleware

**Why Not Warp**:
- Filter-based approach can be complex
- Steeper learning curve
- Less intuitive error handling

## Consequences

### Positive

- Clean, maintainable codebase
- Strong type safety catches errors at compile time
- Excellent performance characteristics
- Easy integration with Tokio ecosystem
- Future-proof with official Tokio backing

### Negative

- Team needs to learn Axum-specific patterns
- Smaller community compared to Actix-Web
- Some middleware may need Tower adapters

### Neutral

- Committed to Tokio ecosystem
- May need to implement some utilities ourselves
- Code patterns specific to Axum

## Implementation

Basic setup:

```rust
use axum::{
    routing::{get, post},
    Router,
};

let app = Router::new()
    .route("/api/v1/users", get(list_users).post(create_user))
    .layer(TraceLayer::new_for_http())
    .layer(CompressionLayer::new());
```

## References

- [Axum Documentation](https://docs.rs/axum/)
- [Tokio Project](https://tokio.rs/)
- [Tower Middleware](https://docs.rs/tower/)
- [Web Framework Benchmarks](https://www.techempower.com/benchmarks/)

## Notes

Decision made: 2025-11-01
Last updated: 2025-11-22
