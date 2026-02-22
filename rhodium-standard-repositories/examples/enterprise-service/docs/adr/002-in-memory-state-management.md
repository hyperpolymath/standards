# ADR 002: In-Memory State Management

## Status

Accepted (Temporary)

## Context

For the initial implementation and demonstration purposes, we need a way to store and manage user data. We need to decide between using a persistent database or in-memory storage.

### Requirements

- Simple setup for examples and demonstrations
- Easy to understand for new users
- No external dependencies for basic usage
- Thread-safe concurrent access
- Fast read/write operations

## Decision

We will use **in-memory storage with RwLock** for the initial implementation.

## Rationale

### Why In-Memory Storage

1. **Simplicity**: No database setup required
   - Zero configuration
   - No connection strings or credentials
   - Works out of the box

2. **Demonstration Focus**: Emphasizes architecture, not database operations
   - Clear code examples
   - Easy to understand
   - Focus on API design

3. **Performance**: Fastest possible operations
   - No network latency
   - No serialization overhead
   - Ideal for benchmarking

4. **Development**: Easier development and testing
   - No database migrations
   - Deterministic state
   - Fast test execution

### Why RwLock

- **Concurrent Reads**: Multiple readers simultaneously
- **Write Safety**: Exclusive write access
- **Standard Library**: No external dependencies
- **Simple API**: Easy to understand and use

## Trade-offs

### Advantages

- ✅ Zero setup time
- ✅ Maximum performance
- ✅ Easy to understand
- ✅ No external dependencies
- ✅ Perfect for examples

### Disadvantages

- ❌ Data lost on restart
- ❌ No persistence
- ❌ Limited scalability
- ❌ Single-instance only
- ❌ Not production-ready

## Consequences

### Immediate

- Example service runs without any setup
- Users can focus on API design and architecture
- Fast development and iteration
- Simple test setup

### Future

- Must be replaced with persistent storage for production
- Migration path needed to database
- Current API design should support database backend
- State abstraction should allow easy replacement

## Migration Path

Future database implementation should:

1. Replace `RwLock<HashMap>` with database connection pool
2. Implement repository pattern for data access
3. Add database migrations
4. Update configuration for connection strings
5. Maintain same public API

Example future structure:

```rust
// Current
pub struct AppState {
    pub users: RwLock<HashMap<String, User>>,
}

// Future
pub struct AppState {
    pub db_pool: PgPool,
}
```

## Implementation

Current implementation:

```rust
use std::collections::HashMap;
use std::sync::RwLock;

pub struct AppState {
    pub users: RwLock<HashMap<String, User>>,
}

impl AppState {
    pub fn new() -> Self {
        Self {
            users: RwLock::new(HashMap::new()),
        }
    }
}
```

Usage pattern:

```rust
// Read access
let users = state.users.read().unwrap();
users.get(&id)

// Write access
let mut users = state.users.write().unwrap();
users.insert(id, user);
```

## Alternatives Considered

### 1. SQLite

**Pros**:
- Persistent storage
- SQL interface
- File-based (no server)

**Cons**:
- Requires file system
- More complex setup
- Slower than in-memory

### 2. PostgreSQL/MySQL

**Pros**:
- Production-ready
- Full ACID compliance
- Scalable

**Cons**:
- Requires database server
- Complex setup for examples
- Not suitable for demonstrations

### 3. Redis

**Pros**:
- In-memory performance
- Persistent options
- Pub/sub capabilities

**Cons**:
- External dependency
- Additional service to run
- Overkill for simple examples

## References

- [Rust RwLock Documentation](https://doc.rust-lang.org/std/sync/struct.RwLock.html)
- [Repository Pattern](https://martinfowler.com/eaaCatalog/repository.html)
- [ADR 003: Future Database Integration](003-future-database-integration.md)

## Notes

**Important**: This is explicitly a temporary decision for demonstration purposes. Production deployments MUST use persistent storage.

See SECURITY.md for security implications.

Decision made: 2025-11-01
Last updated: 2025-11-22
Review by: 2026-01-01 (or when moving to production)
