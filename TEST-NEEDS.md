# TEST-NEEDS: standards

## Current State

| Category | Count | Details |
|----------|-------|---------|
| **Source modules** | 358+ | Massive monorepo: 0-ai-gatekeeper-protocol (mcp-repo-guardian, repo-guardian-fs), a2ml, axel-protocol, groove-protocol, contractiles, and many more sub-projects |
| **Unit tests** | ~5 | A handful of Zig integration_test.zig files (template placeholders) |
| **Integration tests** | 0 | No real integration tests |
| **E2E tests** | 0 | None |
| **Benchmarks** | 0 | None |
| **Fuzz tests** | 0 | placeholder.txt only |

## What's Missing

### P2P Tests (CRITICAL)
- [ ] No tests for mcp-repo-guardian manifest parsing and enforcement
- [ ] No tests for repo-guardian-fs FUSE operations
- [ ] No tests for a2ml validation actions

### E2E Tests (CRITICAL)
- [ ] No test for mcp-repo-guardian running against a real repo
- [ ] No test for repo-guardian-fs mounting and access control
- [ ] No test for axel-protocol negotiation
- [ ] No test for groove-protocol service discovery

### Aspect Tests
- [ ] **Security**: Security-focused standards (gatekeeper, guardian) with ZERO security tests
- [ ] **Performance**: No throughput tests for manifest parsing, FUSE overhead
- [ ] **Concurrency**: No concurrent access tests for repo-guardian-fs
- [ ] **Error handling**: No malformed manifest tests, corrupted session tests

### Build & Execution
- [ ] All Zig integration_test.zig files appear to be template placeholders, not real tests
- [ ] No Idris2 ABI compilation verification
- [ ] No ReScript build tests for mcp-repo-guardian

### Benchmarks Needed
- [ ] Manifest parsing throughput
- [ ] FUSE filesystem overhead vs native
- [ ] Protocol negotiation latency

### Self-Tests
- [ ] No self-diagnostic for any standard implementation

## FLAGGED ISSUES
- **358 source files with ~5 template placeholder tests** -- this is a monorepo-scale testing void
- **Security-focused repos (gatekeeper, guardian-fs) have 0 security tests** -- the irony is painful
- **All Zig test files are template copies** -- not real tests
- **fuzz/placeholder.txt** -- fake fuzz testing claim

## Priority: P0 (CRITICAL)
