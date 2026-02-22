# Repository Guardian Filesystem

**SPDX-License-Identifier: PMPL-1.0-or-later**

FUSE filesystem wrapper that enforces AI.a2ml manifest acknowledgment before allowing file access in repositories.

## Overview

`repo-guardian-fs` provides **universal enforcement** of the AI Gatekeeper Protocol by mounting repositories through a virtual filesystem that:

- ✅ Blocks ALL file operations until manifest acknowledged
- ✅ Works with ANY AI agent/tool (not just MCP-compatible)
- ✅ OS-level enforcement (cannot be bypassed)
- ✅ Session-based access control
- ✅ Automatic session timeout

## How It Works

```
┌──────────────┐
│  AI Agent    │ → Attempts to read file
│ (any tool)   │
└──────┬───────┘
       │
       ↓ File access via /mnt/guarded-repos
┌──────────────────────────────┐
│  Guardian Filesystem (FUSE)  │
│  "❌ EACCES - Not acknowledged"│
└──────┬───────────────────────┘
       │
       ↓ Agent acknowledges via sideband
┌──────────────────────────────┐
│  Session marked acknowledged │
└──────┬───────────────────────┘
       │
       ↓ ✅ File access granted
┌──────────────┐
│  Real Repos  │
└──────────────┘
```

## Installation

### Prerequisites

- Rust 1.70+ (`rustup install stable`)
- FUSE3 (`sudo dnf install fuse3` on Fedora)
- Linux kernel with FUSE support

### Build

```bash
cargo build --release
```

### Install

```bash
sudo cp target/release/repo-guardian-fs /usr/local/bin/
```

## Usage

### Basic Mount

```bash
# Mount repos with enforcement
repo-guardian-fs \
  --source ~/Documents/hyperpolymath-repos \
  --mount /mnt/guarded-repos
```

### With Options

```bash
# Strict mode, 1 hour session timeout, allow root
repo-guardian-fs \
  --source ~/Documents/hyperpolymath-repos \
  --mount /mnt/guarded-repos \
  --strict \
  --session-timeout 3600 \
  --allow-root
```

### Unmount

```bash
fusermount3 -u /mnt/guarded-repos
```

## Access Control

### Session Lifecycle

1. **Initial Access** - Agent attempts to read file → `EACCES` (Permission denied)
2. **Acknowledgment** - Agent acknowledges manifest (via sideband mechanism)
3. **Granted Access** - Session marked as acknowledged
4. **Operations** - All file operations now allowed
5. **Timeout** - Session expires after inactivity

### Acknowledgment Mechanism

**Current Implementation:** Sessions auto-acknowledge for testing

**Production TODO:** Implement sideband acknowledgment via:
- Unix domain socket (e.g., `/tmp/repo-guardian.sock`)
- Agent sends: `ACK <session-id> <manifest-hash>`
- Server validates hash and marks session acknowledged

## Configuration

### Command-Line Options

| Option | Description | Default |
|--------|-------------|---------|
| `--source DIR` | Source directory with repos | Required |
| `--mount DIR` | Mount point | Required |
| `--strict` | Block ALL ops until ack | `true` |
| `--session-timeout SEC` | Session timeout seconds | `3600` |
| `--allow-root` | Allow root access | `false` |
| `--allow-other` | Allow other users | `false` |

### Environment Variables

- `RUST_LOG=info` - Set logging level (debug, info, warn, error)

## Architecture

### Components

**manifest.rs** - Manifest parsing and validation
- Finds AI.a2ml files (0-AI-MANIFEST.a2ml, AI.a2ml, !AI.a2ml)
- Extracts canonical locations and invariants
- Computes SHA-256 hash for attestation

**session_manager.rs** - Session tracking
- Creates sessions per process ID (UID)
- Tracks acknowledgment status
- Handles session timeout and cleanup

**filesystem.rs** - FUSE implementation
- Intercepts all file operations
- Enforces acknowledgment check
- Passes through to real filesystem

**main.rs** - Entry point
- Parses CLI arguments
- Mounts FUSE filesystem
- Handles signals and unmount

### Security Model

**Threat Model:**
- AI agents may attempt to bypass manifest reading
- Agents may fork processes to reset sessions
- Malicious tools may try to access repos directly

**Mitigations:**
- OS-level enforcement - cannot bypass FUSE
- Per-session tracking - each process must acknowledge
- Timeout enforcement - stale sessions auto-expire
- Read-only mode (TODO) - prevent accidental writes

## Limitations

### Current Limitations

1. **No write support** - Read-only enforcement
2. **Simple inode tracking** - All files report same inode
3. **Auto-acknowledgment** - Testing only, needs sideband
4. **No persistence** - Sessions lost on restart
5. **Basic error handling** - Needs improvement

### Production TODOs

- [ ] Implement sideband acknowledgment (Unix socket)
- [ ] Add write operation support
- [ ] Proper inode and file handle tracking
- [ ] Persistent session database
- [ ] Extended attributes support
- [ ] Symlink handling
- [ ] Performance optimization (caching)
- [ ] Comprehensive error handling
- [ ] Systemd service unit
- [ ] Configuration file support

## Testing

### Unit Tests

```bash
cargo test
```

### Integration Test

```bash
# Terminal 1: Mount filesystem
cargo run -- --source /tmp/test-repos --mount /tmp/test-mount

# Terminal 2: Try to access
cd /tmp/test-mount
ls  # Should work or be denied based on acknowledgment

# Terminal 3: Unmount
fusermount3 -u /tmp/test-mount
```

## Comparison with MCP Server

| Feature | MCP Server | FUSE Wrapper |
|---------|-----------|--------------|
| **Platform** | Claude + MCP-compatible | Universal |
| **Enforcement** | Tool-level | OS-level |
| **Bypass Risk** | Agent can use native FS | Cannot bypass |
| **Complexity** | Lower | Higher |
| **Performance** | Native | FUSE overhead |
| **Setup** | Config file | Mount command |

**Recommendation:** Use MCP server for Claude, FUSE wrapper for other agents.

## Integration

### With Claude (MCP)

Claude users should use `mcp-repo-guardian` for better integration.

### With Other Agents

Mount repos through guardian filesystem:

```bash
repo-guardian-fs \
  --source ~/Documents/hyperpolymath-repos \
  --mount /mnt/guarded-repos

# Point agent to /mnt/guarded-repos instead of real location
```

## Troubleshooting

### Permission Denied

**Symptom:** All operations return `EACCES`

**Fix:** Check session acknowledgment status, ensure manifest exists

### Mount Fails

**Symptom:** `fusermount3: mount failed: Operation not permitted`

**Fix:** Ensure FUSE installed, user has permissions, mount point exists

### Performance Issues

**Symptom:** Slow file access

**Fix:** FUSE has overhead, consider MCP server for performance-critical use

## Development

### Building from Source

```bash
git clone https://github.com/hyperpolymath/repo-guardian-fs
cd repo-guardian-fs
cargo build --release
```

### Contributing

See `CONTRIBUTING.md` in the repository.

## License

PMPL-1.0-or-later

## Related

- [AI Gatekeeper Protocol](https://github.com/hyperpolymath/0-ai-gatekeeper-protocol)
- [MCP Repository Guardian](https://github.com/hyperpolymath/mcp-repo-guardian)
- [FUSE3 Library](https://crates.io/crates/fuse3)

## Authors

Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

---

**Status:** Alpha - Proof of concept implementation. Production use requires additional work (see TODOs above).
