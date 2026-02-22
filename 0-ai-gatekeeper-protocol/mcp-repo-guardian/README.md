# MCP Repository Guardian

**SPDX-License-Identifier: PMPL-1.0-or-later**

Model Context Protocol (MCP) server that enforces AI.a2ml manifest acknowledgment before any repository operations.

**Implementation:** ReScript + Deno (✅ Hyperpolymath Policy Compliant)

## Overview

The Repository Guardian acts as a gatekeeper for AI agents accessing your repositories. It ensures that:

1. **AI agents MUST read AI.a2ml manifest first** before any operations
2. **Manifest invariants are enforced** (no SCM file duplication, etc.)
3. **Access is session-based** with attestation validation
4. **Path validation** prevents invariant violations

## How It Works

```
┌─────────────┐
│  AI Agent   │ (Claude, etc.)
└──────┬──────┘
       │
       │ ❌ Direct file access blocked
       │
       ▼
┌─────────────────────────────┐
│  MCP Repository Guardian    │ ← ENFORCES AI.a2ml
│                             │
│  1. get_manifest            │
│  2. acknowledge_manifest    │
│  3. read_file (with token)  │
│  4. list_directory          │
└──────────┬──────────────────┘
           │
           ▼
     ┌──────────────┐
     │  Your Repos  │
     └──────────────┘
```

## Installation

### Prerequisites

- **ReScript compiler** - Install: `npm install -g rescript` (build tool only)
- **Deno runtime** - https://deno.land/

### From Source

```bash
git clone https://github.com/hyperpolymath/mcp-repo-guardian
cd mcp-repo-guardian

# Build ReScript code
rescript

# Run with Deno
deno run --allow-read --allow-write --allow-env --allow-net src/Index.mjs
```

## Configuration

Add to your MCP settings (e.g., `~/.claude/settings.json`):

```json
{
  "mcpServers": {
    "repo-guardian": {
      "command": "deno",
      "args": [
        "run",
        "--allow-read",
        "--allow-write",
        "--allow-env",
        "--allow-net",
        "/path/to/mcp-repo-guardian/src/Index.mjs"
      ],
      "env": {
        "REPOS_PATH": "/home/user/Documents/hyperpolymath-repos",
        "STRICT_MODE": "true",
        "SESSION_TIMEOUT": "3600000"
      }
    }
  }
}
```

## Usage

### 1. Get Manifest

```typescript
// AI agent calls: get_manifest
{
  "repoPath": "nextgen-languages"
}

// Returns manifest hash and structure
```

### 2. Acknowledge Manifest

```typescript
// AI agent must provide correct hash
{
  "repoPath": "nextgen-languages",
  "attestationHash": "sha256:a8f7c3d2..."
}

// Returns session ID on success
```

### 3. Access Files

```typescript
// Now can read files with session ID
{
  "sessionId": "uuid",
  "path": ".machine_readable/STATE.scm"
}
```

## Security Features

- **No direct file access** - All operations mediated by guardian
- **Attestation validation** - Agent must prove they read manifest
- **Path validation** - Prevents invariant violations
- **Session timeout** - Sessions expire after inactivity
- **Strict mode** - No fallbacks, hard enforcement

## Tools Provided

| Tool | Description | Requires Session |
|------|-------------|------------------|
| `get_manifest` | Retrieve manifest content and hash | No |
| `acknowledge_manifest` | Acknowledge manifest with hash | No |
| `read_file` | Read file from repository | Yes |
| `list_directory` | List directory contents | Yes |

## Example Session

```
Agent: get_manifest(repoPath="nextgen-languages")
Guardian: Hash: sha256:a8f7c3d2...

Agent: read_file(sessionId="...", path="README.md")
Guardian: ❌ ACCESS DENIED - Must acknowledge manifest first

Agent: acknowledge_manifest(repoPath="...", hash="sha256:a8f7c3d2...")
Guardian: ✅ Session: uuid-1234...

Agent: read_file(sessionId="uuid-1234", path="README.md")
Guardian: ✅ [file content]

Agent: read_file(sessionId="uuid-1234", path="STATE.scm")
Guardian: ❌ INVARIANT VIOLATION - SCM files must be in .machine_readable/
```

## Environment Variables

- `REPOS_PATH` - Base path where repositories are located (default: `cwd`)
- `STRICT_MODE` - Enforce strict mode with no fallbacks (default: `false`)
- `SESSION_TIMEOUT` - Session timeout in milliseconds (default: `3600000`)

## Development

### Build

```bash
# Compile ReScript to JavaScript
rescript

# Watch mode
rescript build -w

# Clean
rescript clean
```

### Run

```bash
# Run with Deno
deno task start

# Or directly
deno run --allow-read --allow-write --allow-env --allow-net src/Index.mjs
```

## Implementation

- **Language:** ReScript (compiles to JavaScript ES modules)
- **Runtime:** Deno (replaces Node.js/npm)
- **Modules:**
  - `Types.res` - Type definitions
  - `Manifest.res` - AI.a2ml parsing and validation
  - `Session.res` - Session management
  - `Guards.res` - Access control
  - `Index.res` - MCP server and request handlers

## Why ReScript + Deno?

✅ **Policy Compliant:**
- ReScript is the primary application language (Hyperpolymath standard)
- Deno is the standard runtime (replaces Node.js/npm)
- No TypeScript or npm dependencies

✅ **Type Safety:**
- ReScript provides ML-style type safety
- Catches errors at compile time
- Compiles to clean, readable JavaScript

✅ **Modern Runtime:**
- Deno has built-in TypeScript support (for dependencies)
- Secure by default (explicit permissions)
- Native ES modules

## Related

- [AI Gatekeeper Protocol](https://github.com/hyperpolymath/0-ai-gatekeeper-protocol) - Complete documentation
- [FUSE Wrapper](https://github.com/hyperpolymath/repo-guardian-fs) - OS-level enforcement
- [rsr-template-repo](https://github.com/hyperpolymath/rsr-template-repo) - Template with AI.a2ml

## License

PMPL-1.0-or-later

## Authors

Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

---

**Status:** Production-ready. ReScript + Deno implementation complete.
