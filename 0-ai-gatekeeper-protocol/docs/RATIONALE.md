# AI Gatekeeper Protocol - Rationale

**SPDX-License-Identifier: CC-BY-4.0**

## The Problem

### Context Loss Across Sessions

AI agents (Claude, Gemini, OpenAI, etc.) lose context when:
- Sessions crash or timeout
- Users switch between different AI platforms
- Long time passes between work sessions
- Context windows get compacted

This leads to:
- **Repeated explanations** - "For the 10th time, SCM files go in .machine_readable/"
- **Duplicate files** - Agent creates STATE.scm in root despite it existing in .machine_readable/
- **Invariant violations** - Agent doesn't know about project-specific rules
- **Wasted resources** - Time, computational credits, user frustration

### Platform Fragmentation

Different AI platforms have different:
- Context management strategies
- File access patterns
- Memory/continuation capabilities
- Integration points

A user working with:
- Claude on Monday
- Gemini on Tuesday
- GitHub Copilot on Wednesday

Results in each agent making the SAME mistakes the others did.

### Architectural Drift

Without a gatekeeper:
- Agents create files wherever they think is best
- "Helpful" refactoring violates design decisions
- Stale duplicates proliferate
- No way to enforce invariants mechanically

## The Solution: AI Gatekeeper Protocol

### Single Source of Truth

**Every repo has ONE manifest file** (AI.a2ml or 0-AI-MANIFEST.a2ml) that:
- ✅ Declares canonical file locations
- ✅ States critical invariants
- ✅ Explains repository structure
- ✅ Provides session startup checklist

### Platform-Agnostic Standard

The manifest is:
- **Plain text** - Any AI can read it
- **Structured** - Consistent format across repos
- **Self-documenting** - Explains itself
- **Universal** - Not tied to Claude, Gemini, or any specific platform

### Mechanical Enforcement

For platforms that support it:
- **MCP server** - Hard enforcement for Claude and MCP-compatible agents
- **FUSE wrapper** - OS-level enforcement for ANY tool
- **CI/CD validation** - GitHub Actions catch violations
- **Bot fleet integration** - Automated bots respect protocol

### Attestation Pattern

Instead of hoping agents read documentation:
1. Agent MUST read manifest
2. Agent MUST compute hash of manifest content
3. Agent MUST provide hash to prove they read it
4. Only then granted access to files

This proves:
- ✅ Agent actually read the manifest (not skimmed)
- ✅ Agent has correct version (hash changes if updated)
- ✅ Session state is trackable

## Real-World Example

### Before Gatekeeper Protocol

**Session 1 (Claude):**
```
User: "Check the project state"
Claude: *creates STATE.scm in root*
User: "No! SCM files go in .machine_readable/"
Claude: "Sorry, let me fix that" *moves file*
```

**Session 2 (Gemini, next day):**
```
User: "Check the project state"
Gemini: *creates STATE.scm in root*
User: "NO! AGAIN?! How many times do I have to say this?!"
Gemini: "Apologies, let me move it"
```

**Session 3 (Claude, after crash):**
```
User: "What's the current state?"
Claude: *finds TWO STATE.scm files - root (stale) and .machine_readable/ (current)*
Claude: "There seem to be inconsistencies..."
User: *loses mind*
```

### After Gatekeeper Protocol

**Session 1 (Claude with MCP):**
```
Claude: *attempts to read file*
MCP Guardian: "⚠️ ACCESS DENIED - Must acknowledge manifest first"
Claude: *reads 0-AI-MANIFEST.a2ml*
Claude: *calls acknowledge_manifest with hash*
MCP Guardian: "✅ Session granted - SCM files in .machine_readable/ only"
Claude: *reads .machine_readable/STATE.scm correctly*
```

**Session 2 (Gemini, next day):**
```
Gemini: *reads 0-AI-MANIFEST.a2ml (first file alphabetically)*
Gemini: "I see. SCM files must be in .machine_readable/ directory only."
User: "Yes! Thank you for reading that!"
Gemini: *works correctly*
```

**Session 3 (Claude, after crash):**
```
Claude: *reads 0-AI-MANIFEST.a2ml*
Claude: "SCM files located in .machine_readable/, checking STATE.scm there"
User: "Perfect, exactly right"
```

## Benefits

### For Users
- ✅ No repeated explanations across sessions
- ✅ No repeated explanations across AI platforms
- ✅ Architectural decisions preserved mechanically
- ✅ Confidence that agents won't break things
- ✅ Less frustration, more productivity

### For AI Agents
- ✅ Clear, unambiguous instructions on repository structure
- ✅ Context preserved across sessions
- ✅ Reduced chance of making mistakes
- ✅ Better collaboration across different AI platforms
- ✅ Attestation proves understanding

### For Ecosystem
- ✅ Standardized approach to AI-repository interaction
- ✅ Interoperability across platforms
- ✅ Foundation for advanced tooling (MCP servers, FUSE wrappers)
- ✅ Scalable to thousands of repositories
- ✅ Open source - anyone can adopt

## Design Principles

### 1. Fail-Safe Defaults

If an agent doesn't read the manifest:
- MCP server blocks access (hard fail)
- CI/CD catches violations (post-commit)
- Alphabetical naming ensures visibility (0-AI-MANIFEST.a2ml sorts first)

### 2. Defense in Depth

Multiple enforcement layers:
- **Prevention** - MCP server blocks before mistake
- **Detection** - CI/CD catches violations
- **Correction** - Bot fleet fixes automatically
- **Documentation** - Clear error messages guide agents

### 3. Platform Agnostic

Works with:
- ✅ Claude (via MCP)
- ✅ Gemini (via manifest reading)
- ✅ OpenAI (via manifest reading)
- ✅ GitHub Copilot (via CI/CD validation)
- ✅ Any future AI platform

### 4. Human-Readable

Manifests are plain text, not:
- Binary formats
- Encrypted data
- Platform-specific encodings
- Obscure schemas

Anyone (human or AI) can read and understand.

### 5. Incremental Adoption

Can be adopted gradually:
1. Start with manifest files (no enforcement)
2. Add CI/CD validation (post-commit detection)
3. Deploy MCP server (pre-operation blocking)
4. Add FUSE wrapper (universal enforcement)

## Comparison to Alternatives

### Alternative 1: Hope and Repetition

**Current state for most users:**
- Rely on AI reading previous context
- Repeat instructions each session
- Accept that mistakes will happen

**Problems:**
- Doesn't scale across platforms
- Frustrating for users
- Wastes resources

### Alternative 2: Platform-Specific Solutions

**Example:** Claude-only `.claude/CLAUDE.md` file

**Problems:**
- Doesn't help Gemini, OpenAI, etc.
- Fragmented approaches
- User maintains multiple instruction sets

### Alternative 3: Passive Documentation

**Example:** README.md with instructions

**Problems:**
- Agents often don't read README first
- Not enforced mechanically
- No attestation proving understanding
- Gets buried in large repos

### Our Approach: Active Gatekeeper

- ✅ Universal (works across platforms)
- ✅ Enforced (MCP/FUSE blocking)
- ✅ Attested (hash proves reading)
- ✅ Visible (0-prefix sorts first)
- ✅ Standardized (consistent format)

## Success Criteria

The protocol is successful if:

1. **Reduction in duplicate files** - Metrics show fewer SCM files in wrong locations
2. **Reduced user frustration** - Less time spent re-explaining
3. **Cross-platform consistency** - Same behavior from Claude, Gemini, etc.
4. **Adoption** - Other projects/users adopt the protocol
5. **Bot integration** - Automated tools respect manifest invariants

## Future Directions

### 1. Formal Specification

Create RFC-style spec (AI-MANIFEST-SPEC.adoc) defining:
- Required sections
- Syntax rules
- Media type
- Validation schema

### 2. Tooling Ecosystem

- Manifest generators
- Validation tools
- Migration helpers
- IDE plugins

### 3. Platform Integration

Work with:
- Anthropic (Claude) - Native MCP support
- Google (Gemini) - Propose integration
- OpenAI - API wrapper support
- GitHub (Copilot) - Native support

### 4. Community Standards

- Submit to standardization bodies
- Create open governance
- Gather feedback from users
- Iterate on format

## Conclusion

The AI Gatekeeper Protocol solves a real problem:
- ✅ Context loss across sessions and platforms
- ✅ Repeated mistakes by different AI agents
- ✅ Architectural drift and invariant violations
- ✅ User frustration and wasted resources

Through a combination of:
- 📄 Universal manifest files
- 🔒 Mechanical enforcement
- ✅ Attestation proving understanding
- 🌍 Platform-agnostic design

The result: Users work with AI agents that respect their architecture, preserve their decisions, and don't repeat the same mistakes session after session.

---

**Related Documents:**
- [ARCHITECTURE.md](ARCHITECTURE.md) - Technical implementation
- [INTEGRATION.md](INTEGRATION.md) - Platform-specific integration
- [AI-MANIFEST-SPEC.adoc](AI-MANIFEST-SPEC.adoc) - Formal specification
