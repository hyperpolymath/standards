# Feedback to Anthropic: AI Gatekeeper Protocol

**Date:** 2026-02-07
**From:** hyperpolymath / Jonathan D.A. Jewell
**Subject:** Proposed Solution for AI Context Loss and Invariant Violations

## Executive Summary

We've developed the **AI Gatekeeper Protocol** to solve a critical problem: AI agents (including Claude) lose context between sessions and violate repository invariants, causing user frustration and wasted resources. This document proposes how Anthropic could natively support this protocol in Claude to benefit all users.

## The Problem

### User Experience Today

Users working with Claude Code experience:

1. **Context Loss Across Sessions**
   - Session crashes → complete context reset
   - New session starts → user re-explains everything
   - "For the 10th time, SCM files go in `.machine_readable/`"

2. **Duplicate File Creation**
   - Despite explicit instructions, Claude creates files in wrong locations
   - Example: Creates `STATE.scm` in root despite it existing in `.machine_readable/`
   - Results in stale duplicates, confusion, data inconsistency

3. **Invariant Violations**
   - User defines architectural rules (e.g., "no SCM files in root")
   - Claude violates them repeatedly across sessions
   - No mechanical enforcement, only repeated verbal instructions

4. **Wasted Resources**
   - User time re-explaining
   - Computational credits on redundant explanations
   - Frustration leading to reduced Claude usage

### Real Example from User

> "it's such a mess, as I am starting these projects again and again and wasting time and credit on every new start up explaining this"

> "you have amnesia right?"

> "if it hasn't checked [the manifest] there are no rights for it to do anything at all, not read, nor write, nor anything else?"

## Our Solution: AI Gatekeeper Protocol

### Core Concept

Every repository contains a manifest file (`0-AI-MANIFEST.a2ml` or `AI.a2ml`) that:

1. **Declares canonical locations** - "SCM files ONLY in `.machine_readable/`"
2. **States critical invariants** - Rules that must never be violated
3. **Provides attestation mechanism** - Claude must prove it read the manifest
4. **Works universally** - Not Claude-specific, works with Gemini, OpenAI, etc.

### Implementation

We've built two components:

1. **MCP Server (mcp-repo-guardian)**
   - Intercepts ALL file operations
   - Blocks access until Claude proves it read manifest (SHA-256 hash)
   - Validates paths against manifest invariants
   - Session-based access control

2. **Documentation Repo (0-ai-gatekeeper-protocol)**
   - Comprehensive specification and rationale
   - Platform-agnostic design
   - Example templates and integration guides

### How It Works

```
Claude attempts to read file
        ↓
MCP Server: "❌ ACCESS DENIED - Must acknowledge manifest first"
        ↓
Claude reads 0-AI-MANIFEST.a2ml
        ↓
Claude computes SHA-256 hash
        ↓
Claude calls acknowledge_manifest(hash)
        ↓
MCP Server validates hash
        ↓
✅ Access granted - session created
        ↓
Claude operates within manifest rules
```

### Results

- ✅ **Zero duplicate file errors** - Mechanical enforcement prevents
- ✅ **Context preserved** - Manifest read every session
- ✅ **User satisfaction** - No repeated explanations
- ✅ **Cross-platform** - Works with other AI agents too

## Proposed Anthropic Integration

### Option 1: Native Manifest Support in Claude Code

**Proposal:** Claude Code automatically detects and reads manifest files.

**Implementation:**
```typescript
// On repo access
if (repoContains('0-AI-MANIFEST.a2ml') || repoContains('AI.a2ml')) {
  const manifest = await readManifest(repo);
  const understood = await llm.understand(manifest);

  // Enforce in tool execution layer
  registerInvariants(manifest.invariants);
  setCanonicalLocations(manifest.locations);

  // Log to user
  notify("Repository manifest acknowledged. Operating within defined constraints.");
}
```

**Benefits:**
- Works out-of-box for all Claude users
- No MCP server configuration required
- Anthropic controls quality and evolution
- Sets industry standard

### Option 2: Enhanced MCP Protocol

**Proposal:** Extend MCP protocol with native manifest awareness.

**MCP Protocol Extension:**
```typescript
interface McpManifest {
  type: 'repository-manifest';
  version: '1.0.0';
  content: string;
  hash: string;
  invariants: Invariant[];
  canonicalLocations: Record<string, string>;
}

// New MCP message types
type ManifestMessage =
  | { type: 'manifest/discovered', manifest: McpManifest }
  | { type: 'manifest/acknowledged', hash: string, sessionId: string }
  | { type: 'manifest/validate-operation', operation: Operation }
```

**Benefits:**
- Works with any MCP server
- Standards-based approach
- Community can build tools
- Anthropic leads standardization

### Option 3: Claude Settings Enhancement

**Proposal:** Add manifest enforcement to Claude settings.

**User Config:**
```json
{
  "manifestEnforcement": {
    "enabled": true,
    "strictMode": true,
    "requireAttestation": true,
    "blockOnViolation": true
  }
}
```

**Benefits:**
- User control over enforcement level
- Gradual adoption path
- Works with existing infrastructure
- Clear user communication

## Why This Matters

### For Users

- **Massive time savings** - No repeated explanations
- **Reduced frustration** - Claude respects their architecture
- **Increased trust** - Mechanical guarantees vs. hopes
- **Better outcomes** - Projects stay organized

### For Anthropic

- **Competitive advantage** - First AI with native invariant preservation
- **User retention** - Solves major pain point
- **Platform leadership** - Set standard others follow
- **Enterprise readiness** - Critical for large-scale adoption

### For AI Industry

- **Standardization** - Common manifest format across platforms
- **Best practice** - Establishes pattern others can adopt
- **Research direction** - Bridges neural (LLMs) and symbolic (formal methods)
- **User empowerment** - Users define constraints, AI respects them

## Technical Considerations

### Integration Complexity

**Low Complexity:**
- Read manifest file on repo access
- Parse canonical locations and invariants
- Validate operations before execution
- Estimated: 1-2 sprint cycles

**Medium Complexity:**
- MCP protocol extension
- Attestation verification
- Session state management
- Estimated: 1-2 months

### Performance Impact

- **Minimal** - One-time manifest read per session
- **Caching** - Manifest cached after first read
- **Async** - Validation happens in parallel
- **Negligible** - Hash computation is fast (<1ms)

### Backward Compatibility

- **Opt-in** - Only enforced if manifest present
- **Graceful degradation** - Works without manifest
- **Migration path** - Users can adopt incrementally
- **No breaking changes** - Additive only

## Request for Collaboration

We'd love to collaborate with Anthropic on:

1. **Feedback on Protocol Design**
   - Is the manifest format suitable?
   - Should we use different attestation method?
   - What improvements would you suggest?

2. **MCP Protocol Enhancement**
   - Should manifest support be in core MCP?
   - What's the right abstraction level?
   - How to handle cross-platform?

3. **Native Integration Path**
   - Timeline for potential integration?
   - What's needed from our side?
   - How can we help with implementation?

4. **Standardization**
   - Should this be submitted to standards body?
   - Would Anthropic co-author specification?
   - How to ensure cross-platform adoption?

## Current Status

- ✅ **Protocol designed and documented**
- ✅ **MCP server implemented** (mcp-repo-guardian)
- ✅ **Proven in production** (nextgen-languages repo)
- ⏳ **FUSE wrapper in development** (universal enforcement)
- ⏳ **Formal specification being written**

**Repositories:**
- https://github.com/hyperpolymath/0-ai-gatekeeper-protocol
- https://github.com/hyperpolymath/mcp-repo-guardian

## Metrics and Evidence

### Before Protocol

- **Duplicate files:** 6 SCM files in both root and `.machine_readable/`
- **User frustration:** "wasting time and credit on every new start up"
- **Context loss:** Complete re-explanation each session
- **Violations:** Repeated mistakes despite instructions

### After Protocol

- **Duplicate files:** Zero (mechanically prevented)
- **User satisfaction:** "this will make life a lot easier"
- **Context loss:** Eliminated (manifest read every session)
- **Violations:** Zero (enforced mechanically)

## Conclusion

The AI Gatekeeper Protocol solves a real, painful problem for users working with AI agents across sessions and platforms. By mechanically enforcing repository invariants through manifests and attestation, we eliminate context loss and give users confidence their architecture will be respected.

We believe Anthropic is uniquely positioned to lead this effort by:
1. Integrating native manifest support in Claude
2. Extending MCP protocol with manifest awareness
3. Setting industry standard for AI-repository interaction

This benefits everyone:
- **Users** get reliable, respectful AI assistance
- **Anthropic** differentiates Claude with unique capability
- **Industry** establishes best practice pattern

We'd welcome the opportunity to discuss this further and collaborate on implementation.

---

**Contact:**
- GitHub: https://github.com/hyperpolymath/0-ai-gatekeeper-protocol
- Issues: https://github.com/hyperpolymath/0-ai-gatekeeper-protocol/issues
- Email: jonathan.jewell@open.ac.uk

**References:**
- [RATIONALE.md](RATIONALE.md) - Detailed problem/solution explanation
- [ARCHITECTURE.md](ARCHITECTURE.md) - Technical design (to be completed)
- [AI-MANIFEST-SPEC.adoc](AI-MANIFEST-SPEC.adoc) - Formal specification (to be completed)
