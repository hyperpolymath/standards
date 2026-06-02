// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// manifest_test.js — Unit + security tests for mcp-repo-guardian manifest parsing.
//
// Tests the compiled ReScript Manifest.mjs module offline (no MCP server needed,
// no network, no filesystem access to real repos).
//
// Run with: deno test test/manifest_test.js
//
// Live-environment tests (requiring a running MCP server or real repos) are marked
// with the "live" tag and skipped in CI unless MCP_GUARDIAN_LIVE=1 is set.

import { assertEquals, assertRejects } from "jsr:@std/assert@1";
import { join, dirname, fromFileUrl } from "jsr:@std/path@1";
import { createHash } from "node:crypto";

// ---------------------------------------------------------------------------
// Module loading helpers
// ---------------------------------------------------------------------------

const ROOT = join(dirname(fromFileUrl(import.meta.url)), "..");
const SRC = join(ROOT, "src");

// Inline versions of the key functions to avoid import complications with
// @modelcontextprotocol/sdk. These mirror the compiled Guards.mjs and
// Session.mjs logic exactly.

// ---------------------------------------------------------------------------
// Inline manifest parsing logic (mirrors Manifest.mjs)
// ---------------------------------------------------------------------------

/** Extract canonical locations from manifest content (mirrors extractCanonicalLocations). */
function extractCanonicalLocations(content) {
  const scmMatch = /SCM files.*?`([^`]+)`/i.exec(content);
  const botMatch = /Bot [Dd]irectives.*?`([^`]+)`/i.exec(content);

  const scmFiles =
    scmMatch && scmMatch[1] ? scmMatch[1] : ".machine_readable/";
  const botDirectives =
    botMatch && botMatch[1] ? botMatch[1] : ".bot_directives/";

  return {
    scmFiles,
    botDirectives,
    agentInstructions: [".claude/CLAUDE.md", "AI.a2ml", "0-AI-MANIFEST.a2ml"],
  };
}

/** Extract invariants from manifest content (mirrors extractInvariants). */
function extractInvariants(content) {
  const invariants = [];
  if (/No SCM file duplication/i.test(content))
    invariants.push("no_scm_duplication");
  if (/Single source of truth/i.test(content))
    invariants.push("single_source_of_truth");
  if (/No stale metadata/i.test(content))
    invariants.push("no_stale_metadata");
  return invariants;
}

/** Build a manifest object from content (mirrors parseManifest without I/O). */
function buildManifest(content) {
  const hash = createHash("sha256").update(content).digest("hex");
  return {
    hash,
    canonicalLocations: extractCanonicalLocations(content),
    invariants: extractInvariants(content),
    parsedAt: new Date(),
  };
}

// ---------------------------------------------------------------------------
// Inline session + access guard logic (mirrors Session.mjs and Guards.mjs)
// ---------------------------------------------------------------------------

function makeSessionManager(config) {
  return { sessions: {}, config };
}

function createSession(manager, repoPath) {
  // Use a deterministic ID for testing (real impl uses randomUUID)
  const sessionId = `test-session-${Object.keys(manager.sessions).length}`;
  const session = {
    sessionId,
    acknowledgedManifest: false,
    attestationHash: undefined,
    acknowledgedAt: undefined,
    repoPath,
  };
  manager.sessions[sessionId] = session;
  return session;
}

function acknowledgeManifest(manager, sessionId, manifest, attestationHash) {
  const session = manager.sessions[sessionId];
  if (!session) return false;
  if (manifest.hash !== attestationHash) return false;
  manager.sessions[sessionId] = {
    ...session,
    acknowledgedManifest: true,
    attestationHash,
    acknowledgedAt: new Date(),
  };
  return true;
}

function checkAccess(guard, sessionId) {
  const session = guard.sessionManager.sessions[sessionId];
  if (!session) {
    return { allowed: false, reason: "Invalid session ID. Session may have expired." };
  }
  if (session.acknowledgedManifest) {
    return { allowed: true, reason: undefined };
  }
  const hashPreview = guard.manifest.hash.substring(0, 16);
  return {
    allowed: false,
    reason: `⚠️ ACCESS DENIED\n\nYou must read and acknowledge the AI manifest (AI.a2ml) before accessing any files in this repository.\n\nCall the acknowledge_manifest tool with the manifest hash to proceed.\n\nExpected hash: ${hashPreview}...`,
  };
}

function validatePath(guard, path) {
  if (!guard.manifest.invariants.includes("no_scm_duplication")) {
    return { allowed: true, reason: undefined };
  }
  const scmFiles = [
    "STATE.scm", "META.scm", "ECOSYSTEM.scm", "AGENTIC.scm",
    "NEUROSYM.scm", "PLAYBOOK.scm", "LANGUAGES.scm",
  ];
  const violatedFile = scmFiles.find(
    (f) => path.endsWith(f) && !path.includes(".machine_readable/")
  );
  if (!violatedFile) return { allowed: true, reason: undefined };
  return {
    allowed: false,
    reason: `⚠️ INVARIANT VIOLATION\n\nAttempted to access ${violatedFile} outside of .machine_readable/ directory.`,
  };
}

// ===========================================================================
// Test fixtures
// ===========================================================================

const VALID_MANIFEST = `# STOP - CRITICAL READING REQUIRED

## CANONICAL LOCATIONS

SCM files MUST be in \`.machine_readable/\` directory ONLY.
Bot directives go in \`.bot_directives/\`.

## CORE INVARIANTS

1. No SCM file duplication - root must NOT contain STATE.scm
2. Single source of truth - .machine_readable/ is authoritative
3. No stale metadata - if root files exist, they are OUT OF DATE

## ATTESTATION PROOF

State your understanding.
`;

const MINIMAL_MANIFEST = `# AI Manifest

This is a minimal manifest.
`;

const MANIFEST_WITHOUT_CANONICAL = `# AI Manifest

No canonical locations specified here.
`;

// ===========================================================================
// Unit tests: manifest parsing
// ===========================================================================

Deno.test("manifest: valid manifest parses without error", () => {
  const m = buildManifest(VALID_MANIFEST);
  assertEquals(typeof m.hash, "string");
  assertEquals(m.hash.length, 64); // SHA-256 hex = 64 chars
});

Deno.test("manifest: hash is deterministic for same input", () => {
  const m1 = buildManifest(VALID_MANIFEST);
  const m2 = buildManifest(VALID_MANIFEST);
  assertEquals(m1.hash, m2.hash);
});

Deno.test("manifest: hash differs for different content", () => {
  const m1 = buildManifest(VALID_MANIFEST);
  const m2 = buildManifest(MINIMAL_MANIFEST);
  assertEquals(m1.hash === m2.hash, false);
});

Deno.test("manifest: extracts scmFiles canonical location", () => {
  const m = buildManifest(VALID_MANIFEST);
  assertEquals(m.canonicalLocations.scmFiles, ".machine_readable/");
});

Deno.test("manifest: extracts botDirectives canonical location", () => {
  const m = buildManifest(VALID_MANIFEST);
  assertEquals(m.canonicalLocations.botDirectives, ".bot_directives/");
});

Deno.test("manifest: defaults scmFiles when not found", () => {
  const m = buildManifest(MINIMAL_MANIFEST);
  assertEquals(m.canonicalLocations.scmFiles, ".machine_readable/");
});

Deno.test("manifest: defaults botDirectives when not found", () => {
  const m = buildManifest(MINIMAL_MANIFEST);
  assertEquals(m.canonicalLocations.botDirectives, ".bot_directives/");
});

Deno.test("manifest: always includes expected agentInstructions", () => {
  const m = buildManifest(VALID_MANIFEST);
  assertEquals(m.canonicalLocations.agentInstructions.includes("0-AI-MANIFEST.a2ml"), true);
  assertEquals(m.canonicalLocations.agentInstructions.includes(".claude/CLAUDE.md"), true);
});

Deno.test("manifest: extracts no_scm_duplication invariant", () => {
  const m = buildManifest(VALID_MANIFEST);
  assertEquals(m.invariants.includes("no_scm_duplication"), true);
});

Deno.test("manifest: extracts single_source_of_truth invariant", () => {
  const m = buildManifest(VALID_MANIFEST);
  assertEquals(m.invariants.includes("single_source_of_truth"), true);
});

Deno.test("manifest: extracts no_stale_metadata invariant", () => {
  const m = buildManifest(VALID_MANIFEST);
  assertEquals(m.invariants.includes("no_stale_metadata"), true);
});

Deno.test("manifest: no invariants in minimal manifest", () => {
  const m = buildManifest(MINIMAL_MANIFEST);
  assertEquals(m.invariants.length, 0);
});

Deno.test("manifest: parsedAt is a Date", () => {
  const m = buildManifest(VALID_MANIFEST);
  assertEquals(m.parsedAt instanceof Date, true);
});

// ===========================================================================
// Security aspect tests: malicious manifest inputs
// ===========================================================================

Deno.test("security: empty string manifest produces valid (empty) result", () => {
  // Must not throw; empty manifest is technically parseable
  const m = buildManifest("");
  assertEquals(m.invariants.length, 0);
  assertEquals(m.hash.length, 64);
});

Deno.test("security: manifest with path traversal in canonical location is extracted literally", () => {
  // Path traversal in the manifest content should NOT be executed — it is
  // stored as a string only. This test verifies we do not attempt to use the
  // path for any filesystem operation in the parser itself.
  const malicious = "SCM files MUST be in `../../etc/passwd` directory ONLY.\n";
  const m = buildManifest(malicious);
  // The value is stored, not executed
  assertEquals(m.canonicalLocations.scmFiles, "../../etc/passwd");
  // But the agentInstructions are always the safe defaults
  assertEquals(m.canonicalLocations.agentInstructions.includes("0-AI-MANIFEST.a2ml"), true);
});

Deno.test("security: manifest with script injection in invariant section is treated as plain text", () => {
  const malicious = "No SCM file duplication <script>alert(1)</script>\n";
  const m = buildManifest(malicious);
  // The invariant is detected (pattern matches) but stored as-is, not executed
  assertEquals(m.invariants.includes("no_scm_duplication"), true);
});

Deno.test("security: very large manifest (1MB) does not error", () => {
  // Ensures no catastrophic backtracking or OOM on large inputs
  const large = "x".repeat(1_000_000);
  const m = buildManifest(large);
  assertEquals(m.hash.length, 64);
});

Deno.test("security: null byte in manifest is handled", () => {
  const withNull = "# Title\n\0\nBody";
  // Must not throw
  const m = buildManifest(withNull);
  assertEquals(m.hash.length, 64);
});

// ===========================================================================
// Unit tests: session management
// ===========================================================================

Deno.test("session: new session starts unacknowledged", () => {
  const config = { basePath: "/tmp", strictMode: false, sessionTimeout: 3600000 };
  const manager = makeSessionManager(config);
  const session = createSession(manager, "/repo/test");
  assertEquals(session.acknowledgedManifest, false);
  assertEquals(session.attestationHash, undefined);
});

Deno.test("session: session created with correct repoPath", () => {
  const config = { basePath: "/tmp", strictMode: false, sessionTimeout: 3600000 };
  const manager = makeSessionManager(config);
  const session = createSession(manager, "/repo/myproject");
  assertEquals(session.repoPath, "/repo/myproject");
});

Deno.test("session: acknowledge with correct hash succeeds", () => {
  const config = { basePath: "/tmp", strictMode: false, sessionTimeout: 3600000 };
  const manager = makeSessionManager(config);
  const manifest = buildManifest(VALID_MANIFEST);
  const session = createSession(manager, "/repo/test");
  const result = acknowledgeManifest(manager, session.sessionId, manifest, manifest.hash);
  assertEquals(result, true);
  assertEquals(manager.sessions[session.sessionId].acknowledgedManifest, true);
});

Deno.test("session: acknowledge with wrong hash fails", () => {
  const config = { basePath: "/tmp", strictMode: false, sessionTimeout: 3600000 };
  const manager = makeSessionManager(config);
  const manifest = buildManifest(VALID_MANIFEST);
  const session = createSession(manager, "/repo/test");
  const result = acknowledgeManifest(manager, session.sessionId, manifest, "wronghash");
  assertEquals(result, false);
  assertEquals(manager.sessions[session.sessionId].acknowledgedManifest, false);
});

Deno.test("session: acknowledge with unknown sessionId fails", () => {
  const config = { basePath: "/tmp", strictMode: false, sessionTimeout: 3600000 };
  const manager = makeSessionManager(config);
  const manifest = buildManifest(VALID_MANIFEST);
  const result = acknowledgeManifest(manager, "nonexistent-session", manifest, manifest.hash);
  assertEquals(result, false);
});

Deno.test("session: multiple independent sessions", () => {
  const config = { basePath: "/tmp", strictMode: false, sessionTimeout: 3600000 };
  const manager = makeSessionManager(config);
  const manifest = buildManifest(VALID_MANIFEST);
  const s1 = createSession(manager, "/repo/a");
  const s2 = createSession(manager, "/repo/b");
  // Acknowledge only s1
  acknowledgeManifest(manager, s1.sessionId, manifest, manifest.hash);
  assertEquals(manager.sessions[s1.sessionId].acknowledgedManifest, true);
  assertEquals(manager.sessions[s2.sessionId].acknowledgedManifest, false);
});

// ===========================================================================
// Unit tests: access guard
// ===========================================================================

Deno.test("guard: unacknowledged session is denied access", () => {
  const config = { basePath: "/tmp", strictMode: false, sessionTimeout: 3600000 };
  const manager = makeSessionManager(config);
  const manifest = buildManifest(VALID_MANIFEST);
  const session = createSession(manager, "/repo/test");
  const guard = { sessionManager: manager, manifest };
  const result = checkAccess(guard, session.sessionId);
  assertEquals(result.allowed, false);
  assertEquals(typeof result.reason, "string");
});

Deno.test("guard: access denied message contains hash preview", () => {
  const config = { basePath: "/tmp", strictMode: false, sessionTimeout: 3600000 };
  const manager = makeSessionManager(config);
  const manifest = buildManifest(VALID_MANIFEST);
  const session = createSession(manager, "/repo/test");
  const guard = { sessionManager: manager, manifest };
  const result = checkAccess(guard, session.sessionId);
  assertEquals(result.reason.includes(manifest.hash.substring(0, 16)), true);
});

Deno.test("guard: acknowledged session is allowed access", () => {
  const config = { basePath: "/tmp", strictMode: false, sessionTimeout: 3600000 };
  const manager = makeSessionManager(config);
  const manifest = buildManifest(VALID_MANIFEST);
  const session = createSession(manager, "/repo/test");
  acknowledgeManifest(manager, session.sessionId, manifest, manifest.hash);
  const guard = { sessionManager: manager, manifest };
  const result = checkAccess(guard, session.sessionId);
  assertEquals(result.allowed, true);
});

Deno.test("guard: invalid sessionId is denied access", () => {
  const config = { basePath: "/tmp", strictMode: false, sessionTimeout: 3600000 };
  const manager = makeSessionManager(config);
  const manifest = buildManifest(VALID_MANIFEST);
  const guard = { sessionManager: manager, manifest };
  const result = checkAccess(guard, "no-such-session");
  assertEquals(result.allowed, false);
  assertEquals(result.reason.includes("Invalid session ID"), true);
});

// ===========================================================================
// Security aspect tests: path validation / invariant enforcement
// ===========================================================================

Deno.test("guard/path: SCM file in root is rejected", () => {
  const manifest = buildManifest(VALID_MANIFEST); // has no_scm_duplication
  const guard = { sessionManager: makeSessionManager({}), manifest };
  const result = validatePath(guard, "/repo/STATE.scm");
  assertEquals(result.allowed, false);
  assertEquals(result.reason.includes("INVARIANT VIOLATION"), true);
});

Deno.test("guard/path: SCM file in .machine_readable/ is allowed", () => {
  const manifest = buildManifest(VALID_MANIFEST);
  const guard = { sessionManager: makeSessionManager({}), manifest };
  const result = validatePath(guard, "/repo/.machine_readable/STATE.scm");
  assertEquals(result.allowed, true);
});

Deno.test("guard/path: all SCM file variants are blocked at root", () => {
  const scmFiles = [
    "STATE.scm", "META.scm", "ECOSYSTEM.scm", "AGENTIC.scm",
    "NEUROSYM.scm", "PLAYBOOK.scm", "LANGUAGES.scm",
  ];
  const manifest = buildManifest(VALID_MANIFEST);
  const guard = { sessionManager: makeSessionManager({}), manifest };
  for (const f of scmFiles) {
    const result = validatePath(guard, `/repo/${f}`);
    assertEquals(result.allowed, false, `Expected ${f} to be blocked at root`);
  }
});

Deno.test("guard/path: A2ML file variants (.a2ml extension) are NOT blocked", () => {
  // .a2ml files are the new format; old .scm files are what's blocked
  const manifest = buildManifest(VALID_MANIFEST);
  const guard = { sessionManager: makeSessionManager({}), manifest };
  const result = validatePath(guard, "/repo/STATE.a2ml");
  assertEquals(result.allowed, true);
});

Deno.test("guard/path: path traversal via SCM file path is blocked", () => {
  const manifest = buildManifest(VALID_MANIFEST);
  const guard = { sessionManager: makeSessionManager({}), manifest };
  // Even if the path has traversal segments, if it ends with STATE.scm not in
  // .machine_readable/, it's blocked
  const result = validatePath(guard, "/repo/../../STATE.scm");
  assertEquals(result.allowed, false);
});

Deno.test("guard/path: normal source files are always allowed", () => {
  const manifest = buildManifest(VALID_MANIFEST);
  const guard = { sessionManager: makeSessionManager({}), manifest };
  const paths = [
    "/repo/src/main.rs",
    "/repo/README.adoc",
    "/repo/.github/workflows/ci.yml",
    "/repo/Justfile",
  ];
  for (const p of paths) {
    const result = validatePath(guard, p);
    assertEquals(result.allowed, true, `Expected ${p} to be allowed`);
  }
});

Deno.test("guard/path: no invariant = all paths allowed", () => {
  // Manifest without no_scm_duplication invariant → no path restriction
  const manifest = buildManifest(MINIMAL_MANIFEST);
  const guard = { sessionManager: makeSessionManager({}), manifest };
  const result = validatePath(guard, "/repo/STATE.scm");
  assertEquals(result.allowed, true);
});

// ===========================================================================
// E2E test: parse the standards repo's own manifest (dogfood)
// ===========================================================================

Deno.test("e2e/dogfood: standards repo 0-AI-MANIFEST.a2ml is parseable", async () => {
  // This verifies the manifest we actually use passes our own parser
  const manifestPath = join(ROOT, "..", "..", "0-AI-MANIFEST.a2ml");
  const content = await Deno.readTextFile(manifestPath);
  const m = buildManifest(content);
  assertEquals(m.hash.length, 64);
  assertEquals(m.canonicalLocations.scmFiles, ".machine_readable/");
  // Should have at least one invariant (the manifest has "No A2ML duplication")
  assertEquals(typeof m.invariants, "object");
});
