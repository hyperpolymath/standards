// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// manifest_bench.js — Throughput benchmarks for mcp-repo-guardian manifest parsing.
//
// Measures the throughput of core manifest operations:
//   1. Hash computation (SHA-256 on manifest content)
//   2. Canonical location extraction (regex parsing)
//   3. Invariant extraction (line-by-line parsing)
//   4. Full manifest build (all operations combined)
//
// Run with: deno run --allow-read bench/manifest_bench.js
//
// These benchmarks establish a baseline for CI regression detection.
// Network I/O benchmarks (MCP server calls) require a live environment.

import { createHash } from "node:crypto";

// ---------------------------------------------------------------------------
// Benchmark infrastructure
// ---------------------------------------------------------------------------

const ITERS = 50_000;

function bench(name, iters, fn) {
  // Warmup
  for (let i = 0; i < 500; i++) fn();

  const start = performance.now();
  for (let i = 0; i < iters; i++) fn();
  const end = performance.now();

  const totalMs = end - start;
  const nsPerIter = (totalMs * 1_000_000) / iters;
  const itersPerSec = iters / (totalMs / 1_000);

  return { name, iters, totalMs, nsPerIter, itersPerSec };
}

function printResult(r) {
  const ns = r.nsPerIter.toFixed(1).padStart(10);
  const throughput = (r.itersPerSec / 1_000_000).toFixed(3).padStart(8);
  console.log(`  ${r.name.padEnd(50)} ${ns} ns/iter  (${throughput} M/s)`);
}

// ---------------------------------------------------------------------------
// Benchmark subjects — inline manifest parsing logic
// ---------------------------------------------------------------------------

const VALID_MANIFEST = `# STOP - CRITICAL READING REQUIRED

## CANONICAL LOCATIONS

SCM files MUST be in \`.machine_readable/\` directory ONLY.
Bot directives go in \`.bot_directives/\`.

## CORE INVARIANTS

1. No SCM file duplication - root must NOT contain STATE.scm
2. Single source of truth - .machine_readable/ is authoritative
3. No stale metadata - if root files exist, they are OUT OF DATE

## SESSION STARTUP CHECKLIST

- Read THIS file first
- Understand canonical locations
- Know the invariants
`;

function benchHash() {
  return createHash("sha256").update(VALID_MANIFEST).digest("hex");
}

function benchExtractCanonicalLocations() {
  const scmMatch = /SCM files.*?`([^`]+)`/i.exec(VALID_MANIFEST);
  const botMatch = /Bot [Dd]irectives.*?`([^`]+)`/i.exec(VALID_MANIFEST);
  return {
    scmFiles: scmMatch?.[1] ?? ".machine_readable/",
    botDirectives: botMatch?.[1] ?? ".bot_directives/",
  };
}

function benchExtractInvariants() {
  const invariants = [];
  if (/No SCM file duplication/i.test(VALID_MANIFEST)) invariants.push("no_scm_duplication");
  if (/Single source of truth/i.test(VALID_MANIFEST)) invariants.push("single_source_of_truth");
  if (/No stale metadata/i.test(VALID_MANIFEST)) invariants.push("no_stale_metadata");
  return invariants;
}

function benchFullManifestBuild() {
  const hash = createHash("sha256").update(VALID_MANIFEST).digest("hex");
  const scmMatch = /SCM files.*?`([^`]+)`/i.exec(VALID_MANIFEST);
  const botMatch = /Bot [Dd]irectives.*?`([^`]+)`/i.exec(VALID_MANIFEST);
  const invariants = [];
  if (/No SCM file duplication/i.test(VALID_MANIFEST)) invariants.push("no_scm_duplication");
  if (/Single source of truth/i.test(VALID_MANIFEST)) invariants.push("single_source_of_truth");
  if (/No stale metadata/i.test(VALID_MANIFEST)) invariants.push("no_stale_metadata");
  return {
    hash,
    canonicalLocations: {
      scmFiles: scmMatch?.[1] ?? ".machine_readable/",
      botDirectives: botMatch?.[1] ?? ".bot_directives/",
    },
    invariants,
    parsedAt: new Date(),
  };
}

function benchSessionManagerOps() {
  // Simulate a full session lifecycle: create → acknowledge → check access
  const config = { basePath: "/tmp", strictMode: false, sessionTimeout: 3600000 };
  const sessions = {};

  // create session
  const sessionId = `bench-${Math.random()}`;
  sessions[sessionId] = {
    sessionId,
    acknowledgedManifest: false,
    attestationHash: undefined,
    acknowledgedAt: undefined,
    repoPath: "/repo/test",
  };

  // acknowledge
  const hash = createHash("sha256").update(VALID_MANIFEST).digest("hex");
  sessions[sessionId] = { ...sessions[sessionId], acknowledgedManifest: true, attestationHash: hash };

  // check access
  const session = sessions[sessionId];
  return session.acknowledgedManifest;
}

// ---------------------------------------------------------------------------
// Run benchmarks
// ---------------------------------------------------------------------------

console.log("");
console.log("mcp-repo-guardian Manifest Parsing Benchmarks");
console.log("==============================================");
console.log(`Iterations: ${ITERS.toLocaleString()}`);
console.log("");
console.log(`  ${"Benchmark".padEnd(50)} ${"Time".padStart(12)}  Throughput`);
console.log(`  ${"-".repeat(76)}`);

const results = [
  bench("sha256_hash (manifest ~400 bytes)", ITERS, benchHash),
  bench("extract_canonical_locations (2 regex)", ITERS, benchExtractCanonicalLocations),
  bench("extract_invariants (3 regex)", ITERS, benchExtractInvariants),
  bench("full_manifest_build (hash + 5 regex + date)", ITERS, benchFullManifestBuild),
  bench("session_lifecycle (create + ack + check)", ITERS, benchSessionManagerOps),
];

for (const r of results) {
  printResult(r);
}

console.log("");
console.log("Baseline notes:");
console.log("  - full_manifest_build: single-call cost for a complete manifest parse");
console.log("  - sha256_hash: V8 native SHA-256 implementation");
console.log("  - Network/MCP benchmarks require live environment (MCP_GUARDIAN_LIVE=1)");
console.log("");
