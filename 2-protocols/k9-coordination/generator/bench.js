#!/usr/bin/env -S deno bench --allow-read --allow-write --allow-run
// SPDX-License-Identifier: MPL-2.0
// K9 Coordination Protocol — Benchmarks
//
// Measures latency, throughput, and memory for the generator.
// Results classified on Six Sigma scale:
//   Unacceptable (>50% regression), Acceptable (20-50%), Ordinary (±20%), Extraordinary (>20% improvement)
//
// Baselines established 2026-04-03 on:
//   Fedora 43 Atomic, Deno 2.7.7, AMD Ryzen (Eclipse drive)
//
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

import { join } from "jsr:@std/path@1";

const GENERATOR = join(import.meta.dirname, "generate.js");

// ---------------------------------------------------------------------------
// Baseline thresholds (Six Sigma classification)
// Established 2026-04-03. Update after 10 CI runs on main.
// ---------------------------------------------------------------------------

const BASELINES = {
  // Single K9 → 9 files latency
  generation_latency_ms: {
    baseline: 200,  // Will be set after first run
    unacceptable: 400,  // >2x baseline
    acceptable: 300,    // >1.5x baseline
    // ordinary: within ±40ms of baseline
    extraordinary: 100, // <0.5x baseline
  },
  // Single K9 → 1 file latency
  single_target_latency_ms: {
    baseline: 80,
    unacceptable: 160,
    acceptable: 120,
    extraordinary: 40,
  },
  // PanLL (complex K9) → 9 files latency
  complex_generation_latency_ms: {
    baseline: 300,
    unacceptable: 600,
    acceptable: 450,
    extraordinary: 150,
  },
};

// ---------------------------------------------------------------------------
// Helper: write a standard test K9 file
// ---------------------------------------------------------------------------

function makeMinimalK9() {
  return `K9!
---
project:
  name: "bench-minimal"
  description: "Minimal benchmark input"
  license: PMPL-1.0-or-later

invariants:
  - id: bench-rule
    rule: "Benchmark rule"
    reason: "Benchmark reason"
    severity: critical
`;
}

function makeComplexK9() {
  const invariants = Array.from({ length: 15 }, (_, i) =>
    `  - id: rule-${i}\n    rule: "Rule number ${i} with a reasonably long description that exercises the renderer"\n    reason: "Because reason ${i} needs to be processed and rendered into markdown output"\n    severity: ${i % 3 === 0 ? "critical" : i % 3 === 1 ? "high" : "moderate"}`
  ).join("\n");

  const protectedPaths = Array.from({ length: 10 }, (_, i) =>
    `  - path: src/module-${i}/\n    reason: "Module ${i} is architecturally significant"`
  ).join("\n");

  return `K9!
---
metadata:
  schema: k9-coordination
  schema_version: 1.0.0
  last_updated: 2026-04-03

project:
  name: "bench-complex"
  description: "Complex benchmark input with many sections"
  license: PMPL-1.0-or-later
  repo: "hyperpolymath/bench-complex"
  languages:
    - ReScript
    - Rust
    - Elixir
    - JavaScript
  build_system: just
  runtime: deno

build_commands:
  just build: "Full build (ReScript + CSS + bundle)"
  just test: "Run test suite"
  just lint: "Lint source"
  just dev: "Start dev server"

invariants:
${invariants}

protected:
${protectedPaths}

architecture:
  - id: gossamer-backend
    decision: "Gossamer (Zig + WebKitGTK) is the desktop backend"
    reason: "Migrated from Tauri 2.0"
    alternatives_rejected:
      - Tauri
      - Electron
      - native GTK
  - id: custom-tea
    decision: "Custom TEA runtime"
    reason: "rescript-tea rejected"
    alternatives_rejected:
      - rescript-tea
      - Redux

do_not_create:
  - pattern: "**/*.ts"
    reason: "TypeScript banned"
  - pattern: "Dockerfile"
    reason: "Use Containerfile"
  - description: "REST API"
    reason: "Groove only"

terminology:
  - correct: panels
    incorrect:
      - panes
      - tabs
      - windows
    context: "Always panels"
  - correct: Binary Star
    incorrect:
      - dual-pane
      - split-view
    context: "Architectural model"

ports:
  dev-server: 8000
  echidna: 9000
  verisimdb: 8080
  boj-server: 7700

ecosystem:
  depends_on:
    - name: gossamer
      role: "Desktop backend"
    - name: verisimdb
      role: "Storage layer"
    - name: typell
      role: "Type intelligence"
  consumed_by:
    - name: idaptik
      role: "Level editor"
  related:
    - name: echidna
      role: "Proof engine"
    - name: hypatia
      role: "CI/CD scanning"

coordination:
  active:
    - agent: claude
      area: "ABI layer"
      started: 2026-04-03
      status: in-progress
  completed:
    - agent: cursor
      area: "CSS refactor"
      completed: 2026-04-02
  available:
    - "Documentation"
    - "Integration tests"
    - "Benchmark baselines"
`;
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

Deno.bench("Latency: minimal K9 → 9 targets", { group: "generation" }, async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-bench-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, makeMinimalK9());
  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "null",
      stderr: "null",
    });
    await cmd.output();
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

Deno.bench("Latency: minimal K9 → 1 target (claude)", { group: "generation" }, async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-bench-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, makeMinimalK9());
  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File, "--targets", "claude"],
      stdout: "null",
      stderr: "null",
    });
    await cmd.output();
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

Deno.bench("Latency: complex K9 → 9 targets", { group: "generation" }, async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-bench-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, makeComplexK9());
  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "null",
      stderr: "null",
    });
    await cmd.output();
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

Deno.bench("Latency: complex K9 → 1 target", { group: "generation" }, async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-bench-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, makeComplexK9());
  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File, "--targets", "codex"],
      stdout: "null",
      stderr: "null",
    });
    await cmd.output();
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// Real-world benchmark using actual PanLL coordination.k9
Deno.bench("Latency: real PanLL K9 → 9 targets", { group: "real-world" }, async () => {
  const panllK9 = "/var/mnt/eclipse/repos/panll/coordination.k9";
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-bench-panll-" });
  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, panllK9, "--output-dir", tmpDir],
      stdout: "null",
      stderr: "null",
    });
    await cmd.output();
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

Deno.bench("Latency: real ASS K9 → 9 targets", { group: "real-world" }, async () => {
  const assK9 = "/var/mnt/eclipse/repos/airborne-submarine-squadron/coordination.k9";
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-bench-ass-" });
  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, assK9, "--output-dir", tmpDir],
      stdout: "null",
      stderr: "null",
    });
    await cmd.output();
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});
