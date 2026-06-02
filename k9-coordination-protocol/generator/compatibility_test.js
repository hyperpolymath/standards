#!/usr/bin/env -S deno test --allow-read --allow-write --allow-run
// SPDX-License-Identifier: AGPL-3.0-or-later
// K9 Coordination Protocol — Compatibility Tests
// Verify backward/forward compatibility of K9 schema versions.
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

import { assertEquals, assertStringIncludes, assert } from "jsr:@std/assert@1";
import { join } from "jsr:@std/path@1";
import { existsSync } from "jsr:@std/fs@1/exists";

const GENERATOR = join(import.meta.dirname, "generate.js");

// ---------------------------------------------------------------------------
// Compat: minimal v1.0.0 schema generates correctly
// ---------------------------------------------------------------------------

Deno.test("Compat: v1.0.0 minimal schema", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-compat-min-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
metadata:
  schema: k9-coordination
  schema_version: 1.0.0

project:
  name: "compat-test"
  description: "Minimal v1.0.0"

invariants:
  - id: basic
    rule: "Basic rule"
    reason: "Basic reason"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped", stderr: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 0);
    assert(existsSync(join(tmpDir, "AGENTS.md")));
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Compat: v1.0.0 full schema with all optional sections
// ---------------------------------------------------------------------------

Deno.test("Compat: v1.0.0 full schema with all sections", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-compat-full-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
metadata:
  schema: k9-coordination
  schema_version: 1.0.0
  generated_by: k9-coordination-generator
  last_updated: 2026-04-03

project:
  name: "full-test"
  description: "Full v1.0.0 with all sections"
  license: PMPL-1.0-or-later
  repo: "hyperpolymath/full-test"
  languages:
    - Rust
    - ReScript
  build_system: just
  runtime: deno

build_commands:
  just build: "Full build"
  just test: "Run tests"

invariants:
  - id: no-ts
    rule: "No TypeScript"
    reason: "ReScript only"
    severity: critical
  - id: moderate-rule
    rule: "Prefer X"
    reason: "Because Y"
    severity: moderate

protected:
  - path: src/
    reason: "Source code"
  - path: .machine_readable/
    reason: "A2ML files"

architecture:
  - id: gossamer
    decision: "Gossamer backend"
    reason: "Migrated from Tauri"
    alternatives_rejected:
      - Tauri
      - Electron

do_not_create:
  - pattern: "**/*.ts"
    reason: "TypeScript banned"
  - description: "REST API"
    reason: "Groove only"

terminology:
  - correct: panels
    incorrect:
      - panes
      - tabs
    context: "Always panels"

ports:
  dev: 8000
  api: 3000

ecosystem:
  depends_on:
    - name: gossamer
      role: "Backend"
  consumed_by:
    - name: other-project
      role: "Consumer"
  related:
    - name: sibling
      role: "Sibling"

coordination:
  active:
    - agent: claude
      area: "Main feature"
      started: 2026-04-03
      status: in-progress
  completed:
    - agent: cursor
      area: "CSS refactor"
      completed: 2026-04-02
  available:
    - "Documentation"
    - "Integration tests"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped", stderr: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 0);

    const content = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));

    // Verify all sections rendered
    assertStringIncludes(content, "full-test");
    assertStringIncludes(content, "Build Commands");
    assertStringIncludes(content, "INVARIANTS");
    assertStringIncludes(content, "Protected Files");
    assertStringIncludes(content, "Architecture Decisions");
    assertStringIncludes(content, "Do NOT Create");
    assertStringIncludes(content, "Terminology");
    assertStringIncludes(content, "Port Assignments");
    assertStringIncludes(content, "Ecosystem Context");
    assertStringIncludes(content, "Active Coordination");
    assertStringIncludes(content, "Available work");
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Compat: missing optional sections don't break generation
// ---------------------------------------------------------------------------

Deno.test("Compat: missing optional sections generate cleanly", async () => {
  const optionalSections = [
    "architecture", "do_not_create", "terminology", "ports",
    "ecosystem", "coordination", "build_commands", "protected",
  ];

  // Test with ONLY project + invariants (all optional sections missing)
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-compat-missing-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "bare-bones"
invariants:
  - id: one-rule
    rule: "One rule"
    reason: "One reason"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped", stderr: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 0);

    const content = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));
    assertStringIncludes(content, "bare-bones");
    assertStringIncludes(content, "One rule");

    // Should NOT contain headers for missing optional sections
    for (const section of ["Architecture Decisions", "Do NOT Create",
      "Terminology", "Port Assignments", "Ecosystem Context", "Active Coordination"]) {
      assertEquals(
        content.includes(section), false,
        `Should not render ${section} header when data is absent`
      );
    }
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Compat: unknown future sections are ignored (forward compat)
// ---------------------------------------------------------------------------

Deno.test("Compat: unknown sections are silently ignored (forward compatibility)", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-compat-future-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
metadata:
  schema: k9-coordination
  schema_version: 2.0.0

project:
  name: "future-test"

invariants:
  - id: x
    rule: "x"
    reason: "x"

future_section_v2:
  fancy_feature: "enabled"
  quantum_bits: 42

another_future_thing:
  - wow: "new"
  - such: "future"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped", stderr: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 0, "Future sections should not cause failure");

    const content = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));
    assertStringIncludes(content, "future-test");
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Compat: real-world K9 files parse and generate (backward compat)
// ---------------------------------------------------------------------------

Deno.test("Compat: real ASS coordination.k9 from disk", async () => {
  const assK9 = "/var/mnt/eclipse/repos/airborne-submarine-squadron/coordination.k9";
  if (!existsSync(assK9)) return;

  const tmpDir = await Deno.makeTempDir({ prefix: "k9-compat-ass-" });
  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, assK9, "--output-dir", tmpDir],
      stdout: "piped", stderr: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 0);
    assert(existsSync(join(tmpDir, "AGENTS.md")));
    assert(existsSync(join(tmpDir, ".cursorrules")));
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

Deno.test("Compat: real PanLL coordination.k9 from disk", async () => {
  const panllK9 = "/var/mnt/eclipse/repos/panll/coordination.k9";
  if (!existsSync(panllK9)) return;

  const tmpDir = await Deno.makeTempDir({ prefix: "k9-compat-panll-" });
  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, panllK9, "--output-dir", tmpDir],
      stdout: "piped", stderr: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 0);
    assert(existsSync(join(tmpDir, "AGENTS.md")));
    assert(existsSync(join(tmpDir, ".cursorrules")));
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});
