#!/usr/bin/env -S deno test --allow-read --allow-write --allow-run
// SPDX-License-Identifier: PMPL-1.0-or-later
// K9 Coordination Protocol — Contract / Invariant Tests
// Verify that declared invariants and contracts actually hold.
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

import { assertEquals, assertStringIncludes, assert } from "jsr:@std/assert@1";
import { join } from "jsr:@std/path@1";
import { existsSync } from "jsr:@std/fs@1/exists";

const GENERATOR = join(import.meta.dirname, "generate.js");

// ---------------------------------------------------------------------------
// Contract: Every generated file MUST reference coordination.k9 as source
// ---------------------------------------------------------------------------

Deno.test("Contract: all generated files reference coordination.k9", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-contract-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "contract-test"
invariants:
  - id: test
    rule: "test"
    reason: "test"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped", stderr: "piped",
    });
    await cmd.output();

    const generatedFiles = [
      join(tmpDir, ".claude", "CLAUDE.generated.md"),
      join(tmpDir, ".github", "copilot-instructions.md"),
      join(tmpDir, "AGENTS.md"),
      join(tmpDir, ".cursorrules"),
      join(tmpDir, ".windsurfrules"),
      join(tmpDir, "GEMINI.md"),
      join(tmpDir, ".clinerules"),
      join(tmpDir, ".junie", "guidelines.md"),
      join(tmpDir, ".q", "rules", "coordination.md"),
    ];

    for (const f of generatedFiles) {
      assert(existsSync(f), `File missing: ${f}`);
      const content = await Deno.readTextFile(f);
      assertStringIncludes(content, "coordination.k9", `${f} must reference coordination.k9`);
      assertStringIncludes(content, "do not edit directly", `${f} must warn against direct edits`);
      assertStringIncludes(content, "Generated", `${f} must state it was generated`);
    }
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Contract: invariants section renders with severity markers
// ---------------------------------------------------------------------------

Deno.test("Contract: critical invariants render with [CRITICAL] marker", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-contract-sev-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "severity-test"
invariants:
  - id: critical-rule
    rule: "Never do X"
    reason: "Because Y"
    severity: critical
  - id: high-rule
    rule: "Prefer A over B"
    reason: "Because C"
    severity: high
  - id: default-rule
    rule: "Default severity"
    reason: "Should be critical"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File, "--targets", "codex"],
      stdout: "piped", stderr: "piped",
    });
    await cmd.output();

    const content = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));
    assertStringIncludes(content, "[CRITICAL] critical-rule");
    assertStringIncludes(content, "[HIGH] high-rule");
    assertStringIncludes(content, "[CRITICAL] default-rule"); // default severity
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Contract: protected paths render in a table
// ---------------------------------------------------------------------------

Deno.test("Contract: protected paths render as table", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-contract-prot-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "protected-test"
invariants:
  - id: x
    rule: "x"
    reason: "x"
protected:
  - path: src/tea/
    reason: "Custom TEA runtime"
  - path: .machine_readable/
    reason: "A2ML state files"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File, "--targets", "cursor"],
      stdout: "piped", stderr: "piped",
    });
    await cmd.output();

    const content = await Deno.readTextFile(join(tmpDir, ".cursorrules"));
    assertStringIncludes(content, "src/tea/");
    assertStringIncludes(content, "Custom TEA runtime");
    assertStringIncludes(content, ".machine_readable/");
    assertStringIncludes(content, "Protected Files");
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Contract: terminology renders with correct/incorrect markers
// ---------------------------------------------------------------------------

Deno.test("Contract: terminology uses correct/incorrect format", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-contract-term-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "terminology-test"
invariants:
  - id: x
    rule: "x"
    reason: "x"
terminology:
  - correct: panels
    incorrect:
      - panes
      - tabs
    context: "Always say panels"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File, "--targets", "gemini"],
      stdout: "piped", stderr: "piped",
    });
    await cmd.output();

    const content = await Deno.readTextFile(join(tmpDir, "GEMINI.md"));
    assertStringIncludes(content, '"panels"');
    assertStringIncludes(content, '"panes"');
    assertStringIncludes(content, '"tabs"');
    assertStringIncludes(content, "Terminology");
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Contract: ecosystem dependencies render
// ---------------------------------------------------------------------------

Deno.test("Contract: ecosystem section renders dependencies", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-contract-eco-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "ecosystem-test"
invariants:
  - id: x
    rule: "x"
    reason: "x"
ecosystem:
  depends_on:
    - name: gossamer
      role: "Desktop backend"
  consumed_by:
    - name: idaptik
      role: "Level editor"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File, "--targets", "codex"],
      stdout: "piped", stderr: "piped",
    });
    await cmd.output();

    const content = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));
    assertStringIncludes(content, "gossamer");
    assertStringIncludes(content, "Desktop backend");
    assertStringIncludes(content, "idaptik");
    assertStringIncludes(content, "Level editor");
    assertStringIncludes(content, "Depends on");
    assertStringIncludes(content, "Consumed by");
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Contract: K9 magic number is enforced
// ---------------------------------------------------------------------------

Deno.test("Contract: files without K9! magic number are rejected", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-contract-magic-" });
  const badFile = join(tmpDir, "bad.k9");

  // No magic number
  await Deno.writeTextFile(badFile, `project:\n  name: "bad"\n`);
  const cmd1 = new Deno.Command("deno", {
    args: ["run", "--allow-read", "--allow-write", GENERATOR, badFile],
    stdout: "piped", stderr: "piped",
  });
  const { code: code1 } = await cmd1.output();
  assertEquals(code1, 1, "Should reject file without K9! magic");

  // Wrong magic number
  await Deno.writeTextFile(badFile, `YAML!\nproject:\n  name: "bad"\n`);
  const cmd2 = new Deno.Command("deno", {
    args: ["run", "--allow-read", "--allow-write", GENERATOR, badFile],
    stdout: "piped", stderr: "piped",
  });
  const { code: code2 } = await cmd2.output();
  assertEquals(code2, 1, "Should reject file with wrong magic");

  await Deno.remove(tmpDir, { recursive: true });
});

// ---------------------------------------------------------------------------
// Contract: all 9 target names are valid and produce distinct files
// ---------------------------------------------------------------------------

Deno.test("Contract: all 9 targets produce files at distinct paths", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-contract-paths-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "path-test"
invariants:
  - id: x
    rule: "x"
    reason: "x"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped", stderr: "piped",
    });
    await cmd.output();

    const expectedPaths = [
      ".claude/CLAUDE.generated.md",
      ".github/copilot-instructions.md",
      "AGENTS.md",
      ".cursorrules",
      ".windsurfrules",
      "GEMINI.md",
      ".clinerules",
      ".junie/guidelines.md",
      ".q/rules/coordination.md",
    ];

    // Verify uniqueness
    const unique = new Set(expectedPaths);
    assertEquals(unique.size, 9, "All 9 paths must be distinct");

    // Verify each exists
    for (const p of expectedPaths) {
      assert(existsSync(join(tmpDir, p)), `Missing: ${p}`);
    }

    // Verify each has the correct tool header
    const headers = new Map([
      [".claude/CLAUDE.generated.md", "Claude Code"],
      [".github/copilot-instructions.md", "GitHub Copilot"],
      ["AGENTS.md", "OpenAI Codex"],
      [".cursorrules", "Cursor"],
      [".windsurfrules", "Windsurf"],
      ["GEMINI.md", "Gemini CLI"],
      [".clinerules", "Cline"],
      [".junie/guidelines.md", "JetBrains Junie"],
      [".q/rules/coordination.md", "Amazon Q Developer"],
    ]);

    for (const [path, toolName] of headers) {
      const content = await Deno.readTextFile(join(tmpDir, path));
      assertStringIncludes(content, toolName, `${path} should identify ${toolName}`);
    }
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});
