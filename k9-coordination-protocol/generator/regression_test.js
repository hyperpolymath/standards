#!/usr/bin/env -S deno test --allow-read --allow-write --allow-run
// SPDX-License-Identifier: PMPL-1.0-or-later
// K9 Coordination Protocol — Regression Tests
//
// Tests for specific bugs that were found and fixed. Each fixed bug becomes
// a permanent test so it can never recur.
//
// Convention: test name starts with issue ref or date, e.g.:
//   "Regression #001 (2026-04-03): ..."
//
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

import { assertEquals, assertStringIncludes, assert } from "jsr:@std/assert@1";
import { join } from "jsr:@std/path@1";
import { existsSync } from "jsr:@std/fs@1/exists";

const GENERATOR = join(import.meta.dirname, "generate.js");

// ---------------------------------------------------------------------------
// Regression #001 (2026-04-03): Deno 2.x std library import change
//
// Bug: Generator used `import { parse as parseArgs } from
//   "https://deno.land/std@0.224.0/cli/parse_args.ts"` which fails on
//   Deno 2.x because the export was renamed.
// Fix: Changed to `import { parseArgs } from "jsr:@std/cli@1/parse-args"`
// Guard: Verify generator loads and executes without import errors.
// ---------------------------------------------------------------------------

Deno.test("Regression #001 (2026-04-03): Deno 2.x std library imports work", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-reg001-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "import-test"
invariants:
  - id: x
    rule: "x"
    reason: "x"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped",
      stderr: "piped",
    });
    const { code, stderr } = await cmd.output();
    const err = new TextDecoder().decode(stderr);

    // Must not fail with import error
    assertEquals(err.includes("does not provide an export"), false,
      "Import error detected — Deno std library imports are broken");
    assertEquals(err.includes("SyntaxError"), false,
      "Syntax error in imports");
    assertEquals(code, 0, `Generator failed: ${err}`);
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Regression #002 (2026-04-03): CRLF line endings don't crash parser
//
// Bug: Windows-origin K9 files with \r\n could cause parser to include
//   \r in key names or values, breaking lookups.
// Fix: Parser handles CRLF via trimStart/trim which strips \r.
// Guard: Verify CRLF file parses correctly and values are clean.
// ---------------------------------------------------------------------------

Deno.test("Regression #002 (2026-04-03): CRLF line endings parse cleanly", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-reg002-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File,
    "K9!\r\n---\r\nproject:\r\n  name: \"crlf-clean\"\r\n  license: PMPL\r\ninvariants:\r\n  - id: crlf-rule\r\n    rule: \"No CRLF issues\"\r\n    reason: \"Windows compat\"\r\n");

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File, "--targets", "codex"],
      stdout: "piped",
      stderr: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 0);

    const content = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));
    assertStringIncludes(content, "crlf-clean");
    assertStringIncludes(content, "crlf-rule");
    // Ensure no \r leaked into output
    assertEquals(content.includes("\r"), false, "Carriage return leaked into output");
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Regression #003 (2026-04-03): Empty invariants list doesn't crash
//
// Bug potential: If a K9 file has `invariants:` with no children, the
//   generator could crash trying to iterate an empty or undefined array.
// Guard: Verify empty invariants section generates without error.
// ---------------------------------------------------------------------------

Deno.test("Regression #003 (2026-04-03): Empty invariants section doesn't crash", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-reg003-" });
  const k9File = join(tmpDir, "coordination.k9");
  // invariants key with no list items — parser may return {} or undefined
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "empty-invariants"

invariants:

protected:
  - path: src/
    reason: "Source code"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File, "--targets", "codex"],
      stdout: "piped",
      stderr: "piped",
    });
    const { code, stderr } = await cmd.output();
    const err = new TextDecoder().decode(stderr);

    // Should not crash — either produce output or skip invariants section
    assertEquals(code, 0, `Crashed on empty invariants: ${err}`);
    assert(existsSync(join(tmpDir, "AGENTS.md")));
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Regression #004 (2026-04-03): Quotes in K9 values don't break markdown
//
// Bug potential: K9 values containing markdown special characters (*, `, |, [, ])
//   could corrupt the generated markdown tables or headings.
// Guard: Special chars in values produce valid output.
// ---------------------------------------------------------------------------

Deno.test("Regression #004 (2026-04-03): Markdown special chars in values don't break output", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-reg004-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "markdown-special"
invariants:
  - id: pipe-test
    rule: "Don't use | in tables"
    reason: "Tables use | as delimiter"
    severity: critical
  - id: backtick-test
    rule: "Code uses backticks"
    reason: "Backtick content preserved"
protected:
  - path: "src/core/"
    reason: "Contains [important] files"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File, "--targets", "codex"],
      stdout: "piped",
      stderr: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 0);

    const content = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));
    // Output should exist and contain the invariant IDs
    assertStringIncludes(content, "pipe-test");
    assertStringIncludes(content, "backtick-test");
    assertStringIncludes(content, "src/core/");
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Regression #005 (2026-04-03): --targets with invalid name gives clear error
//
// Bug potential: Passing `--targets nonexistent` could crash with unhelpful
//   "Cannot read properties of undefined" instead of a clear error message.
// Guard: Invalid target name exits with code 1 and helpful message.
// ---------------------------------------------------------------------------

Deno.test("Regression #005 (2026-04-03): Invalid --targets gives clear error", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-reg005-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "targets-test"
invariants:
  - id: x
    rule: "x"
    reason: "x"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: [
        "run", "--allow-read", "--allow-write", GENERATOR,
        k9File, "--targets", "nonexistent,alsofake",
      ],
      stdout: "piped",
      stderr: "piped",
    });
    const { code, stderr } = await cmd.output();
    const err = new TextDecoder().decode(stderr);

    assertEquals(code, 1, "Should exit 1 on invalid targets");
    assertStringIncludes(err, "Unknown targets", "Should name the invalid targets");
    assertStringIncludes(err, "nonexistent");
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Regression #006 (2026-04-03): Generator doesn't overwrite existing .claude/CLAUDE.md
//
// Bug: Generator writes to .claude/CLAUDE.generated.md (not CLAUDE.md) to avoid
//   clobbering existing project-specific Claude instructions.
// Guard: An existing .claude/CLAUDE.md is NOT overwritten.
// ---------------------------------------------------------------------------

Deno.test("Regression #006 (2026-04-03): Existing .claude/CLAUDE.md is not overwritten", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-reg006-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "no-clobber"
invariants:
  - id: x
    rule: "x"
    reason: "x"
`);

  // Create an existing CLAUDE.md that must not be touched
  const claudeDir = join(tmpDir, ".claude");
  await Deno.mkdir(claudeDir, { recursive: true });
  const existingClaudeMd = join(claudeDir, "CLAUDE.md");
  await Deno.writeTextFile(existingClaudeMd, "# Existing project instructions\nDo not delete me.\n");

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped",
      stderr: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 0);

    // CLAUDE.md must be unchanged
    const preserved = await Deno.readTextFile(existingClaudeMd);
    assertEquals(preserved, "# Existing project instructions\nDo not delete me.\n",
      "Existing CLAUDE.md was modified!");

    // CLAUDE.generated.md should exist separately
    assert(existsSync(join(claudeDir, "CLAUDE.generated.md")));
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});
