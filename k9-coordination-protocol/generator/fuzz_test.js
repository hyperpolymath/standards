#!/usr/bin/env -S deno test --allow-read --allow-write --allow-run
// SPDX-License-Identifier: MPL-2.0
// K9 Coordination Protocol — Fuzz Tests
// Feed malformed and adversarial input to find crashes and undefined behaviour.
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

import { assertEquals, assert } from "jsr:@std/assert@1";
import { join } from "jsr:@std/path@1";

const GENERATOR = join(import.meta.dirname, "generate.js");

// Helper to run generator and return exit code (must not crash with unhandled exception)
async function runGenerator(k9Content) {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-fuzz-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, k9Content);
  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped",
      stderr: "piped",
    });
    const { code, stderr } = await cmd.output();
    const err = new TextDecoder().decode(stderr);
    // Should either succeed (0) or fail gracefully (1) — never crash with unhandled error
    assert(code === 0 || code === 1, `Unexpected exit code ${code}: ${err}`);
    return { code, tmpDir };
  } catch {
    return { code: -1, tmpDir };
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
}

// ---------------------------------------------------------------------------
// Fuzz: empty file
// ---------------------------------------------------------------------------

Deno.test("Fuzz: empty file", async () => {
  const { code } = await runGenerator("");
  assertEquals(code, 1);
});

// ---------------------------------------------------------------------------
// Fuzz: only magic number
// ---------------------------------------------------------------------------

Deno.test("Fuzz: only K9! magic number", async () => {
  const { code } = await runGenerator("K9!");
  // Should succeed with empty output, not crash
  assert(code === 0 || code === 1);
});

// ---------------------------------------------------------------------------
// Fuzz: K9! followed by garbage
// ---------------------------------------------------------------------------

Deno.test("Fuzz: K9! followed by binary garbage", async () => {
  const garbage = Array.from({ length: 200 }, () => String.fromCharCode(Math.floor(Math.random() * 256))).join("");
  const { code } = await runGenerator(`K9!\n${garbage}`);
  assert(code === 0 || code === 1);
});

// ---------------------------------------------------------------------------
// Fuzz: deeply nested indentation
// ---------------------------------------------------------------------------

Deno.test("Fuzz: deeply nested indentation (100 levels)", async () => {
  let content = "K9!\n---\n";
  for (let i = 0; i < 100; i++) {
    content += " ".repeat(i * 2) + `level${i}:\n`;
  }
  content += " ".repeat(200) + "value: deep\n";
  const { code } = await runGenerator(content);
  assert(code === 0 || code === 1);
});

// ---------------------------------------------------------------------------
// Fuzz: extremely long lines
// ---------------------------------------------------------------------------

Deno.test("Fuzz: extremely long line (10KB value)", async () => {
  const longValue = "x".repeat(10000);
  const { code } = await runGenerator(`K9!\n---\nproject:\n  name: "${longValue}"\ninvariants:\n  - id: x\n    rule: "x"\n    reason: "x"\n`);
  assert(code === 0 || code === 1);
});

// ---------------------------------------------------------------------------
// Fuzz: special characters in values
// ---------------------------------------------------------------------------

Deno.test("Fuzz: special characters in values", async () => {
  const specials = [
    '`backticks`',
    '<script>alert("xss")</script>',
    '${env.HOME}',
    'line1\nline2',
    '🎯 emoji 🚀',
    'null',
    'undefined',
    'true',
    '0',
    '-1',
    'NaN',
    'Infinity',
    '../../../etc/passwd',
  ];

  for (const special of specials) {
    const escaped = special.replace(/"/g, "'").replace(/\n/g, " ");
    const { code } = await runGenerator(`K9!\n---\nproject:\n  name: "${escaped}"\ninvariants:\n  - id: special\n    rule: "${escaped}"\n    reason: "test"\n`);
    assert(code === 0 || code === 1, `Crashed on special: ${special}`);
  }
});

// ---------------------------------------------------------------------------
// Fuzz: tabs instead of spaces
// ---------------------------------------------------------------------------

Deno.test("Fuzz: tabs instead of spaces for indentation", async () => {
  const { code } = await runGenerator(`K9!\n---\nproject:\n\tname: "tab-test"\ninvariants:\n\t- id: x\n\t\trule: "x"\n\t\treason: "x"\n`);
  assert(code === 0 || code === 1);
});

// ---------------------------------------------------------------------------
// Fuzz: mixed indentation
// ---------------------------------------------------------------------------

Deno.test("Fuzz: mixed tabs and spaces", async () => {
  const { code } = await runGenerator(`K9!\n---\nproject:\n  \tname: "mixed"\n\t  description: "chaos"\n`);
  assert(code === 0 || code === 1);
});

// ---------------------------------------------------------------------------
// Fuzz: duplicate keys
// ---------------------------------------------------------------------------

Deno.test("Fuzz: duplicate keys in same section", async () => {
  const { code } = await runGenerator(`K9!\n---\nproject:\n  name: "first"\n  name: "second"\n  name: "third"\n`);
  assert(code === 0 || code === 1);
});

// ---------------------------------------------------------------------------
// Fuzz: Windows line endings (CRLF)
// ---------------------------------------------------------------------------

Deno.test("Fuzz: Windows CRLF line endings", async () => {
  const { code } = await runGenerator(`K9!\r\n---\r\nproject:\r\n  name: "crlf-test"\r\ninvariants:\r\n  - id: x\r\n    rule: "x"\r\n    reason: "x"\r\n`);
  assert(code === 0 || code === 1);
});

// ---------------------------------------------------------------------------
// Fuzz: 100 random malformed inputs (pure noise after K9!)
// ---------------------------------------------------------------------------

Deno.test("Fuzz: 100 random malformed inputs don't crash", async () => {
  for (let i = 0; i < 100; i++) {
    const numLines = 1 + Math.floor(Math.random() * 20);
    const lines = ["K9!"];
    for (let j = 0; j < numLines; j++) {
      const indent = " ".repeat(Math.floor(Math.random() * 8));
      const lineType = Math.random();
      if (lineType < 0.3) {
        lines.push(`${indent}key${j}: value${j}`);
      } else if (lineType < 0.5) {
        lines.push(`${indent}- item${j}`);
      } else if (lineType < 0.7) {
        lines.push(`${indent}# comment`);
      } else if (lineType < 0.8) {
        lines.push("");
      } else {
        const randomChars = Array.from(
          { length: Math.floor(Math.random() * 40) },
          () => String.fromCharCode(32 + Math.floor(Math.random() * 94))
        ).join("");
        lines.push(`${indent}${randomChars}`);
      }
    }

    const { code } = await runGenerator(lines.join("\n"));
    assert(code === 0 || code === 1, `Fuzz case ${i} crashed`);
  }
});
