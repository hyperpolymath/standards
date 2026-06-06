#!/usr/bin/env -S deno test --allow-read --allow-write --allow-run
// SPDX-License-Identifier: MPL-2.0
// K9 Coordination Protocol — Property-Based Tests
// Generative tests verifying properties across random inputs.
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

import { assertEquals, assert, assertStringIncludes } from "jsr:@std/assert@1";
import { join } from "jsr:@std/path@1";

const GENERATOR = join(import.meta.dirname, "generate.js");

// ---------------------------------------------------------------------------
// Helpers — lightweight property-based generation (no external dep)
// ---------------------------------------------------------------------------

function randomString(minLen = 1, maxLen = 50) {
  const len = minLen + Math.floor(Math.random() * (maxLen - minLen));
  const chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 -_";
  return Array.from({ length: len }, () => chars[Math.floor(Math.random() * chars.length)]).join("");
}

function randomId() {
  const parts = ["no", "use", "require", "ban", "enforce", "check", "verify", "allow"];
  const things = ["typescript", "docker", "npm", "tauri", "python", "go", "mocks", "globals"];
  return `${parts[Math.floor(Math.random() * parts.length)]}-${things[Math.floor(Math.random() * things.length)]}`;
}

function randomSeverity() {
  const sevs = ["critical", "high", "moderate"];
  return sevs[Math.floor(Math.random() * sevs.length)];
}

function generateRandomK9() {
  const numInvariants = 1 + Math.floor(Math.random() * 8);
  const numProtected = Math.floor(Math.random() * 5);

  const invariants = Array.from({ length: numInvariants }, () => {
    const id = randomId();
    return `  - id: ${id}\n    rule: "${randomString(10, 80).replace(/"/g, "'")}"\n    reason: "${randomString(10, 80).replace(/"/g, "'")}"\n    severity: ${randomSeverity()}`;
  }).join("\n");

  const protectedPaths = Array.from({ length: numProtected }, () => {
    const path = `src/${randomString(3, 15).replace(/[^a-z]/g, "")}/`;
    return `  - path: ${path}\n    reason: "${randomString(10, 40).replace(/"/g, "'")}"`;
  }).join("\n");

  const name = randomString(3, 20).replace(/[^a-zA-Z0-9-]/g, "-").toLowerCase();

  return `K9!
# Generated test file
---
metadata:
  schema: k9-coordination
  schema_version: 1.0.0

project:
  name: "${name}"
  description: "${randomString(20, 100).replace(/"/g, "'")}"
  license: PMPL-1.0-or-later

invariants:
${invariants}
${numProtected > 0 ? `\nprotected:\n${protectedPaths}` : ""}
`;
}

// ---------------------------------------------------------------------------
// Property: For ANY valid K9 input, generator exits 0 and produces 9 files
// ---------------------------------------------------------------------------

const NUM_RANDOM_CASES = 25;

Deno.test(`Property: generator succeeds on ${NUM_RANDOM_CASES} random valid K9 inputs`, async () => {
  for (let i = 0; i < NUM_RANDOM_CASES; i++) {
    const tmpDir = await Deno.makeTempDir({ prefix: `k9-prop-${i}-` });
    const k9File = join(tmpDir, "coordination.k9");
    const k9Content = generateRandomK9();
    await Deno.writeTextFile(k9File, k9Content);

    try {
      const cmd = new Deno.Command("deno", {
        args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
        stdout: "piped",
        stderr: "piped",
      });
      const { code, stderr } = await cmd.output();
      const err = new TextDecoder().decode(stderr);

      assertEquals(code, 0, `Case ${i} failed (exit ${code}): ${err}\nK9:\n${k9Content}`);
    } finally {
      await Deno.remove(tmpDir, { recursive: true });
    }
  }
});

// ---------------------------------------------------------------------------
// Property: all invariant IDs in input appear in all generated outputs
// ---------------------------------------------------------------------------

Deno.test("Property: invariant IDs survive generation across all targets", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-prop-ids-" });
  const ids = Array.from({ length: 5 }, () => randomId());
  const uniqueIds = [...new Set(ids)];

  const invariants = uniqueIds.map(id =>
    `  - id: ${id}\n    rule: "Rule for ${id}"\n    reason: "Reason for ${id}"`
  ).join("\n");

  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "id-survival-test"
invariants:
${invariants}
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped", stderr: "piped",
    });
    await cmd.output();

    const targets = [
      "AGENTS.md", ".cursorrules", ".windsurfrules", "GEMINI.md", ".clinerules",
    ];

    for (const target of targets) {
      const content = await Deno.readTextFile(join(tmpDir, target));
      for (const id of uniqueIds) {
        assertStringIncludes(content, id, `${target} missing invariant ID: ${id}`);
      }
    }
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// ---------------------------------------------------------------------------
// Property: output content is deterministic for the same input
// ---------------------------------------------------------------------------

Deno.test("Property: same input always produces identical output", async () => {
  const k9Content = generateRandomK9();

  const results = [];
  for (let run = 0; run < 3; run++) {
    const tmpDir = await Deno.makeTempDir({ prefix: `k9-prop-det-${run}-` });
    const k9File = join(tmpDir, "coordination.k9");
    await Deno.writeTextFile(k9File, k9Content);

    try {
      const cmd = new Deno.Command("deno", {
        args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
        stdout: "piped", stderr: "piped",
      });
      await cmd.output();

      const agents = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));
      results.push(agents);
    } finally {
      await Deno.remove(tmpDir, { recursive: true });
    }
  }

  assertEquals(results[0], results[1], "Run 1 and 2 differ");
  assertEquals(results[1], results[2], "Run 2 and 3 differ");
});

// ---------------------------------------------------------------------------
// Property: project name in K9 always appears in output
// ---------------------------------------------------------------------------

Deno.test("Property: project name always appears in generated output", async () => {
  for (let i = 0; i < 10; i++) {
    const name = `test-${randomString(5, 15).replace(/[^a-z0-9]/g, "")}`;
    const tmpDir = await Deno.makeTempDir({ prefix: "k9-prop-name-" });
    const k9File = join(tmpDir, "coordination.k9");
    await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "${name}"
invariants:
  - id: x
    rule: "x"
    reason: "x"
`);

    try {
      const cmd = new Deno.Command("deno", {
        args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File, "--targets", "codex"],
        stdout: "piped", stderr: "piped",
      });
      await cmd.output();
      const content = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));
      assertStringIncludes(content, name);
    } finally {
      await Deno.remove(tmpDir, { recursive: true });
    }
  }
});
