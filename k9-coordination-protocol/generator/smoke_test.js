#!/usr/bin/env -S deno test --allow-read --allow-write --allow-run
// SPDX-License-Identifier: AGPL-3.0-or-later
// K9 Coordination Protocol — Smoke Tests
// Fast sanity checks (<30s). Run these first.
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

import { assertEquals, assertStringIncludes } from "jsr:@std/assert@1";
import { join } from "jsr:@std/path@1";
import { existsSync } from "jsr:@std/fs@1/exists";

const GENERATOR = join(import.meta.dirname, "generate.js");

Deno.test("Smoke: generator script exists and is readable", () => {
  assertEquals(existsSync(GENERATOR), true, "generate.js not found");
});

Deno.test("Smoke: generator prints usage on no args", async () => {
  const cmd = new Deno.Command("deno", {
    args: ["run", "--allow-read", "--allow-write", GENERATOR],
    stderr: "piped",
    stdout: "piped",
  });
  const { code, stderr } = await cmd.output();
  const err = new TextDecoder().decode(stderr);
  assertEquals(code, 1);
  assertStringIncludes(err, "Usage");
});

Deno.test("Smoke: generator processes a minimal K9 and produces output", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-smoke-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "smoke-test"
invariants:
  - id: x
    rule: "x"
    reason: "x"
`);
  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File, "--targets", "claude"],
      stdout: "piped",
      stderr: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 0);
    assertEquals(existsSync(join(tmpDir, ".claude", "CLAUDE.generated.md")), true);
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

Deno.test("Smoke: spec file exists", () => {
  const specPath = join(import.meta.dirname, "..", "spec", "COORDINATION-K9-SPEC.adoc");
  assertEquals(existsSync(specPath), true, "Spec file missing");
});

Deno.test("Smoke: README exists", () => {
  const readmePath = join(import.meta.dirname, "..", "README.adoc");
  assertEquals(existsSync(readmePath), true, "README missing");
});

Deno.test("Smoke: example K9 file exists and starts with K9!", async () => {
  const examplePath = join(import.meta.dirname, "..", "examples", "minimal.k9");
  assertEquals(existsSync(examplePath), true, "Example file missing");
  const content = await Deno.readTextFile(examplePath);
  assertEquals(content.startsWith("K9!"), true, "Example doesn't start with K9!");
});
