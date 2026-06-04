#!/usr/bin/env -S deno test --allow-read --allow-write
// SPDX-License-Identifier: MPL-2.0
// K9 Coordination Protocol — Generator Tests
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

import { assertEquals, assertThrows, assertStringIncludes } from "jsr:@std/assert@1";
import { join } from "jsr:@std/path@1";
import { existsSync } from "jsr:@std/fs@1/exists";

// ---------------------------------------------------------------------------
// Import the generator internals by re-exporting them
// We need to extract the parser and renderer — quickest way is to import
// the module and test via subprocess for integration tests, and inline
// the parser for unit tests.
// ---------------------------------------------------------------------------

// Since the generator is a script with no exports, we'll test it two ways:
// 1. Unit tests: inline the parser logic (copied, kept in sync)
// 2. Integration tests: run the generator as a subprocess

// --- Parser (extracted for unit testing) ---

function parseK9Kennel(text) {
  const lines = text.split("\n");
  if (!lines[0]?.trim().startsWith("K9!")) {
    throw new Error("Not a valid K9 file — must start with 'K9!' magic number");
  }

  const contentLines = [];
  let pastHeader = false;
  for (let i = 1; i < lines.length; i++) {
    const trimmed = lines[i].trim();
    if (!pastHeader) {
      if (trimmed === "---") {
        pastHeader = true;
        continue;
      }
      if (trimmed === "" || trimmed.startsWith("#")) continue;
      pastHeader = true;
    }
    if (pastHeader) {
      contentLines.push(lines[i]);
    }
  }

  return parseYamlLikeBlock(contentLines, 0).value;
}

function parseYamlLikeBlock(lines, baseIndent) {
  const result = {};
  let i = 0;

  while (i < lines.length) {
    const line = lines[i];
    const trimmed = line.trimStart();
    if (trimmed === "" || trimmed.startsWith("#")) { i++; continue; }
    const currentIndent = line.length - line.trimStart().length;
    if (currentIndent < baseIndent) break;

    if (trimmed.startsWith("- ")) {
      const listResult = parseList(lines.slice(i), currentIndent);
      return { value: listResult.value, linesConsumed: i + listResult.linesConsumed };
    }

    const colonIdx = trimmed.indexOf(":");
    if (colonIdx === -1) { i++; continue; }

    const key = trimmed.substring(0, colonIdx).trim();
    const afterColon = trimmed.substring(colonIdx + 1).trim();

    if (afterColon === "|") {
      const mlResult = parseMultilineString(lines.slice(i + 1), currentIndent);
      result[key] = mlResult.value;
      i += 1 + mlResult.linesConsumed;
    } else if (afterColon === "") {
      const nextNonEmpty = findNextNonEmpty(lines, i + 1);
      if (nextNonEmpty !== null && lines[nextNonEmpty].trimStart().startsWith("- ")) {
        const listResult = parseList(
          lines.slice(nextNonEmpty),
          lines[nextNonEmpty].length - lines[nextNonEmpty].trimStart().length
        );
        result[key] = listResult.value;
        i = nextNonEmpty + listResult.linesConsumed;
      } else {
        const childIndent = nextNonEmpty !== null
          ? lines[nextNonEmpty].length - lines[nextNonEmpty].trimStart().length
          : currentIndent + 2;
        const nested = parseYamlLikeBlock(lines.slice(i + 1), childIndent);
        result[key] = nested.value;
        i += 1 + nested.linesConsumed;
      }
    } else {
      result[key] = parseScalar(afterColon);
      i++;
    }
  }

  return { value: result, linesConsumed: i };
}

function parseList(lines, baseIndent) {
  const items = [];
  let i = 0;

  while (i < lines.length) {
    const line = lines[i];
    const trimmed = line.trimStart();
    if (trimmed === "" || trimmed.startsWith("#")) { i++; continue; }
    const currentIndent = line.length - line.trimStart().length;
    if (currentIndent < baseIndent) break;
    if (currentIndent > baseIndent && !trimmed.startsWith("- ")) { i++; continue; }
    if (!trimmed.startsWith("- ")) break;

    const itemContent = trimmed.substring(2).trim();

    if (itemContent.includes(": ") || itemContent.endsWith(":")) {
      const itemLines = [" ".repeat(baseIndent + 2) + itemContent];
      let j = i + 1;
      while (j < lines.length) {
        const jTrimmed = lines[j].trimStart();
        const jIndent = lines[j].length - jTrimmed.length;
        if (jTrimmed === "" || jTrimmed.startsWith("#")) { j++; continue; }
        if (jIndent <= baseIndent) break;
        if (jTrimmed.startsWith("- ") && jIndent === baseIndent) break;
        itemLines.push(lines[j]);
        j++;
      }
      const parsed = parseYamlLikeBlock(itemLines, baseIndent + 2);
      items.push(parsed.value);
      i = j;
    } else {
      items.push(parseScalar(itemContent));
      i++;
    }
  }

  return { value: items, linesConsumed: i };
}

function parseMultilineString(lines, parentIndent) {
  const collected = [];
  let i = 0;
  let blockIndent = null;

  while (i < lines.length) {
    const line = lines[i];
    const trimmed = line.trimStart();
    const currentIndent = line.length - trimmed.length;
    if (trimmed === "") { collected.push(""); i++; continue; }
    if (blockIndent === null) {
      if (currentIndent <= parentIndent) break;
      blockIndent = currentIndent;
    }
    if (currentIndent < blockIndent) break;
    collected.push(line.substring(blockIndent));
    i++;
  }

  while (collected.length > 0 && collected[collected.length - 1] === "") collected.pop();
  return { value: collected.join("\n"), linesConsumed: i };
}

function findNextNonEmpty(lines, startIdx) {
  for (let i = startIdx; i < lines.length; i++) {
    const trimmed = lines[i].trim();
    if (trimmed !== "" && !trimmed.startsWith("#")) return i;
  }
  return null;
}

function parseScalar(s) {
  if (s.startsWith('"') && s.endsWith('"')) return s.slice(1, -1);
  if (s.startsWith("'") && s.endsWith("'")) return s.slice(1, -1);
  if (s === "true") return true;
  if (s === "false") return false;
  if (s === "null") return null;
  if (/^-?\d+$/.test(s)) return parseInt(s, 10);
  if (/^-?\d+\.\d+$/.test(s)) return parseFloat(s);
  return s;
}

// ---------------------------------------------------------------------------
// Unit Tests — Parser
// ---------------------------------------------------------------------------

Deno.test("Parser: rejects files without K9! magic number", () => {
  assertThrows(() => parseK9Kennel("not a k9 file\nfoo: bar"), Error, "K9!");
});

Deno.test("Parser: parses minimal K9 file", () => {
  const data = parseK9Kennel(`K9!
---
project:
  name: "test-project"
  description: "A test"
`);
  assertEquals(data.project.name, "test-project");
  assertEquals(data.project.description, "A test");
});

Deno.test("Parser: parses scalar types correctly", () => {
  const data = parseK9Kennel(`K9!
---
values:
  string_quoted: "hello world"
  string_bare: hello
  number_int: 42
  number_float: 3.14
  bool_true: true
  bool_false: false
  null_val: null
`);
  assertEquals(data.values.string_quoted, "hello world");
  assertEquals(data.values.string_bare, "hello");
  assertEquals(data.values.number_int, 42);
  assertEquals(data.values.number_float, 3.14);
  assertEquals(data.values.bool_true, true);
  assertEquals(data.values.bool_false, false);
  assertEquals(data.values.null_val, null);
});

Deno.test("Parser: parses simple lists", () => {
  const data = parseK9Kennel(`K9!
---
languages:
  - Rust
  - ReScript
  - Elixir
`);
  assertEquals(data.languages, ["Rust", "ReScript", "Elixir"]);
});

Deno.test("Parser: parses list of maps (invariants)", () => {
  const data = parseK9Kennel(`K9!
---
invariants:
  - id: no-typescript
    rule: "Do not use TypeScript"
    reason: "ReScript preferred"
    severity: critical
  - id: no-docker
    rule: "Use Containerfile"
    reason: "Podman not Docker"
    severity: high
`);
  assertEquals(data.invariants.length, 2);
  assertEquals(data.invariants[0].id, "no-typescript");
  assertEquals(data.invariants[0].rule, "Do not use TypeScript");
  assertEquals(data.invariants[0].severity, "critical");
  assertEquals(data.invariants[1].id, "no-docker");
  assertEquals(data.invariants[1].severity, "high");
});

Deno.test("Parser: parses multiline strings", () => {
  const data = parseK9Kennel(`K9!
---
project:
  description: |
    This is a multiline
    description that spans
    several lines.
  name: "after-multiline"
`);
  assertStringIncludes(data.project.description, "This is a multiline");
  assertStringIncludes(data.project.description, "several lines.");
  assertEquals(data.project.name, "after-multiline");
});

Deno.test("Parser: parses nested maps", () => {
  const data = parseK9Kennel(`K9!
---
metadata:
  schema: k9-coordination
  schema_version: 1.0.0
project:
  name: "nested-test"
`);
  assertEquals(data.metadata.schema, "k9-coordination");
  assertEquals(data.metadata.schema_version, "1.0.0");
  assertEquals(data.project.name, "nested-test");
});

Deno.test("Parser: handles comments in body", () => {
  const data = parseK9Kennel(`K9!
# Header comment
---
# Comment before section
project:
  # Comment inside section
  name: "with-comments"
  # Another comment
  license: PMPL-1.0-or-later
`);
  assertEquals(data.project.name, "with-comments");
  assertEquals(data.project.license, "PMPL-1.0-or-later");
});

Deno.test("Parser: handles K9 without --- separator", () => {
  const data = parseK9Kennel(`K9!
# Just a comment
project:
  name: "no-separator"
`);
  assertEquals(data.project.name, "no-separator");
});

Deno.test("Parser: parses protected paths", () => {
  const data = parseK9Kennel(`K9!
---
protected:
  - path: src/tea/
    reason: "Custom TEA runtime"
  - path: .machine_readable/
    reason: "A2ML state files"
`);
  assertEquals(data.protected.length, 2);
  assertEquals(data.protected[0].path, "src/tea/");
  assertEquals(data.protected[1].path, ".machine_readable/");
});

Deno.test("Parser: parses do_not_create with mixed types", () => {
  const data = parseK9Kennel(`K9!
---
do_not_create:
  - pattern: "**/*.ts"
    reason: "TypeScript banned"
  - description: "REST API parallel to Groove"
    reason: "Groove is the protocol"
`);
  assertEquals(data.do_not_create.length, 2);
  assertEquals(data.do_not_create[0].pattern, "**/*.ts");
  assertEquals(data.do_not_create[1].description, "REST API parallel to Groove");
});

Deno.test("Parser: parses terminology", () => {
  const data = parseK9Kennel(`K9!
---
terminology:
  - correct: panels
    incorrect:
      - panes
      - tabs
    context: "PanLL uses panels"
`);
  assertEquals(data.terminology.length, 1);
  assertEquals(data.terminology[0].correct, "panels");
  assertEquals(data.terminology[0].incorrect, ["panes", "tabs"]);
});

Deno.test("Parser: parses ecosystem dependencies", () => {
  const data = parseK9Kennel(`K9!
---
ecosystem:
  depends_on:
    - name: gossamer
      role: "Desktop backend"
    - name: verisimdb
      role: "Storage layer"
  related:
    - name: echidna
      role: "Proof engine"
`);
  assertEquals(data.ecosystem.depends_on.length, 2);
  assertEquals(data.ecosystem.depends_on[0].name, "gossamer");
  assertEquals(data.ecosystem.related.length, 1);
  assertEquals(data.ecosystem.related[0].name, "echidna");
});

Deno.test("Parser: parses ports as numbers", () => {
  const data = parseK9Kennel(`K9!
---
ports:
  dev-server: 8000
  echidna: 9000
`);
  assertEquals(data.ports["dev-server"], 8000);
  assertEquals(data.ports.echidna, 9000);
});

Deno.test("Parser: parses build_commands", () => {
  const data = parseK9Kennel(`K9!
---
build_commands:
  just build: "Full build"
  just test: "Run tests"
`);
  assertEquals(data.build_commands["just build"], "Full build");
  assertEquals(data.build_commands["just test"], "Run tests");
});

// ---------------------------------------------------------------------------
// Integration Tests — Generator subprocess
// ---------------------------------------------------------------------------

const GENERATOR = join(import.meta.dirname, "generate.js");

Deno.test("Integration: generator rejects missing argument", async () => {
  const cmd = new Deno.Command("deno", {
    args: ["run", "--allow-read", "--allow-write", GENERATOR],
    stderr: "piped",
    stdout: "piped",
  });
  const { code } = await cmd.output();
  assertEquals(code, 1);
});

Deno.test("Integration: generator rejects non-K9 file", async () => {
  const tmpFile = await Deno.makeTempFile({ suffix: ".k9" });
  await Deno.writeTextFile(tmpFile, "not a k9 file\nfoo: bar");
  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, tmpFile],
      stderr: "piped",
      stdout: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 1);
  } finally {
    await Deno.remove(tmpFile);
  }
});

Deno.test("Integration: generates all 9 targets from minimal K9", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-coord-test-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
metadata:
  schema: k9-coordination
  schema_version: 1.0.0

project:
  name: "test-project"
  description: "Integration test project"
  license: PMPL-1.0-or-later

invariants:
  - id: test-rule
    rule: "Test rule"
    reason: "Test reason"
    severity: critical

protected:
  - path: src/
    reason: "Source code"
`);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped",
      stderr: "piped",
    });
    const { code, stdout } = await cmd.output();
    const output = new TextDecoder().decode(stdout);

    assertEquals(code, 0);
    assertStringIncludes(output, "Generated 9 AI coordination files");

    // Check all expected files exist
    const expectedFiles = [
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

    for (const f of expectedFiles) {
      assertEquals(existsSync(f), true, `Missing: ${f}`);
    }

    // Verify content consistency — all files should have the same project name
    for (const f of expectedFiles) {
      const content = await Deno.readTextFile(f);
      assertStringIncludes(content, "test-project", `${f} missing project name`);
      assertStringIncludes(content, "Test rule", `${f} missing invariant rule`);
      assertStringIncludes(content, "Test reason", `${f} missing invariant reason`);
      assertStringIncludes(content, "coordination.k9", `${f} missing source reference`);
    }
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

Deno.test("Integration: --targets flag limits output", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-coord-test-" });
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
        "run", "--allow-read", "--allow-write",
        GENERATOR, k9File, "--targets", "claude,cursor",
      ],
      stdout: "piped",
      stderr: "piped",
    });
    const { code, stdout } = await cmd.output();
    const output = new TextDecoder().decode(stdout);

    assertEquals(code, 0);
    assertStringIncludes(output, "Generated 2 AI coordination files");

    // Only claude and cursor should exist
    assertEquals(existsSync(join(tmpDir, ".claude", "CLAUDE.generated.md")), true);
    assertEquals(existsSync(join(tmpDir, ".cursorrules")), true);
    assertEquals(existsSync(join(tmpDir, "AGENTS.md")), false);
    assertEquals(existsSync(join(tmpDir, "GEMINI.md")), false);
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

Deno.test("Integration: generates correctly from real ASS coordination.k9", async () => {
  const assK9 = "/var/mnt/eclipse/repos/airborne-submarine-squadron/coordination.k9";
  if (!existsSync(assK9)) {
    console.log("  (skipped — ASS coordination.k9 not found)");
    return;
  }

  const tmpDir = await Deno.makeTempDir({ prefix: "k9-coord-ass-" });
  try {
    const cmd = new Deno.Command("deno", {
      args: [
        "run", "--allow-read", "--allow-write",
        GENERATOR, assK9, "--output-dir", tmpDir,
      ],
      stdout: "piped",
      stderr: "piped",
    });
    const { code, stdout } = await cmd.output();
    const output = new TextDecoder().decode(stdout);

    assertEquals(code, 0);
    assertStringIncludes(output, "Generated 9 AI coordination files");

    const agents = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));
    assertStringIncludes(agents, "Airborne Submarine Squadron");
    assertStringIncludes(agents, "no-typescript");
    assertStringIncludes(agents, "AGPL-3.0-or-later");
    assertStringIncludes(agents, "6860");
    assertStringIncludes(agents, "AffineScript");
    assertStringIncludes(agents, "attack submarine");
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

Deno.test("Integration: generates correctly from real PanLL coordination.k9", async () => {
  const panllK9 = "/var/mnt/eclipse/repos/panll/coordination.k9";
  if (!existsSync(panllK9)) {
    console.log("  (skipped — PanLL coordination.k9 not found)");
    return;
  }

  const tmpDir = await Deno.makeTempDir({ prefix: "k9-coord-panll-" });
  try {
    const cmd = new Deno.Command("deno", {
      args: [
        "run", "--allow-read", "--allow-write",
        GENERATOR, panllK9, "--output-dir", tmpDir,
      ],
      stdout: "piped",
      stderr: "piped",
    });
    const { code, stdout } = await cmd.output();
    const output = new TextDecoder().decode(stdout);

    assertEquals(code, 0);
    assertStringIncludes(output, "Generated 9 AI coordination files");

    const agents = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));
    assertStringIncludes(agents, "PanLL");
    assertStringIncludes(agents, "custom-tea-runtime");
    assertStringIncludes(agents, "Binary Star");
    assertStringIncludes(agents, "Anti-Crash");
    assertStringIncludes(agents, "Vexometer");
    assertStringIncludes(agents, "OrbitalSync");
    assertStringIncludes(agents, "panels");
    assertStringIncludes(agents, "rescript-tea");
    assertStringIncludes(agents, "8000");
    assertStringIncludes(agents, "gossamer");

    // Verify terminology section rendered
    assertStringIncludes(agents, '"panels"');
    assertStringIncludes(agents, '"panes"');
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

Deno.test("Integration: idempotent — running twice produces identical output", async () => {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-coord-idem-" });
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(k9File, `K9!
---
project:
  name: "idempotent-test"
  description: "Should be identical on re-run"
invariants:
  - id: rule-1
    rule: "Rule one"
    reason: "Reason one"
protected:
  - path: src/
    reason: "Source"
`);

  try {
    // First run
    const run1 = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped", stderr: "piped",
    });
    await run1.output();
    const firstContent = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));

    // Second run
    const run2 = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", GENERATOR, k9File],
      stdout: "piped", stderr: "piped",
    });
    await run2.output();
    const secondContent = await Deno.readTextFile(join(tmpDir, "AGENTS.md"));

    // Content should be identical (date is same day, so should match)
    assertEquals(firstContent, secondContent);
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});
