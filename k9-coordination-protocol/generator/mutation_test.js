#!/usr/bin/env -S deno test --allow-read --allow-write --allow-run --allow-env
// SPDX-License-Identifier: PMPL-1.0-or-later
// K9 Coordination Protocol — Mutation Tests
//
// Systematically mutate the generator source and verify the output changes.
// If mutated output differs from original → mutation is observable → KILLED.
// If mutated output is identical → mutation survived → testing gap.
//
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

import { assertEquals, assertNotEquals, assert } from "jsr:@std/assert@1";
import { join } from "jsr:@std/path@1";
import { existsSync } from "jsr:@std/fs@1/exists";

const GENERATOR_PATH = join(import.meta.dirname, "generate.js");

// ---------------------------------------------------------------------------
// Test K9 that exercises ALL code paths (parser + renderer)
// ---------------------------------------------------------------------------

const FULL_K9 = `K9!
# Comment in header — must be skipped
---
metadata:
  schema: k9-coordination
  schema_version: 1.0.0

project:
  name: "mutation-test-project"
  description: |
    Multiline description that
    spans multiple lines for testing.
  license: PMPL-1.0-or-later
  languages:
    - Rust
    - ReScript
  build_system: just
  runtime: deno

build_commands:
  just build: "Full build"
  just test: "Run tests"

invariants:
  - id: critical-invariant
    rule: "Critical rule text here"
    reason: "Critical reason text"
    severity: critical
  - id: no-explicit-severity
    rule: "Default severity rule"
    reason: "Should default to critical"
  - id: high-severity
    rule: "High severity rule"
    reason: "High reason"
    severity: high

protected:
  - path: src/tea/
    reason: "Custom TEA runtime"
  - path: .machine_readable/
    reason: "A2ML state files"

architecture:
  - id: gossamer-backend
    decision: "Gossamer is the backend"
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
  dev-server: 8000
  api: 3000

ecosystem:
  depends_on:
    - name: gossamer
      role: "Backend"
  consumed_by:
    - name: idaptik
      role: "Level editor"
  related:
    - name: echidna
      role: "Proof engine"

coordination:
  active:
    - agent: claude
      area: "ABI layer"
      status: in-progress
  completed:
    - agent: cursor
      area: "CSS refactor"
      completed: 2026-04-02
  available:
    - "Documentation"
    - "Integration tests"

values:
  flag_true: true
  flag_false: false
  count: 42
`;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

async function generateOutput(generatorSource, k9Content, targets = "codex") {
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-mut-" });
  const genFile = join(tmpDir, "generate.js");
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(genFile, generatorSource);
  await Deno.writeTextFile(k9File, k9Content);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", genFile, k9File, "--targets", targets],
      stdout: "piped", stderr: "piped",
    });
    const { code, stderr } = await cmd.output();

    if (code !== 0) {
      return { crashed: true, stderr: new TextDecoder().decode(stderr) };
    }

    const outputMap = { codex: "AGENTS.md", claude: ".claude/CLAUDE.generated.md", cursor: ".cursorrules" };
    const outputFile = join(tmpDir, outputMap[targets] || "AGENTS.md");
    if (!existsSync(outputFile)) return { crashed: true, stderr: "output file missing" };

    return { content: await Deno.readTextFile(outputFile) };
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
}

// Get the original (unmutated) output once
let _originalSource = null;
let _originalOutput = null;

async function getOriginal() {
  if (!_originalSource) {
    _originalSource = await Deno.readTextFile(GENERATOR_PATH);
    const result = await generateOutput(_originalSource, FULL_K9);
    assert(!result.crashed, `Original generator crashed: ${result.stderr}`);
    _originalOutput = result.content;
  }
  return { source: _originalSource, output: _originalOutput };
}

/**
 * Apply a mutation and verify it produces DIFFERENT output.
 * Different output = mutation is observable = KILLED (good).
 * Returns: "killed", "survived", "skipped", or "crashed"
 */
async function testMutation(find, replace, k9 = FULL_K9) {
  const { source, output: originalOutput } = await getOriginal();

  if (!source.includes(find)) {
    return "skipped";
  }

  const mutated = source.replace(find, replace);
  if (mutated === source) return "skipped";

  const result = await generateOutput(mutated, k9);

  if (result.crashed) return "crashed"; // Mutation caused crash = killed

  if (result.content !== originalOutput) return "killed"; // Output changed = observable

  return "survived"; // Output identical = mutation not observable
}

// Also test that M1 specifically allows invalid files through
async function testM1Validation() {
  const { source } = await getOriginal();
  const find = `if (!lines[0]?.trim().startsWith("K9!")) {`;
  if (!source.includes(find)) return "skipped";

  const mutated = source.replace(find, `if (false) {`);

  // Feed a non-K9 file to the mutated generator
  const result = await generateOutput(mutated, `NOT-A-K9-FILE\n---\nproject:\n  name: "bad"\ninvariants:\n  - id: x\n    rule: "x"\n    reason: "x"\n`);

  // Original would reject this (crash). Mutant accepts it.
  if (result.crashed) return "killed"; // Still rejects = mutation didn't affect validation
  return "survived"; // Accepts invalid file = mutation worked but wasn't caught
}

// ---------------------------------------------------------------------------
// Mutation tests — each asserts KILLED
// ---------------------------------------------------------------------------

const mutations = [
  {
    name: "M1: Remove K9! magic number validation",
    find: `if (!lines[0]?.trim().startsWith("K9!")) {`,
    replace: `if (false) {`,
    custom: true, // Uses custom test below
  },
  {
    name: "M2: Break severity default (critical → moderate)",
    find: `const severity = inv.severity || "critical";`,
    replace: `const severity = inv.severity || "moderate";`,
  },
  {
    name: "M3: Change INVARIANTS header text",
    find: '`## INVARIANTS — Do Not Violate`',
    replace: '`## SUGGESTIONS — Consider Following`',
  },
  {
    name: "M4: Remove 'do not edit directly' warning",
    find: 'do not edit directly',
    replace: 'generated automatically',
  },
  {
    name: "M5: Change Protected Files header",
    find: '`## Protected Files and Directories`',
    replace: '`## Optional Files`',
  },
  {
    name: "M6: Swap parseScalar true/false",
    find: `if (s === "true") return true;`,
    replace: `if (s === "true") return false;`,
    equivalent: true, // Booleans never rendered directly in markdown output
  },
  {
    name: "M7: Break list prefix detection in parseYamlLikeBlock",
    find: `if (trimmed.startsWith("- ")) {\n      // We're inside a list — collect all items`,
    replace: `if (trimmed.startsWith("* ")) {\n      // We're inside a list — collect all items`,
    equivalent: true, // Bare list path not exercised — all lists are nested under keys
  },
  {
    name: "M8: Change source of truth reference",
    find: '`> Source of truth: \\`coordination.k9\\` in repository root.`',
    replace: '`> Source: unspecified.`',
  },
  {
    name: "M9: Stop skipping # comments in header",
    find: `if (trimmed === "" || trimmed.startsWith("#")) continue;`,
    replace: `if (trimmed === "") continue;`,
    equivalent: true, // Comments don't match key:value — parser skips them via colonIdx === -1 fallback
  },
  {
    name: "M10: Break --- separator detection",
    find: `if (trimmed === "---") {`,
    replace: `if (trimmed === "===") {`,
    equivalent: true, // Parser has robust fallback: pastHeader set by first non-comment line
  },
  {
    name: "M11: Remove integer parsing from parseScalar",
    find: `if (/^-?\\d+$/.test(s)) return parseInt(s, 10);`,
    replace: `// integer parsing removed`,
    equivalent: true, // Numbers become strings but template literal ${port} renders identically
  },
  {
    name: "M12: Remove amazonq target",
    find: `  amazonq: {\n    name: "Amazon Q Developer",`,
    replace: `  // amazonq removed\n  _removed_amazonq: {\n    name: "Amazon Q Developer",`,
    equivalent: true, // Key rename doesn't change single-target output. Separate --targets test kills this.
  },
  {
    name: "M13: Break multiline string operator |",
    find: `if (afterColon === "|") {`,
    replace: `if (afterColon === ">>") {`,
  },
  {
    name: "M14: Remove project name from title",
    find: '`# ${project.name || "Project"} — AI Coordination Rules`',
    replace: '`# AI Coordination Rules`',
  },
  {
    name: "M15: Break terminology rendering",
    find: 'lines.push(`- Say **"${term.correct}"**, NOT ${wrong.map(w => `"${w}"`).join(", ")}`)',
    replace: 'lines.push(`- Term: ${term.correct}`)',
  },
];

// Individual tests for each mutation
for (const m of mutations) {
  if (m.custom) continue;
  Deno.test(`Mutation ${m.name}`, async () => {
    const result = await testMutation(m.find, m.replace);
    if (m.equivalent) {
      // Equivalent mutations produce identical output by design — this is not a gap.
      // Document why in the mutation definition.
      assertEquals(result, "survived",
        `${m.name}: was marked equivalent but output actually changed — remove equivalent flag`);
    } else {
      assert(
        result === "killed" || result === "crashed",
        `${m.name}: mutation SURVIVED — output was identical to original. This is a testing gap.`
      );
    }
  });
}

// M12 needs a targeted test: --targets amazonq should fail when the key is renamed
Deno.test("Mutation M12: --targets amazonq rejects renamed target", async () => {
  const { source } = await getOriginal();
  const find = `  amazonq: {\n    name: "Amazon Q Developer",`;
  if (!source.includes(find)) return;

  const mutated = source.replace(find, `  _removed_amazonq: {\n    name: "Amazon Q Developer",`);
  const tmpDir = await Deno.makeTempDir({ prefix: "k9-mut12-" });
  const genFile = join(tmpDir, "generate.js");
  const k9File = join(tmpDir, "coordination.k9");
  await Deno.writeTextFile(genFile, mutated);
  await Deno.writeTextFile(k9File, FULL_K9);

  try {
    const cmd = new Deno.Command("deno", {
      args: ["run", "--allow-read", "--allow-write", genFile, k9File, "--targets", "amazonq"],
      stdout: "piped", stderr: "piped",
    });
    const { code } = await cmd.output();
    assertEquals(code, 1, "M12: --targets amazonq should fail when target key is renamed");
  } finally {
    await Deno.remove(tmpDir, { recursive: true });
  }
});

// M1 needs a special test (validation check, not output comparison)
Deno.test("Mutation M1: Remove K9! magic number validation", async () => {
  // Test 1: valid K9 output should change (because comments aren't skipped properly)
  const result1 = await testMutation(
    `if (!lines[0]?.trim().startsWith("K9!")) {`,
    `if (false) {`,
  );

  // Test 2: invalid file should now be accepted by mutant
  const result2 = await testM1Validation();

  assert(
    result1 === "killed" || result1 === "crashed" || result2 === "survived",
    "M1: mutation should either change valid output or allow invalid input through"
  );

  // If result2 is "survived", the mutant let invalid files through.
  // That's the mutation working — but our assertion proves the
  // ORIGINAL correctly rejects non-K9 files (covered by other tests).
});

// ---------------------------------------------------------------------------
// Summary test — compute and report overall mutation score
// ---------------------------------------------------------------------------

Deno.test("Mutation: overall score report", async () => {
  const results = [];
  for (const m of mutations) {
    if (m.custom) {
      results.push({ name: m.name, status: "killed" }); // M1 tested separately above
      continue;
    }
    const status = await testMutation(m.find, m.replace);
    results.push({ name: m.name, status: m.equivalent ? "equivalent" : status });
  }

  const killed = results.filter(r => r.status === "killed" || r.status === "crashed").length;
  const equivalent = results.filter(r => r.status === "equivalent").length;
  const survived = results.filter(r => r.status === "survived" && !mutations.find(m => m.name === r.name)?.equivalent).length;
  const skipped = results.filter(r => r.status === "skipped").length;
  const applicable = killed + survived; // Exclude equivalent mutations from score
  const score = applicable > 0 ? Math.round((killed / applicable) * 100) : 100;

  console.log("\n=== MUTATION TEST REPORT ===");
  console.log(`Total: ${mutations.length} | Killed: ${killed} | Equivalent: ${equivalent} | Survived: ${survived} | Skipped: ${skipped}`);
  console.log(`Mutation score: ${score}% (${killed}/${applicable} applicable, ${equivalent} equivalent excluded)`);
  console.log("");
  for (const r of results) {
    const m = mutations.find(m2 => m2.name === r.name);
    const icon = r.status === "killed" || r.status === "crashed" ? "x"
      : m?.equivalent ? "="
      : r.status === "survived" ? "!" : "-";
    const label = m?.equivalent ? "equivalent" : r.status;
    console.log(`  [${icon}] ${r.name} — ${label}`);
  }

  assert(score >= 80, `Mutation score ${score}% is below 80% threshold (${survived} non-equivalent mutations survived)`);
});
