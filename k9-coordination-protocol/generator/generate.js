#!/usr/bin/env -S deno run --allow-read --allow-write
// SPDX-License-Identifier: PMPL-1.0-or-later
// K9 Coordination Protocol — Generator
// Reads a coordination.k9 file and emits AI tool instruction files.
//
// Usage:
//   deno run --allow-read --allow-write generate.js <path-to-coordination.k9> [--targets t1,t2,...] [--output-dir dir]
//
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

import { parseArgs } from "jsr:@std/cli@1/parse-args";
import { dirname, join, resolve } from "jsr:@std/path@1";
import { ensureDir } from "jsr:@std/fs@1/ensure-dir";

// ---------------------------------------------------------------------------
// K9 Kennel-level parser (YAML-like subset)
// ---------------------------------------------------------------------------

/**
 * Parses a K9 Kennel-level file into a JS object.
 * Handles: scalars, lists (- item), multiline strings (|), nested maps.
 * Does NOT handle Nickel/Yard-level features.
 */
function parseK9Kennel(text) {
  const lines = text.split("\n");
  if (!lines[0]?.trim().startsWith("K9!")) {
    throw new Error("Not a valid K9 file — must start with 'K9!' magic number");
  }

  // Strip magic number, comments, blank lines before --- separator, and --- itself
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
      // No --- separator, content starts immediately
      pastHeader = true;
    }
    if (pastHeader) {
      contentLines.push(lines[i]);
    }
  }

  return parseYamlLikeBlock(contentLines, 0).value;
}

/**
 * Recursively parse indented YAML-like blocks.
 * Returns { value, linesConsumed }.
 */
function parseYamlLikeBlock(lines, baseIndent) {
  const result = {};
  let i = 0;

  while (i < lines.length) {
    const line = lines[i];
    const trimmed = line.trimStart();

    // Skip blank lines and comments
    if (trimmed === "" || trimmed.startsWith("#")) {
      i++;
      continue;
    }

    const currentIndent = line.length - line.trimStart().length;

    // If dedented past our base, we're done with this block
    if (currentIndent < baseIndent) break;

    // List item at current level
    if (trimmed.startsWith("- ")) {
      // We're inside a list — collect all items
      const listResult = parseList(lines.slice(i), currentIndent);
      // This is a bare list, return it directly
      return { value: listResult.value, linesConsumed: i + listResult.linesConsumed };
    }

    // Key-value pair
    const colonIdx = trimmed.indexOf(":");
    if (colonIdx === -1) {
      i++;
      continue;
    }

    const key = trimmed.substring(0, colonIdx).trim();
    const afterColon = trimmed.substring(colonIdx + 1).trim();

    if (afterColon === "|") {
      // Multiline string — collect indented lines
      const mlResult = parseMultilineString(lines.slice(i + 1), currentIndent);
      result[key] = mlResult.value;
      i += 1 + mlResult.linesConsumed;
    } else if (afterColon === "") {
      // Nested block — look ahead to determine if list or map
      const nextNonEmpty = findNextNonEmpty(lines, i + 1);
      if (nextNonEmpty !== null && lines[nextNonEmpty].trimStart().startsWith("- ")) {
        const listResult = parseList(
          lines.slice(nextNonEmpty),
          lines[nextNonEmpty].length - lines[nextNonEmpty].trimStart().length
        );
        result[key] = listResult.value;
        i = nextNonEmpty + listResult.linesConsumed;
      } else {
        // Nested map
        const childIndent = nextNonEmpty !== null
          ? lines[nextNonEmpty].length - lines[nextNonEmpty].trimStart().length
          : currentIndent + 2;
        const nested = parseYamlLikeBlock(lines.slice(i + 1), childIndent);
        result[key] = nested.value;
        i += 1 + nested.linesConsumed;
      }
    } else {
      // Inline value
      result[key] = parseScalar(afterColon);
      i++;
    }
  }

  return { value: result, linesConsumed: i };
}

/**
 * Parse a list of - items, which may contain scalars or nested maps.
 */
function parseList(lines, baseIndent) {
  const items = [];
  let i = 0;

  while (i < lines.length) {
    const line = lines[i];
    const trimmed = line.trimStart();

    if (trimmed === "" || trimmed.startsWith("#")) {
      i++;
      continue;
    }

    const currentIndent = line.length - line.trimStart().length;
    if (currentIndent < baseIndent) break;
    if (currentIndent > baseIndent && !trimmed.startsWith("- ")) {
      // Continuation of previous item (nested content)
      i++;
      continue;
    }

    if (!trimmed.startsWith("- ")) break;

    const itemContent = trimmed.substring(2).trim();

    // Check if this list item has nested content
    const nextLine = findNextNonEmpty(lines, i + 1);
    const nextIndent = nextLine !== null
      ? lines[nextLine].length - lines[nextLine].trimStart().length
      : baseIndent;

    if (itemContent.includes(": ") || itemContent.endsWith(":")) {
      // This list item is a map — parse the item line + any indented children
      const itemLines = [" ".repeat(baseIndent + 2) + itemContent];
      let j = i + 1;
      while (j < lines.length) {
        const jTrimmed = lines[j].trimStart();
        const jIndent = lines[j].length - jTrimmed.length;
        if (jTrimmed === "" || jTrimmed.startsWith("#")) {
          j++;
          continue;
        }
        if (jIndent <= baseIndent) break;
        if (jTrimmed.startsWith("- ") && jIndent === baseIndent) break;
        itemLines.push(lines[j]);
        j++;
      }
      const parsed = parseYamlLikeBlock(itemLines, baseIndent + 2);
      items.push(parsed.value);
      i = j;
    } else if (nextLine !== null && nextIndent > currentIndent + 1 && !lines[nextLine].trimStart().startsWith("- ")) {
      // Nested block under a bare list item
      i++;
      continue;
    } else {
      // Simple scalar item
      items.push(parseScalar(itemContent));
      i++;
    }
  }

  return { value: items, linesConsumed: i };
}

/**
 * Parse a multiline string block (after |).
 */
function parseMultilineString(lines, parentIndent) {
  const collected = [];
  let i = 0;
  let blockIndent = null;

  while (i < lines.length) {
    const line = lines[i];
    const trimmed = line.trimStart();
    const currentIndent = line.length - trimmed.length;

    if (trimmed === "") {
      collected.push("");
      i++;
      continue;
    }

    if (blockIndent === null) {
      if (currentIndent <= parentIndent) break;
      blockIndent = currentIndent;
    }

    if (currentIndent < blockIndent) break;

    collected.push(line.substring(blockIndent));
    i++;
  }

  // Trim trailing empty lines
  while (collected.length > 0 && collected[collected.length - 1] === "") {
    collected.pop();
  }

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
// Markdown rendering — from parsed K9 data to AI instruction content
// ---------------------------------------------------------------------------

function renderMarkdown(data, targetName) {
  const lines = [];
  const project = data.project || {};
  const invariants = data.invariants || [];
  const protectedPaths = data.protected || [];
  const architecture = data.architecture || [];
  const doNotCreate = data.do_not_create || [];
  const terminology = data.terminology || [];
  const ecosystem = data.ecosystem || {};
  const coordination = data.coordination || {};
  const ports = data.ports || {};
  const buildCommands = data.build_commands || {};

  lines.push(`# ${project.name || "Project"} — AI Coordination Rules`);
  lines.push("");
  lines.push(`> **Auto-generated from \`coordination.k9\`** — do not edit directly.`);
  lines.push(`> Re-generate with: \`deno run --allow-read --allow-write generate.js coordination.k9\``);
  lines.push(`> Source of truth: \`coordination.k9\` in repository root.`);
  lines.push("");

  // Project overview
  if (project.description) {
    lines.push(`## Project`);
    lines.push("");
    lines.push(project.description);
    lines.push("");
    if (project.languages) {
      const langs = Array.isArray(project.languages) ? project.languages : [project.languages];
      lines.push(`**Languages:** ${langs.join(", ")}`);
    }
    if (project.license) lines.push(`**License:** ${project.license}`);
    if (project.build_system) lines.push(`**Build system:** ${project.build_system}`);
    if (project.runtime) lines.push(`**Runtime:** ${project.runtime}`);
    lines.push("");
  }

  // Build commands
  if (Object.keys(buildCommands).length > 0) {
    lines.push(`## Build Commands`);
    lines.push("");
    lines.push("| Command | Description |");
    lines.push("|---------|-------------|");
    for (const [cmd, desc] of Object.entries(buildCommands)) {
      lines.push(`| \`${cmd}\` | ${desc} |`);
    }
    lines.push("");
  }

  // Invariants — the critical section
  if (invariants.length > 0) {
    lines.push(`## INVARIANTS — Do Not Violate`);
    lines.push("");
    lines.push("These rules are non-negotiable. Violating them will break the project");
    lines.push("or contradict deliberate architectural decisions.");
    lines.push("");
    for (const inv of invariants) {
      const severity = inv.severity || "critical";
      const marker = severity === "critical" ? "CRITICAL" : severity.toUpperCase();
      lines.push(`### [${marker}] ${inv.id}`);
      lines.push("");
      lines.push(`**Rule:** ${inv.rule}`);
      lines.push("");
      lines.push(`**Why:** ${inv.reason}`);
      lines.push("");
    }
  }

  // Protected paths
  if (protectedPaths.length > 0) {
    lines.push(`## Protected Files and Directories`);
    lines.push("");
    lines.push("Do NOT delete, reorganise, or replace these without explicit user approval:");
    lines.push("");
    lines.push("| Path | Reason |");
    lines.push("|------|--------|");
    for (const p of protectedPaths) {
      lines.push(`| \`${p.path}\` | ${p.reason} |`);
    }
    lines.push("");
  }

  // Architecture decisions
  if (architecture.length > 0) {
    lines.push(`## Architecture Decisions (Deliberate)`);
    lines.push("");
    lines.push("These choices may look unusual but are intentional:");
    lines.push("");
    for (const arch of architecture) {
      lines.push(`### ${arch.id}`);
      lines.push("");
      lines.push(`**Decision:** ${arch.decision}`);
      lines.push("");
      lines.push(`**Why:** ${arch.reason}`);
      if (arch.alternatives_rejected) {
        const alts = Array.isArray(arch.alternatives_rejected)
          ? arch.alternatives_rejected
          : [arch.alternatives_rejected];
        lines.push("");
        lines.push(`**Rejected alternatives:** ${alts.join(", ")}`);
      }
      lines.push("");
    }
  }

  // Do not create
  if (doNotCreate.length > 0) {
    lines.push(`## Do NOT Create`);
    lines.push("");
    lines.push("These files, patterns, or systems must NOT be introduced:");
    lines.push("");
    for (const item of doNotCreate) {
      if (typeof item === "string") {
        lines.push(`- ${item}`);
      } else {
        const label = item.pattern || item.description;
        lines.push(`- **${label}** — ${item.reason}`);
      }
    }
    lines.push("");
  }

  // Terminology
  if (terminology.length > 0) {
    lines.push(`## Terminology`);
    lines.push("");
    lines.push("Use the correct terms for this project:");
    lines.push("");
    for (const term of terminology) {
      const wrong = Array.isArray(term.incorrect) ? term.incorrect : [term.incorrect];
      lines.push(`- Say **"${term.correct}"**, NOT ${wrong.map(w => `"${w}"`).join(", ")}`);
      if (term.context) lines.push(`  - ${term.context}`);
    }
    lines.push("");
  }

  // Ports
  if (Object.keys(ports).length > 0) {
    lines.push(`## Port Assignments`);
    lines.push("");
    lines.push("| Service | Port |");
    lines.push("|---------|------|");
    for (const [service, port] of Object.entries(ports)) {
      lines.push(`| ${service} | ${port} |`);
    }
    lines.push("");
  }

  // Ecosystem
  if (ecosystem.depends_on || ecosystem.consumed_by || ecosystem.related) {
    lines.push(`## Ecosystem Context`);
    lines.push("");
    if (ecosystem.depends_on) {
      lines.push("**Depends on:**");
      for (const dep of ecosystem.depends_on) {
        lines.push(`- **${dep.name}** — ${dep.role}`);
      }
      lines.push("");
    }
    if (ecosystem.consumed_by) {
      lines.push("**Consumed by:**");
      for (const dep of ecosystem.consumed_by) {
        lines.push(`- **${dep.name}** — ${dep.role}`);
      }
      lines.push("");
    }
    if (ecosystem.related) {
      lines.push("**Related projects:**");
      for (const dep of ecosystem.related) {
        lines.push(`- **${dep.name}** — ${dep.role}`);
      }
      lines.push("");
    }
  }

  // Coordination (Phase 2)
  if (coordination.active || coordination.completed || coordination.available) {
    lines.push(`## Active Coordination`);
    lines.push("");
    if (coordination.active) {
      lines.push("**Currently in progress:**");
      lines.push("");
      for (const entry of coordination.active) {
        const status = entry.status || "in-progress";
        lines.push(`- **${entry.agent}**: ${entry.area} (${status})`);
        if (entry.blocker) lines.push(`  - Blocked: ${entry.blocker}`);
      }
      lines.push("");
    }
    if (coordination.completed) {
      lines.push("**Recently completed:**");
      lines.push("");
      for (const entry of coordination.completed) {
        lines.push(`- **${entry.agent}**: ${entry.area} (${entry.completed})`);
      }
      lines.push("");
    }
    if (coordination.available) {
      lines.push("**Available work:**");
      lines.push("");
      for (const entry of coordination.available) {
        lines.push(`- ${typeof entry === "string" ? entry : entry.description}`);
      }
      lines.push("");
    }
  }

  return lines.join("\n");
}

// ---------------------------------------------------------------------------
// Target definitions — where each AI tool expects its config
// ---------------------------------------------------------------------------

const TARGETS = {
  claude: {
    name: "Claude Code",
    path: (dir) => join(dir, ".claude", "CLAUDE.generated.md"),
    header: "<!-- K9 Coordination Protocol — Generated for Claude Code -->",
  },
  copilot: {
    name: "GitHub Copilot",
    path: (dir) => join(dir, ".github", "copilot-instructions.md"),
    header: "<!-- K9 Coordination Protocol — Generated for GitHub Copilot -->",
  },
  codex: {
    name: "OpenAI Codex",
    path: (dir) => join(dir, "AGENTS.md"),
    header: "<!-- K9 Coordination Protocol — Generated for OpenAI Codex -->",
  },
  cursor: {
    name: "Cursor",
    path: (dir) => join(dir, ".cursorrules"),
    header: "<!-- K9 Coordination Protocol — Generated for Cursor -->",
  },
  windsurf: {
    name: "Windsurf (Codeium)",
    path: (dir) => join(dir, ".windsurfrules"),
    header: "<!-- K9 Coordination Protocol — Generated for Windsurf -->",
  },
  gemini: {
    name: "Gemini CLI",
    path: (dir) => join(dir, "GEMINI.md"),
    header: "<!-- K9 Coordination Protocol — Generated for Gemini CLI -->",
  },
  cline: {
    name: "Cline",
    path: (dir) => join(dir, ".clinerules"),
    header: "<!-- K9 Coordination Protocol — Generated for Cline -->",
  },
  junie: {
    name: "JetBrains Junie",
    path: (dir) => join(dir, ".junie", "guidelines.md"),
    header: "<!-- K9 Coordination Protocol — Generated for JetBrains Junie -->",
  },
  amazonq: {
    name: "Amazon Q Developer",
    path: (dir) => join(dir, ".q", "rules", "coordination.md"),
    header: "<!-- K9 Coordination Protocol — Generated for Amazon Q Developer -->",
  },
};

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  const args = parseArgs(Deno.args, {
    string: ["targets", "output-dir"],
    alias: { t: "targets", o: "output-dir" },
  });

  const k9Path = args._[0];
  if (!k9Path) {
    console.error("Usage: generate.js <path-to-coordination.k9> [--targets t1,t2,...] [--output-dir dir]");
    console.error("");
    console.error("Targets: " + Object.keys(TARGETS).join(", "));
    Deno.exit(1);
  }

  const resolvedPath = resolve(String(k9Path));
  const outputDir = args["output-dir"] ? resolve(args["output-dir"]) : dirname(resolvedPath);

  // Parse K9 file
  const raw = await Deno.readTextFile(resolvedPath);
  let data;
  try {
    data = parseK9Kennel(raw);
  } catch (err) {
    console.error(`Error parsing ${resolvedPath}: ${err.message}`);
    Deno.exit(1);
  }

  // Determine which targets to generate
  const selectedTargets = args.targets
    ? args.targets.split(",").map((t) => t.trim().toLowerCase())
    : Object.keys(TARGETS);

  const invalidTargets = selectedTargets.filter((t) => !TARGETS[t]);
  if (invalidTargets.length > 0) {
    console.error(`Unknown targets: ${invalidTargets.join(", ")}`);
    console.error(`Valid targets: ${Object.keys(TARGETS).join(", ")}`);
    Deno.exit(1);
  }

  // Generate each target
  let generated = 0;
  for (const targetKey of selectedTargets) {
    const target = TARGETS[targetKey];
    const filePath = target.path(outputDir);
    const markdown = renderMarkdown(data, targetKey);

    const content = [
      target.header,
      `<!-- Source: coordination.k9 | Generated: ${new Date().toISOString().split("T")[0]} -->`,
      `<!-- Re-generate: deno run --allow-read --allow-write generate.js coordination.k9 -->`,
      "",
      markdown,
    ].join("\n");

    await ensureDir(dirname(filePath));
    await Deno.writeTextFile(filePath, content);
    console.log(`  ✓ ${target.name.padEnd(22)} → ${filePath}`);
    generated++;
  }

  console.log(`\nGenerated ${generated} AI coordination files from ${resolvedPath}`);
}

main();
