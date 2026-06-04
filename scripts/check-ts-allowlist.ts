// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// check-ts-allowlist.ts — Deno port of the inline python3 heredoc that used
// to live in `.github/workflows/governance-reusable.yml` step
// "Check for TypeScript".
//
// Why this file exists: estate language policy bans Python in all repos
// (SaltStack exception removed 2026-01-03). The governance-reusable
// workflow that enforces the policy was itself written in inline Python —
// a self-referential violation, structurally identical to the CSA001
// self-loop fixed in hypatia#328. This script eliminates the violation.
//
// Behaviour MUST stay byte-identical to the previous Python implementation:
//   * Walk every `*.ts` / `*.tsx` file under cwd, skipping dotted dirs.
//   * Allow files in the built-in directory/path allowlist
//     (bindings/tests/scripts/vendor/examples/ffi/benchmarks/cli, plus any
//     segment containing 'vscode' or starting with 'deno-').
//   * Allow specific filename patterns: `*.d.ts`, `mod.ts`, `lsp-server.ts`,
//     `lsp.ts`, `*-lsp.ts`, `*.bench.ts`, `*_bench.ts`.
//   * Load per-repo exemption table from `.claude/CLAUDE.md` heading
//     `TypeScript Exemptions` (regex: `TypeScript [Ee]xemptions`). Table
//     rows have `| \`glob\` | …` shape.
//   * Exit 1 with the formatted error block if any non-exempt files remain;
//     otherwise print the success line.
//
// Permission scope is `--allow-read` only. No network, no env, no write.

const DIR_NAMES_ALLOWED = new Set([
  "bindings", "tests", "test", "scripts",
  "mcp-adapter", "cli", "vendor", "examples", "ffi",
  "node_modules", "benchmarks",
]);

function builtinAllowed(p: string): boolean {
  if (p.endsWith(".d.ts")) return true;
  const base = p.split("/").pop()!;
  if (base === "mod.ts") return true;
  if (
    base === "lsp-server.ts" || base === "lsp_server.ts" || base === "lsp.ts" ||
    base.endsWith("-lsp.ts")
  ) return true;
  if (base.endsWith(".bench.ts") || base.endsWith("_bench.ts")) return true;
  const segs = p.split("/");
  for (let i = 0; i < segs.length - 1; i++) {
    const s = segs[i];
    if (DIR_NAMES_ALLOWED.has(s)) return true;
    if (s.includes("vscode")) return true;
    if (s.startsWith("deno-")) return true;
  }
  return false;
}

function globToRegex(g: string): RegExp {
  // The Python implementation stripped a leading "./" via `.lstrip('./')`
  // which is a multi-char strip (any leading '.' OR '/' character),
  // matching `./foo` -> `foo` and `../foo` -> `foo` alike. The intent
  // (matching the original behaviour) is to normalise leading-path-cruft
  // off the glob before regex-translating it.
  let g2 = g;
  while (g2.length > 0 && (g2[0] === "." || g2[0] === "/")) g2 = g2.slice(1);
  let out = "";
  const regexEsc = ".+(){}[]|^$\\";
  for (const c of g2) {
    if (c === "*") out += ".*";
    else if (c === "?") out += ".";
    else if (regexEsc.includes(c)) out += "\\" + c;
    else out += c;
  }
  return new RegExp("^" + out + "$");
}

interface Exemption { raw: string; rx: RegExp; }

async function loadExemptionsFromClaudeMd(): Promise<Exemption[]> {
  // Layer 2 — heading-table exemptions parsed from `.claude/CLAUDE.md`.
  //
  // Heading regex relaxation (was: literal `TypeScript [Ee]xemptions`):
  // now matches any markdown heading containing the substring sequence
  // (TypeScript|JavaScript|TS|JS|.tsx?) … Exemption(s). Picks up
  // `### TypeScript / JavaScript Exemptions (Approved)` (the
  // affinescript form), the singular `### TypeScript Exemption`, and
  // `.ts` / `.tsx`-mentioning variants. Anchored to a markdown heading
  // prefix so prose mentions of the phrase elsewhere in the file do
  // NOT trigger table parsing.
  //
  // Multi-table support: scans every heading; on hitting any heading
  // that's NOT an exemption-section heading we leave table-mode (the
  // original "break on first heading" was correct for the heredoc but
  // a multi-section file would miss the second exemption table).
  const exemptions: Exemption[] = [];
  let text: string;
  try {
    text = await Deno.readTextFile(".claude/CLAUDE.md");
  } catch {
    return exemptions;
  }
  const tsHeading =
    /^#{1,4}\s+.*(?:TypeScript|JavaScript|TS|JS|\.tsx?)\b[^#\n]*[Ee]xemption/;
  const anyHeading = /^#{1,4}\s/;
  let inTable = false;
  for (const line of text.split("\n")) {
    if (tsHeading.test(line)) {
      inTable = true;
      continue;
    }
    if (inTable && anyHeading.test(line)) {
      // A different heading — leave table mode but keep scanning for
      // another exemption section in the same file.
      inTable = false;
      continue;
    }
    if (inTable && line.startsWith("|")) {
      const m = line.match(/^\|\s*`([^`]+)`/);
      if (m) {
        exemptions.push({ raw: m[1], rx: globToRegex(m[1]) });
      }
    }
  }
  return exemptions;
}

async function loadExemptionsFromAllowlistFile(): Promise<Exemption[]> {
  // Layer 2.5 — optional plain-text allowlist at the repo root.
  // One glob per line. Lines starting with `#` are comments; blank
  // lines are ignored. Decouples gate-pass from documentation prose
  // (the CLAUDE.md heading-table is the documented variant; this
  // file is the typed-infrastructure variant). Both sources merge
  // additively — either alone is sufficient.
  const exemptions: Exemption[] = [];
  let text: string;
  try {
    text = await Deno.readTextFile(".governance-allowlist");
  } catch {
    return exemptions;
  }
  for (const rawLine of text.split("\n")) {
    const line = rawLine.trim();
    if (line === "" || line.startsWith("#")) continue;
    exemptions.push({ raw: line, rx: globToRegex(line) });
  }
  return exemptions;
}

async function loadExemptions(): Promise<Exemption[]> {
  const fromCm = await loadExemptionsFromClaudeMd();
  const fromAllow = await loadExemptionsFromAllowlistFile();
  return [...fromCm, ...fromAllow];
}

function exempt(p: string, exemptions: Exemption[]): boolean {
  for (const e of exemptions) {
    if (e.rx.test(p)) return true;
    let bare = e.raw;
    while (bare.length > 0 && (bare[0] === "." || bare[0] === "/")) bare = bare.slice(1);
    if (p === bare) return true;
    if (e.raw.endsWith("/") && p.startsWith(bare)) return true;
  }
  return false;
}

async function* walkTs(dir: string): AsyncIterable<string> {
  for await (const entry of Deno.readDir(dir)) {
    const name = entry.name;
    // Skip dotfiles/dotted dirs (matching Python's check on path parts).
    if (name.startsWith(".") && name !== "." && name !== "..") continue;
    const full = dir === "." ? name : `${dir}/${name}`;
    if (entry.isDirectory) {
      yield* walkTs(full);
    } else if (entry.isFile) {
      if (name.endsWith(".ts") || name.endsWith(".tsx")) {
        yield full;
      }
    }
  }
}

async function main() {
  const exemptions = await loadExemptions();
  const found: string[] = [];
  for await (const f of walkTs(".")) {
    found.push(f);
  }
  const bad = found
    .filter((f) => !(builtinAllowed(f) || exempt(f, exemptions)))
    .sort();
  if (bad.length > 0) {
    console.log("❌ TypeScript files detected outside the allowlist.\n");
    for (const f of bad) console.log(`  ${f}`);
    console.log("");
    console.log("To resolve, choose one:");
    console.log("  (a) migrate the file to AffineScript");
    console.log("  (b) move to an allowlisted bridge path");
    console.log("  (c) add an entry to a 'TypeScript Exemptions' table in .claude/CLAUDE.md (Layer 2)");
    console.log("  (d) add a line to .governance-allowlist at the repo root (Layer 2.5 — typed infrastructure file)");
    console.log("");
    console.log("See docs/EXEMPTION-MECHANISMS.adoc for the full mechanism reference.");
    if (exemptions.length > 0) {
      console.log(`\n(Currently ${exemptions.length} exemption(s) parsed across both layers.)`);
    }
    Deno.exit(1);
  }
  console.log(`✅ No TypeScript files outside allowlist (${exemptions.length} per-repo exemption(s) parsed across CLAUDE.md + .governance-allowlist).`);
}

if (import.meta.main) {
  await main();
}
