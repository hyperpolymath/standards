// SPDX-License-Identifier: MPL-2.0
// standards#1021 — comment-preservation proof for YAML rewriters (YAML-POLICY §4).
//
//   bun prove.js <file.yml ...>
//
// Runs three rewrite arms over copies of the given files, never the files
// themselves, and reports per arm: comments preserved/total, moved, dropped,
// idempotency (pass 2 byte-equal to pass 1), data equality, and blank-line loss
// (reported, not failed on).
//
//   identity   yq -i '.'                 — the Y-2 no-op rewrite
//   pin-bump   yq -i '... .uses |= sub()' — a realistic Y-2 edit
//   kyaml      yq -o kyaml '.'           — the Y-3 conversion
//
// kubectl's KYAML printer is NOT covered by this harness.
//
// A rewriter that is clean on pass 1 can still move comments on pass 2 (measured:
// yq relocates a comment block in tag-ruleset-canon.yml only on its second run),
// so comments are also compared original -> pass 2 ("pass-2 drift").
//
// data-equal compares the parsed value of original and pass 1 with THIS parser.
// It is reported, not part of the verdict: where it differs the cause may be the
// two parsers disagreeing about the ORIGINAL (measured: a clip block scalar at
// EOF with no final newline), which is not a rewriter defect.
//
// Exit 0: every arm preserves every comment on both passes and is idempotent.
// Exit 1: at least one arm lost, moved or added a comment, or was not idempotent.
// Exit 2: the harness itself is not trustworthy — a calibration control or a
//         mutant was not detected, or there was nothing to measure.
import { compare, comments } from "./oracle.js";
import { parse } from "yaml";
import { mkdtempSync, readFileSync, writeFileSync, copyFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, basename, dirname } from "node:path";

const here = dirname(new URL(import.meta.url).pathname);
const scratch = mkdtempSync(join(process.env.TMPDIR ?? tmpdir(), "yaml-comment-proof-"));
const die = (msg) => { console.error(`HARNESS INVALID: ${msg}`); process.exit(2); };

function yq(args, input) {
  const r = Bun.spawnSync(["yq", ...args], { stdin: input === undefined ? "ignore" : Buffer.from(input) });
  if (r.exitCode !== 0) throw new Error(`yq ${args.join(" ")}: ${r.stderr.toString().trim()}`);
  return r.stdout.toString();
}
function yqInPlace(expr, src) {
  const f = join(scratch, "inplace.yml");
  writeFileSync(f, src);
  yq(["-i", expr, f]);
  return readFileSync(f, "utf8");
}
const PIN = "f".repeat(40);
const ARMS = {
  identity: (s) => yqInPlace(".", s),
  "pin-bump": (s) => yqInPlace(`(.jobs[].steps[]? | select(.uses != null) | .uses) |= sub("@[0-9a-f]{40}$"; "@${PIN}")`, s),
  kyaml: (s) => yq(["-p", "yaml", "-o", "kyaml", "."], s),
};
const dataOf = (s) => JSON.stringify(parse(s, { version: "1.2" }));
const blanks = (s) => s.split("\n").filter((l) => l.trim() === "").length;

console.log(`yq: ${yq(["--version"]).trim()}`);

// ── 1. Calibration: the oracle must pass a known-good pair and fail two known-bad ones.
{
  const block = readFileSync(join(here, "fixtures/calibration.block.yml"), "utf8");
  const kyaml = readFileSync(join(here, "fixtures/calibration.kyaml.yml"), "utf8");
  const good = compare(block, kyaml);
  if (good.total < 9 || good.preserved !== good.total) die(`calibration pair not PRESERVED (${good.preserved}/${good.total})`);
  const moved = block.replace("# before-entry comment on jobs\njobs:", "jobs:").replace("permissions:", "# before-entry comment on jobs\npermissions:");
  const m = compare(block, moved);
  if (m.moved.length !== 1 || m.moved[0].from.path !== "jobs") die("calibration MOVED control not detected");
  const dropped = block.replace(" # v4.2.2", "");
  const d = compare(block, dropped);
  if (d.dropped.length !== 1 || d.dropped[0].path !== "jobs/build/steps/0/uses") die("calibration DROPPED control not detected");
  console.log(`calibration: pair ${good.preserved}/${good.total} PRESERVED; moved control red; dropped control red`);
}

// ── 2. Corpus.
const files = process.argv.slice(2);
if (files.length === 0) die("no files given");
const corpus = [];
for (const f of files) {
  const src = readFileSync(f, "utf8");
  try { corpus.push({ f, src, cs: comments(src) }); }
  catch (e) { console.log(`skip (original does not parse): ${f}: ${e.message}`); }
}
const totalComments = corpus.reduce((n, x) => n + x.cs.length, 0);
const pinComments = corpus.reduce((n, x) => n + x.cs.filter((c) => c.kind === "trailing" && /\/uses$/.test(c.path)).length, 0);
if (corpus.length === 0 || totalComments === 0) die(`nothing to measure (files=${corpus.length}, comments=${totalComments})`);
console.log(`corpus: ${corpus.length} files, ${totalComments} comments, ${pinComments} trailing pin comments on uses:`);

// ── 3. Mutants on the real corpus: each must be caught, naming file and path.
{
  const host = corpus.find((x) => x.cs.some((c) => c.kind === "trailing" && /\/uses$/.test(c.path)));
  if (!host) die("no file with a trailing pin comment to mutate");
  const lines = host.src.split("\n");
  const i = lines.findIndex((l) => /^\s*-?\s*uses:\s*\S+@[0-9a-f]{40}\s+#/.test(l));
  const j = lines.findIndex((l, k) => k > i && /^\s+[\w-]+:\s*\S/.test(l) && !l.includes("#"));
  if (i < 0 || j < 0) die("mutant sites not found");
  const cm = lines[i].slice(lines[i].indexOf(" #"));
  const drop = lines.slice(); drop[i] = lines[i].slice(0, lines[i].indexOf(" #"));
  const move = drop.slice(); move[j] = lines[j] + cm;
  for (const [name, mut, want] of [["dropped", drop.join("\n"), "dropped"], ["moved", move.join("\n"), "moved"]]) {
    if (mut === host.src) die(`mutant ${name} is identical to its source`);
    try { yq(["."], mut); comments(mut); } catch (e) { die(`mutant ${name} does not parse: ${e.message}`); }
    const r = compare(host.src, mut);
    const hit = r[want][0];
    if (!hit) die(`mutant ${name} SURVIVED on ${host.f}`);
    const at = want === "moved" ? `${hit.from.path} -> ${hit.to.path}` : hit.path;
    console.log(`mutant ${name}: killed — ${basename(host.f)} ${at}`);
  }
}

// ── 4. Arms.
let red = false;
const rows = [];
const losses = [];
for (const [arm, run] of Object.entries(ARMS)) {
  const t = { total: 0, preserved: 0, moved: 0, dropped: 0, added: 0, drift: 0, idem: 0, data: 0, blank: 0, errors: 0 };
  for (const { f, src } of corpus) {
    let p1, p2;
    try { p1 = run(src); p2 = run(p1); } catch (e) { t.errors++; losses.push({ arm, f, error: e.message }); continue; }
    const r = compare(src, p1);
    t.total += r.total; t.preserved += r.preserved;
    t.moved += r.moved.length; t.dropped += r.dropped.length; t.added += r.added.length;
    const r2 = compare(src, p2);
    t.drift += r2.moved.length + r2.dropped.length + r2.added.length;
    for (const x of r2.moved) losses.push({ arm, f, kind: "pass-2 moved", path: `${x.from.path} (${x.from.kind}) -> ${x.to.path} (${x.to.kind})`, text: x.from.text });
    for (const x of r2.dropped) losses.push({ arm, f, kind: "pass-2 dropped", ...x });
    if (p1 === p2) t.idem++;
    if (arm !== "pin-bump" && dataOf(src) === dataOf(p1)) t.data++;
    t.blank += Math.max(0, blanks(src) - blanks(p1));
    for (const x of r.dropped) losses.push({ arm, f, kind: "dropped", ...x });
    for (const x of r.moved) losses.push({ arm, f, kind: "moved", path: `${x.from.path} (${x.from.kind}) -> ${x.to.path} (${x.to.kind})`, text: x.from.text });
    for (const x of r.added) losses.push({ arm, f, kind: "added", ...x });
  }
  const ok = t.preserved === t.total && t.added === 0 && t.drift === 0 && t.idem === corpus.length && t.errors === 0;
  if (!ok) red = true;
  rows.push([arm, `${t.preserved}/${t.total}`, t.moved, t.dropped, t.added, t.drift, `${t.idem}/${corpus.length}`,
    arm === "pin-bump" ? "n/a" : `${t.data}/${corpus.length}`, t.blank, t.errors, ok ? "PASS" : "FAIL"]);
}
console.log("\n| arm | comments preserved | moved | dropped | added | pass-2 drift | idempotent | data-equal | blank lines lost | errors | verdict |");
console.log("|---|---|---|---|---|---|---|---|---|---|---|");
for (const r of rows) console.log(`| ${r.join(" | ")} |`);
if (losses.length) {
  console.log(`\nfirst losses (of ${losses.length}):`);
  for (const l of losses.slice(0, 40)) console.log(`  [${l.arm}] ${l.kind ?? "error"} ${basename(l.f)} ${l.path ?? ""} ${l.kind ? `(${l.text ?? ""})` : l.error}`);
}
if (process.env.PROOF_JSON) writeFileSync(process.env.PROOF_JSON, JSON.stringify({ rows, losses }, null, 2));
process.exit(red ? 1 : 0);
