// SPDX-License-Identifier: MPL-2.0
// bun test for deed_lint.js (port of deed_lint.py). Run: bun test 1-formats/deed/tools/
import { describe, expect, test } from "bun:test";
import { readdirSync, readFileSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { validate, LintError, SELF_TEST_CASES } from "./deed_lint.js";

const HERE = dirname(fileURLToPath(import.meta.url));
const LINT = join(HERE, "deed_lint.js");
const FIX = join(HERE, "fixtures");
const HDR = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n";

/** Run validate on source; returns null if valid, else the error message. */
function verdict(src, filename) {
  try {
    validate(src, filename);
    return null;
  } catch (e) {
    if (e instanceof LintError) return e.message;
    throw e;
  }
}

// Expected verdicts of the 14 embedded Python self-test cases (name, valid?).
const EXPECTED = [
  ["valid-minimal", true],
  ["valid-nested", true],
  ["valid-booleans-uuid", true],
  ["invalid-equals", false],
  ["invalid-section", false],
  ["invalid-missing-schema", false],
  ["invalid-head", false],
  ["invalid-true-literal", false],
  ["invalid-tab", false],
  ["invalid-escape-u", false],
  ["invalid-trailing", false],
  ["invalid-no-header", false],
  ["valid-quoted-list-symbols-007", true],
  ["invalid-unbalanced", false],
];

describe("embedded self-test cases", () => {
  test("14 cases, names and order match the Python", () => {
    expect(SELF_TEST_CASES.map((c) => [c.name, c.expectOk])).toEqual(EXPECTED);
  });
  for (const [name, ok] of EXPECTED) {
    test(`${name} -> ${ok ? "valid" : "error"}`, () => {
      const c = SELF_TEST_CASES.find((x) => x.name === name);
      expect(c).toBeDefined();
      expect(verdict(c.src) === null).toBe(ok);
    });
  }
});

// fixture filename -> regex the failure message must match (rule it names)
const RULE = {
  "inequals_chora.deed": /'=' as a field separator/,
  "inescape-u_chora.deed": /illegal escape/,
  "inhead_chora.deed": /invalid doc-head/,
  "inmissing-schema_chora.deed": /exactly one :schema-version/,
  "inno-header_chora.deed": /at least one SPDX header/,
  "insection_chora.deed": /expected field/,
  "intab_chora.deed": /HTAB \(tab\)/,
  "intrailing_chora.deed": /trailing content/,
  "intrue-literal_chora.deed": /bare symbol 'true' is forbidden/,
  "inunbalanced_chora.deed": /unbalanced parens/,
};

describe("fixtures/valid", () => {
  for (const f of readdirSync(join(FIX, "valid")).filter((n) => n.endsWith(".deed")).sort()) {
    test(f, () => {
      const p = join(FIX, "valid", f);
      expect(verdict(readFileSync(p, "utf8"), p)).toBeNull();
    });
  }
});

describe("fixtures/invalid", () => {
  const files = readdirSync(join(FIX, "invalid")).filter((n) => n.endsWith(".deed")).sort();
  test("every invalid fixture has a rule mapping", () => {
    expect(files).toEqual(Object.keys(RULE).sort());
  });
  for (const f of files) {
    test(`${f} fails for its named rule`, () => {
      const p = join(FIX, "invalid", f);
      const msg = verdict(readFileSync(p, "utf8"), p);
      expect(msg).not.toBeNull();
      expect(msg).toMatch(RULE[f]);
    });
  }
});

describe("extra rules", () => {
  test("filename dispatch mismatch", () => {
    expect(verdict(HDR + '(repo-deed :schema-version "1")\n', "x_praxis.deed")).toMatch(/doc-head\/filename mismatch/);
  });
  test("filename matching no pattern", () => {
    expect(verdict(HDR + '(repo-deed :schema-version "1")\n', "x.deed")).toMatch(/no deed dispatch pattern/);
  });
  test("estate_chora.deed needs estate-deed", () => {
    expect(verdict(HDR + '(estate-deed :schema-version "1")\n', "estate_chora.deed")).toBeNull();
    expect(verdict(HDR + '(repo-deed :schema-version "1")\n', "estate_chora.deed")).toMatch(/mismatch/);
  });
  test("bare CR rejected", () => {
    expect(verdict(HDR + '(repo-deed\r:schema-version "1")\n')).toMatch(/bare CR/);
  });
  test("CRLF accepted", () => {
    expect(verdict(";; SPDX-License-Identifier: X\r\n(repo-deed\r\n :schema-version \"1\")\r\n")).toBeNull();
  });
  test("no trailing newline accepted", () => {
    expect(verdict(HDR + '(repo-deed :schema-version "1")')).toBeNull();
  });
  test("bad boolean token #true", () => {
    expect(verdict(HDR + '(repo-deed :schema-version "1" :a #true)\n')).toMatch(/booleans are exactly/);
  });
  test("non-string schema-version", () => {
    expect(verdict(HDR + "(repo-deed :schema-version 1)\n")).toMatch(/must be a STRING/);
  });
  test("number followed by identifier chars", () => {
    expect(verdict(HDR + '(repo-deed :schema-version "1" :a 12ab)\n')).toMatch(/malformed token/);
  });
  test("only symbols/lists may be quoted", () => {
    expect(verdict(HDR + '(repo-deed :schema-version "1" :a \'"s")\n')).toMatch(/may be quoted/);
  });
  test("unterminated string", () => {
    expect(verdict(HDR + '(repo-deed :schema-version "1" :a "oops)')).toMatch(/unterminated string/);
  });
  test("error carries line number", () => {
    const e = (() => { try { validate(HDR + "(chora-deed)\n"); } catch (x) { return x; } })();
    expect(e.line).toBe(2);
    expect(String(e)).toMatch(/^line 2: /);
  });
});

/** Spawn `bun deed_lint.js ...args`; returns {code, out}. */
function cli(...args) {
  const r = Bun.spawnSync([process.execPath, LINT, ...args]);
  return { code: r.exitCode, out: r.stdout.toString() + r.stderr.toString() };
}

describe("CLI", () => {
  test("valid fixture exits 0", () => {
    const p = join(FIX, "valid", "minimal_chora.deed");
    const r = cli(p);
    expect(r.code).toBe(0);
    expect(r.out).toContain(`OK   ${p}`);
  });
  test("invalid fixture exits nonzero", () => {
    const p = join(FIX, "invalid", "intab_chora.deed");
    const r = cli(p);
    expect(r.code).not.toBe(0);
    expect(r.out).toContain(`FAIL ${p}: line 2: HTAB`);
  });
  test("mixed files exit nonzero", () => {
    expect(cli(join(FIX, "valid", "minimal_chora.deed"), join(FIX, "invalid", "intab_chora.deed")).code).toBe(1);
  });
  test("missing file exits nonzero", () => {
    expect(cli(join(FIX, "nope_chora.deed")).code).toBe(1);
  });
  test("no args prints usage, exit 2", () => {
    const r = cli();
    expect(r.code).toBe(2);
    expect(r.out).toContain("deed-lint");
  });
  test("--self-test exits 0", () => {
    const r = cli("--self-test");
    expect(r.code).toBe(0);
    expect(r.out).toContain("SELF-TEST OK");
  });
  test("--fixtures exits 0", () => {
    const r = cli("--fixtures", FIX);
    expect(r.code).toBe(0);
    expect(r.out).toContain("FIXTURES OK");
  });
});
