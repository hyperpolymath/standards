// SPDX-License-Identifier: PMPL-1.0-or-later
// AXEL Protocol - DNS TXT Parser Tests
// Tests the strict parsing behavior of the ReScript AXEL-STS parser.
//
// These tests validate the compiled JavaScript output of AxelSts.res.
// Run after `deno task build` to ensure .res.js files are current.

import { assertEquals } from "jsr:@std/assert@1";
import { join, dirname, fromFileUrl } from "jsr:@std/path@1";

const ROOT = join(dirname(fromFileUrl(import.meta.url)), "..");

// Dynamic import of the compiled ReScript module
const mod = await import(join(ROOT, "src", "AxelSts.res.js"));
const { parse } = mod;

// ReScript compiles result<a, b> to { TAG: "Ok", _0: a } or { TAG: "Error", _0: b }
function isOk(result: { TAG: string }): boolean {
  return result.TAG === "Ok";
}

function isError(result: { TAG: string }): boolean {
  return result.TAG === "Error";
}

function getOkValue(result: { TAG: string; _0: unknown }): unknown {
  return result._0;
}

// --- Valid records ---

Deno.test("parse valid minimal record", () => {
  const result = parse("v=AXEL1; id=20260212T1200Z");
  assertEquals(isOk(result), true);
  const val = getOkValue(result) as { version: string; id: string };
  assertEquals(val.version, "AXEL1");
  assertEquals(val.id, "20260212T1200Z");
});

Deno.test("parse valid record with extra whitespace", () => {
  const result = parse("  v=AXEL1 ;  id=my-policy-id  ");
  assertEquals(isOk(result), true);
  const val = getOkValue(result) as { version: string; id: string };
  assertEquals(val.version, "AXEL1");
  assertEquals(val.id, "my-policy-id");
});

Deno.test("parse valid record with unknown keys (ignored)", () => {
  const result = parse("v=AXEL1; id=test-1; foo=bar; baz=qux");
  assertEquals(isOk(result), true);
  const val = getOkValue(result) as { version: string; id: string };
  assertEquals(val.version, "AXEL1");
  assertEquals(val.id, "test-1");
});

Deno.test("parse valid record with id containing equals sign", () => {
  const result = parse("v=AXEL1; id=base64=encoded=id");
  assertEquals(isOk(result), true);
  const val = getOkValue(result) as { version: string; id: string };
  assertEquals(val.id, "base64=encoded=id");
});

// --- Invalid records: missing version ---

Deno.test("reject: missing version", () => {
  const result = parse("id=test-1");
  assertEquals(isError(result), true);
});

Deno.test("reject: empty payload", () => {
  const result = parse("");
  assertEquals(isError(result), true);
});

Deno.test("reject: whitespace-only payload", () => {
  const result = parse("   ");
  assertEquals(isError(result), true);
});

// --- Invalid records: wrong version ---

Deno.test("reject: wrong version AXEL2", () => {
  const result = parse("v=AXEL2; id=test-1");
  assertEquals(isError(result), true);
});

Deno.test("reject: version without value", () => {
  const result = parse("v=; id=test-1");
  assertEquals(isError(result), true);
});

// --- Invalid records: missing or empty id ---

Deno.test("reject: missing id", () => {
  const result = parse("v=AXEL1");
  assertEquals(isError(result), true);
});

Deno.test("reject: empty id", () => {
  const result = parse("v=AXEL1; id=");
  assertEquals(isError(result), true);
});

Deno.test("reject: whitespace-only id", () => {
  const result = parse("v=AXEL1; id=   ");
  assertEquals(isError(result), true);
});

// --- Must NOT parse full RR lines ---

Deno.test("reject: full RR line (not payload)", () => {
  const result = parse(
    '_axel.example.com. 3600 IN TXT "v=AXEL1; id=test-1"'
  );
  assertEquals(isError(result), true);
});

// --- Must NOT default version ---

Deno.test("reject: no default version when missing", () => {
  const result = parse("id=test-1; mode=enforce");
  assertEquals(isError(result), true);
});
