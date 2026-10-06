#!/usr/bin/env bun
// SPDX-License-Identifier: MPL-2.0
// Port of deed_lint.py to plain JavaScript (bun, zero dependencies).
const DOC = String.raw`deed-lint — conformance validator for the DEED grammar (deed.abnf v1.0.0).

Stdlib-only. Implements the normative grammar faithfully:
  * header = 1* spdx-line                 (";;" SP "SPDX-" …)
  * form   = "(" doc-head 1*(sep (field/clause)) [sep] ")"
  * doc-head ∈ {estate-deed, repo-deed, estate-atlas-deed, praxis-deed}
  * field  = keyword sep value            clause = "(" symbol *(sep (field/clause)) [sep] ")"
  * value  = string / symbol / integer / boolean / uuid5 / quoted / list
  * boolean = #t | #f  (lowercase ONLY; true/false/1/0 are parse errors)
  * uuid5 = %s"#u5" string                (body is the RFC 4122 §4.3 NAME input)
  * escapes exactly {" \\ \n \t}      (\r, \uXXXX and all others INVALID)
  * sep    = 1*(SP / line-end / comment)  (HTAB is INVALID — K9-consistent)
  * symbol = ALPHA *(ALPHA/DIGIT/./"*"//"<"/">"/"="/"!"/"?"/"+"/"-"/"_")
  * quoted = "'" (symbol / list)          (only symbols/lists may be quoted)
Semantic checks (validator-enforced, per grammar):
  * :schema-version STRING exactly once at form top level
  * entire input consumed
  * filename↔doc-head dispatch (estate-first side condition, stem≠"estate")

Usage: deed_lint.js FILE...     exit 0 iff all files conform
       deed_lint.js --self-test
`;

import { readdirSync, readFileSync } from "node:fs";
import { basename, join } from "node:path";

const SYMBOL_START = /^[A-Za-z]$/;
const SYMBOL_CONT = /^[A-Za-z0-9.*/<>=!?+_-]$/;

/** Python-style repr() of a string, used so messages match the Python linter. */
function pyRepr(s) {
  const q = s.includes("'") && !s.includes('"') ? '"' : "'";
  let out = q;
  for (const ch of s) {
    const o = ch.codePointAt(0);
    if (ch === "\\") out += "\\\\";
    else if (ch === q) out += "\\" + q;
    else if (ch === "\n") out += "\\n";
    else if (ch === "\r") out += "\\r";
    else if (ch === "\t") out += "\\t";
    else if (o < 0x20 || o === 0x7f) out += "\\x" + o.toString(16).padStart(2, "0");
    else out += ch;
  }
  return out + q;
}

/** A single conformance failure with a 1-based line number. */
export class LintError extends Error {
  /** Build an error from its message and 1-based line number. */
  constructor(msg, line) {
    super(msg);
    this.name = "LintError";
    this.msg = msg;
    this.line = line;
  }

  /** Render as `line N: message`, like the Python __str__. */
  toString() {
    return `line ${this.line}: ${this.msg}`;
  }
}

/** Cursor over the source text. */
class Lexer {
  /** Create a lexer positioned at the start of `text`. */
  constructor(text) {
    this.t = text;
    this.i = 0;
    this.n = text.length;
  }

  /** 1-based line number of offset `at` (default: current position). */
  line(at = null) {
    const end = at === null ? this.i : at;
    let c = 1;
    for (let k = 0; k < end && k < this.n; k++) if (this.t[k] === "\n") c++;
    return c;
  }

  /** Character at offset i+k, or "" past the end. */
  peek(k = 0) {
    const j = this.i + k;
    return j < this.n ? this.t[j] : "";
  }

  /** token-sep = 1*(SP / line-end / comment). Returns 1 if any separator was seen, else 0. */
  skipSep() {
    let seen = 0;
    while (this.i < this.n) {
      const c = this.t[this.i];
      if (c === " ") {
        this.i += 1;
        seen = 1;
      } else if (c === "\n") {
        this.i += 1;
        seen = 1;
      } else if (c === "\r") {
        if (this.t.slice(this.i, this.i + 2) === "\r\n") {
          this.i += 2;
          seen = 1;
        } else {
          throw new LintError("bare CR is not a line-end (CRLF or LF only)", this.line());
        }
      } else if (c === ";") {
        while (this.i < this.n && this.t[this.i] !== "\r" && this.t[this.i] !== "\n") this.i += 1;
        seen = 1;
      } else {
        break;
      }
    }
    return seen;
  }
}

/** string = DQUOTE *( str-char / escape ) DQUOTE ; exactly 4 escapes. */
function lexString(lx) {
  const start = lx.i;
  const out = [];
  lx.i += 1;
  for (;;) {
    if (lx.i >= lx.n) throw new LintError("unterminated string", lx.line(start));
    const c = lx.t[lx.i];
    if (c === '"') {
      lx.i += 1;
      return ["string", out.join("")];
    }
    if (c === "\\") {
      if (lx.i + 1 >= lx.n) throw new LintError("dangling backslash", lx.line(start));
      const e = lx.t[lx.i + 1];
      if (e !== '"' && e !== "\\" && e !== "n" && e !== "t") {
        throw new LintError(
          `illegal escape \\${pyRepr(e)} — only \\" \\\\ \\n \\t exist in DEED ` +
            "(\\r and \\uXXXX are parse errors; embed non-ASCII as raw UTF-8)",
          lx.line(start),
        );
      }
      out.push(e === "n" ? "\n" : e === "t" ? "\t" : e);
      lx.i += 2;
      continue;
    }
    const o = c.codePointAt(0);
    if (o < 0x20) {
      throw new LintError(
        `raw control character U+${o.toString(16).toUpperCase().padStart(4, "0")} inside string (use legal escapes)`,
        lx.line(),
      );
    }
    out.push(c);
    lx.i += 1;
  }
}

/** Lex an optionally negative decimal integer; returns ["integer", null] or null. */
function lexNumber(lx) {
  const m = /^-?[0-9]+/.exec(lx.t.slice(lx.i));
  if (m) {
    lx.i += m[0].length;
    return ["integer", null];
  }
  return null;
}

/** Lex a symbol (ALPHA followed by symbol-continuation chars); returns ["symbol", s] or null. */
function lexSymbol(lx) {
  if (!SYMBOL_START.test(lx.peek())) return null;
  let j = lx.i + 1;
  while (j < lx.n && SYMBOL_CONT.test(lx.t[j])) j += 1;
  const s = lx.t.slice(lx.i, j);
  lx.i = j;
  return ["symbol", s];
}

/** True if c is an ASCII digit. */
function isDigit(c) {
  return c !== "" && c >= "0" && c <= "9";
}

/** value = string / symbol / integer / boolean / uuid5 / quoted / list */
function lexValue(lx) {
  const c = lx.peek();
  if (c === '"') return lexString(lx);
  if (c === "#") {
    const two = lx.t.slice(lx.i, lx.i + 3);
    if (two.startsWith("#u5")) {
      lx.i += 3;
      if (lx.peek() !== '"') {
        throw new LintError('uuid5 must be followed immediately by a string: #u5"name"', lx.line());
      }
      lexString(lx);
      return ["uuid5", null];
    }
    if (two.slice(0, 2) === "#t" || two.slice(0, 2) === "#f") {
      const nxt = lx.peek(2);
      if (nxt && !" \r\n()".includes(nxt)) {
        const tok = lx.t.slice(lx.i + 2, lx.i + 24).trim().split(/\s+/)[0].slice(0, 20);
        throw new LintError(
          `booleans are exactly #t/#f (got ${pyRepr(two + tok)}); true/false/1/0 are parse errors`,
          lx.line(),
        );
      }
      lx.i += 2;
      return ["boolean", two];
    }
    throw new LintError('unrecognised #-form: only #t, #f, #u5"…" are legal', lx.line());
  }
  if (c === "(") {
    lx.i += 1;
    const items = [];
    for (;;) {
      lx.skipSep();
      if (lx.peek() === ")") {
        lx.i += 1;
        return ["list", items];
      }
      items.push(lexValue(lx));
    }
  }
  if (c === "'") {
    lx.i += 1;
    lx.skipSep();
    const v = lexValue(lx);
    if (v[0] !== "symbol" && v[0] !== "list") {
      throw new LintError(`only symbols and lists may be quoted, not ${v[0]}`, lx.line());
    }
    return ["quoted", v];
  }
  if (c === ":") throw new LintError("stray keyword — a keyword may only lead a field", lx.line());
  if (isDigit(c) || (c === "-" && isDigit(lx.peek(1)))) {
    const v = lexNumber(lx);
    const nxt = lx.peek();
    if (nxt && (SYMBOL_CONT.test(nxt) || /^\p{L}$/u.test(nxt))) {
      throw new LintError("malformed token: number followed by identifier characters", lx.line());
    }
    return v;
  }
  const v = lexSymbol(lx);
  if (v) return v;
  throw new LintError(
    `cannot lex value starting at ${pyRepr(c)} ('=' as a field separator is not a deed)`,
    lx.line(),
  );
}

/** field = ":" keyword sep value, or clause = "(" symbol *(field / clause) ")". */
function lexFieldOrClause(lx) {
  const c = lx.peek();
  if (c === ":") {
    lx.i += 1;
    const kw = lexSymbol(lx);
    if (!kw) throw new LintError("malformed keyword: ':' must be followed by a symbol", lx.line());
    if (!lx.skipSep()) {
      throw new LintError(`keyword :${kw[1]} must be followed by a separator before its value`, lx.line());
    }
    const val = lexValue(lx);
    if (val[0] === "symbol" && ["true", "false", "yes", "no"].includes(val[1])) {
      // Grammar note: "Never true, false, yes, no." These lex as symbols, so the
      // ban is enforced here as a value-level semantic rule.
      throw new LintError(
        `boolean meaning must use #t/#f — bare symbol ${pyRepr(val[1])} is forbidden as a value`,
        lx.line(),
      );
    }
    return ["field", kw[1], val];
  }
  if (c === "(") {
    lx.i += 1;
    const head = lexSymbol(lx);
    if (!head) {
      throw new LintError("clause '(' must be followed immediately by a clause symbol (no separator)", lx.line());
    }
    const items = [];
    for (;;) {
      lx.skipSep();
      if (lx.peek() === ")") {
        lx.i += 1;
        return ["clause", head[1], items];
      }
      if (lx.peek() === "" && lx.i >= lx.n) {
        throw new LintError(`unbalanced parens: clause (${head[1]}) never closes`, lx.line());
      }
      items.push(lexFieldOrClause(lx));
    }
  }
  if (c === "") throw new LintError("unexpected end of input (unbalanced parens)", lx.line());
  throw new LintError(`expected field (':keyword …') or clause ('(symbol …)'), got ${pyRepr(c)}`, lx.line());
}

/** Parse the single top-level form; returns [docHead, items]. No separator after '('. */
function parseForm(lx) {
  if (lx.peek() !== "(") throw new LintError("a deed form must start with '('", lx.line());
  lx.i += 1;
  const head = lexSymbol(lx);
  const heads = ["estate-deed", "repo-deed", "estate-atlas-deed", "praxis-deed"];
  if (!head || !heads.includes(head[1])) {
    const got = head ? head[1] : lx.peek();
    throw new LintError(`invalid doc-head ${pyRepr(got)}; valid heads: ${heads.join(", ")}`, lx.line());
  }
  if (!lx.skipSep()) {
    throw new LintError(`doc-head ${head[1]} must be followed by a separator before the first field`, lx.line());
  }
  const items = [];
  for (;;) {
    lx.skipSep();
    if (lx.peek() === ")") {
      lx.i += 1;
      break;
    }
    if (lx.peek() === "") throw new LintError("unbalanced parens: form never closes", lx.line());
    items.push(lexFieldOrClause(lx));
  }
  const schema = items.filter((it) => it[0] === "field" && it[1] === "schema-version");
  if (schema.length !== 1) {
    throw new LintError(
      `form must carry exactly one :schema-version STRING field (found ${schema.length})`,
      lx.line(),
    );
  }
  if (schema[0][2][0] !== "string") throw new LintError(":schema-version must be a STRING value", lx.line());
  lx.skipSep();
  if (lx.i < lx.n) {
    throw new LintError("trailing content after the form's closing ')' — a deed is exactly one form", lx.line());
  }
  return [head[1], items];
}

/** header = 1*spdx-line; each ";; SPDX-" line must be non-empty. Returns the line count. */
function parseHeader(lx) {
  let count = 0;
  for (;;) {
    if (lx.t.startsWith(";; SPDX-", lx.i)) {
      const eol = lx.t.indexOf("\n", lx.i);
      if (eol === -1) throw new LintError("SPDX header line has no line-end", lx.line());
      const payload = lx.t.slice(lx.i + 8, eol);
      if (!payload.trim()) throw new LintError("SPDX header line is empty after ';; SPDX-'", lx.line());
      lx.i = eol + 1;
      count += 1;
      continue;
    }
    break;
  }
  if (count === 0) {
    throw new LintError(
      "deed must begin with at least one SPDX header line (';; SPDX-License-Identifier: …')",
      lx.line(),
    );
  }
  return count;
}

/** Validate DEED source text. Returns [head, items] on success; throws LintError. */
export function validate(text, filename = null) {
  const tab = text.indexOf("\t");
  if (tab !== -1) {
    let ln = 1;
    for (let k = 0; k < tab; k++) if (text[k] === "\n") ln++;
    throw new LintError("HTAB (tab) is an invalid separator anywhere in a deed (K9-consistent)", ln);
  }
  const lx = new Lexer(text);
  parseHeader(lx);
  lx.skipSep();
  const [head, items] = parseForm(lx);
  if (filename) checkFilenameDispatch(filename, head);
  return [head, items];
}

/** estate-file exact-first; stems may contain dots (split on the final suffix). */
export function checkFilenameDispatch(filename, head) {
  const base = basename(filename);
  let want;
  if (base === "estate_chora.deed") {
    want = "estate-deed";
  } else if (base === "ATLAS.deed") {
    want = "estate-atlas-deed";
  } else if (base.endsWith("_praxis.deed")) {
    want = "praxis-deed";
  } else if (base.endsWith("_chora.deed")) {
    const stem = base.slice(0, -"_chora.deed".length);
    if (stem === "estate") return;
    if (!/^[A-Za-z0-9\-._]+$/.test(stem)) throw new LintError(`illegal deed filename stem ${pyRepr(stem)}`, 1);
    want = "repo-deed";
  } else {
    throw new LintError(
      `filename ${pyRepr(base)} matches no deed dispatch pattern ` +
        "(estate_chora.deed | ATLAS.deed | <stem>_chora.deed | <stem>_praxis.deed)",
      1,
    );
  }
  if (head !== want) {
    throw new LintError(`doc-head/filename mismatch: ${base} dispatches to ${want} but parses as ${head}`, 1);
  }
}

const H = ";; SPDX-License-Identifier: CC-BY-SA-4.0\n";

/** The embedded self-test corpus (same 14 cases, order and expectations as the Python). */
export const SELF_TEST_CASES = [
  ["valid-minimal", true,
    H + '(repo-deed :schema-version "1.0.0" :canonical-name "x" :repo-uuid #u5"github.com/o/x" :beholding-chora #u5"estate/chora")\n'],
  ["valid-nested", true,
    H + '(repo-deed\n  :schema-version "1.0.0"\n:canonical-name "x" ; comment between\n  (lineage :type hub :parent "" :previous-names ()) )\n'],
  ["valid-booleans-uuid", true,
    H + '(repo-deed :schema-version "1.0.0" (status :present #t :ended #f :note "legal escapes: \\n and \\t and \\\\ and \\"q\\""))\n'],
  ["invalid-equals", false,
    H + '(repo-deed :schema-version "1.0.0" :canonical-name = "x")\n'],
  ["invalid-section", false,
    H + '(repo-deed :schema-version "1.0.0"\n[status]\nphase = "active")\n'],
  ["invalid-missing-schema", false,
    H + '(repo-deed :canonical-name "x")\n'],
  ["invalid-head", false,
    H + '(chora-deed :schema-version "1.0.0")\n'],
  ["invalid-true-literal", false,
    H + '(repo-deed :schema-version "1.0.0" (status :present true))\n'],
  ["invalid-tab", false,
    H + '(repo-deed\t:schema-version "1.0.0")\n'],
  ["invalid-escape-u", false,
    H + '(repo-deed :schema-version "1.0.0" (m :s "bad \\u0041"))\n'],
  ["invalid-trailing", false,
    H + '(repo-deed :schema-version "1.0.0") trailing\n'],
  ["invalid-no-header", false,
    '(repo-deed :schema-version "1.0.0")\n'],
  // Python labels this "invalid-string-after-head" then overrides index 12 to
  // this valid case (proves quoted lists, 007 and bare symbols parse).
  ["valid-quoted-list-symbols-007", true,
    H + '(repo-deed :schema-version "not-string-issue" :other 007 :sym github-actions :q \'(a b))\n'],
  ["invalid-unbalanced", false,
    H + '(repo-deed :schema-version "1.0.0" (status :present #t)\n'],
].map(([name, expectOk, src]) => ({ name, expectOk, src }));

/** Validate source; returns [ok, errorString]. */
function tryValidate(src, filename) {
  try {
    validate(src, filename);
    return [true, ""];
  } catch (e) {
    if (e instanceof LintError) return [false, e.toString()];
    throw e;
  }
}

/** Run the embedded corpus, printing PASS/FAIL per case; returns exit code. */
function selfTest() {
  let ok = true;
  for (const { name, expectOk, src } of SELF_TEST_CASES) {
    const [got, err] = tryValidate(src);
    const passed = got === expectOk;
    ok = ok && passed;
    const detail = passed ? "" : `  (expected ${expectOk ? "valid" : "error"}; got ${got ? "valid" : err})`;
    console.log(`${passed ? "PASS " : "FAIL "}${name}${detail}`);
  }
  console.log("SELF-TEST " + (ok ? "OK" : "FAILED"));
  return ok ? 0 : 1;
}

/** valid/ must parse; invalid/ must fail. Returns exit code. */
function fixtures(d) {
  let bad = 0;
  for (const [sub, expect] of [["valid", true], ["invalid", false]]) {
    let names = [];
    try {
      names = readdirSync(join(d, sub)).filter((n) => n.endsWith(".deed")).sort();
    } catch {
      // missing directory: nothing to check (Python glob yields no matches)
    }
    for (const n of names) {
      const f = join(d, sub, n);
      let got, err;
      try {
        [got, err] = tryValidate(readFileSync(f, "utf8"), f);
      } catch (e) {
        got = false;
        err = String(e.message);
      }
      if (got !== expect) bad += 1;
      console.log(`${got === expect ? "PASS " : "FAIL "}${f}` + (got === expect ? "" : `  (unexpected: ${err || "valid"})`));
    }
  }
  return bad ? 1 : 0;
}

/** CLI entry: dispatch on --self-test / --fixtures / file list; returns exit code. */
function main(argv) {
  if (argv.includes("--self-test")) return selfTest();
  if (argv.includes("--fixtures")) {
    const d = argv[argv.indexOf("--fixtures") + 1];
    if (d === undefined) {
      console.log("--fixtures requires a DIR argument");
      return 2;
    }
    const rc = fixtures(d);
    console.log("FIXTURES " + (rc === 0 ? "OK" : "FAILED"));
    return rc;
  }
  const files = argv.filter((a) => !a.startsWith("-"));
  if (files.length === 0) {
    console.log(DOC);
    return 2;
  }
  let bad = 0;
  for (const f of files) {
    try {
      validate(readFileSync(f, "utf8"), f);
      console.log(`OK   ${f}`);
    } catch (e) {
      bad += 1;
      console.log(`FAIL ${f}: ${e instanceof LintError ? e.toString() : e.message}`);
    }
  }
  return bad ? 1 : 0;
}

if (import.meta.main) process.exit(main(process.argv.slice(2)));
