// SPDX-License-Identifier: MPL-2.0
// Comment-association oracle for standards#1021 (YAML-POLICY §4).
//
// Every comment is recorded as (node path, position, text) and two documents are
// compared on that triple. A comment that survives but re-attaches to another
// node is a FAILURE (MOVED), not a pass: presence-only comparison is what #1021
// rejects.
//
// Parser: eemeli/yaml, deliberately independent of go-yaml (which yq and kubectl
// share), so a comment-handling bug in the rewriter cannot cancel itself out in
// the checker.
//
// Association is by SOURCE POSITION, not by the AST's comment slots. Measured on
// fixtures/calibration.*: the AST hangs the same comment on different slots in
// block vs flow syntax (a pin comment is `trailing steps/0/uses` in block style
// and `after-collection steps/0` in KYAML), so slot-based association would call
// every KYAML rewrite a move. The positional rule is syntax-independent:
//   trailing — the comment shares a line with the END of a scalar that precedes
//              it; attach to the last such scalar.
//   before   — otherwise, attach to the first scalar (key or value) starting
//              after it.
//   document-end — no scalar follows.
import { parseDocument, Parser, isMap, isSeq, isPair, isScalar } from "yaml";

/** Return a lookup from UTF-16 source offsets to zero-based line numbers. */
function lineIndex(src) {
  const starts = [0];
  for (let i = 0; i < src.length; i++) if (src[i] === "\n") starts.push(i + 1);
  return (off) => {
    let lo = 0, hi = starts.length - 1;
    while (lo < hi) {
      const mid = (lo + hi + 1) >> 1;
      if (starts[mid] <= off) lo = mid; else hi = mid - 1;
    }
    return lo;
  };
}

/**
 * Return {offset, text} comments in source order, deduplicated by UTF-16 offset.
 * Text excludes the leading # and surrounding whitespace.
 */
function commentTokens(src) {
  const seen = new Map();
  const walk = (x) => {
    if (x == null || typeof x !== "object") return;
    if (Array.isArray(x)) { x.forEach(walk); return; }
    if (x.type === "comment" && typeof x.source === "string") {
      seen.set(x.offset, x.source.replace(/^#/, "").trim());
    }
    for (const v of Object.values(x)) if (v && typeof v === "object") walk(v);
  };
  for (const tok of new Parser().parse(src)) walk(tok);
  return [...seen.entries()].sort((a, b) => a[0] - b[0]).map(([offset, text]) => ({ offset, text }));
}

/**
 * Return ranged keys and non-collection nodes (including aliases) in source order.
 * Paths join mapping keys and zero-based sequence indices with / without escaping;
 * a root leaf uses <root>. The [start, end) ranges use UTF-16 source offsets.
 */
function scalars(doc) {
  const out = [];
  const walk = (node, path) => {
    if (node == null) return;
    if (isMap(node) || isSeq(node)) {
      node.items.forEach((item, i) => {
        if (isPair(item)) {
          const k = isScalar(item.key) ? String(item.key.value) : String(item.key);
          const p = [...path, k];
          if (item.key?.range) out.push({ path: p.join("/"), start: item.key.range[0], end: item.key.range[1] });
          walk(item.value, p);
        } else {
          walk(item, [...path, String(i)]);
        }
      });
    } else if (node.range) {
      out.push({ path: path.join("/") || "<root>", start: node.range[0], end: node.range[1] });
    }
  };
  walk(doc.contents, []);
  return out.sort((a, b) => a.start - b.start || a.end - b.end);
}

/**
 * Return {path, kind, text} comments in source order, or [] if there are none.
 * Text excludes the leading # and surrounding whitespace. A comment is trailing
 * when a preceding node ends on its line; otherwise it is before the next node,
 * or document-end at <root> if no node follows. Paths use unescaped / separators
 * between mapping keys and zero-based sequence indices.
 * Throws an Error prefixed with "parse:" for the first YAML 1.2 parse error.
 */
export function comments(src) {
  const doc = parseDocument(src, { version: "1.2" });
  if (doc.errors.length) throw new Error(`parse: ${doc.errors[0].message}`);
  const lineOf = lineIndex(src);
  const nodes = scalars(doc);
  return commentTokens(src).map(({ offset, text }) => {
    const line = lineOf(offset);
    let trail = null;
    for (const n of nodes) if (n.end <= offset && lineOf(Math.max(n.end - 1, n.start)) === line) trail = n;
    if (trail) return { path: trail.path, kind: "trailing", text };
    const next = nodes.find((n) => n.start > offset);
    return next ? { path: next.path, kind: "before", text } : { path: "<root>", kind: "document-end", text };
  });
}

const key = (c) => `${c.path}\u0000${c.kind}\u0000${c.text}`;

/**
 * Compare comments in the original and replacement YAML, counting duplicates.
 * Return {total, preserved, dropped, moved, added}: total counts original comments;
 * preserved counts matching path, kind and normalised text. Unmatched originals
 * pair with the first remaining replacement comment of the same text as moved
 * {from, to} records; unpaired originals are dropped and replacements are added.
 * Propagates parse errors from comments() for either source.
 */
export function compare(origSrc, newSrc) {
  const a = comments(origSrc);
  const b = comments(newSrc);
  const pool = new Map();
  for (const c of b) pool.set(key(c), (pool.get(key(c)) ?? 0) + 1);
  const missing = [];
  let preserved = 0;
  for (const c of a) {
    const k = key(c);
    if (pool.get(k) > 0) { pool.set(k, pool.get(k) - 1); preserved += 1; } else missing.push(c);
  }
  const extra = [];
  for (const c of b) {
    const k = key(c);
    if (pool.get(k) > 0) { pool.set(k, pool.get(k) - 1); extra.push(c); }
  }
  // Same text at a different (path, kind) is MOVED; text found nowhere is DROPPED.
  const dropped = [], moved = [];
  for (const c of missing) {
    const j = extra.findIndex((e) => e.text === c.text);
    if (j >= 0) { moved.push({ from: c, to: extra[j] }); extra.splice(j, 1); } else dropped.push(c);
  }
  return { total: a.length, preserved, dropped, moved, added: extra };
}
