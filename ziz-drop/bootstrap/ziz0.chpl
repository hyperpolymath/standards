// SPDX-License-Identifier: MPL-2.0
// ziz0 — bootstrap interpreter for Žiz, in Chapel.
//
// Scope (milestone 0, see DESIGN.adoc §Bootstrap plan):
//   * sexp reader (no layout reader yet — that arrives via tree-sitter)
//   * eager evaluator with lexical closures
//   * printer
//   * in-memory Judgement Evidence Graph populated by observation
//
// Deliberately NOT here: FPGA anything, multi-locale anything, backends,
// triads, boundary operators.
//
// STATUS: written without a Chapel compiler available. Expect small fixes on
// first `chpl` run; the structure is what matters. Target: Chapel 2.x.
//
// Build:  chpl --fast bootstrap/ziz0.chpl -o ziz0
// Run:    ./ziz0 --file=examples/hello.ziz
//         ./ziz0 --file=examples/fact.ziz --jeg=observe

use IO, Map, List;

config const file: string = "";
config const jeg: string = "off";   // "off" | "observe"
config const repl: bool = false;

// --------------------------------------------------------------------------
// Value domain (unityped: every term is a Value)
// --------------------------------------------------------------------------

enum Tag { Nil, Bool, Int, Real, Str, Sym, Pair, Vec, MapV, Fn, Prim, Env, Node, Claim, Err }

class Value {
  var tag: Tag;
  var b: bool;
  var i: int(64);
  var r: real(64);
  var s: string;                          // Str, Sym, Err message, Prim name
  var car: shared Value?;                 // Pair
  var cdr: shared Value?;
  var items: list(shared Value);          // Vec, Fn params
  var kv: map(string, shared Value);      // MapV, Env frame
  var body: shared Value?;                // Fn body, Claim subject
  var env: shared Value?;                 // Fn closure env, Env parent
  var prim: int;                          // Prim dispatch id
  var nodeId: int;                        // CST anchor (reader-assigned)

  proc init(tag: Tag) { this.tag = tag; }
}

type V = shared Value;

proc mkNil(): V { return new shared Value(Tag.Nil); }
proc mkBool(x: bool): V { var v = new shared Value(Tag.Bool); v.b = x; return v; }
proc mkInt(x: int): V { var v = new shared Value(Tag.Int); v.i = x; return v; }
proc mkReal(x: real): V { var v = new shared Value(Tag.Real); v.r = x; return v; }
proc mkStr(x: string): V { var v = new shared Value(Tag.Str); v.s = x; return v; }
proc mkSym(x: string): V { var v = new shared Value(Tag.Sym); v.s = x; return v; }
proc mkErr(x: string): V { var v = new shared Value(Tag.Err); v.s = x; return v; }
proc mkPair(a: V, d: V): V { var v = new shared Value(Tag.Pair); v.car = a; v.cdr = d; return v; }
proc mkPrim(id: int, name: string): V { var v = new shared Value(Tag.Prim); v.prim = id; v.s = name; return v; }
proc mkEnv(parent: V?): V { var v = new shared Value(Tag.Env); v.env = parent; return v; }

proc isNil(v: V): bool { return v.tag == Tag.Nil; }
proc truthy(v: V): bool { return !(v.tag == Tag.Nil || (v.tag == Tag.Bool && !v.b)); }

proc listFrom(ref xs: list(V)): V {
  var acc = mkNil();
  for idx in 0..#xs.size by -1 do acc = mkPair(xs[idx], acc);
  return acc;
}

iter listItems(v: V): V {
  var cur = v;
  while cur.tag == Tag.Pair {
    yield cur.car!;
    cur = cur.cdr!;
  }
}

proc listLen(v: V): int { var n = 0; for _ in listItems(v) do n += 1; return n; }

// --------------------------------------------------------------------------
// Environment (first-class; see DESIGN.adoc §Reflexivity)
// --------------------------------------------------------------------------

proc envLookup(e: V, name: string): V? {
  var cur: V? = e;
  while cur != nil {
    if cur!.kv.contains(name) then return cur!.kv[name];
    cur = cur!.env;
  }
  return nil;
}

proc envDefine(e: V, name: string, v: V) { e.kv[name] = v; }

proc envSet(e: V, name: string, v: V): bool {
  var cur: V? = e;
  while cur != nil {
    if cur!.kv.contains(name) { cur!.kv[name] = v; return true; }
    cur = cur!.env;
  }
  return false;
}

// --------------------------------------------------------------------------
// Reader (sexp table only). ASCII-only surface syntax.
// --------------------------------------------------------------------------

record Reader {
  var src: string;
  var pos: int = 0;
  var nextNode: int = 1;

  proc ref peek(): string { return if pos < src.size then src[pos] else ""; }
  proc ref adv(): string { const c = peek(); pos += 1; return c; }
  proc ref atEnd(): bool { return pos >= src.size; }

  proc ref skipWs() {
    while !atEnd() {
      const c = peek();
      if c == ";" { while !atEnd() && peek() != "\n" do adv(); }
      else if c == " " || c == "\t" || c == "\n" || c == "\r" { adv(); }
      else break;
    }
  }

  proc isDelim(c: string): bool {
    return c == "" || c == " " || c == "\t" || c == "\n" || c == "\r" ||
           c == "(" || c == ")" || c == "[" || c == "]" || c == "{" || c == "}" ||
           c == "\"" || c == ";";
  }

  proc ref read(): V {
    skipWs();
    if atEnd() then return mkErr("eof");
    const c = peek();
    select c {
      when "(" { adv(); return readSeq(")"); }
      when "[" { adv(); var l = readSeq("]"); var v = new shared Value(Tag.Vec);
                 for x in listItems(l) do v.items.pushBack(x); return v; }
      when "{" { adv(); var l = readSeq("}"); var v = new shared Value(Tag.MapV);
                 var k: V? = nil;
                 for x in listItems(l) { if k == nil then k = x; else { v.kv[show(k!)] = x; k = nil; } }
                 return v; }
      when ")" { adv(); return mkErr("unexpected )"); }
      when "]" { adv(); return mkErr("unexpected ]"); }
      when "}" { adv(); return mkErr("unexpected }"); }
      when "'" { adv(); return mkPair(mkSym("quote"), mkPair(read(), mkNil())); }
      when "`" { adv(); return mkPair(mkSym("quasiquote"), mkPair(read(), mkNil())); }
      when "," { adv();
                 if peek() == "@" { adv(); return mkPair(mkSym("unquote-splicing"), mkPair(read(), mkNil())); }
                 return mkPair(mkSym("unquote"), mkPair(read(), mkNil())); }
      when "\"" { adv(); return readString(); }
      otherwise { return readAtom(); }
    }
  }

  proc ref readSeq(close: string): V {
    var xs: list(V);
    while true {
      skipWs();
      if atEnd() then return mkErr("unterminated list");
      if peek() == close { adv(); break; }
      xs.pushBack(read());
    }
    var v = listFrom(xs);
    if v.tag == Tag.Pair { v.nodeId = nextNode; nextNode += 1; }
    return v;
  }

  proc ref readString(): V {
    var out = "";
    while !atEnd() {
      const c = adv();
      if c == "\"" then return mkStr(out);
      if c == "\\" {
        const e = adv();
        select e {
          when "n" do out += "\n";
          when "t" do out += "\t";
          when "r" do out += "\r";
          when "\"" do out += "\"";
          when "\\" do out += "\\";
          otherwise do out += e;
        }
      } else out += c;
    }
    return mkErr("unterminated string");
  }

  proc ref readAtom(): V {
    var tok = "";
    while !isDelim(peek()) do tok += adv();
    if tok == "nil" then return mkNil();
    if tok == "true" then return mkBool(true);
    if tok == "false" then return mkBool(false);
    try { return mkInt(tok: int); } catch { }
    try { if tok.find(".") != -1 then return mkReal(tok: real); } catch { }
    var v = mkSym(tok);
    v.nodeId = nextNode; nextNode += 1;
    return v;
  }
}

// --------------------------------------------------------------------------
// Printer
// --------------------------------------------------------------------------

proc show(v: V): string {
  select v.tag {
    when Tag.Nil do return "nil";
    when Tag.Bool do return if v.b then "true" else "false";
    when Tag.Int do return v.i: string;
    when Tag.Real do return v.r: string;
    when Tag.Str do return "\"" + v.s.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    when Tag.Sym do return v.s;
    when Tag.Err do return "#<error " + v.s + ">";
    when Tag.Prim do return "#<prim " + v.s + ">";
    when Tag.Fn do return "#<fn>";
    when Tag.Env do return "#<env>";
    when Tag.Node do return "(node " + v.nodeId: string + ")";
    when Tag.Claim do return "#<claim " + show(v.body!) + ">";
    when Tag.Vec {
      var s = "[";
      for (x, k) in zip(v.items, 0..) { if k > 0 then s += " "; s += show(x); }
      return s + "]";
    }
    when Tag.MapV {
      var s = "{"; var first = true;
      for k in v.kv.keys() { if !first then s += " "; first = false; s += k + " " + show(v.kv[k]); }
      return s + "}";
    }
    when Tag.Pair {
      var s = "("; var cur = v; var first = true;
      while cur.tag == Tag.Pair {
        if !first then s += " "; first = false;
        s += show(cur.car!);
        cur = cur.cdr!;
      }
      if !isNil(cur) then s += " . " + show(cur);
      return s + ")";
    }
  }
  return "#<?>";
}

// --------------------------------------------------------------------------
// Judgement Evidence Graph (in-memory; docs/JEG.adoc)
//
// Litmus test: apply() below is the code path where evaluating (fact 3)
// creates evidence edges.
// --------------------------------------------------------------------------

record Judgement { var subject: string; var predicate: string; var args: string; }
record Evidence  { var forId: int; var kind: string; var by: string; var where_: string; }

class JEG {
  var judgements: list(Judgement);
  var index: map(string, int);           // structural key -> id (hash-consing)
  var evidence: list(Evidence);

  proc assert_(subject: string, predicate: string, args: string,
               kind: string, by: string, where_: string) {
    const key = subject + "\x01" + predicate + "\x01" + args;
    var id: int;
    if index.contains(key) then id = index[key];
    else {
      judgements.pushBack(new Judgement(subject, predicate, args));
      id = judgements.size - 1;
      index[key] = id;
    }
    evidence.pushBack(new Evidence(id, kind, by, where_));
  }

  proc dump() {
    writeln("; --- JEG: ", judgements.size, " judgements, ", evidence.size, " evidence edges");
    for (j, id) in zip(judgements, 0..) {
      var kinds: map(string, int);
      for e in evidence do if e.forId == id then kinds[e.kind] += 1;
      var ks = "";
      for k in kinds.keys() do ks += " " + k + "=" + kinds[k]: string;
      writeln("(", j.subject, " ", j.predicate, if j.args != "" then " " + j.args else "", ")  ;", ks);
    }
  }
}

var theJEG = new shared JEG();

proc subjectOf(v: V): string {
  if v.tag == Tag.Sym then return "sym:" + v.s;
  if v.nodeId != 0 then return "node:" + v.nodeId: string;
  return "anon:" + show(v);
}

proc tagName(v: V): string { return ":" + (v.tag: string).toLower(); }

// --------------------------------------------------------------------------
// Primitives
// --------------------------------------------------------------------------

enum P { Add, Sub, Mul, Div, Lt, Gt, Le, Ge, NumEq, Eq, Cons, Car, Cdr, ListP, Print,
         Eval, CurrentEnv, JegDump, Not, IsNil, Len, Str }

proc numOf(v: V): real { return if v.tag == Tag.Int then v.i: real else v.r; }

proc arith(op: P, ref args: list(V)): V {
  if args.size == 0 then return mkErr("arity");
  var allInt = true;
  for a in args do if a.tag != Tag.Int then allInt = false;
  if allInt {
    var acc = args[0].i;
    if args.size == 1 && op == P.Sub then return mkInt(-acc);
    for idx in 1..<args.size {
      const x = args[idx].i;
      select op {
        when P.Add do acc += x;
        when P.Sub do acc -= x;
        when P.Mul do acc *= x;
        when P.Div { if x == 0 then return mkErr("divide by zero"); acc /= x; }
        otherwise do return mkErr("bad arith op");
      }
    }
    return mkInt(acc);
  }
  var acc: real = numOf(args[0]);
  for idx in 1..<args.size {
    const a = args[idx];
    if a.tag != Tag.Int && a.tag != Tag.Real then return mkErr("not a number: " + show(a));
    const x = numOf(a);
    select op {
      when P.Add do acc += x;
      when P.Sub do acc -= x;
      when P.Mul do acc *= x;
      when P.Div do acc /= x;
      otherwise do return mkErr("bad arith op");
    }
  }
  return mkReal(acc);
}

proc valEq(a: V, b: V): bool {
  if a.tag != b.tag then return false;
  select a.tag {
    when Tag.Nil do return true;
    when Tag.Bool do return a.b == b.b;
    when Tag.Int do return a.i == b.i;
    when Tag.Real do return a.r == b.r;
    when Tag.Str do return a.s == b.s;
    when Tag.Sym do return a.s == b.s;
    when Tag.Pair do return valEq(a.car!, b.car!) && valEq(a.cdr!, b.cdr!);
    otherwise do return a == b;
  }
}

proc applyPrim(p: V, ref args: list(V), env: V): V {
  const op = try! (p.prim: P);
  select op {
    when P.Add do return arith(op, args);
    when P.Sub do return arith(op, args);
    when P.Mul do return arith(op, args);
    when P.Div do return arith(op, args);
    when P.Lt do return mkBool(numOf(args[0]) < numOf(args[1]));
    when P.Gt do return mkBool(numOf(args[0]) > numOf(args[1]));
    when P.Le do return mkBool(numOf(args[0]) <= numOf(args[1]));
    when P.Ge do return mkBool(numOf(args[0]) >= numOf(args[1]));
    when P.NumEq do return mkBool(numOf(args[0]) == numOf(args[1]));
    when P.Eq do return mkBool(valEq(args[0], args[1]));
    when P.Cons do return mkPair(args[0], args[1]);
    when P.Car do return if args[0].tag == Tag.Pair then args[0].car! else mkErr("car of non-pair");
    when P.Cdr do return if args[0].tag == Tag.Pair then args[0].cdr! else mkErr("cdr of non-pair");
    when P.ListP do return listFrom(args);
    when P.Print { for a in args do write(if a.tag == Tag.Str then a.s else show(a)); writeln(); return mkNil(); }
    when P.Eval do return eval(args[0], if args.size > 1 then args[1] else env);
    when P.CurrentEnv do return env;
    when P.JegDump { theJEG.dump(); return mkNil(); }
    when P.Not do return mkBool(!truthy(args[0]));
    when P.IsNil do return mkBool(isNil(args[0]));
    when P.Len do return mkInt(if args[0].tag == Tag.Vec then args[0].items.size else listLen(args[0]));
    when P.Str { var s = ""; for a in args do s += if a.tag == Tag.Str then a.s else show(a); return mkStr(s); }
  }
  return mkErr("unknown prim");
}

proc installPrims(g: V) {
  const names = ["+", "-", "*", "/", "<", ">", "<=", ">=", "=", "eq?", "cons", "car", "cdr",
                 "list", "print", "eval", "current-env", "jeg-dump", "not", "nil?", "len", "str"];
  for (n, k) in zip(names, 0..) do envDefine(g, n, mkPrim(k, n));
}

// --------------------------------------------------------------------------
// Evaluator
// --------------------------------------------------------------------------

proc quasi(x: V, env: V): V {
  if x.tag != Tag.Pair then return x;
  const head = x.car!;
  if head.tag == Tag.Sym && head.s == "unquote" then return eval(x.cdr!.car!, env);
  var out: list(V);
  for item in listItems(x) {
    if item.tag == Tag.Pair && item.car!.tag == Tag.Sym && item.car!.s == "unquote-splicing" {
      for y in listItems(eval(item.cdr!.car!, env)) do out.pushBack(y);
    } else out.pushBack(quasi(item, env));
  }
  return listFrom(out);
}

proc apply(f: V, ref args: list(V), env: V, callNode: V): V {
  const subj = subjectOf(callNode.car!);
  const where_ = "node:" + callNode.nodeId: string;
  if jeg == "observe" {
    theJEG.assert_(subj, ":callable", "", "observed", "ziz0", where_);
    theJEG.assert_(subj, ":arity", args.size: string, "observed", "ziz0", where_);
    for (a, k) in zip(args, 0..) do
      theJEG.assert_(subj, ":takes", k: string + " " + tagName(a), "observed", "ziz0", where_);
  }
  var result: V;
  select f.tag {
    when Tag.Prim do result = applyPrim(f, args, env);
    when Tag.Fn {
      if f.items.size != args.size then return mkErr("arity mismatch calling fn");
      var frame = mkEnv(f.env);
      for (p, a) in zip(f.items, args) do envDefine(frame, p.s, a);
      result = eval(f.body!, frame);
    }
    otherwise do return mkErr("not callable: " + show(f));
  }
  if jeg == "observe" then
    theJEG.assert_(subj, ":returns", tagName(result), "observed", "ziz0", where_);
  return result;
}

proc eval(x: V, env: V): V {
  select x.tag {
    when Tag.Sym {
      const v = envLookup(env, x.s);
      return if v != nil then v! else mkErr("unbound: " + x.s);
    }
    when Tag.Pair {
      const head = x.car!;
      if head.tag == Tag.Sym {
        select head.s {
          when "quote" do return x.cdr!.car!;
          when "quasiquote" do return quasi(x.cdr!.car!, env);
          when "if" {
            const c = eval(x.cdr!.car!, env);
            const rest = x.cdr!.cdr!;
            if truthy(c) then return eval(rest.car!, env);
            return if rest.cdr!.tag == Tag.Pair then eval(rest.cdr!.car!, env) else mkNil();
          }
          when "define" {
            const target = x.cdr!.car!;
            if target.tag == Tag.Pair {
              var fn = new shared Value(Tag.Fn);
              for p in listItems(target.cdr!) do fn.items.pushBack(p);
              fn.body = mkPair(mkSym("begin"), x.cdr!.cdr!);
              fn.env = env;
              envDefine(env, target.car!.s, fn);
              if jeg == "observe" then
                theJEG.assert_("sym:" + target.car!.s, ":defined-at", "", "observed", "ziz0",
                               "node:" + x.nodeId: string);
              return mkSym(target.car!.s);
            }
            const v = eval(x.cdr!.cdr!.car!, env);
            envDefine(env, target.s, v);
            return mkSym(target.s);
          }
          when "set!" {
            const v = eval(x.cdr!.cdr!.car!, env);
            return if envSet(env, x.cdr!.car!.s, v) then v else mkErr("set! of unbound");
          }
          when "lambda" {
            var fn = new shared Value(Tag.Fn);
            for p in listItems(x.cdr!.car!) do fn.items.pushBack(p);
            fn.body = mkPair(mkSym("begin"), x.cdr!.cdr!);
            fn.env = env;
            return fn;
          }
          when "begin" {
            var last = mkNil();
            for form in listItems(x.cdr!) do last = eval(form, env);
            return last;
          }
          when "let" {
            var frame = mkEnv(env);
            for binding in listItems(x.cdr!.car!) do
              envDefine(frame, binding.car!.s, eval(binding.cdr!.car!, env));
            var last = mkNil();
            for form in listItems(x.cdr!.cdr!) do last = eval(form, frame);
            return last;
          }
          when "claim" {
            // (claim subject predicate args... :evidence kind [:by who])
            const subj = x.cdr!.car!;
            var rest: list(V);
            for item in listItems(x.cdr!.cdr!) do rest.pushBack(item);
            var predicate = "", args = "", kind = "asserted", by = "author";
            var idx = 0;
            while idx < rest.size {
              const it = rest[idx];
              if it.tag == Tag.Sym && it.s == ":evidence" { kind = rest[idx+1].s.strip(":"); idx += 2; continue; }
              if it.tag == Tag.Sym && it.s == ":by" { by = show(rest[idx+1]).strip("\""); idx += 2; continue; }
              if predicate == "" then predicate = show(it);
              else args += (if args != "" then " " else "") + show(it);
              idx += 1;
            }
            theJEG.assert_(subjectOf(subj), predicate, args, kind, by, "node:" + x.nodeId: string);
            var c = new shared Value(Tag.Claim); c.body = subj;
            return c;
          }
        }
      }
      const f = eval(head, env);
      if f.tag == Tag.Err then return f;
      var args: list(V);
      for a in listItems(x.cdr!) {
        const v = eval(a, env);
        if v.tag == Tag.Err then return v;
        args.pushBack(v);
      }
      return apply(f, args, env, x);
    }
    otherwise do return x;   // self-evaluating
  }
}

// --------------------------------------------------------------------------
// Driver
// --------------------------------------------------------------------------

proc runSource(src: string, env: V, echo: bool) {
  var rd = new Reader(src);
  while true {
    var form = rd.read();
    if form.tag == Tag.Err && form.s == "eof" then break;
    const v = eval(form, env);
    if echo || v.tag == Tag.Err then writeln(if v.tag == Tag.Err then ";; " + show(v) else show(v));
  }
}

proc main() {
  var global = mkEnv(nil);
  installPrims(global);

  if file != "" {
    var src: string;
    try {
      var f = open(file, ioMode.r);
      var r = f.reader();
      r.readAll(src);
      r.close(); f.close();
    } catch e {
      writeln("cannot read ", file, ": ", e.message());
      exit(1);
    }
    runSource(src, global, echo=false);
    if jeg == "observe" then theJEG.dump();
  }

  if repl || file == "" {
    writeln("ziz0 -- unityped, homoiconic, no typechecker. Ctrl-D exits.");
    var line: string;
    while true {
      write("ziz> ");
      if !stdin.readLine(line) then break;
      runSource(line, global, echo=true);
    }
  }
}
