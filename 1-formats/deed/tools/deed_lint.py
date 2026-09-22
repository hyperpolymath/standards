#!/usr/bin/env python3
"""deed-lint — conformance validator for the DEED grammar (deed.abnf v1.0.0).

Stdlib-only. Implements the normative grammar faithfully:
  * header = 1* spdx-line                 (";;" SP "SPDX-" …)
  * form   = "(" doc-head 1*(sep (field/clause)) [sep] ")"
  * doc-head ∈ {estate-deed, repo-deed, estate-atlas-deed, praxis-deed}
  * field  = keyword sep value            clause = "(" symbol *(sep (field/clause)) [sep] ")"
  * value  = string / symbol / integer / boolean / uuid5 / quoted / list
  * boolean = #t | #f  (lowercase ONLY; true/false/1/0 are parse errors)
  * uuid5 = %s"#u5" string                (body is the RFC 4122 §4.3 NAME input)
  * escapes exactly {" \\\\ \\n \\t}      (\\r, \\uXXXX and all others INVALID)
  * sep    = 1*(SP / line-end / comment)  (HTAB is INVALID — K9-consistent)
  * symbol = ALPHA *(ALPHA/DIGIT/./"*"//"<"/">"/"="/"!"/"?"/"+"/"-"/"_")
  * quoted = "'" (symbol / list)          (only symbols/lists may be quoted)
Semantic checks (validator-enforced, per grammar):
  * :schema-version STRING exactly once at form top level
  * entire input consumed
  * filename↔doc-head dispatch (estate-first side condition, stem≠"estate")

Usage: deed_lint.py FILE...     exit 0 iff all files conform
       deed_lint.py --self-test
"""
import os
import re
import sys

__all__ = ["validate", "LintError", "check_filename_dispatch"]

SYMBOL_START = re.compile(r"[A-Za-z]")
SYMBOL_CONT = re.compile(r"[A-Za-z0-9.*/<>=!?+_-]")


class LintError(ValueError):
    """A single conformance failure with a 1-based line number."""

    def __init__(self, msg, line):
        super().__init__(msg)
        self.msg, self.line = msg, line

    def __str__(self):
        return f"line {self.line}: {self.msg}"


class _Lexer:
    def __init__(self, text):
        self.t = text
        self.i = 0
        self.n = len(text)

    def line(self, at=None):
        return self.t.count("\n", 0, self.i if at is None else at) + 1

    def peek(self, k=0):
        j = self.i + k
        return self.t[j] if j < self.n else ""

    def skip_sep(self):
        """token-sep = 1*(SP / line-end / comment). Returns #separators seen."""
        seen = 0
        while self.i < self.n:
            c = self.t[self.i]
            if c == " ":
                self.i += 1
                seen = 1
            elif c == "\n":
                self.i += 1
                seen = 1
            elif c == "\r":
                if self.t[self.i : self.i + 2] == "\r\n":
                    self.i += 2
                    seen = 1
                else:
                    raise LintError("bare CR is not a line-end (CRLF or LF only)", self.line())
            elif c == ";":
                while self.i < self.n and self.t[self.i] not in "\r\n":
                    self.i += 1
                seen = 1  # comment; the terminating line-end is consumed next loop
            else:
                break
        return seen


def _lex_string(lx):
    """string = DQUOTE *( str-char / escape ) DQUOTE ; exactly 4 escapes."""
    start = lx.i
    out = []
    lx.i += 1  # opening quote
    while True:
        if lx.i >= lx.n:
            raise LintError("unterminated string", lx.line(start))
        c = lx.t[lx.i]
        if c == '"':
            lx.i += 1
            return ("string", "".join(out))
        if c == "\\":
            if lx.i + 1 >= lx.n:
                raise LintError("dangling backslash", lx.line(start))
            e = lx.t[lx.i + 1]
            if e not in ('"', "\\", "n", "t"):
                raise LintError(
                    f"illegal escape \\{e!r} — only \\\" \\\\ \\n \\t exist in DEED "
                    "(\\r and \\uXXXX are parse errors; embed non-ASCII as raw UTF-8)",
                    lx.line(start),
                )
            out.append({"n": "\n", "t": "\t"}.get(e, e))
            lx.i += 2
            continue
        o = ord(c)
        if o < 0x20 or c == '"' or c == "\\":
            raise LintError(f"raw control character U+{o:04X} inside string (use legal escapes)", lx.line())
        out.append(c)
        lx.i += 1


def _lex_number(lx):
    m = re.match(r"-?[0-9]+", lx.t[lx.i :])
    if m:
        lx.i += m.end()
        return ("integer", None)
    return None


def _lex_symbol(lx):
    if not SYMBOL_START.match(lx.peek()):
        return None
    j = lx.i + 1
    while j < lx.n and SYMBOL_CONT.match(lx.t[j]):
        j += 1
    s = lx.t[lx.i : j]
    lx.i = j
    return ("symbol", s)


def _lex_value(lx):
    """value = string / symbol / integer / boolean / uuid5 / quoted / list"""
    c = lx.peek()
    if c == '"':
        return _lex_string(lx)
    if c == "#":
        two = lx.t[lx.i : lx.i + 3]
        if two.startswith("#u5"):
            lx.i += 3
            if lx.peek() != '"':
                raise LintError('uuid5 must be followed immediately by a string: #u5"name"', lx.line())
            _lex_string(lx)
            return ("uuid5", None)
        if two[:2] in ("#t", "#f"):
            nxt = lx.peek(2)
            if nxt and nxt not in " \r\n()":
                raise LintError(
                    f"booleans are exactly #t/#f (got {two + lx.t[lx.i+2:lx.i+24].split()[0][:20]!r}); "
                    "true/false/1/0 are parse errors",
                    lx.line(),
                )
            lx.i += 2
            return ("boolean", two)
        raise LintError(f"unrecognised #-form: only #t, #f, #u5\"…\" are legal", lx.line())
    if c == "(":
        lx.i += 1
        items = []
        while True:
            have = lx.skip_sep()
            if lx.peek() == ")":
                lx.i += 1
                return ("list", items)
            items.append(_lex_value(lx))
    if c == "'" :
        # quoted = ' (symbol / list)
        lx.i += 1
        lx.skip_sep()
        v = _lex_value(lx)
        if v[0] not in ("symbol", "list"):
            raise LintError(f"only symbols and lists may be quoted, not {v[0]}", lx.line())
        return ("quoted", v)
    if c == ":":
        raise LintError("stray keyword — a keyword may only lead a field", lx.line())
    if c and c.isdigit() or (c == "-" and lx.peek(1).isdigit()):
        v = _lex_number(lx)
        nxt = lx.peek()
        if nxt and (SYMBOL_CONT.match(nxt) or nxt.isalpha()):
            raise LintError("malformed token: number followed by identifier characters", lx.line())
        return v
    v = _lex_symbol(lx)
    if v:
        return v
    raise LintError(f"cannot lex value starting at {c!r} ('=' as a field separator is not a deed)", lx.line())


def _lex_field_or_clause(lx):
    c = lx.peek()
    if c == ":":
        lx.i += 1
        kw = _lex_symbol(lx)
        if not kw:
            raise LintError("malformed keyword: ':' must be followed by a symbol", lx.line())
        if not lx.skip_sep():
            raise LintError(f"keyword :{kw[1]} must be followed by a separator before its value", lx.line())
        val = _lex_value(lx)
        if val[0] == "symbol" and val[1] in ("true", "false", "yes", "no"):
            # Grammar note on the boolean production: "Never true, false,
            # yes, no." These lex as symbols, so the ban is enforced here
            # as a value-level semantic rule.
            raise LintError(
                f"boolean meaning must use #t/#f — bare symbol {val[1]!r} is forbidden as a value", lx.line()
            )
        return ("field", kw[1], val)
    if c == "(":
        lx.i += 1
        head = _lex_symbol(lx)
        if not head:
            raise LintError("clause '(' must be followed immediately by a clause symbol (no separator)", lx.line())
        items = []
        while True:
            lx.skip_sep()
            if lx.peek() == ")":
                lx.i += 1
                return ("clause", head[1], items)
            if lx.peek() == "" and lx.i >= lx.n:
                raise LintError(f"unbalanced parens: clause ({head[1]}) never closes", lx.line())
            items.append(_lex_field_or_clause(lx))
    if c == "" :
        raise LintError("unexpected end of input (unbalanced parens)", lx.line())
    raise LintError(f"expected field (':keyword …') or clause ('(symbol …)'), got {c!r}", lx.line())


def _parse_form(lx):
    """"(" doc-head 1*(token-sep (field/clause)) [token-sep] ")" — no sep after '('."""
    if lx.peek() != "(":
        raise LintError("a deed form must start with '('", lx.line())
    lx.i += 1
    head = _lex_symbol(lx)
    heads = ("estate-deed", "repo-deed", "estate-atlas-deed", "praxis-deed")
    if not head or head[1] not in heads:
        got = head[1] if head else lx.peek()
        raise LintError(f"invalid doc-head {got!r}; valid heads: {', '.join(heads)}", lx.line())
    if not lx.skip_sep():
        raise LintError(f"doc-head {head[1]} must be followed by a separator before the first field", lx.line())
    items = []
    while True:
        have_sep = lx.skip_sep()
        if lx.peek() == ")":
            lx.i += 1
            break
        if lx.peek() == "":
            raise LintError("unbalanced parens: form never closes", lx.line())
        items.append(_lex_field_or_clause(lx))
    schema = [it for it in items if it[0] == "field" and it[1] == "schema-version"]
    if len(schema) != 1:
        raise LintError(
            f"form must carry exactly one :schema-version STRING field (found {len(schema)})", lx.line()
        )
    if schema[0][2][0] != "string":
        raise LintError(":schema-version must be a STRING value", lx.line())
    if lx.skip_sep() != 0 and lx.i >= lx.n:
        return head[1], items
    if lx.i < lx.n:
        raise LintError(
            "trailing content after the form's closing ')' — a deed is exactly one form", lx.line()
        )
    return head[1], items


def _parse_header(lx):
    """header = 1* spdx-line ; spdx-line = ";;" SP %s"SPDX-" 1*text-char line-end"""
    count = 0
    while True:
        if lx.t[lx.i :].startswith(";; SPDX-"):
            eol = lx.t.find("\n", lx.i)
            if eol == -1:
                raise LintError("SPDX header line has no line-end", lx.line())
            payload = lx.t[lx.i + 8 : eol]
            if not payload.strip():
                raise LintError("SPDX header line is empty after ';; SPDX-'", lx.line())
            lx.i = eol + 1
            count += 1
            continue
        break
    if count == 0:
        raise LintError(
            "deed must begin with at least one SPDX header line (';; SPDX-License-Identifier: …')", lx.line()
        )
    return count


def validate(text, filename=None):
    """Validate DEED source text. Returns (head, items) on success; raises LintError."""
    if "\t" in text:
        raise LintError("HTAB (tab) is an invalid separator anywhere in a deed (K9-consistent)", text.count("\n", 0, text.find("\t")) + 1)
    try:
        text.encode("utf-8")
    except UnicodeEncodeError as e:  # pragma: no cover
        raise LintError(f"invalid UTF-8: {e}", 1)
    lx = _Lexer(text)
    _parse_header(lx)
    lx.skip_sep()
    head, items = _parse_form(lx)
    if filename:
        check_filename_dispatch(filename, head)
    return head, items


def check_filename_dispatch(filename, head):
    """estate-file exact-first; stems may contain dots (split on the final suffix)."""
    import os

    base = os.path.basename(filename)
    if base == "estate_chora.deed":
        want = "estate-deed"
    elif base == "ATLAS.deed":
        want = "estate-atlas-deed"
    elif base.endswith("_praxis.deed"):
        want = "praxis-deed"
    elif base.endswith("_chora.deed"):
        stem = base[: -len("_chora.deed")]
        if stem == "estate":
            return  # handled by estate-file branch (exact-first); unreachable
        if not re.fullmatch(r"[A-Za-z0-9-._]+", stem) or not stem:
            raise LintError(f"illegal deed filename stem {stem!r}", 1)
        want = "repo-deed"
    else:
        raise LintError(
            f"filename {base!r} matches no deed dispatch pattern "
            "(estate_chora.deed | ATLAS.deed | <stem>_chora.deed | <stem>_praxis.deed)",
            1,
        )
    if head != want:
        raise LintError(
            f"doc-head/filename mismatch: {base} dispatches to {want} but parses as {head}", 1
        )


_SELF_TEST = r"""
# (valid head fields)
1. valid minimal deed               : OK
"""

_FIXTURES = [
    ("valid-minimal", True,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version "1.0.0" :canonical-name "x" :repo-uuid #u5"github.com/o/x" :beholding-chora #u5"estate/chora")\n'),
    ("valid-nested", True,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed\n  :schema-version "1.0.0"\n:canonical-name "x" ; comment between\n  (lineage :type hub :parent "" :previous-names ()) )\n'),
    ("valid-booleans-uuid", True,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version "1.0.0" (status :present #t :ended #f :note "legal escapes: \\n and \\t and \\\\ and \\"q\\""))\n'),
    ("invalid-equals", False,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version "1.0.0" :canonical-name = "x")\n'),
    ("invalid-section", False,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version "1.0.0"\n[status]\nphase = "active")\n'),
    ("invalid-missing-schema", False,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :canonical-name "x")\n'),
    ("invalid-head", False,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(chora-deed :schema-version "1.0.0")\n'),
    ("invalid-true-literal", False,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version "1.0.0" (status :present true))\n'),
    ("invalid-tab", False,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed\t:schema-version "1.0.0")\n'),
    ("invalid-escape-u", False,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version "1.0.0" (m :s "bad \\u0041"))\n'),
    ("invalid-trailing", False,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version "1.0.0") trailing\n'),
    ("invalid-no-header", False,
     '(repo-deed :schema-version "1.0.0")\n'),
    ("invalid-string-after-head", False,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version "not-string-issue" :other 007 :sym github-actions :q \'(a b))\n'),  # actually VALID — keep to prove quoted lists + 007 parse
    ("invalid-unbalanced", False,
     ';; SPDX-License-Identifier: CC-BY-SA-4.0\n(repo-deed :schema-version "1.0.0" (status :present #t)\n'),
]
# corrected expectation: the fixture labelled invalid-string-after-head above is actually valid
_FIXTURES[12] = ("valid-quoted-list-symbols-007", True, _FIXTURES[12][2])


def _self_test():
    ok = True
    for name, expect_ok, src in _FIXTURES:
        try:
            validate(src)
            got = True
            err = ""
        except LintError as e:
            got = False
            err = str(e)
        passed = got == expect_ok
        ok &= passed
        status = "PASS " if passed else "FAIL "
        detail = "" if passed else f"  (expected {'valid' if expect_ok else 'error'}; got {'valid' if got else err})"
        print(f"{status}{name}{detail}")
    print("SELF-TEST " + ("OK" if ok else "FAILED"))
    return 0 if ok else 1


def _fixtures(d):
    """valid/ must parse; invalid/ must fail. Returns exit code."""
    import glob
    bad = 0
    for sub, expect in (("valid", True), ("invalid", False)):
        for f in sorted(glob.glob(os.path.join(d, sub, "*.deed"))):
            try:
                # NOSONAR pythonsecurity:S8707 — local CLI linter: paths come
                # from the operator's argv / a fixed fixtures dir, not a trust
                # boundary (CI passes only hardcoded repo paths).
                validate(open(f, encoding="utf-8").read(), filename=f)  # NOSONAR
                got, err = True, ""
            except LintError as e:
                got, err = False, str(e)
            mark = "PASS " if got == expect else "FAIL "
            if got != expect:
                bad += 1
            print(f"{mark}{f}" + ("" if got == expect else f"  (unexpected: {err or 'valid'})"))
    return 1 if bad else 0


def main(argv):
    if "--self-test" in argv:
        return _self_test()
    if "--fixtures" in argv:
        d = argv[argv.index("--fixtures") + 1]
        rc = _fixtures(d)
        print("FIXTURES " + ("OK" if rc == 0 else "FAILED"))
        return rc
    files = [a for a in argv[1:] if not a.startswith("-")]
    if not files:
        print(__doc__)
        return 2
    bad = 0
    for f in files:
        try:
            # NOSONAR pythonsecurity:S8707 — see _fixtures above: operator argv.
            with open(f, encoding="utf-8") as fh:  # NOSONAR
                validate(fh.read(), filename=f)
            print(f"OK   {f}")
        except LintError as e:
            bad += 1
            print(f"FAIL {f}: {e}")
        except OSError as e:
            bad += 1
            print(f"FAIL {f}: {e}")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
