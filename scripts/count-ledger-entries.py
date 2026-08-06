#!/usr/bin/env python3
"""Count exemption ENTRIES in a ledger read from stdin. Used by the ratchet.

WHY THIS IS A SEPARATE FILE. It was first embedded in the ratchet as
`python3 -c '...'`. The regex needs both `'''` and `"` for TOML's string forms,
and passing that through a single-quoted shell argument mangled it into invalid
Python. A `|| echo 0` fallback then swallowed the SyntaxError, so the count
silently became 0 on both sides of the comparison and the ledger was skipped
entirely — a check that reported OK while measuring nothing.

⚠ NO FALLBACK. If this cannot run, the ratchet must fail. A counter that
returns 0 on error is indistinguishable from an empty ledger, and "empty" is
the state that passes.

WHY ENTRIES AND NOT LINES, for TOML. A non-comment line count for
`.gitleaks.toml` counts `paths = [`, the closing `]`, and every structural
line. Reformatting one array into several lines then reads as growth, while
adding an entry to an existing single-line array reads as no change at all.
Both directions are wrong, and the second is the dangerous one: it lets an
exemption be added invisibly.
"""
import re
import sys

# TOML string forms, in precedence order: literal-multiline, literal, basic.
_STRING = re.compile(
    r"'''.*?'''"          # '''literal multiline'''
    r"|'[^'\n]+'"         # 'literal'
    r'|"[^"\n]+"',        # "basic"
    re.S)

_ARRAY = re.compile(r"^\s*(?:paths|regexes)\s*=\s*\[(.*?)\]", re.S | re.M)


def count(src: str) -> int:
    n = 0
    for m in _ARRAY.finditer(src):
        body = re.sub(r"#[^\n]*", "", m.group(1))   # strip trailing comments
        n += len(_STRING.findall(body))
    return n


if __name__ == "__main__":
    print(count(sys.stdin.read()))
