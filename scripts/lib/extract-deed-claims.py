#!/usr/bin/env python3
# SPDX-License-Identifier: MPL-2.0
"""Extract complete (claim ...) clauses from a praxis deed, one per output line.

Deed whitespace is not a semantic delimiter. Ignore line comments, respect
quoted strings and escaped quotes, and fail rather than outputting an empty
success when the input contains no claims or an unterminated claim.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path


def extract(data: str) -> list[str]:
    claims: list[str] = []
    i = 0
    quoted = False
    escaped = False
    comment = False
    while i < len(data):
        ch = data[i]
        if comment:
            if ch == "\n":
                comment = False
            i += 1
            continue
        if quoted:
            if escaped:
                escaped = False
            elif ch == "\\":
                escaped = True
            elif ch == '"':
                quoted = False
            i += 1
            continue
        if ch == ";":
            comment = True
            i += 1
            continue
        if ch == '"':
            quoted = True
            i += 1
            continue
        if ch != "(" or not re.match(r"claim(?=[\s)])", data[i + 1 :]):
            i += 1
            continue

        depth = 0
        body: list[str] = []
        string = False
        escape = False
        line_comment = False
        while i < len(data):
            ch = data[i]
            if line_comment:
                if ch == "\n":
                    line_comment = False
                    body.append(" ")
                i += 1
                continue
            if string:
                if ch == "\n":
                    body.append("\\n")
                else:
                    body.append(ch)
                if escape:
                    escape = False
                elif ch == "\\":
                    escape = True
                elif ch == '"':
                    string = False
            elif ch == ";":
                line_comment = True
            elif ch == '"':
                string = True
                body.append(ch)
            elif ch == "(":
                depth += 1
                body.append(ch)
            elif ch == ")":
                depth -= 1
                body.append(ch)
                if depth < 0:
                    raise ValueError("unmatched closing parenthesis in claim")
                if depth == 0:
                    i += 1
                    claims.append("".join(body))
                    break
            else:
                body.append(" " if ch.isspace() else ch)
            i += 1
        else:
            raise ValueError("unterminated deed claim")

    if quoted:
        raise ValueError("unterminated quoted string")
    if not claims:
        raise ValueError("deed has no claim clauses")
    return claims


def main() -> int:
    if len(sys.argv) != 2:
        print("usage: extract-deed-claims.py PATH", file=sys.stderr)
        return 2
    try:
        source = Path(sys.argv[1]).read_text(encoding="utf-8")
        claims = extract(source)
    except (OSError, UnicodeError, ValueError) as exc:
        print(f"invalid claim deed: {exc}", file=sys.stderr)
        return 2
    for claim in claims:
        print(claim)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
