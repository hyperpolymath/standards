#!/usr/bin/env python3
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# state_scm_to_v2.py — convert a v1 STATE.scm (s-expr machine state) to the
# canonical STATE.a2ml v2 "thin session journal" directive format.
#
# Spec: standards/a2ml-templates/STATE.a2ml.v2.spec.adoc
# v2 deliberately discards everything derivable; keeps only:
#   phase, next_action, last_action, updated, and @blockers entries.
#
# Usage:
#   state_scm_to_v2.py <STATE.scm>            # print v2 to stdout (dry-run)
#   state_scm_to_v2.py <STATE.scm> --write    # write .machine_readable/STATE.a2ml
#
# Exit codes: 0 ok, 2 parse/usage error, 3 not a (state ...) form.

import sys, os, re, datetime

def tokenize(s):
    # Strip ;; line comments, then tokenize parens and "strings" and atoms.
    s = re.sub(r';;[^\n]*', '', s)
    toks, i, n = [], 0, len(s)
    while i < n:
        c = s[i]
        if c in '()':
            toks.append(c); i += 1
        elif c == '"':
            j = i + 1; buf = []
            while j < n and s[j] != '"':
                if s[j] == '\\' and j + 1 < n:
                    buf.append(s[j+1]); j += 2
                else:
                    buf.append(s[j]); j += 1
            toks.append(('str', ''.join(buf))); i = j + 1
        elif c.isspace():
            i += 1
        else:
            j = i
            while j < n and not s[j].isspace() and s[j] not in '()"':
                j += 1
            toks.append(('atom', s[i:j])); i = j
    return toks

def parse(toks):
    # Returns nested lists; strings -> ('s', value), atoms -> ('a', value).
    pos = 0
    def rd():
        nonlocal pos
        t = toks[pos]; pos += 1
        if t == '(':
            lst = []
            while toks[pos] != ')':
                lst.append(rd())
            pos += 1
            return lst
        if t == ')':
            raise ValueError('unexpected )')
        return t  # ('str',..) or ('atom',..)
    forms = []
    while pos < len(toks):
        forms.append(rd())
    return forms

def head(node):
    return node[0][1] if (isinstance(node, list) and node and isinstance(node[0], tuple)) else None

def find(node, name):
    """First child list whose head atom == name."""
    if not isinstance(node, list):
        return None
    for ch in node:
        if isinstance(ch, list) and ch and isinstance(ch[0], tuple) and ch[0][1] == name:
            return ch
    return None

def first_string(node):
    """First string literal anywhere under node (depth-first)."""
    if isinstance(node, tuple):
        return node[1] if node[0] == 'str' else None
    if isinstance(node, list):
        for ch in node:
            r = first_string(ch)
            if r:
                return r
    return None

def all_strings(node):
    out = []
    if isinstance(node, tuple):
        if node[0] == 'str':
            out.append(node[1])
    elif isinstance(node, list):
        for ch in node:
            out.extend(all_strings(ch))
    return out

def q(s):
    return '"' + str(s).replace('\\', '\\\\').replace('"', '\\"') + '"'

def slug(s):
    return re.sub(r'[^a-z0-9]+', '-', s.lower()).strip('-')[:40] or 'blocker'

def convert(path):
    src = open(path, encoding='utf-8', errors='replace').read()
    forms = parse(tokenize(src))
    state = next((f for f in forms if head(f) == 'state'), None)
    if state is None:
        sys.stderr.write(f"ERR: no (state ...) form in {path}\n")
        sys.exit(3)

    # phase  <- (current-position (phase "..")) | (current-phase "..")
    phase = None
    cp = find(state, 'current-position')
    if cp:
        ph = find(cp, 'phase')
        if ph:
            phase = first_string(ph)
    if not phase:
        cph = find(state, 'current-phase')
        if cph:
            phase = first_string(cph)
    phase = phase or 'unknown'

    # next_action <- critical-next-actions, first string (immediate first)
    next_action = 'TODO — review and set'
    cna = find(state, 'critical-next-actions')
    if cna:
        imm = find(cna, 'immediate')
        next_action = first_string(imm) or first_string(cna) or next_action

    # last_action <- last session-history entry's first accomplishment string
    last_action = 'migrated from v1 STATE.scm'
    sh = find(state, 'session-history')
    if sh:
        sessions = [c for c in sh if isinstance(c, list) and head(c) == 'session']
        if sessions:
            acc = find(sessions[-1], 'accomplishments')
            last_action = first_string(acc) or first_string(sessions[-1]) or last_action

    # updated <- (metadata (updated|last-updated "..")) else today
    updated = None
    md = find(state, 'metadata')
    if md:
        for key in ('updated', 'last-updated'):
            u = find(md, key)
            if u:
                updated = first_string(u)
                break
    updated = updated or datetime.date.today().isoformat()

    # blockers <- (blockers-and-issues (critical ..)(high ..)(medium ..)(low ..))
    blockers = []
    bi = find(state, 'blockers-and-issues')
    if bi:
        for sev in ('critical', 'high', 'medium', 'low'):
            grp = find(bi, sev)
            if grp:
                for desc in all_strings(grp):
                    blockers.append((sev, desc))

    lines = []
    lines.append('# SPDX-License-Identifier: PMPL-1.0-or-later')
    lines.append(f'# Migrated from {os.path.basename(path)} by state_scm_to_v2.py on '
                 f'{datetime.date.today().isoformat()}')
    lines.append('')
    lines.append('@state(version="2.0"):')
    lines.append(f'phase: {q(phase)}')
    lines.append(f'next_action: {q(next_action)}')
    lines.append(f'last_action: {q(last_action)}')
    lines.append(f'updated: {updated}')
    if blockers:
        lines.append('')
        lines.append('@blockers:')
        for sev, desc in blockers:
            lines.append(f'- id: {slug(desc)}')
            lines.append(f'  description: {q(desc)}')
            lines.append(f'  waiting_on: {q(sev + "-priority — internal")}')
            lines.append(f'  since: {updated}')
        lines.append('@end')
    lines.append('')
    lines.append('@end')
    return '\n'.join(lines) + '\n'

def main():
    args = [a for a in sys.argv[1:] if a != '--write']
    write = '--write' in sys.argv
    if len(args) != 1:
        sys.stderr.write("usage: state_scm_to_v2.py <STATE.scm> [--write]\n")
        sys.exit(2)
    scm = args[0]
    out = convert(scm)
    if write:
        dest = os.path.join(os.path.dirname(scm), 'STATE.a2ml')
        open(dest, 'w', encoding='utf-8').write(out)
        sys.stderr.write(f"wrote {dest}\n")
    else:
        sys.stdout.write(out)

if __name__ == '__main__':
    main()
