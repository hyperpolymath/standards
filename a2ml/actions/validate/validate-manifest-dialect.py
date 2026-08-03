#!/usr/bin/env python3
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# validate-manifest-dialect.py — reference validator for MANIFEST-DIALECT-SPEC.adoc
#
# Checks every .a2ml file whose first non-comment line is `---` and which has a
# matching close delimiter. Files that open `---` and never close it are NOT this
# dialect (spec §8) and are skipped, not failed.
#
# Exits non-zero if any file fails, so it can gate CI.
#
# Current corpus: 894/894 conform.
import sys,re,subprocess,collections
files=subprocess.run(['git','ls-files','*.a2ml'],capture_output=True,text=True).stdout.split()
KEY=re.compile(r'^([ ]*)([A-Za-z_][\w.\-]*)[ ]*:[ ]?(.*)$')
SEQ=re.compile(r'^([ ]*)-[ ]+\S')
EXCL=[('anchor',re.compile(r':\s*&\w|^\s*&\w')),('alias',re.compile(r':\s*\*\w')),
      ('tag',re.compile(r':\s*!!?\w')),('flow-map',re.compile(r':\s*\{')),
      ('single-quote',re.compile(r":\s*'"))]
tot=ok=0; fails=collections.Counter(); examples=collections.defaultdict(list)
for f in files:
    L=open(f,encoding='utf-8',errors='replace').read().split('\n')
    i=0
    while i<len(L) and (not L[i].strip() or re.match(r'^\s*(#|//)',L[i])): i+=1
    if not (i<len(L) and L[i].strip()=='---'): continue
    j=next((k for k in range(i+1,len(L)) if L[k].strip()=='---'), None)
    if j is None: continue          # §8: not this dialect
    tot+=1; errs=[]; blk=None
    for n,ln in enumerate(L[i+1:j], start=i+2):
        s=ln.rstrip('\r')
        if blk is not None:
            ind=len(s)-len(s.lstrip(' '))
            if s.strip() and ind>blk: continue      # opaque block content
            blk=None
        if not s.strip() or re.match(r'^\s*#',s): continue
        if '\t' in s: errs.append('tab-indent')
        for nm,rx in EXCL:
            if rx.search(s): errs.append(f'excluded:{nm}')
        m=KEY.match(s)
        if m:
            if len(m.group(1))%2: errs.append('odd-indent')
            if re.match(r'^[|>][-+]?$',m.group(3).strip()): blk=len(m.group(1))
        elif SEQ.match(s): pass
        elif re.match(r'^[ ]*[|>][-+]?\s*$',s): pass
        else: errs.append('unparsed-line')
    if errs:
        for e in set(errs):
            fails[e]+=1
            if len(examples[e])<3: examples[e].append(f"{f}")
    else: ok+=1
print(f"  well-formed frontmatter files : {tot}")
print(f"  CONFORM to the spec as written: {ok}  ({100*ok//tot}%)")
print(f"  rejected                      : {tot-ok}")
for k,v in fails.most_common():
    print(f"    {k:22} {v}   e.g. {examples[k][0] if examples[k] else ''}")

sys.exit(0 if tot-ok==0 else 1)
