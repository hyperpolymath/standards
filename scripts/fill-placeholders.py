#!/usr/bin/env python3
"""Fill template placeholders — without destroying the machinery that fills them.

WHY THIS EXISTS. A previous estate-wide sweep filled `{{TOKEN}}` placeholders
by plain text replacement across every file. It produced 141 review findings
across 90 repositories, and one of them is severe:

    sed "s/{{PROJECT_NAME}}/$name/g"   became   sed "s/Conative Gating/$name/g"
    sed -e "s/{{DATE}}/$DATE/g"        became   sed -e "s/2026-08-05/$DATE/g"

Those are the scripts whose JOB is to perform template substitution. Filling
the left-hand side of their own `sed` expressions means template application
silently stops working — and stays invisible until someone mints a new
repository from the template and gets a half-substituted tree.

THE GENERAL RULE, which plain replacement cannot express: a placeholder is
sometimes a VALUE TO FILL and sometimes the SUBJECT BEING DISCUSSED. Only the
first should be substituted. Three contexts make it the subject:

  1. THE LEFT-HAND SIDE OF A SUBSTITUTION — `s/{{X}}/.../`, `s|{{X}}|...|`.
     The token is a search pattern, not a value.
  2. FILES WHOSE TOPIC IS THE TOKEN — REQUIRES_INITIALISATION.md lists the
     tokens still to fill; QUICKSTART docs say "Replace {{DEPS}} with actuals".
     Filling those produced "Replace laminar, laminar, {{DEPS}} with actuals".
  3. ANOTHER TOOL'S OWN DELIMITERS — `just` uses `{{ARGS}}` natively in recipe
     bodies. It is not a template placeholder and never was; substituting or
     reporting it produces a permanently "uninitialised" repository.

⚠ THE EXCLUSIONS ARE THE POINT. A version of this that fills everything is
what already ran, and it is why 289 pull requests have to be closed. If you
extend this script, extend the exclusions with it and add a test.
"""
import argparse
import json
import os
import re
import sys

# A value can be correct AND wrong depending on the slot it lands in. These
# patterns mark lines where the text must be a machine-safe identifier or URL
# component — a Guix channel/package name, a BibTeX cite key, a URL path, a
# container tag. Substituting a human display name here produced, verbatim:
#
#     (name 'BoJ Server Mk2)
#     url "https://github.com/x/BoJ Server Mk2"
#     @software{BoJ Server Mk2_2026,
#
# all of which are syntactically invalid, and one repository managed
# `Octad-Recover (working title — see naming section below)` in the same slots.
SLUG_CONTEXT = re.compile(
    r"\(name\s+['\"]"          # Guix / Scheme  (name 'foo)
    r"|@[a-z]+\{"               # BibTeX entry key
    r"|https?://"               # any URL
    r"|-t\s+\S*$"               # container tag position
    r"|^\s*(name|package|slug|id)\s*[:=]",   # manifest identifier fields
    re.I | re.M)

# A value safe to drop into an identifier or URL component.
SLUG_SAFE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]*$")

# `just` uses these natively in recipe bodies; they are not template tokens.
FOREIGN_TOKENS = {"ARGS", "invocation_directory", "justfile", "os", "arch"}

# Files whose SUBJECT is the placeholder set. Substituting inside them turns
# instructions into nonsense.
SUBJECT_FILES = re.compile(
    r"(^|/)(REQUIRES_INITIALISATION\.(md|adoc)"
    r"|QUICKSTART[^/]*\.(md|adoc)"
    r"|PLACEHOLDERS?\.(md|adoc)"
    r"|TEMPLATE[^/]*\.(md|adoc))$", re.I)

# Anything ending .template is the template itself — its tokens must survive.
TEMPLATE_FILES = re.compile(r"\.template$|(^|/)templates?/", re.I)

# `s<delim>{{TOKEN}}<delim>` — the token is a search pattern here.
def _lhs_spans(line, token):
    """Character spans of TOKEN occurrences that sit on a substitution LHS."""
    spans = []
    for m in re.finditer(r"s([/|#,!@])\{\{" + re.escape(token) + r"\}\}\1", line):
        spans.append(m.span())
    return spans


def substitute_line(line, mapping, refusals=None):
    """Replace tokens in one line, leaving substitution LHS occurrences alone.

    Refuses to place a value that is not slug-safe into an identifier or URL
    slot, and records the refusal instead. Emitting invalid syntax silently is
    worse than leaving the token visible: an unfilled `{{PROJECT_NAME}}` is
    obviously unfinished, whereas `(name \'BoJ Server Mk2)` looks plausible and
    fails later, somewhere else.
    """
    slug_slot = bool(SLUG_CONTEXT.search(line))
    for token, value in mapping.items():
        if token in FOREIGN_TOKENS:
            continue
        needle = "{{" + token + "}}"
        if needle not in line:
            continue
        if slug_slot and not SLUG_SAFE.match(value):
            # Prefer an explicit slug if the caller supplied one.
            slug = mapping.get("PROJECT_SLUG") or mapping.get("REPO_SLUG")
            if slug and SLUG_SAFE.match(slug):
                line = line.replace(needle, slug)
                continue
            if refusals is not None:
                refusals.append((token, value, line.strip()[:90]))
            continue
        protected = _lhs_spans(line, token)
        if not protected:
            line = line.replace(needle, value)
            continue
        # Rebuild the line, skipping occurrences inside a protected span.
        out, i = [], 0
        for m in re.finditer(re.escape(needle), line):
            s, e = m.span()
            inside = any(ps <= s and e <= pe for ps, pe in protected)
            out.append(line[i:s])
            out.append(needle if inside else value)
            i = e
        out.append(line[i:])
        line = "".join(out)
    return line


def should_skip(path):
    if SUBJECT_FILES.search(path):
        return "file's subject IS the placeholder set"
    if TEMPLATE_FILES.search(path):
        return "template source — its tokens must survive"
    return None


def run(root, mapping, apply):
    changed, skipped, protected_hits, refusals = [], [], 0, []
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in (".git", "node_modules")]
        for fn in filenames:
            p = os.path.join(dirpath, fn)
            rel = os.path.relpath(p, root)
            reason = should_skip(rel)
            if reason:
                skipped.append((rel, reason))
                continue
            try:
                src = open(p, encoding="utf-8").read()
            except (UnicodeDecodeError, OSError):
                continue
            if "{{" not in src:
                continue
            lines = src.splitlines(True)
            new = []
            for l in lines:
                nl = substitute_line(l, mapping, refusals)
                if nl == l and any("{{" + t + "}}" in l for t in mapping):
                    protected_hits += 1
                new.append(nl)
            out = "".join(new)
            if out != src:
                changed.append(rel)
                if apply:
                    open(p, "w", encoding="utf-8").write(out)
    return changed, skipped, protected_hits, refusals


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("root")
    ap.add_argument("--map", required=True, help="JSON file of {TOKEN: value}")
    ap.add_argument("--apply", action="store_true")
    a = ap.parse_args()
    mapping = json.load(open(a.map))
    changed, skipped, prot, refusals = run(a.root, mapping, a.apply)
    print(f"  {'changed' if a.apply else 'would change'}: {len(changed)} file(s)")
    for c in changed[:20]:
        print(f"    {c}")
    print(f"  skipped (subject/template): {len(skipped)}")
    for s, why in skipped[:10]:
        print(f"    {s}  — {why}")
    print(f"  occurrences left in place (substitution LHS): {prot}")
    if refusals:
        print(f"  REFUSED — value not slug-safe for an identifier/URL slot: {len(refusals)}")
        for tok, val, ctx in refusals[:10]:
            print(f"    {{{{{tok}}}}} = {val!r}")
            print(f"      in: {ctx}")
        print("    Supply PROJECT_SLUG (or REPO_SLUG) in the map so these slots")
        print("    get a machine-safe value; the display name stays in prose.")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
