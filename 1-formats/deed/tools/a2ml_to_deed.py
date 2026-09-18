#!/usr/bin/env python3
"""a2ml→deed translator engine (campaign #837, conformance-lane half).

STDLIB-ONLY. Everything emitted is validated by deed_lint BEFORE it may be
written — an invalid deed can never leave the tool (the lane property).

Rules enforced (from the mapping specs in 1-formats/deed/mappings/):
  * `--beholding-chora NAME` is REQUIRED for real emission (refuse-to-emit
    otherwise; grammar: a repo-deed MUST name the chora it reads, and the
    body of #u5 literals is the RFC 4122 §4.3 NAME, never hex).
  * CLADE uuid: re-derived via RFC 4122 §4.3 (URL namespace) and compared
    against the instance — fail-closed on mismatch, never copied (P-1).
  * Booleans: a2ml true/false → deed #t/#f (only legal booleans).
  * Enums → symbols against closed sets; any untabled field FAILS the
    translation (P-2a table-closed) — it never passes through on guesswork.
  * Strings emit with exactly the four legal escapes; \\r or control bytes
    fail closed.
  * SPDX header lines of the a2ml source pass through as the deed's header
    (translation changes grammar, not licence facts).
  * STATE (family 3): classification report only — no translation pending
    the owner's ruling (#843).
  * Scorecards (family 5): scan/report only — no translation (#845), plus
    the absolute-path leak list.

Usage:
  a2ml_to_deed.py clade|meta-ecosystem|agentic|neurosym|playbook
                  FILE.a2ml --canonical-name N --beholding-chora C [--out F]
  a2ml_to_deed.py full DESCRITILES_DIR --canonical-name N --beholding-chora C [--out F]
  a2ml_to_deed.py state-scan FILE...
  a2ml_to_deed.py scorecard-scan DIR
"""
import argparse
import os
import re
import sys
import tomllib
import uuid

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deed_lint

# ---------------------------------------------------------------- helpers

SYMBOL_RE = re.compile(r"[A-Za-z][A-Za-z0-9.*/<>=!?+_-]*\Z")


class TranslateError(Exception):
    pass


def esc(s):
    """Emit one legal deed string literal; fail closed on un-representable bytes."""
    if not isinstance(s, str):
        raise TranslateError(f"expected str, got {type(s).__name__}")
    for ch in s:
        if ch in "\r" or (ord(ch) < 0x20 and ch not in "\n\t"):
            raise TranslateError(f"source string contains un-representable control byte U+{ord(ch):04X}")
    body = s.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n").replace("\t", "\\t")
    return f'"{body}"'


def sym(s, what="symbol", closed=None):
    if not isinstance(s, str) or not SYMBOL_RE.fullmatch(s):
        raise TranslateError(f"{what}: {s!r} is not a legal deed symbol")
    if closed is not None and s not in closed:
        raise TranslateError(f"{what}: {s!r} is not in the closed set {sorted(closed)} — table-closed fail")
    return s


def val(v, what="value", closed=None):
    if isinstance(v, bool):
        return "#t" if v else "#f"
    if isinstance(v, int):
        return str(v)
    if isinstance(v, str):
        if closed is not None:
            return sym(v, what, closed)
        return esc(v)
    if isinstance(v, list):
        return "(" + " ".join(val(x, what) for x in v) + ")"
    raise TranslateError(f"{what}: unsupported type {type(v).__name__}")


def spdx_of(path, profile=None):
    headers = []
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            m = re.match(r"^;;?\s*(SPDX-\S.*)$", line.rstrip("\n")) or re.match(
                r"^#\s*(SPDX-\S.*)$", line.rstrip("\n")
            )
            if m:
                headers.append(";; " + m.group(1))
            elif line.strip() and not line.startswith("#"):
                break
    if not headers:
        headers = [";; SPDX-License-Identifier: MPL-2.0"]
    return headers


def parse_a2ml(path):
    src = open(path, encoding="utf-8").read()
    prof = re.findall(r"^\s*@profile\(\s*id\s*=\s*([^\s)]+)\s*\)", src, flags=re.M)
    body = re.sub(r"^\s*@profile\([^\n]*\)\s*\n", "", src, flags=re.M)
    try:
        return tomllib.loads(body), (prof[0] if prof else None)
    except tomllib.TOMLDecodeError as e:
        raise TranslateError(f"source does not parse as a2ml/TOML ({e}) — P-2 fail-closed")


def require(data, section, keys, fam, required=None, defaults=None):
    if section not in data:
        raise TranslateError(f"{fam}: required section [{section}] missing")
    unknown = set(data[section]) - set(keys)
    if unknown:
        raise TranslateError(f"{fam}: untabled field(s) {sorted(unknown)} in [{section}] — P-2a fail-closed")
    for k in (required or []):
        if k not in data[section]:
            raise TranslateError(f"{fam}: required field {section}.{k} missing (registry gate territory — instance deficiency)")
    merged = dict(defaults or {})
    merged.update(data[section])
    return merged


CLADES = {
    "fv", "nl", "rm", "gv", "db", "ap", "ix", "dx", "pt", "ax", "gm", "sc", "UNASSIGNED"
}
LINEAGE = {"standalone", "monorepo", "monorepo-child", "inflated", "deflated", "hub", "satellite"}
PHASES = {"reserved", "incubating", "active", "dormant", "merged", "superseded", "archived", "extinct"}
META_TYPES = {"library", "binary", "service", "website", "monorepo"}
PKGMGR = {"guix", "cargo", "mix"}
RELATIONSHIPS = {
    "standard-source", "build-tooling", "validation-tooling", "signing-tooling", "verification-tooling"
}
SCAN_DEPTH = {"quick", "standard", "deep"}

ALLOWED_SECTIONS = {
    "clade-family": {"identity", "clade", "forges", "lineage", "status"},
    "meta": {"metadata", "project-info", "development-practices", "maintenance-axes",
             "scoping", "axis-2-maintenance-rules", "architecture-decisions",
             "axis-3-audit-rules", "design-rationale"},
    "ecosystem": {"metadata", "position", "pipeline", "related-projects"},
}


def emit_clade_clauses(path, data):
    extra = set(data) - {"identity", "clade", "forges", "lineage", "status"}
    if extra:
        raise TranslateError(f"clade: unknown top-level section(s) {sorted(extra)} — fail-closed")
    ident = require(data, "identity",
                ["uuid", "primary-forge", "primary-owner", "canonical-name", "prefixed-name"],
                "clade", required=["uuid", "primary-forge", "primary-owner", "canonical-name"],
                defaults={"prefixed-name": ""})
    owner, name = ident["primary-owner"], ident["canonical-name"]
    derived = uuid.uuid5(uuid.NAMESPACE_URL, f"github.com/{owner}/{name}")
    if str(derived) != ident["uuid"]:
        raise TranslateError(
            f"P-1 fail-closed: re-derived uuid {derived} != stored {ident['uuid']} — never copied, never emitted"
        )
    cl = []
    cl.append("  (identity :primary-forge " + sym(ident["primary-forge"], "identity.primary-forge"))
    cl.append("            :owner          " + esc(owner))
    cl.append("            :prefixed-name  " + esc(ident["prefixed-name"]) + ")")
    c = require(data, "clade", ["primary", "primary-name", "secondary", "assigned", "rationale"],
            "clade", required=["primary", "primary-name"],
            defaults={"secondary": [], "assigned": "", "rationale": ""})
    cl.append("")
    cl.append("  (clade    :primary       " + sym(c["primary"], "clade.primary", CLADES))
    cl.append("            :primary-name  " + esc(c["primary-name"]))
    cl.append("            :secondary     (" + " ".join(sym(x, "clade.secondary", CLADES) for x in c["secondary"]) + ")")
    cl.append("            :assigned      " + esc(c["assigned"]))
    cl.append("            :rationale     " + esc(c["rationale"]) + ")")
    fg = require(data, "forges", ["github", "gitlab", "bitbucket"], "clade",
             required=["github"], defaults={"gitlab": "", "bitbucket": ""})
    cl.append("")
    cl.append("  (forges   :github    " + esc(fg["github"]))
    cl.append("            :gitlab    " + esc(fg["gitlab"]))
    cl.append("            :bitbucket " + esc(fg["bitbucket"]) + ")")
    ln = require(data, "lineage", ["type", "parent", "born", "previous-names", "instantiated-from"],
             "clade", required=["type"], defaults={"parent": "", "born": "", "previous-names": [], "instantiated-from": ""})
    cl.append("")
    cl.append("  (lineage  :type              " + sym(ln["type"], "lineage.type", LINEAGE))
    cl.append("            :parent            " + esc(ln["parent"]))
    cl.append("            :born              " + esc(ln["born"]))
    cl.append("            :previous-names    " + val(ln["previous-names"]))
    cl.append("            :instantiated-from " + esc(ln["instantiated-from"]) + ")")
    st = require(data, "status",
             ["phase", "since", "present", "aliases", "merged-into", "superseded-by", "successors", "ended", "history"],
             "clade", required=["phase", "present"],
             defaults={"since": "", "aliases": [], "merged-into": "", "superseded-by": "", "successors": [], "ended": "", "history": []})
    cl.append("")
    cl.append("  (status   :phase         " + sym(st["phase"], "status.phase", PHASES))
    cl.append("            :since         " + esc(st["since"]))
    cl.append("            :present       " + val(st["present"]))
    cl.append("            :aliases       " + val(st["aliases"]))
    cl.append("            :merged-into   " + esc(st["merged-into"]))
    cl.append("            :superseded-by " + esc(st["superseded-by"]))
    cl.append("            :successors    " + val(st["successors"]))
    cl.append("            :ended         " + esc(st["ended"]))
    hist = st.get("history") or []
    if hist:
        cl.append("            (history")
        for h in hist:
            unknown = set(h) - {"phase", "since", "note"}
            if unknown:
                raise TranslateError(f"clade: history entry has untabled key(s) {sorted(unknown)}")
            missing = set(("phase", "since", "note")) - set(h)
            if missing:
                raise TranslateError(f"clade: history entry missing required key(s) {sorted(missing)}")
            cl.append("              (entry :phase " + sym(h["phase"], "history.phase", PHASES))
            cl.append("                     :since " + esc(h["since"]))
            cl.append("                     :note  " + esc(h["note"]) + ")")
        cl.append("            ))")
    else:
        cl[-1] = cl[-1] + ")"
    return ident["canonical-name"], "\n".join(cl)




def emit_meta_clause(data):
    extra = set(data) - ALLOWED_SECTIONS["meta"]
    if extra:
        raise TranslateError(f"meta: unknown section(s) {sorted(extra)} — fail-closed")
    L = ["  (meta"]
    md = require(data, "metadata", ["version", "last-updated"], "meta")
    L.append(f"        :version       {esc(md['version'])}")
    L.append(f"        :last-updated  {esc(md['last-updated'])}")
    pi = require(data, "project-info", ["type", "languages", "license", "author"], "meta")
    L.append(f"        :type          {sym(pi['type'], 'project-info.type', META_TYPES)}")
    L.append(f"        :languages     ({' '.join(sym(x, 'languages') for x in pi['languages'])})")
    L.append(f"        :license       {sym(pi['license'], 'project-info.license')}")
    L.append(f"        :author        {esc(pi['author'])}")
    dp = require(data, "development-practices", ["build-tool", "container-runtime", "ci-platform", "package-manager"], "meta")
    L.append(f"        :build-tool        {sym(dp['build-tool'])}")
    L.append(f"        :container-runtime {sym(dp['container-runtime'])}")
    L.append(f"        :ci-platform       {sym(dp['ci-platform'])}")
    L.append(f"        :package-manager   {sym(dp['package-manager'], 'package-manager', PKGMGR)}")
    ma = require(data, "maintenance-axes", ["scoping-first", "execution-order", "axis-1", "axis-2", "axis-3"], "meta")
    L.append(f"        :scoping-first   {val(ma['scoping-first'])}")
    for k in ("execution-order", "axis-1", "axis-2", "axis-3"):
        L.append(f"        :{k}  {esc(ma[k])}" if k != "axis-1" else f"        :{k}           {esc(ma[k])}")
    sc = require(data, "scoping", ["sources", "marker-scan", "idris-unsound-scan"], "meta")
    L.append("        (scoping")
    L.append(f"          :sources            {esc(sc['sources'])}")
    L.append(f"          :marker-scan        {esc(sc['marker-scan'])}")
    L.append(f"          :idris-unsound-scan {esc(sc['idris-unsound-scan'])})")
    a2 = require(data, "axis-2-maintenance-rules",
                 ["corrective-first", "adaptive-second", "adaptive-focus", "perfective-third", "perfective-source"], "meta")
    L.append(f"        :corrective-first  {val(a2['corrective-first'])}")
    L.append(f"        :adaptive-second   {val(a2['adaptive-second'])}")
    L.append(f"        :adaptive-focus    {esc(a2['adaptive-focus'])}")
    L.append(f"        :perfective-third  {val(a2['perfective-third'])}")
    L.append(f"        :perfective-source {esc(a2['perfective-source'])}")
    a3 = data.get("axis-3-audit-rules") or {}
    if a3:
        merged3 = require(data, "axis-3-audit-rules",
                          ["audit-focus", "compliance-focus", "drift-risk-example", "effects-evidence"], "meta",
                          defaults={"audit-focus": "", "compliance-focus": "", "drift-risk-example": "", "effects-evidence": ""})
        L.append("        (axis-3")
        L.append(f"          :audit-focus        {esc(merged3['audit-focus'])}")
        L.append(f"          :compliance-focus   {esc(merged3['compliance-focus'])}")
        L.append(f"          :drift-risk-example {esc(merged3['drift-risk-example'])}")
        L.append(f"          :effects-evidence   {esc(merged3['effects-evidence'])})")
    adrs = (data.get("architecture-decisions") or {}).get("adr") or []
    ADR_STATUSES = {"proposed", "accepted", "deprecated", "superseded", "rejected"}
    for adr in adrs:
        unknown = set(adr) - {"id", "title", "status", "date"}
        if unknown:
            raise TranslateError(f"meta: ADR entry untabled key(s) {sorted(unknown)}")
        L.append(f"        (adr :id {esc(adr['id'])} :title {esc(adr['title'])} :status {sym(adr['status'], 'adr.status', ADR_STATUSES)} :date {esc(adr['date'])})")
    if any((data.get("architecture-decisions") or {}).keys() - {"adr"}):
        raise TranslateError("meta: [architecture-decisions] has non-adr keys — fail-closed (family-2 §3)")
    if data.get("design-rationale"):
        raise TranslateError("meta: [design-rationale] populated — fail-closed pending table rows (family-2 §3)")
    return "\n".join(L) + ")"


def emit_ecosystem_clause(data, canonical_name):
    extra = set(data) - ALLOWED_SECTIONS["ecosystem"]
    if extra:
        raise TranslateError(f"ecosystem: unknown section(s) {sorted(extra)} — fail-closed")
    md = require(data, "metadata", ["project", "ecosystem"], "ecosystem")
    if md["project"] != canonical_name:
        raise TranslateError(
            f"P-6 fail-closed: ecosystem project {md['project']!r} != deed canonical-name {canonical_name!r}"
        )
    pos = require(data, "position", ["type", "purpose", "what-this-is-not"], "ecosystem")
    pipe = require(data, "pipeline", ["position", "chain", "notes", "coordination"], "ecosystem")
    rp = require(data, "related-projects", ["projects"], "ecosystem")
    L = ["  (ecosystem"]
    L.append(f"             :project           {esc(md['project'])}")
    L.append(f"             :ecosystem         {esc(md['ecosystem'])}")
    L.append(f"             :position-type     {esc(pos['type'])}")
    L.append(f"             :purpose           {esc(pos['purpose'])}")
    L.append("             :not               (" + " ".join(esc(x) for x in pos["what-this-is-not"]) + ")")
    L.append(f"             :pipeline-position {esc(pipe['position'])}")
    L.append(f"             :chain             {esc(pipe['chain'])}")
    L.append(f"             :pipeline-notes    {esc(pipe['notes'])}")
    coord = pipe["coordination"]
    L.append("             :coordination      " + (sym(coord, "coordination") if coord else '""'))
    for r in rp["projects"]:
        unknown = set(r) - {"name", "relationship", "notes"}
        if unknown:
            raise TranslateError(f"ecosystem: related-projects entry untabled key(s) {sorted(unknown)}")
        L.append(f"             (related :name {esc(r['name'])} :relationship {sym(r['relationship'], 'relationship', RELATIONSHIPS)} :notes {esc(r['notes'])})")
    if rp["projects"]:
        L[-1] = L[-1] + ")"
    else:
        L[-1] = L[-1] + ")"
    return "\n".join(L)


def emit_profiled_clause(fam, data, profile):
    if fam == "agentic":
        extra = set(data) - {"metadata", "agent-permissions", "agent-constraints", "maintenance-integrity", "automation-hooks", "methodology"}
        if extra:
            raise TranslateError(f"agentic: unknown section(s) {sorted(extra)}")
        md = require(data, "metadata", ["version", "last-updated"], "agentic")
        ap = require(data, "agent-permissions",
                     ["can-edit-source", "can-edit-tests", "can-edit-docs", "can-edit-config", "can-create-files"], "agentic")
        mi = require(data, "maintenance-integrity",
                     ["fail-closed", "require-evidence-per-step", "allow-silent-skip",
                      "require-rerun-after-fix", "release-claim-requires-hard-pass"], "agentic")
        for sec in ("agent-constraints", "automation-hooks"):
            if data.get(sec):
                raise TranslateError(
                    f"agentic: [{sec}] carries FIELDS — fail-closed (D4-1: comment-canon prose must stay prose; divergent field content earns its own table rows)")
        L = [f"  (agentic (profile :id {esc(profile)})" if profile else "  (agentic",
             f"            :version {esc(md['version'])} :last-updated {esc(md['last-updated'])}",
             "            (permissions  :source         " + val(ap["can-edit-source"]),
             "                          :tests          " + val(ap["can-edit-tests"]),
             "                          :docs           " + val(ap["can-edit-docs"]),
             "                          :config         " + val(ap["can-edit-config"]),
             "                          :create-files   " + val(ap["can-create-files"]) + ")",
             "            (integrity    :fail-closed                     " + val(mi["fail-closed"]),
             "                          :require-evidence-per-step       " + val(mi["require-evidence-per-step"]),
             "                          :allow-silent-skip               " + val(mi["allow-silent-skip"]),
             "                          :require-rerun-after-fix         " + val(mi["require-rerun-after-fix"]),
             "                          :release-claim-requires-hard-pass " + val(mi["release-claim-requires-hard-pass"]) + ")"]
        mo = data.get("methodology")
        if mo is not None:
            mo = require(data, "methodology", ["instructions-dir", "default-mode"], "agentic",
                         defaults={"instructions-dir": "", "default-mode": ""})
            line = "            (methodology"
            if mo["instructions-dir"]:
                line += f" :instructions-dir {esc(mo['instructions-dir'])}"
            if mo["default-mode"]:
                line += f" :default-mode {sym(mo['default-mode'], 'methodology.default-mode')}"
            L.append(line + ")")
            L[-1] = L[-1] + ")"
        else:
            L[-1] = L[-1] + ")"
        return "\n".join(L)
    if fam == "neurosym":
        extra = set(data) - {"metadata", "hypatia-config", "symbolic-rules", "neural-config"}
        if extra:
            raise TranslateError(f"neurosym: unknown section(s) {sorted(extra)}")
        md = require(data, "metadata", ["version", "last-updated"], "neurosym")
        hc = require(data, "hypatia-config", ["scan-enabled", "scan-depth", "report-format"], "neurosym")
        for sec in ("symbolic-rules", "neural-config"):
            if data.get(sec):
                raise TranslateError(f"neurosym: [{sec}] populated — fail-closed pending table rows")
        L = [f"  (neurosym (profile :id {esc(profile)})" if profile else "  (neurosym",
             f"             :version {esc(md['version'])} :last-updated {esc(md['last-updated'])}",
             "             (hypatia :scan-enabled  " + val(hc["scan-enabled"]),
             "                      :scan-depth    " + sym(hc["scan-depth"], "scan-depth", SCAN_DEPTH),
             f"                      :report-format {esc(hc['report-format'])}))"]
        return "\n".join(L)
    if fam == "playbook":
        extra = set(data) - {"metadata", "deployment", "incident-response", "release-process",
                             "docs-format", "maintenance-operations", "rsr-repo-skeleton"}
        if extra:
            raise TranslateError(f"playbook: unknown section(s) {sorted(extra)}")
        md = require(data, "metadata", ["version", "last-updated"], "playbook")
        for sec in ("deployment", "incident-response", "release-process", "docs-format", "maintenance-operations"):
            if data.get(sec):
                raise TranslateError(f"playbook: [{sec}] populated — fail-closed pending table rows")
        skel = ""
        if "rsr-repo-skeleton" in data:
            sk = require(data, "rsr-repo-skeleton",
                         ["skeleton-version", "last-updated", "authority-allowlist", "enforcement-workflow"],
                         "playbook", required=["skeleton-version"],
                         defaults={"last-updated": "", "authority-allowlist": "", "enforcement-workflow": ""})
            skel = ("            (skeleton :version              " + esc(sk["skeleton-version"]) +
                    "\n                      :last-updated         " + esc(sk["last-updated"]) +
                    "\n                      :authority-allowlist  " + esc(sk["authority-allowlist"]) +
                    "\n                      :enforcement-workflow " + esc(sk["enforcement-workflow"]) + ")")
        L = [f"  (playbook (profile :id {esc(profile)})" if profile else "  (playbook",
             f"            :version {esc(md['version'])} :last-updated {esc(md['last-updated'])}"]
        close = "            (deployment) (incident-response) (release-process) (docs-format) (maintenance-operations))"
        if skel:
            L.append(skel)
        L.append(close)
        return "\n".join(L)
    raise TranslateError(f"unknown family {fam!r}")


def compose_deed(headers, canonical_name, beholding, clauses, out_name=None):
    body = "\n\n".join(c for c in clauses if c)
    text = "\n".join(headers) + "\n" + \
        "(repo-deed\n" + \
        '  :schema-version  "1.0.0"\n' + \
        f"  :canonical-name  {esc(canonical_name)}\n" + \
        f"  :beholding-chora #u5{esc(beholding)}\n\n" + \
        body + "\n)\n"
    fname = out_name or f"{canonical_name}_chora.deed"
    try:
        deed_lint.validate(text, filename=fname)
    except deed_lint.LintError as e:
        raise TranslateError(f"POST-CONDITION FAILED — emitted deed does not conform ({e}); nothing written")
    return text, fname


def state_report(path, data):
    statal, journal, derivable, collision = [], [], [], []
    md = data.get("metadata", {})
    pc = data.get("project-context", {})
    pos = data.get("position", {})
    if "status" in md:
        collision.append(f'[metadata] status={md["status"]!r} — duplicates CLADE status.phase (CLADE wins, ruled vocabulary)')
    if "phase" in pos:
        statal.append(f'[position] phase={pos["phase"]!r} → (status …) clause')
    if "maturity" in pos:
        statal.append(f'[position] maturity={pos["maturity"]!r} → proposed (status … :maturity …) — one-field vocabulary extension, in the ruling request')
    if "completion-percentage" in pc:
        derivable.append(f'[project-context] completion-percentage={pc["completion-percentage"]} — derivable from milestone rows; dropped by v2 doctrine')
    for sec in ("route-to-mvp", "blockers-and-issues", "critical-next-actions", "maintenance-status", "ecosystem"):
        if data.get(sec):
            n = len(data[sec].get("milestones", data[sec].get("actions", []))) if isinstance(data[sec], dict) else 0
            journal.append(f"[{sec}] — journal content ({n} rows); does NOT translate under option B; tombstoned to archive")
    return f"""STATE v1 classification — {path}
  STATAL (extract to CLADE status clause): {chr(10).join('  - ' + s for s in statal) or '  - none'}
  COLLISIONS: {chr(10).join('  - ' + s for s in collision) or '  - none'}
  DERIVABLE (dropped): {chr(10).join('  - ' + s for s in derivable) or '  - none'}
  JOURNAL (archive, not translate): {chr(10).join('  - ' + s for s in journal) or '  - none'}
  Ruling pending: standards#843 (family-3 decision spec)."""


def scorecard_scan(root):
    rows = []
    for dirpath, _, files in os.walk(root):
        for f in sorted(files):
            if not f.endswith(".scorecard.a2ml"):
                continue
            p = os.path.join(dirpath, f)
            data, _ = parse_a2ml(p)
            sc = data.get("scorecard", {})
            counts = {}
            leaks = 0
            for tier in ("must", "should", "could"):
                entries = data.get(tier) or []
                pas = sum(1 for e in entries if e.get("status") == "pass")
                fail = sum(1 for e in entries if e.get("status") == "fail")
                counts[tier] = (pas, fail)
                for e in entries:
                    for v in e.values():
                        if isinstance(v, str) and re.search(r"/home/[A-Za-z0-9._-]+/", v):
                            leaks += 1
            rows.append((f, sc.get("spec_id", "?"), sc.get("assessed_date", "?"), sc.get("assessor", "?"),
                         counts["must"], counts["should"], counts["could"], leaks))
    out = ["file\tspec_id\tassessed_date\tassessor\tmust_pass\tmust_fail\tshould_pass\tshould_fail\tcould_pass\tcould_fail\tabsolute_path_leak_lines"]
    out += ["\t".join(map(str, r[:4])) + "\t" + "\t".join(map(str, sum((list(c) for c in r[4:7]), []))) + f"\t{r[7]}"
            for r in rows]
    return "\n".join(out)


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("mode", choices=["clade", "meta-ecosystem", "agentic", "neurosym", "playbook",
                                     "full", "state-scan", "scorecard-scan"])
    ap.add_argument("inputs", nargs="+")
    ap.add_argument("--canonical-name")
    ap.add_argument("--beholding-chora")
    ap.add_argument("--out")
    a = ap.parse_args(argv[1:])

    try:
        if a.mode == "scorecard-scan":
            print(scorecard_scan(a.inputs[0]))
            return 0
        if a.mode == "state-scan":
            for p in a.inputs:
                data, _ = parse_a2ml(p)
                print(state_report(p, data))
            return 0
        for req in ("canonical_name", "beholding_chora"):
            if not getattr(a, req):
                print(f"error: --{req.replace('_', '-')} is required (refuse-to-emit)", file=sys.stderr)
                return 2
        clauses = []
        headers = None
        if a.mode == "full":
            from pathlib import Path
            d = Path(a.inputs[0])
            found = {}
            for ext in (".machine_readable/descriptiles", ".machine_readable"):
                for f in ("CLADE", "META", "ECOSYSTEM", "AGENTIC", "NEUROSYM", "PLAYBOOK"):
                    cand = d / ext / f"{f}.a2ml"
                    if cand.exists() and f not in found:
                        found[f] = cand
            if "CLADE" not in found:
                raise TranslateError(f"full: no CLADE.a2ml under {d}")
            cpath = str(found["CLADE"])
            data, _ = parse_a2ml(cpath)
            can, cl = emit_clade_clauses(cpath, data)
            if can != a.canonical_name:
                raise TranslateError(f"--canonical-name {a.canonical_name!r} != CLADE canonical-name {can!r}")
            headers = spdx_of(cpath)
            clauses.append(cl)
            cname = can
            if "META" in found:
                clauses.append(emit_meta_clause(parse_a2ml(str(found["META"]))[0]))
            if "ECOSYSTEM" in found:
                clauses.append(emit_ecosystem_clause(parse_a2ml(str(found["ECOSYSTEM"]))[0], cname))
            for fam, key in (("AGENTIC", "agentic"), ("NEUROSYM", "neurosym"), ("PLAYBOOK", "playbook")):
                if fam in found:
                    d2, prof = parse_a2ml(str(found[fam]))
                    clauses.append(emit_profiled_clause(key, d2, prof))
            # clade emit already includes identity/clade/forges/lineage/status;
            # head needs :repo-uuid injected before identity clause:
            cl0 = data["identity"]
            clauses[0] = ('  :repo-uuid       #u5"github.com/%s/%s"\n\n' % (cl0["primary-owner"], cname)) + clauses[0]
        else:
            path = a.inputs[0]
            data, prof = parse_a2ml(path)
            headers = spdx_of(path)
            if a.mode == "clade":
                cname, cl0 = emit_clade_clauses(path, data)
                if cname != a.canonical_name:
                    raise TranslateError(f"--canonical-name mismatch: {a.canonical_name} vs {cname}")
                clauses.append('  :repo-uuid       #u5"github.com/%s/%s"\n\n' % (data["identity"]["primary-owner"], cname) + cl0)
            elif a.mode == "meta-ecosystem":
                raise TranslateError("use 'full DESCRITILES_DIR' (meta and ecosystem join the repo deed, they are not standalone docs)")
            else:
                clauses.append(emit_profiled_clause(a.mode, data, prof))
                cname = a.canonical_name
        text, fname = compose_deed(headers, a.canonical_name, a.beholding_chora, clauses, out_name=a.out)
        if a.out:
            with open(a.out, "w", encoding="utf-8") as fh:
                fh.write(text)
            print(f"EMITTED {a.out} (validated by deed_lint, dispatch {fname})")
        else:
            sys.stdout.write(text)
        return 0
    except (TranslateError, deed_lint.LintError) as e:
        print(f"FAIL-CLOSED: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
