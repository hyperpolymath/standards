#!/usr/bin/env python3
"""Rewrite gh-actions-lock's tag-form keys back to the inline SHA refs used in
workflow files.

gh actions-lock prettifies `uses: owner/repo@<sha> # vX` to `owner/repo@vX`
and keys the lockfile by that tag. This estate keeps inline SHA pins in
workflow files (Mustfile actions-sha-pinned, governance linter, hypatia rule,
GitHub sha_pinning_required all enforce them), so after generating the
lockfile we restore the original workflows and re-key the lockfile entries to
the SHA form. Key SHA == recorded commit digest, which the lockfile format
already supports (validator requires key-SHA == digest).

Usage: relock-sha-keys.py <workflows-dir>
"""
import re
import sys
from pathlib import Path

wf_dir = Path(sys.argv[1] if len(sys.argv) > 1 else ".github/workflows")
lock_path = wf_dir / "actions.lock"
text = lock_path.read_text()

# Collect inline SHA refs from workflow files: action -> set of SHAs used.
uses_re = re.compile(r"uses:\s*([A-Za-z0-9_.-]+/[A-Za-z0-9_./-]+)@([0-9a-f]{40})")
inline: dict[str, set] = {}
for f in sorted(wf_dir.glob("*.yml")) + sorted(wf_dir.glob("*.yaml")):
    for m in uses_re.finditer(f.read_text()):
        # Lockfile entries key the repo root; inline refs may use a subpath
        # (github/codeql-action/init@sha) — index by owner/repo only.
        root = "/".join(m.group(1).split("/")[:2])
        inline.setdefault(root.lower(), set()).add(m.group(2))

# Parse lockfile dependency entries: key 'action@ref' with commit 'sha1-<sha>'.
entry_re = re.compile(
    r"^    '([^'@]+)@([^']+)':\n((?:^ {8}.*\n?)*)", re.MULTILINE
)
mapping = {}  # 'action@tagref' -> 'action@sha'
for m in entry_re.finditer(text):
    action, ref, body = m.group(1), m.group(2), m.group(3)
    if re.fullmatch(r"[0-9a-f]{40}", ref):
        continue  # already SHA-keyed
    cm = re.search(r"commit:\s*'sha1-([0-9a-f]{40})'", body)
    if not cm:
        continue
    sha = cm.group(1)
    # Only re-key entries whose digest matches a SHA actually written inline
    # in a workflow file (transitive deps inside external composites keep the
    # ref form their composite wrote).
    if sha in inline.get(action.lower(), set()):
        mapping[f"{action}@{ref}"] = f"{action}@{sha}"

for old, new in mapping.items():
    text = text.replace(f"'{old}'", f"'{new}'")

lock_path.write_text(text)
print(f"re-keyed {len(mapping)} entries:")
for old, new in sorted(mapping.items()):
    print(f"  {old} -> @{new.rsplit('@', 1)[1][:12]}")
