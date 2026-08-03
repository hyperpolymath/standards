#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Re-key gh-actions-lock's tag-form lockfile entries back to the inline SHA
# refs used in the workflow files.
#
# gh actions-lock prettifies `uses: owner/repo@<sha> # vX` to `owner/repo@vX`
# and keys .github/workflows/actions.lock by that tag. This estate keeps
# inline SHA pins in workflow files (Mustfile actions-sha-pinned, governance
# workflow linter, hypatia rule, and GitHub's sha_pinning_required all enforce
# them), so after every `gh actions-lock` regeneration:
#
#   1. restore the SHA-pinned workflow files (git checkout -- ...)
#   2. run this script to re-key the lockfile to match
#
# Entries are only re-keyed when the recorded commit digest matches a SHA
# actually written inline in a workflow file (subpath-aware: the lockfile
# keys `github/codeql-action` while workflows may pin
# `github/codeql-action/init@<sha>`). Transitive deps inside external
# composites keep the ref form their composite wrote. Key SHA == commit
# digest, which the lockfile format already supports.
#
# Usage: scripts/relock-sha-keys.sh [workflows-dir]   (default .github/workflows)

set -euo pipefail
wf_dir="${1:-.github/workflows}"

python3 - "$wf_dir" <<'PY'
import re
import sys
from pathlib import Path

wf_dir = Path(sys.argv[1])
lock_path = wf_dir / "actions.lock"
text = lock_path.read_text()

# Inline SHA refs from workflow files: repo root (lowercase) -> set of SHAs.
uses_re = re.compile(r"uses:\s*([A-Za-z0-9_.-]+/[A-Za-z0-9_./-]+)@([0-9a-f]{40})")
inline = {}
for f in sorted(wf_dir.glob("*.yml")) + sorted(wf_dir.glob("*.yaml")):
    for m in uses_re.finditer(f.read_text()):
        root = "/".join(m.group(1).split("/")[:2]).lower()
        inline.setdefault(root, set()).add(m.group(2))

# Lockfile dependency entries: key 'action@ref' with commit 'sha1-<sha>'.
entry_re = re.compile(r"^    '([^'@]+)@([^']+)':\n((?:^ {8}.*\n?)*)", re.MULTILINE)
mapping = {}
for m in entry_re.finditer(text):
    action, ref, body = m.group(1), m.group(2), m.group(3)
    if re.fullmatch(r"[0-9a-f]{40}", ref):
        continue  # already SHA-keyed
    cm = re.search(r"commit:\s*'sha1-([0-9a-f]{40})'", body)
    if not cm:
        continue
    sha = cm.group(1)
    if sha in inline.get(action.lower(), set()):
        mapping[f"{action}@{ref}"] = f"{action}@{sha}"

for old, new in mapping.items():
    text = text.replace(f"'{old}'", f"'{new}'")

lock_path.write_text(text)
print(f"re-keyed {len(mapping)} entries:")
for old, new in sorted(mapping.items()):
    print(f"  {old} -> @{new.rsplit('@', 1)[1][:12]}")
PY
