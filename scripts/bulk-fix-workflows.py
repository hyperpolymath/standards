#!/usr/bin/env python3
"""
Bulk script to replace duplicate workflow files with calls to root reusable workflows.

Usage: python3 scripts/bulk-fix-workflows.py --dry-run
       python3 scripts/bulk-fix-workflows.py --execute
"""

import os
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).parent.parent.absolute()

# Workflows to fix and their reusable equivalents
WORKFLOWS = {
    "codeql.yml": {
        "reusable": "codeql-reusable.yml",
        "triggers": ["push", "pull_request", "schedule"],
        "content": """# SPDX-License-Identifier: MPL-2.0
name: CodeQL Security Analysis

on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]
  schedule:
    - cron: '0 6 * * 1'

concurrency:
  group: ${{{ github.workflow }}}-${{{ github.ref }}}
  cancel-in-progress: false

permissions:
  contents: read

jobs:
  analyze-js:
    uses: {relpath}/.github/workflows/codeql-reusable.yml
    with:
      language: javascript-typescript
""",
    },
    "governance.yml": {
        "reusable": "governance-reusable.yml",
        "triggers": ["push", "pull_request", "workflow_dispatch"],
        "content": """# SPDX-License-Identifier: MPL-2.0
# governance.yml — single wrapper calling the shared estate governance bundle
# in hyperpolymath/standards instead of carrying per-repo copies.
#
# Replaces the per-repo governance scaffolding removed in the same commit:
#   quality.yml, guix-nix-policy.yml, npm-bun-blocker.yml, ts-blocker.yml,
#   security-policy.yml, rsr-antipattern.yml, wellknown-enforcement.yml,
#   workflow-linter.yml
#
# Load-bearing build/security workflows stay standalone in the repo
# (rust-ci, codeql, dependabot, release, scan/mirror/pages plumbing).

name: Governance

on:
  push:
    branches: [main, master]
  pull_request:
  workflow_dispatch:

permissions:
  contents: read

jobs:
  governance:
    uses: {relpath}/.github/workflows/governance-reusable.yml
""",
    },
    "hypatia-scan.yml": {
        "reusable": "hypatia-scan-reusable.yml",
        "triggers": ["push", "pull_request", "schedule", "workflow_dispatch"],
        "content": """# SPDX-License-Identifier: MPL-2.0
name: Hypatia Security Scan

on:
  push:
    branches: [main, master, develop]
  pull_request:
    branches: [main, master]
  schedule:
    - cron: '0 0 * * 0'
  workflow_dispatch:

permissions:
  actions: read
  contents: read
  security-events: write

jobs:
  scan:
    uses: {relpath}/.github/workflows/hypatia-scan-reusable.yml
    secrets: inherit
""",
    },
    "instant-sync.yml": {
        "reusable": None,  # No reusable, but we have a standard version
        "triggers": ["push", "release"],
        "content": """# SPDX-License-Identifier: MPL-2.0
# Instant Forge Sync - Triggers propagation to all forges on push/release
name: Instant Sync

on:
  push:
    branches: [main, master]
  release:
    types: [published]

concurrency:
  group: ${{{ github.workflow }}}-${{{ github.ref }}}
  cancel-in-progress: true

permissions:
  actions: read
  contents: read

jobs:
  dispatch:
    timeout-minutes: 10
    runs-on: ubuntu-latest
    env:
      FARM_DISPATCH_TOKEN: ${{{ secrets.FARM_DISPATCH_TOKEN }}}
    steps:
      - name: Trigger Propagation
        if: ${{{ env.FARM_DISPATCH_TOKEN != '' }}}
        uses: peter-evans/repository-dispatch@28959ce8df70de7be546dd1250a005dd32156697 # v3
        with:
          token: ${{{ secrets.FARM_DISPATCH_TOKEN }}}
          repository: hyperpolymath/.git-private-farm
          event-type: propagate
          client-payload: |-
            {
              "repo": "${{{ github.event.repository.name }}}",
              "ref": "${{{ github.ref }}}",
              "sha": "${{{ github.sha }}}",
              "forges": ""
            }

      - name: Skipped (FARM_DISPATCH_TOKEN not configured)
        if: ${{{ env.FARM_DISPATCH_TOKEN == '' }}}
        env:
          REPO_NAME: ${{{ github.event.repository.name }}}
        run: |
          echo "::notice::FARM_DISPATCH_TOKEN secret not configured on ${{{ REPO_NAME }}}; skipping cross-repo dispatch."

      - name: Confirm
        env:
          REPO_NAME: ${{{ github.event.repository.name }}}
        run: echo "::notice::Propagation triggered for ${{{ REPO_NAME }}}"
""",
    },
    "mirror.yml": {
        "reusable": "mirror-reusable.yml",
        "triggers": ["push", "workflow_dispatch"],
        "content": """# SPDX-License-Identifier: MPL-2.0
name: Mirror to Git Forges

on:
  push:
    branches: [main]
  workflow_dispatch:

permissions:
  contents: read

jobs:
  mirror:
    uses: {relpath}/.github/workflows/mirror-reusable.yml
    secrets: inherit
""",
    },
    "scorecard.yml": {
        "reusable": "scorecard-reusable.yml",
        "triggers": ["push", "schedule", "workflow_dispatch"],
        "content": """# SPDX-License-Identifier: MPL-2.0
name: OSSF Scorecard

on:
  push:
    branches: [main, master]
  schedule:
    - cron: '0 4 * * *'
  workflow_dispatch:

permissions:
  contents: read

jobs:
  scorecard:
    uses: {relpath}/.github/workflows/scorecard-reusable.yml
""",
    },
    "scorecard-enforcer.yml": {
        "reusable": "scorecard-reusable.yml",
        "triggers": ["push", "schedule", "workflow_dispatch"],
        "content": """# SPDX-License-Identifier: MPL-2.0
# Prevention workflow - runs OpenSSF Scorecard and fails on low scores
name: OpenSSF Scorecard Enforcer

on:
  push:
    branches: [main]
  schedule:
    - cron: '0 6 * * 1'
  workflow_dispatch:

permissions:
  contents: read

jobs:
  scorecard:
    uses: {relpath}/.github/workflows/scorecard-reusable.yml
""",
    },
    "secret-scanner.yml": {
        "reusable": "secret-scanner-reusable.yml",
        "triggers": ["push", "pull_request"],
        "content": """# SPDX-License-Identifier: MPL-2.0
name: Secret Scanner

on:
  pull_request:
  push:
    branches: [main]

concurrency:
  group: ${{{ github.workflow }}}-${{{ github.ref }}}
  cancel-in-progress: true

permissions:
  contents: read

jobs:
  scan:
    permissions:
      contents: read
    uses: {relpath}/.github/workflows/secret-scanner-reusable.yml
    secrets: inherit
""",
    },
}


def get_relative_path(target_dir):
    """Calculate the relative path from target_dir to repo root .github/workflows/"""
    target_path = Path(target_dir).resolve()
    root_path = REPO_ROOT.resolve()
    
    # Count the number of levels between target_dir and repo root
    try:
        rel = target_path.relative_to(root_path)
        depth = len(rel.parts)
    except ValueError:
        depth = 0
    
    # Go up depth levels, then down to .github/workflows/
    return "../" * depth + ".github/workflows/"


def find_workflow_dirs():
    """Find all directories containing .github/workflows/"""
    workflow_dirs = []
    for root, dirs, files in os.walk(REPO_ROOT):
        if ".github/workflows" in dirs:
            workflow_dir = Path(root) / ".github/workflows"
            # Skip the root .github/workflows
            if workflow_dir != REPO_ROOT / ".github/workflows":
                workflow_dirs.append(workflow_dir)
    return workflow_dirs


def needs_fix(workflow_dir, workflow_name):
    """Check if a workflow file needs to be fixed (i.e., doesn't already use reusable)"""
    workflow_file = workflow_dir / workflow_name
    if not workflow_file.exists():
        return False
    
    content = workflow_file.read_text()
    # Check if it already uses a reusable workflow
    if "uses:" in content and "reusable" in content:
        return False
    
    return True


def fix_workflow(workflow_dir, workflow_name, dry_run=True):
    """Replace a workflow file with a call to the reusable version"""
    workflow_file = workflow_dir / workflow_name
    
    if not workflow_file.exists():
        return None
    
    # Calculate relative path
    rel_path = get_relative_path(workflow_dir.parent)
    
    # Get the template
    if workflow_name not in WORKFLOWS:
        return None
    
    template = WORKFLOWS[workflow_name]["content"]
    content = template.format(relpath=rel_path)
    
    if dry_run:
        print(f"  WOULD FIX: {workflow_file}")
        print(f"    Using: {rel_path}.github/workflows/{WORKFLOWS[workflow_name]['reusable']}")
        return (workflow_file, content)
    else:
        workflow_file.write_text(content)
        print(f"  FIXED: {workflow_file}")
        return (workflow_file, content)


def main():
    dry_run = "--dry-run" in sys.argv
    execute = "--execute" in sys.argv
    
    if not execute and not dry_run:
        print("Usage: python3 scripts/bulk-fix-workflows.py --dry-run")
        print("       python3 scripts/bulk-fix-workflows.py --execute")
        return
    
    workflow_dirs = find_workflow_dirs()
    print(f"Found {len(workflow_dirs)} workflow directories")
    print()
    
    total_found = 0
    total_fixed = 0
    
    for workflow_dir in sorted(workflow_dirs):
        # Skip if already processed
        rel_dir = workflow_dir.relative_to(REPO_ROOT)
        print(f"Processing: {rel_dir}")
        
        for workflow_name in WORKFLOWS:
            if needs_fix(workflow_dir, workflow_name):
                total_found += 1
                if dry_run:
                    fix_workflow(workflow_dir, workflow_name, dry_run=True)
                else:
                    fix_workflow(workflow_dir, workflow_name, dry_run=False)
                    total_fixed += 1
    
    print()
    print(f"Summary: {total_found} workflow files need fixing, {total_fixed} fixed")


if __name__ == "__main__":
    main()
