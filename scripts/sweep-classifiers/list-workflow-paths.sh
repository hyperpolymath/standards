#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# list-workflow-paths.sh — enumerate every (repo, path, blob-sha) tuple
# for a given workflow filename across the hyperpolymath estate, INCLUDING
# nested copies in monorepos.
#
# Why this exists (the 3-layer GitHub Code Search undercount):
#
#   Layer 1 — path-prefix filter excludes nested workflows.
#     `path:.github/workflows` matches the path PREFIX, so files like
#     `a2ml/bindings/deno/.github/workflows/secret-scanner.yml` (inside
#     the standards monorepo) are NEVER returned by the path-filtered
#     query. Dropping the `path:` filter helps, but introduces Layer 2.
#
#   Layer 2 — org-scoped /search/code is severely truncated.
#     A broad `filename:codeql.yml org:hyperpolymath` query against the
#     code-search endpoint returns ~190 paths total, but per-repo
#     enumeration of just developer-ecosystem reveals 170 codeql.yml
#     files — i.e. the broad query missed ~518 nested copies
#     estate-wide. The endpoint silently drops results once it hits
#     internal caps.
#
#   Layer 3 — nested workflows don't actually execute.
#     GitHub Actions only runs workflows from the repo-root
#     `.github/workflows/` directory. Nested copies are inert
#     (vendored templates or stale leftover). For SECURITY-driven
#     campaigns (e.g. the secret-scanner shell-secrets propagation),
#     nested copies do NOT close additional attack surface. For
#     SINGLE-SOURCE-OF-TRUTH campaigns they still matter.
#
# This script bypasses Layer 1 and Layer 2 by walking the repo list
# directly and using each repo's git-tree API (which IS exhaustive).
#
# Usage:
#   ./list-workflow-paths.sh <workflow-filename>
#
# Output (TSV to stdout, one row per matching file):
#   <repo>\t<path>\t<blob-sha>\t<top-level|nested>
#
# Example:
#   ./list-workflow-paths.sh codeql.yml \
#     > /tmp/drift-survey/codeql-all-tuples.tsv
#
# Notes:
#   - Requires `gh` CLI + jq.
#   - Honours $GH_ORG (default: hyperpolymath).
#   - Skips archived and disabled repos.
#   - Output is stable-sorted by repo then path.
#   - Each repo costs one Git Tree API call (cheap; uses core
#     bucket, not code_search). 300 repos ≈ 300 API calls;
#     under the 5000/hr core limit.

set -euo pipefail

WORKFLOW="${1:?usage: $0 <workflow-filename>}"
GH_ORG="${GH_ORG:-hyperpolymath}"

# 1. Get every non-archived repo in the org.
gh repo list "$GH_ORG" --limit 1000 --no-archived \
  --json name,defaultBranchRef \
  --jq '.[] | select(.defaultBranchRef != null) |
         "\(.name)\t\(.defaultBranchRef.name)"' \
  | sort \
  | while IFS=$'\t' read -r repo branch; do
      # 2. Pull the full recursive tree for the default branch.
      # 200 (success), 404 (empty repo), 422 (truncated — see below).
      gh api "repos/$GH_ORG/$repo/git/trees/$branch?recursive=1" \
        2>/dev/null \
        | jq -r --arg wf "$WORKFLOW" --arg r "$repo" '
            .tree[]?
            | select(.type == "blob")
            | select(.path | endswith("/.github/workflows/" + $wf)
                          or . == ".github/workflows/" + $wf)
            | "\($r)\t\(.path)\t\(.sha)\t" +
              (if .path == ".github/workflows/" + $wf
                 then "top-level"
                 else "nested"
               end)' \
        2>/dev/null || true
    done

# Caveat — if a repo's tree is large enough to hit the 100k-entry
# truncation limit, the recursive=1 call returns `"truncated": true`
# and may silently drop entries beyond the cap. None of the
# hyperpolymath monorepos are believed to hit this, but if a repo's
# blob count grows past 100k, switch to per-subdir tree calls.
