#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# _lib.sh — Shared helpers for classify-*.sh scripts.
#
# Sourced by every classifier. Not directly executable.
#
# Provides: normalize_input <file>
#
#   Emits one TSV row per workflow file: "<repo>\t<path>\t<sha>".
#   Accepts two input shapes auto-detected from the first line:
#
#     1. JSONL from `gh api /search/code` (legacy):
#          {"repo":"foo","sha":"abc123"}
#        Emitted path is empty (no nested-copy info available).
#
#     2. TSV from `list-workflow-paths.sh` (preferred — handles nested):
#          foo<TAB><path><TAB>abc123<TAB>top-level|nested
#        Emitted path is the original `<path>` value; the scope column
#        is dropped.

normalize_input() {
  local file="$1"
  if [[ "$(head -c1 "$file")" == "{" ]]; then
    jq -r '[.repo, (.path // ""), .sha] | @tsv' "$file"
  else
    awk -F'\t' 'NF >= 3 { print $1 "\t" $2 "\t" $3 }' "$file"
  fi
}
