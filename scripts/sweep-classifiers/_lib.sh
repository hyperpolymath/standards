#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# _lib.sh — Shared helpers for classify-*.sh scripts.
#
# Sourced by every classifier. Not directly executable.
#
# Provides: survey_dir, survey_dir_init, normalize_input <file>
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

# Where the sweep stages its data between invocations.
#
# NOT /tmp. Three separate reasons, in the order they bite:
#   * a predictable, world-writable path is CWE-377 — on a shared machine
#     another user can create /tmp/drift-survey first, and these scripts then
#     read attacker-supplied "blobs" as survey evidence;
#   * TMPDIR is where a machine says temporary files belong, and a hardcoded
#     path ignores it;
#   * the data must be RE-FINDABLE. The classifier runs in a later invocation
#     than the sweep that wrote the JSON, so mktemp is the wrong tool here —
#     a random name the next stage cannot guess is not an improvement.
#
# XDG_STATE_HOME is the right home for it: user-private (0700), persistent
# across reboots, and already where this account keeps per-app state.
# Override for a CI job or a one-off with DRIFT_SURVEY_DIR.
survey_dir() {
  printf '%s\n' "${DRIFT_SURVEY_DIR:-${XDG_STATE_HOME:-${HOME:?HOME must be set to locate the survey directory}/.local/state}/drift-survey}"
}

# Create it private, whatever the ambient umask. A survey directory that other
# accounts can write to is the vulnerability this function exists to avoid.
survey_dir_init() {
  local dir
  dir="$(survey_dir)"
  [ -d "$dir" ] || (umask 077 && mkdir -p "$dir")
  printf '%s\n' "$dir"
}

normalize_input() {
  local file="$1"
  if [[ "$(head -c1 "$file")" == "{" ]]; then
    jq -r '[.repo, (.path // ""), .sha] | @tsv' "$file"
  else
    awk -F'\t' 'NF >= 3 { print $1 "\t" $2 "\t" $3 }' "$file"
  fi
}
