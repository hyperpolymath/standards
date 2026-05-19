#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# propagate-gitignore-67-68.sh — Propagate canonical .gitignore additions
#   for standards#67 (package-lock.json) and standards#68 (.editorconfig /
#   .claude/) to consumer repos.
#
# Mode: READ-ONLY (audit) by default.
#   Pass --fix to stage changes on a branch in each affected repo.
#   The script NEVER commits or pushes — that is the human's job.
#
# Principles (do not violate):
#   * Non-destructive: only modifies .gitignore (never removes tracked files).
#   * Idempotent: entries already present are not duplicated.
#   * No auto-commit / auto-push (estate guardrail: no unattended mutations).
#   * Shell-only: no Python, no SaltStack, no Ruby.
#
# Usage:
#   bash standards/scripts/propagate-gitignore-67-68.sh [--fix] [REPO_DIR]
#
#   REPO_DIR defaults to ~/dev/repos.
#
# With --fix, for each affected repo this script:
#   1. Creates branch chore/gitignore-67-68 off default branch (if not exists).
#   2. Appends missing entries to .gitignore.
#   3. git rm --cached any tracked package-lock.json (staged, not committed).
#   Leaves commits + push to the human operator.
#
# Output: tab-separated audit lines suitable for review.

set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

ISSUES_REF="Refs hyperpolymath/standards#67\nRefs hyperpolymath/standards#68"
BRANCH="chore/gitignore-67-68"
FIX_MODE=false
REPO_ROOT="${HOME}/dev/repos"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --fix) FIX_MODE=true; shift ;;
        *) REPO_ROOT="$1"; shift ;;
    esac
done

# Canonical .gitignore block to inject (only entries not already present)
BLOCK_67='# npm-avoidant (standards#67): estate JS-runtime policy is Deno>Bun>pnpm>npm.
# npm lockfiles must never be committed estate-wide.
package-lock.json
**/package-lock.json'

BLOCK_68='# Agent & editor scaffolding (standards#68) — local-only, never committed
# estate-wide. Owner decision 2026-05-16: gitignore both rather than commit
# per-repo agent/editor config.
.editorconfig
.claude/'

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

log() { printf '%s\n' "$*" >&2; }
sep() { log "---"; }

# Returns 0 if pattern is already in .gitignore (or if no .gitignore present)
already_ignored() {
    local gi="$1" pattern="$2"
    [ -f "$gi" ] && grep -qxF "$pattern" "$gi"
}

append_if_missing() {
    local gi="$1"; shift
    local header="$1"; shift
    local -a entries=("$@")
    local needs_header=false
    for entry in "${entries[@]}"; do
        already_ignored "$gi" "$entry" || needs_header=true
    done
    if $needs_header; then
        printf '\n%s\n' "$header" >> "$gi"
        for entry in "${entries[@]}"; do
            if ! already_ignored "$gi" "$entry"; then
                printf '%s\n' "$entry" >> "$gi"
            fi
        done
    fi
}

# ---------------------------------------------------------------------------
# Main audit loop
# ---------------------------------------------------------------------------

echo "# propagate-gitignore-67-68.sh audit — $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "# mode: $([ "$FIX_MODE" = true ] && echo FIX || echo DRY-RUN)"
echo "# repo_root: $REPO_ROOT"
echo "#"
printf '%-50s\t%s\t%s\t%s\t%s\n' "REPO" "PKG_LOCK_TRACKED" "EDITORCONFIG_TRACKED" "CLAUDE_TRACKED" "GITIGNORE_HAS_67_68"

for repo in "$REPO_ROOT"/*/; do
    [ -d "$repo/.git" ] || continue
    name="${repo%/}"
    name="${name##*/}"

    pkg_lock_tracked=$(git -C "$repo" ls-files 'package-lock.json' '**/package-lock.json' 2>/dev/null | wc -l | tr -d ' ')
    editorconfig_tracked=$(git -C "$repo" ls-files '.editorconfig' 2>/dev/null | wc -l | tr -d ' ')
    claude_tracked=$(git -C "$repo" ls-files '.claude/' 2>/dev/null | wc -l | tr -d ' ')

    gi="$repo/.gitignore"
    gi_has_67=true
    gi_has_68=true
    { already_ignored "$gi" "package-lock.json" && already_ignored "$gi" "**/package-lock.json"; } || gi_has_67=false
    { already_ignored "$gi" ".editorconfig" && already_ignored "$gi" ".claude/"; } || gi_has_68=false
    gi_status="$([ "$gi_has_67" = true ] && echo y || echo N)/$([ "$gi_has_68" = true ] && echo y || echo N)"

    printf '%-50s\t%s\t%s\t%s\t%s\n' \
        "$name" "$pkg_lock_tracked" "$editorconfig_tracked" "$claude_tracked" "$gi_status"

    if $FIX_MODE; then
        needs_work=false
        $gi_has_67 || needs_work=true
        $gi_has_68 || needs_work=true
        [ "$pkg_lock_tracked" -gt 0 ] && needs_work=true

        if $needs_work; then
            default_branch=$(git -C "$repo" remote show origin 2>/dev/null \
                | grep 'HEAD branch' | cut -d' ' -f5 || echo main)
            current_branch=$(git -C "$repo" branch --show-current 2>/dev/null || echo "")

            # Create fix branch if not already on it
            if [ "$current_branch" != "$BRANCH" ]; then
                if git -C "$repo" rev-parse --verify "refs/heads/$BRANCH" >/dev/null 2>&1; then
                    git -C "$repo" checkout "$BRANCH" 2>/dev/null
                else
                    git -C "$repo" fetch origin "$default_branch" 2>/dev/null || true
                    git -C "$repo" checkout -b "$BRANCH" "origin/$default_branch" 2>/dev/null \
                        || git -C "$repo" checkout "$BRANCH" 2>/dev/null
                fi
            fi

            # Ensure .gitignore exists
            [ -f "$gi" ] || touch "$gi"

            # Append missing blocks
            if ! $gi_has_67; then
                append_if_missing "$gi" \
                    "# npm-avoidant (standards#67): estate JS-runtime policy is Deno>Bun>pnpm>npm." \
                    "# npm lockfiles must never be committed estate-wide." \
                    "package-lock.json" \
                    "**/package-lock.json"
            fi
            if ! $gi_has_68; then
                append_if_missing "$gi" \
                    "# Agent & editor scaffolding (standards#68) — local-only, never committed" \
                    "# estate-wide. Owner decision 2026-05-16: gitignore both rather than commit" \
                    "# per-repo agent/editor config." \
                    ".editorconfig" \
                    ".claude/"
            fi

            # Un-track package-lock.json if tracked
            if [ "$pkg_lock_tracked" -gt 0 ]; then
                git -C "$repo" ls-files 'package-lock.json' '**/package-lock.json' 2>/dev/null \
                | while IFS= read -r f; do
                    git -C "$repo" rm --cached "$f" 2>/dev/null && log "  [rm-cached] $name: $f"
                done
            fi

            log "  [fix applied] $name — branch: $BRANCH"
        fi
    fi
done

echo "#"
echo "# Audit complete."
echo "# --fix mode: review per-repo branches and open draft PRs manually."
echo "# References: $ISSUES_REF"
