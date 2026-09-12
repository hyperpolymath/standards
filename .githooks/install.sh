#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Hyperpolymath Estate Git Hooks Installer

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

if ! git -C "$ROOT" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "ERROR: Not a git repository: $ROOT" >&2
  exit 1
fi

GITDIR="$(git -C "$ROOT" rev-parse --git-common-dir 2>/dev/null || git -C "$ROOT" rev-parse --git-dir 2>/dev/null)"
case "$GITDIR" in /*) ;; *) GITDIR="$ROOT/$GITDIR" ;; esac

[ -d "$ROOT/.githooks" ] || { echo "ERROR: .githooks directory not found" >&2; exit 1; }

CURRENT_HOOKS_PATH=$(git -C "$ROOT" config core.hooksPath 2>/dev/null || echo "")

if [ "$CURRENT_HOOKS_PATH" = ".githooks" ]; then
  echo "✅ Hooks already installed"
  ls -la "$ROOT/.githooks/" | grep -E '\.sh$|^d' | tail -n +2 | while read -r line; do
    [ -x "$ROOT/.githooks/$(echo $line | awk '{print $NF}')" ] && echo "  ✅ $(echo $line | awk '{print $NF}')"
  done
  exit 0
fi

echo "Installing git hooks for $ROOT..."
git -C "$ROOT" config core.hooksPath .githooks
chmod +x "$ROOT"/.githooks/*

if [ "$(git -C "$ROOT" config core.hooksPath)" = ".githooks" ]; then
  echo "✅ Hooks installed successfully"
  echo "Test with: echo 'test' > test.txt && git add test.txt && git commit -m 'test'"
  exit 0
else
  echo "❌ Installation failed" >&2
  exit 1
fi
