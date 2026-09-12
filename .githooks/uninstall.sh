#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Hyperpolymath Estate Git Hooks Uninstaller

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

CURRENT_HOOKS_PATH=$(git -C "$ROOT" config core.hooksPath 2>/dev/null || echo "")

if [ -z "$CURRENT_HOOKS_PATH" ]; then
  echo "No hooks installed"
  exit 0
fi

echo "Removing core.hooksPath from $ROOT..."
read -rp "Continue? [y/N]: " CONTINUE
[[ ! "$CONTINUE" =~ ^[Yy]$ ]] && { echo "Cancelled"; exit 0; }

git -C "$ROOT" config --unset core.hooksPath
if [ -z "$(git -C "$ROOT" config core.hooksPath 2>/dev/null || echo "")" ]; then
  echo "✅ Hooks uninstalled"
  exit 0
else
  echo "❌ Uninstall failed" >&2
  exit 1
fi
