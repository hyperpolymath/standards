#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# install.sh — install this repo's git hooks into .git/hooks/.
#
# The hooks in hooks/ (pre-commit language-policy + registry-drift guard, SPDX,
# SHA-pin and permission validators) only run if they are present in
# .git/hooks/. Nothing installed them automatically, so their guards were
# effectively optional (a contributor had to know to `cp` them). This installer
# wires them once. Idempotent; safe to re-run.
#
# Usage:  bash hooks/install.sh            (install / refresh)
#         just hooks-install               (same, via recipe)
#
# It installs a thin .git/hooks/pre-commit that execs hooks/pre-commit from the
# work tree, so the tracked hook stays the single source of truth and updates
# take effect without re-installing.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GITDIR="$(git -C "$ROOT" rev-parse --git-dir 2>/dev/null || true)"
if [ -z "$GITDIR" ]; then
  echo "error: not a git repository: $ROOT" >&2
  exit 1
fi
# Normalise to an absolute path (git-dir may be relative to ROOT).
case "$GITDIR" in
  /*) : ;;
  *)  GITDIR="$ROOT/$GITDIR" ;;
esac

HOOKDIR="$GITDIR/hooks"
mkdir -p "$HOOKDIR"

installed=0
for src in "$ROOT"/hooks/pre-commit; do
  [ -f "$src" ] || continue
  name="$(basename "$src")"
  dst="$HOOKDIR/$name"
  cat > "$dst" <<EOF
#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Auto-installed by hooks/install.sh — execs the tracked hook so the work-tree
# copy stays the single source of truth. Do not edit; edit hooks/$name instead.
exec "\$(git rev-parse --show-toplevel)/hooks/$name" "\$@"
EOF
  chmod +x "$dst"
  echo "installed: $dst -> hooks/$name"
  installed=$((installed + 1))
done

if [ "$installed" -eq 0 ]; then
  echo "error: no hooks found to install under $ROOT/hooks/" >&2
  exit 1
fi
echo "✅ $installed hook(s) installed into $HOOKDIR"
