#!/bin/sh
# SPDX-License-Identifier: MPL-2.0
# Migrate a repo's STATE.a2ml from v1 (Scheme, bloated) to v2 (thin journal).
#
# Extracts: phase, next_action, last_action, blockers.
# See: a2ml-templates/STATE.a2ml.v2.spec.adoc
#
# Usage: state-migrate-v1-to-v2.sh <repo-root>

set -eu

REPO="${1:-.}"
V1="$REPO/.machine_readable/6a2/STATE.a2ml"
V2="$REPO/.machine_readable/STATE.a2ml"

[ -f "$V1" ] || { echo "no v1 STATE.a2ml at $V1"; exit 1; }

# Extract fields from the Scheme s-expressions (best-effort; tolerates messy files).
phase="$(grep -oE '\(current-phase[^)]*\)' "$V1" 2>/dev/null | head -1 | sed 's/.*"\([^"]*\)".*/\1/' || echo 'unknown')"
[ -z "$phase" ] && phase="unknown"

next_action="$(grep -oE '"[^"]{1,200}"' "$V1" 2>/dev/null | head -1 | tr -d '"' || echo 'TODO — review and set')"

# last_action: take the first string found under session-history, or fall back.
last_action="$(grep -A3 'session-history' "$V1" 2>/dev/null | grep -oE '"[^"]{1,200}"' | head -1 | tr -d '"' || echo 'migrated from v1')"

today="$(date -u +%Y-%m-%d)"

cat > "$V2" <<EOF
# SPDX-License-Identifier: MPL-2.0
# Migrated from v1 by state-migrate-v1-to-v2.sh on $today

@state(version="2.0"):
phase: "$phase"
next_action: "$next_action"
last_action: "$last_action"
updated: $today
@end
EOF

echo "wrote $V2 (v2 thin journal)"
echo "  phase:        $phase"
echo "  next_action:  $next_action"
echo "  last_action:  $last_action"
echo ""
echo "Review the migration. Original v1 file preserved at: $V1"
