#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# rsr-selfaudit.sh — run the RSR self-audit as an INFORMATIONAL grade, but fail
# loudly if the audit tool itself errors.
#
# rsr-audit.sh encodes the grade in its exit code: 0=Gold 1=Silver 2=Bronze
# 3=Non-compliant 4=error. This monorepo is not expected to score Gold, so a
# low grade is informational and non-blocking. What must NOT happen is a broken
# audit (exit 4, or an unexpected code) sliding through as green — the previous
# `rsr-audit.sh … || true` swallowed exactly that. Here a grade is reported and
# returns 0; only a genuine audit error returns non-zero.
#
# Usage: rsr-selfaudit.sh [repo-path] [owner/repo]
# Set RSR_REPOSITORY instead of the second argument to include the live
# Actions-permissions requirement in the audit.

set -uo pipefail

REPO="${1:-.}"
LIVE_REPOSITORY="${2:-${RSR_REPOSITORY:-}}"
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AUDIT="$SELF_DIR/../rhodium-standard-repositories/rsr-audit.sh"

if [ -n "$LIVE_REPOSITORY" ]; then
  bash "$SELF_DIR/check-actions-policy.sh" "$LIVE_REPOSITORY" || {
    echo "  -> RSR self-audit: live Actions policy is non-compliant" >&2
    exit 1
  }
fi

bash "$AUDIT" "$REPO" text
rc=$?
case "$rc" in
  0) echo "  -> RSR self-audit: Gold (100%)" ;;
  1) echo "  -> RSR self-audit: Silver (informational, non-blocking)" ;;
  2) echo "  -> RSR self-audit: Bronze (informational, non-blocking)" ;;
  3) echo "  -> RSR self-audit: Non-compliant (informational, non-blocking)" ;;
  *) echo "  -> RSR self-audit ERRORED (exit $rc) — failing loudly" >&2; exit 1 ;;
esac
