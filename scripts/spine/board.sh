#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# spine/board.sh — assemble the ESTATE-BOARD from measured census TSVs.
#
# DESIGN CONSTRAINTS (plan §4, §7.9, §7.10, §7.13 — load-bearing, do not relax):
#   * NEVER reads a working tree. Every input is an origin/HEAD or live-API census.
#   * NEVER scores health by CI colour, conclusion or check-run presence. A broken
#     workflow emits no check run, so a destroyed repo scores GREENER. Soundness is
#     the two-limb structural test only: non-empty jobs map AND a trigger key.
#   * EVERY join uses -F'\t'. Estate paths contain spaces ("_WORK _SET/"); default
#     awk whitespace-splitting once turned 65/43 into 2902/57.
#   * EVERY repo identity is CANONICAL. GitHub silently redirects renamed repos,
#     so a board keyed on remote.origin.url double-counts one repo as two rows.
#   * NO cell is ever a bare UNKNOWN. Absence carries a reason token.
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPORTS="${REPORTS:-/home/hyperpolymath/developer/.claude/reports}"
OUTDIR="${OUTDIR:-$(cd "$HERE/../.." && pwd)}"

ADOC="$OUTDIR/docs/ESTATE-BOARD.adoc"
JSON="$OUTDIR/.machine_readable/estate-board.json"
TSV="$OUTDIR/.machine_readable/estate-board.tsv"
LEDGER="$OUTDIR/.machine_readable/estate-residue-ledger.tsv"

JOIN="$REPORTS/2026-09-15-canonical-repo-join-key.tsv"
POSTURE="$REPORTS/2026-09-15-actions-posture-live.tsv"
POSTSUP="$REPORTS/2026-09-15-actions-posture-supplement.tsv"
SOUND="$REPORTS/2026-09-15-origin-head-workflow-soundness.tsv"
PINS="$REPORTS/2026-09-15-origin-head-reusable-pin-state.tsv"
UNPIN="$REPORTS/2026-09-15-origin-head-unpinned-all-uses.tsv"
SOLE="$REPORTS/2026-09-15-no-sound-head-worktree-is-only-sound-copy.tsv"
SIG="$REPORTS/2026-09-15-origin-head-unsound-by-signature.tsv"

die() { printf 'board.sh: FATAL: %s\n' "$*" >&2; exit 1; }

# Input gate: wc -c, never test -f — a zero-byte file passes test -f.
for f in "$JOIN" "$POSTURE" "$POSTSUP" "$SOUND" "$PINS" "$UNPIN" "$SOLE" "$SIG"; do
  [ -e "$f" ] || die "missing input: $f"
  sz=$(wc -c < "$f")
  [ "$sz" -gt 100 ] || die "input empty or trivial ($sz bytes): $f"
done
[ -e "$HERE/board.awk" ] || die "missing program: $HERE/board.awk"

mkdir -p "$(dirname "$ADOC")" "$(dirname "$JSON")"
GEN_UTC="${GEN_UTC:-$(date -u +%Y-%m-%dT%H:%M:%SZ)}"

awk -F'\t' -f "$HERE/board.awk" \
  -v GEN="$GEN_UTC" \
  -v OUT_TSV="$TSV" -v OUT_JSON="$JSON" -v OUT_ADOC="$ADOC" -v OUT_LEDGER="$LEDGER" \
  -v F_JOIN="$JOIN" -v F_POST="$POSTURE" -v F_SUP="$POSTSUP" -v F_SOUND="$SOUND" \
  -v F_PINS="$PINS" -v F_UNPIN="$UNPIN" -v F_SOLE="$SOLE" -v F_SIG="$SIG" \
  "$JOIN" "$POSTURE" "$POSTSUP" "$SOUND" "$PINS" "$UNPIN" "$SOLE" "$SIG"
