#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# check-exemption-ratchet.sh — exemption ledgers may SHRINK, never grow silently.
#
# WHY THIS EXISTS
# ---------------
# Every large CI failure this estate has diagnosed shares one shape: an
# exemption was added quietly and then never removed. The 2026-08-06 audit
# found, among 424 repositories:
#
#   * a gitleaks gate that had run with NO allowlist for months, then acquired
#     one, with nothing preventing that allowlist from growing to cover a real
#     secret later;
#   * 13,206 banned-language files, most of them declared once and forgotten;
#   * a `.hypatia-baseline.json` that absorbed 255 findings in a single commit,
#     converting a library's central guarantee into accepted debt;
#   * exemption files written in the WRONG FORMAT (hyperpolymath/proven) that
#     therefore suppressed nothing — nobody noticed for months, because a
#     suppression that does not work is silent in exactly the same way as one
#     that does.
#
# A gate that only checks the CURRENT state cannot catch any of that: each
# individual state is "valid". What matters is the DIRECTION OF TRAVEL. This
# check compares the pull request against its base and enforces:
#
#   1. NO SILENT GROWTH.       An exemption ledger may lose entries freely.
#                              Gaining entries requires an explicit, reviewed
#                              declaration in the commit message.
#   2. NO ANONYMOUS ENTRIES.   Every .hypatia-baseline.json entry must carry a
#                              `note` or a `tracking_issue`. "What is this?"
#                              must be answerable without archaeology.
#   3. NO WILDCARD LEDGERS.    A `**` pattern in a banned-language ledger
#                              silently absorbs files added later, which turns
#                              a migration ledger into a permanent blind spot.
#                              Architectural exemptions may use patterns — in
#                              .hypatia-baseline.json, where a note explains
#                              them — but the migration ledger may not.
#
# THE ESCAPE HATCH IS DELIBERATE AND LOUD. Growth is sometimes correct: a
# newly vendored dependency, a newly discovered architectural boundary. To
# allow it, put a line in the commit message:
#
#     Ratchet-exception: <why this ledger must grow>
#
# That makes adding debt possible, attributable and reviewable — which is the
# whole point. A gate nobody can satisfy gets deleted; a gate that costs one
# honest sentence gets obeyed.
#
# EXIT CODES
#   0  ledgers shrank, held steady, or grew with a declared exception
#   1  a ledger grew without one, or an entry is anonymous, or a wildcard
#      appeared in a migration ledger
#   2  invalid invocation or unreadable input
set -euo pipefail

BASE_REF="${1:-}"
if [ -z "$BASE_REF" ]; then
  echo "usage: check-exemption-ratchet.sh <base-ref>" >&2
  exit 2
fi

fail=0

# Entry counting for TOML ledgers lives in a sibling script — see
# count-ledger-entries.sh for why it is not inlined. The workflow stages both
# files together. Overridable for testing; unset is a hard error at use, never
# a silent zero.
COUNTER="${COUNTER:-$(dirname "$0")/count-ledger-entries.sh}"
note() { printf '  %s\n' "$*"; }

# Count entries in a ledger at a given ref. Counting is per-format, because
# "lines" is wrong for JSON and "entries" is wrong for a text ledger.
count_at() {
  local ref="$1" path="$2"
  local blob
  blob="$(git show "${ref}:${path}" 2>/dev/null || true)"
  [ -z "$blob" ] && { echo 0; return; }
  case "$path" in
    *.json)
      printf '%s' "$blob" | jq 'if type=="array" then length else 0 end' 2>/dev/null || echo 0 ;;
    *.toml)
      # ⚠ ENTRY counting, not line counting — see count-ledger-entries.sh for
      # why, and for why it is a separate file rather than an inline
      # `python3 -c`. There is deliberately no `|| echo 0`: a counter that
      # cannot run must FAIL the check, because a count of 0 is
      # indistinguishable from an empty ledger, and empty is what passes.
      printf '%s' "$blob" | "${COUNTER:?COUNTER must point at count-ledger-entries.sh}" ;;
    *)
      # Comments and blank lines are not exemptions.
      #
      # ⚠ `|| true` is LOAD-BEARING. grep exits 1 when nothing matches, which is
      # exactly what a comments-only ledger produces — a perfectly legitimate
      # state, and the one a repository reaches when it finishes paying its debt
      # down. Under `set -euo pipefail` that exit propagated and killed the
      # script mid-report: it printed the first ledger's line, then exited 1
      # with no verdict at all. Exit 1 means "ratchet FAILED", so a repo that
      # had cleared its ledger would be reported as violating the ratchet, with
      # no reason given and nothing to fix. Caught by gitar-bot review, and it
      # is the same failure class this check exists to prevent: a gate that
      # fails for a reason unrelated to what it measures.
      printf '%s' "$blob" | { grep -vEc '^\s*(#|$)' || true; } | tr -d ' \n' ;;
  esac
}

# Does the pull request declare that a SPECIFIC ledger must grow?
#
# ⚠ The declaration names its ledger. A single bare `Ratchet-exception:` used
# to license growth in ALL FOUR ledgers at once — so a PR that legitimately
# needed to add one gitleaks path also silently gained permission to grow the
# Hypatia baseline, the migration ledger and the root allowlist. The whole
# point is that each addition is seen; a blanket permit defeats it.
#
# Accepted forms:
#     Ratchet-exception: <ledger path> — <why>
#     Ratchet-exception(<ledger path>): <why>
# A declaration naming no known ledger is rejected rather than treated as
# blanket permission, because "unparseable" must never mean "allowed".
declared_for() {
  local ledger="$1" msgs
  msgs="$(git log --format=%B "${BASE_REF}..HEAD" 2>/dev/null || true)"
  printf '%s' "$msgs" | grep -iE '^Ratchet-exception' | grep -qF "$ledger"
}

LEDGERS=(
  ".hypatia-baseline.json"
  ".hypatia-ignore"
  ".gitleaks.toml"
  ".machine_readable/root-allow.txt"
)

echo "Exemption ratchet — comparing against ${BASE_REF}"
for path in "${LEDGERS[@]}"; do
  before="$(count_at "$BASE_REF" "$path")"
  after="$(count_at "HEAD" "$path")"
  [ "$before" = "0" ] && [ "$after" = "0" ] && continue
  if [ "$after" -gt "$before" ]; then
    if declared_for "$path"; then
      note "OK (declared)  ${path}: ${before} -> ${after}  [Ratchet-exception present]"
    else
      note "GREW           ${path}: ${before} -> ${after}"
      fail=1
    fi
  elif [ "$after" -lt "$before" ]; then
    note "SHRANK         ${path}: ${before} -> ${after}  <- debt paid down"
  else
    note "unchanged      ${path}: ${before}"
  fi
done

# 2. No anonymous baseline entries.
if [ -f .hypatia-baseline.json ]; then
  anon="$(jq '[.[] | select((has("note")|not) and (has("tracking_issue")|not))] | length' \
          .hypatia-baseline.json 2>/dev/null || echo 0)"
  if [ "${anon:-0}" -gt 0 ]; then
    note "ANONYMOUS      .hypatia-baseline.json: ${anon} entr(y|ies) carry neither a note nor a tracking_issue"
    note "               Every exemption must say what it is. Add \`note\` explaining"
    note "               what the finding actually is, or \`tracking_issue\` naming the"
    note "               work that discharges it."
    fail=1
  fi
fi

# 3. No wildcards in the MIGRATION ledger.
# .hypatia-ignore records work that is meant to disappear. A `**` there absorbs
# files added later, so the ledger silently grows while appearing to hold
# steady — the failure this whole check exists to prevent. Architectural
# exemptions belong in .hypatia-baseline.json with a note explaining them.
if [ -f .hypatia-ignore ]; then
  if grep -vE '^\s*(#|$)' .hypatia-ignore | grep -q '\*\*'; then
    note "WILDCARD       .hypatia-ignore contains a '**' pattern"
    grep -vE '^\s*(#|$)' .hypatia-ignore | grep '\*\*' | sed 's/^/                 /'
    note "               A migration ledger must list paths individually so it can"
    note "               only shrink. Put architectural exemptions in"
    note "               .hypatia-baseline.json with a note instead."
    fail=1
  fi
fi

echo
if [ "$fail" = "0" ]; then
  echo "Exemption ratchet: OK."
else
  cat <<'MSG'
Exemption ratchet: FAILED.

An exemption ledger grew, or an exemption does not say what it is.

If the growth is correct — a newly vendored dependency, a newly discovered
architectural boundary — declare it in the commit message:

    Ratchet-exception: vendored upstream foo/ at v1.2.3; its test corpus
    contains credential-shaped fixtures by design

If it is not correct, remove the finding rather than the report.
MSG
fi
exit "$fail"
