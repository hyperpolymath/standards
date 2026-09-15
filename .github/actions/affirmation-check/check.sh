#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Validates an AFFIRMATION snapshot against AFFIRMATION-STANDARD.adoc.
#
# This gate asks the question the standard asks. Three rules it must respect,
# each of which a previous revision got wrong:
#
#   1. The standard permits the affirmation at the repo ROOT *or* under docs/.
#      The canonical skeleton in rsr-template-repo ships at docs/AFFIRMATION.adoc,
#      so a root-only search reports a present affirmation as missing.
#   2. The standard BANS AFFIRMATION.md ("as README.md is banned"). Accepting it
#      would make the guard permit what the standard forbids, so a .md file is
#      an error naming the fix, not a silently-accepted candidate.
#   3. A template repository ships placeholders BY DESIGN. Without an exemption
#      the gate fails exactly the repos whose job is to carry the template.

set -euo pipefail

root=${GITHUB_WORKSPACE:-.}
required=${AFFIRMATION_REQUIRED:-false}
is_template=${AFFIRMATION_IS_TEMPLATE:-false}
aff_file=

# Rule 2: a banned .md spelling is a named error, never a fallback.
for banned in AFFIRMATION.md docs/AFFIRMATION.md AFFIRMATION; do
  if [[ -f "$root/$banned" ]]; then
    echo "::error::$banned is banned by AFFIRMATION-STANDARD.adoc (AsciiDoc only, as README.md is banned). Rename it to ${banned%.md}.adoc."
    exit 1
  fi
done

# Rule 1: root OR docs/, in the standard's stated order of preference.
for candidate in AFFIRMATION.adoc docs/AFFIRMATION.adoc; do
  if [[ -f "$root/$candidate" ]]; then
    aff_file=$candidate
    break
  fi
done

if [[ -z "$aff_file" ]]; then
  if [[ "$required" == "true" ]]; then
    echo "::error::AFFIRMATION.adoc is required by the declared governance-tier capability (looked in ./ and ./docs/)."
    exit 1
  fi
  echo "::notice::AFFIRMATION is not applicable: governance-tier was not required."
  exit 0
fi

path=$root/$aff_file
echo "Found AFFIRMATION document: $aff_file"

substantive_lines=$(awk '!/^[[:space:]]*(#|\/\/|;|$)/ { count++ } END { print count + 0 }' "$path")
if (( substantive_lines < 5 )); then
  echo "::error::$aff_file has only $substantive_lines substantive lines; it is a stub, not an affirmation."
  exit 1
fi

# Rule 3: a template repo is MEANT to carry placeholders; everyone else is not.
placeholder_re='\{\{|TODO: update|<PROJECT|YOUR_PROJECT|lorem ipsum|example\.com|<full 40-character SHA|<ISO-8601|<YYYY-MM-DD'
if grep -qiE "$placeholder_re" "$path"; then
  if [[ "$is_template" == "true" ]]; then
    echo "::notice::$aff_file carries template placeholders; accepted because this repository is a template."
  else
    echo "::error::$aff_file contains template placeholders (unreplaced {{…}} or <…> anchor fields)."
    exit 1
  fi
fi

# The signature is the signature on the commit containing this content. Text
# such as "Signed:" inside the document proves nothing. Shallow checkouts may
# not contain the file-changing commit, so report that limitation honestly.
signature=N
last_update_ts=
if git -C "$root" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  signature=$(git -C "$root" log -1 --format='%G?' -- "$aff_file" 2>/dev/null || true)
  last_update_ts=$(git -C "$root" log -1 --format='%at' -- "$aff_file" 2>/dev/null || true)
fi

case "$signature" in
  G) echo "Affirmation commit signature verified with a trusted key." ;;
  U) echo "::notice::Affirmation commit has a valid signature from an untrusted or locally unknown key." ;;
  B|R|E) echo "::error::Affirmation commit signature is bad, revoked, or failed verification."; exit 1 ;;
  *) echo "::notice::Affirmation commit signature could not be verified from the available Git history." ;;
esac

# A dated affirmation is a frozen receipt, not a claim that remains current
# forever. Report age without invalidating historical evidence or forcing an
# empty monthly rewrite.
if [[ -n "$last_update_ts" ]]; then
  current_ts=$(date +%s)
  age_days=$(( (current_ts - last_update_ts) / 86400 ))
  if (( age_days > 28 )); then
    echo "::warning::$aff_file is a $age_days-day-old snapshot; verify its anchor before relying on it as current."
  else
    echo "$aff_file snapshot age: $age_days days."
  fi
fi

echo "AFFIRMATION document validation passed."
