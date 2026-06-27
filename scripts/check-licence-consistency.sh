#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# check-licence-consistency.sh
#
# Verifies that a repo's licensing story is internally consistent:
#   (1) A LICENSE / LICENCE / COPYING file is present at repo root.
#   (2) The LICENSE body text classifies to a known licence template.
#   (3) The licence identity is established by EITHER an SPDX-License-Identifier
#       header on the LICENSE file's first few lines, OR — for a verbatim,
#       header-less licence file — the body-text classification itself. The
#       estate template ships LICENSE as plain, unmodified MPL-2.0 text with no
#       SPDX header (SPDX identifiers belong in *source* files, not in the
#       canonical upstream licence text), so a header-less verbatim MPL-2.0
#       LICENSE is consistent — not a finding.
#   (4) If a build manifest declares a licence, it matches that identity.
#   (5) When an SPDX header IS present, the body text must agree with it (loose
#       check — catches the common drift of SPDX=MPL-2.0 but body=PMPL).
#
# Exit codes:
#   0 — all checks pass.
#   1 — at least one check failed.
#   2 — invocation / environment error.
#
# Wired into governance-reusable.yml as the `licence-consistency` job.
#
# Estate policy reference: MPL-1.0 / PMPL-1.0 → MPL-2.0 migration target.

set -u

repo_root="${1:-.}"
cd "$repo_root" || { echo "ERROR: cannot cd to $repo_root" >&2; exit 2; }

failed=0
emit() {
  # 1=level (ERROR/WARN/OK), 2=message
  printf '[%s] %s\n' "$1" "$2"
}

# ─────────────────────────────────────────────────────────────────────────────
# (1) LICENSE file presence
# ─────────────────────────────────────────────────────────────────────────────
lic_file=""
for cand in LICENSE LICENSE.md LICENSE.txt LICENCE LICENCE.md COPYING; do
  if [ -f "$cand" ]; then
    lic_file="$cand"
    break
  fi
done

if [ -z "$lic_file" ]; then
  emit ERROR "No LICENSE / LICENCE / COPYING file at repo root."
  emit ERROR "Estate default is MPL-2.0 — see docs/audits/2026-05-26-estate-licence-debt.md."
  # Cannot proceed with remaining checks without a file.
  exit 1
fi
emit OK "LICENSE file found: $lic_file"

# ─────────────────────────────────────────────────────────────────────────────
# (2) Classify LICENSE body text
#     Computed first because a verbatim, header-less licence file establishes
#     its identity from the body alone (see (3)).
# ─────────────────────────────────────────────────────────────────────────────
# Use a helper because `grep -c` exits non-zero on zero-matches AND prints "0"
# to stdout, so `|| echo 0` concatenates to "0\n0". Pipe through wc -l which
# always returns a single integer.
count_in()   { grep -E  "$1" "$lic_file" 2>/dev/null | wc -l; }
count_in_i() { grep -iE "$1" "$lic_file" 2>/dev/null | wc -l; }
has_mpl2_text=$(count_in 'Mozilla Public License Version 2\.0|Mozilla Public License, version 2\.0')
has_pmpl_text=$(count_in_i 'PMPL-1\.0-or-later|Palimpsest License \(PMPL')
has_apache=$(count_in 'Apache License.*Version 2\.0')
has_mit=$(count_in 'MIT License')
has_gpl3=$(count_in 'GNU GENERAL PUBLIC LICENSE.*Version 3')
has_bsd3=$(count_in 'BSD.*3-Clause')
has_proprietary=$(count_in_i 'All Rights Reserved')

body_class="UNKNOWN"
# Order matters: the legally-binding text dominates classification.
if [ "$has_proprietary" -gt 0 ] && [ "$has_mpl2_text" -eq 0 ]; then
  body_class="PROPRIETARY"
elif [ "$has_mpl2_text" -gt 0 ]; then
  # MPL-2.0 text is present in the body — this is binding even when wrapped
  # in a Palimpsest preamble.
  body_class="MPL-2.0"
elif [ "$has_pmpl_text" -gt 0 ]; then
  body_class="PMPL-1.0"
elif [ "$has_apache" -gt 0 ]; then
  body_class="Apache-2.0"
elif [ "$has_mit" -gt 0 ]; then
  body_class="MIT"
elif [ "$has_gpl3" -gt 0 ]; then
  body_class="GPL-3.0"
elif [ "$has_bsd3" -gt 0 ]; then
  body_class="BSD-3-Clause"
fi

# Normalize for loose, case-insensitive licence comparison.
normalize() {
  echo "$1" | tr '[:upper:]' '[:lower:]' \
    | sed -E 's/-or-later$//;s/^[[:space:]]+|[[:space:]]+$//g'
}

# ─────────────────────────────────────────────────────────────────────────────
# (3) Establish licence identity: SPDX header if present, else verbatim body.
#     `effective_lic` is the canonical identity used by the manifest check (4).
# ─────────────────────────────────────────────────────────────────────────────
spdx_header=$(grep -m1 -E '^[[:space:]]*SPDX-License-Identifier:' "$lic_file" 2>/dev/null \
  | sed -E 's/^[[:space:]]*SPDX-License-Identifier:[[:space:]]*//' \
  | head -c 80 | tr -d '[:space:]')

effective_lic=""
if [ -n "$spdx_header" ]; then
  emit OK "SPDX header: $spdx_header"
  effective_lic="$spdx_header"
elif [ "$body_class" != "UNKNOWN" ]; then
  # No SPDX header, but the body is a recognised verbatim licence. This is the
  # estate template's canonical shape (plain MPL-2.0 text, no header) and is
  # internally consistent — accept it and use the body classification as the
  # licence identity for the manifest cross-check below.
  emit OK "LICENSE has no SPDX header, but its body is verbatim $body_class text — accepted as a canonical licence file."
  effective_lic="$body_class"
else
  emit ERROR "LICENSE file has no 'SPDX-License-Identifier:' header and its body matches no known licence template."
  emit ERROR "Add an SPDX header, or use a recognised verbatim licence text, so downstream scanners (REUSE, cargo-license, etc.) can identify the licence."
  failed=1
fi

# ─────────────────────────────────────────────────────────────────────────────
# (4) Manifest declared licence vs the established identity
# ─────────────────────────────────────────────────────────────────────────────
manifest_path=""
manifest_decl=""

if [ -f "Cargo.toml" ]; then
  manifest_path="Cargo.toml"
  manifest_decl=$(grep -m1 -E '^license[[:space:]]*=' "$manifest_path" 2>/dev/null \
    | sed -E 's/^license[[:space:]]*=[[:space:]]*//' \
    | tr -d '"' | head -c 80 | sed -E 's/[[:space:]]+$//')
elif [ -f "package.json" ]; then
  manifest_path="package.json"
  manifest_decl=$(grep -m1 -E '"license"[[:space:]]*:' "$manifest_path" 2>/dev/null \
    | sed -E 's/.*"license"[[:space:]]*:[[:space:]]*//' \
    | tr -d '",' | head -c 80 | sed -E 's/[[:space:]]+$//')
elif [ -f "pyproject.toml" ]; then
  manifest_path="pyproject.toml"
  manifest_decl=$(grep -m1 -E '^license[[:space:]]*=' "$manifest_path" 2>/dev/null \
    | sed -E 's/^license[[:space:]]*=[[:space:]]*//' \
    | tr -d '"{}' | head -c 80 | sed -E 's/[[:space:]]+$//;s/text[[:space:]]*=[[:space:]]*//')
elif [ -f "mix.exs" ]; then
  manifest_path="mix.exs"
  # mix.exs uses `licenses: ["MPL-2.0"]`
  manifest_decl=$(grep -m1 -E 'licenses[[:space:]]*:' "$manifest_path" 2>/dev/null \
    | sed -E 's/.*licenses[[:space:]]*:[[:space:]]*\[//' \
    | sed -E 's/\].*//' | tr -d '" ' | head -c 80)
elif [ -f "Project.toml" ]; then
  manifest_path="Project.toml"
  manifest_decl=$(grep -m1 -iE '^license[[:space:]]*=' "$manifest_path" 2>/dev/null \
    | sed -E 's/^[Ll]icense[[:space:]]*=[[:space:]]*//' \
    | tr -d '"' | head -c 80 | sed -E 's/[[:space:]]+$//')
elif ls ./*.ipkg >/dev/null 2>&1; then
  manifest_path="$(ls -1 ./*.ipkg | head -1)"
  # ipkg files carry SPDX header in a comment line
  manifest_decl=$(grep -hm1 -iE '^[[:space:]]*--[[:space:]]*SPDX-License-Identifier' "$manifest_path" 2>/dev/null \
    | sed -E 's/.*SPDX-License-Identifier[[:space:]]*:[[:space:]]*//' \
    | head -c 80 | sed -E 's/[[:space:]]+$//')
elif ls ./*.cabal >/dev/null 2>&1; then
  manifest_path="$(ls -1 ./*.cabal | head -1)"
  manifest_decl=$(grep -hm1 -iE '^license[[:space:]]*:' "$manifest_path" 2>/dev/null \
    | sed -E 's/^[lL]icense[[:space:]]*:[[:space:]]*//' \
    | head -c 80 | sed -E 's/[[:space:]]+$//')
fi

if [ -n "$manifest_path" ] && [ -z "$manifest_decl" ]; then
  emit WARN "Manifest $manifest_path present but no licence field detected."
elif [ -n "$manifest_decl" ]; then
  emit OK "Manifest licence ($manifest_path): $manifest_decl"
fi

if [ -n "$effective_lic" ] && [ -n "$manifest_decl" ]; then
  el_norm=$(normalize "$effective_lic")
  mh_norm=$(normalize "$manifest_decl")
  # mh may contain `MIT OR Apache-2.0` — accept if the licence identity is one of them.
  if echo "$mh_norm" | grep -qE "(^|\W)$el_norm(\W|$)"; then
    emit OK "Licence identity matches manifest declaration."
  else
    emit ERROR "Licence-vs-manifest mismatch: licence='$effective_lic' manifest='$manifest_decl' ($manifest_path)."
    failed=1
  fi
fi

# ─────────────────────────────────────────────────────────────────────────────
# (5) When an SPDX header is present, the body text must agree with it.
#     Header-less files already derived their identity from the body in (3),
#     so there is nothing to cross-check here for them.
# ─────────────────────────────────────────────────────────────────────────────
if [ -n "$spdx_header" ]; then
  spdx_norm=$(normalize "$spdx_header")
  body_norm=$(echo "$body_class" | tr '[:upper:]' '[:lower:]')

  if [ "$body_class" = "UNKNOWN" ]; then
    emit WARN "LICENSE body did not match any known licence template. Manual review recommended."
  elif [ "$body_class" = "PROPRIETARY" ] && [ "$spdx_norm" != "licenseref-proprietary" ]; then
    emit ERROR "LICENSE body says 'All Rights Reserved' (proprietary) but SPDX header says '$spdx_header'."
    emit ERROR "Either flip LICENSE body to the SPDX-declared licence text, or set the SPDX header to LicenseRef-Proprietary."
    failed=1
  elif [ "$spdx_norm" = "mpl-2.0" ] && [ "$body_class" = "PMPL-1.0" ]; then
    emit ERROR "SPDX header says MPL-2.0 but LICENSE body text is still PMPL-1.0-or-later."
    emit ERROR "Migrate body to canonical MPL-2.0 text (see hyperpolymath/standards docs/audits/2026-05-26-estate-licence-debt.md)."
    failed=1
  elif [ "$body_norm" = "$spdx_norm" ]; then
    emit OK "LICENSE body text matches SPDX header."
  else
    emit WARN "LICENSE body classification ($body_class) doesn't obviously match SPDX header ($spdx_header). Spot-check."
  fi
fi

# ─────────────────────────────────────────────────────────────────────────────
# Summary
# ─────────────────────────────────────────────────────────────────────────────
if [ "$failed" -eq 0 ]; then
  emit OK "Licence consistency check passed."
  exit 0
else
  emit ERROR "Licence consistency check failed. See messages above."
  exit 1
fi
