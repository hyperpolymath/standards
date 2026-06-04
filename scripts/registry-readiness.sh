#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# registry-readiness.sh — make a Julia package honest for the General registry.
#
# Encodes the owner-approved remediation applied to Axiom.jl (#18) and
# AcceleratorGate.jl (#8): resolve the PMPL/MPL-2.0 dual-license
# contradiction to a single OSI licence (MPL-2.0) with REUSE compliance,
# remove LLM-tell scaffolding, and SANITY-CHECK (not auto-edit) the
# Project.toml for fabricated dependency UUIDs.
#
# Principles (the whole point — do not violate):
#   * No fabrication. Ambiguous things are REPORTED, never invented.
#   * Non-destructive beyond the known-safe set below.
#   * Idempotent. Run it twice → same result.
#   * Run it ON A BRANCH; it does not commit or push.
#
# Usage:  cd <repo> && git switch -c cleanup/registry-readiness && \
#         bash /path/to/standards/scripts/registry-readiness.sh
#
set -euo pipefail
RED=$'\033[31m'; GRN=$'\033[32m'; YEL=$'\033[33m'; NC=$'\033[0m'
say() { printf '%s\n' "$*"; }
ok()  { printf '%s✓%s %s\n' "$GRN" "$NC" "$*"; }
warn(){ printf '%s!%s %s\n' "$YEL" "$NC" "$*"; }
flag(){ printf '%s⚠ NEEDS HUMAN%s %s\n' "$RED" "$NC" "$*"; }

[ -d .git ] || { echo "not a git repo"; exit 1; }
[ -f Project.toml ] || warn "no Project.toml — not a Julia package? continuing anyway"

say "== 1. Licensing → MPL-2.0 (REUSE) =="
if grep -rlq "PMPL-1.0-or-later" $(git ls-files) 2>/dev/null; then
  # 1a. SPDX identifiers + the dual-license contradiction comment
  git ls-files -z | xargs -0 grep -lZ "PMPL-1.0-or-later" 2>/dev/null | xargs -0 -r sed -i \
    -e 's|SPDX-License-Identifier: MPL-2.0|SPDX-License-Identifier: MPL-2.0|g' \
    -e '/(PMPL-1\.0-or-later preferred; MPL-2\.0 required for Julia ecosystem)/d'
  # 1b. canonical LICENSE from the bundled MPL text (do NOT fabricate licence text)
  if [ -f LICENSES/MPL-2.0.txt ]; then
    { echo "SPDX-License-Identifier: MPL-2.0"
      echo "SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>"
      echo; cat LICENSES/MPL-2.0.txt; } > LICENSE
    ok "LICENSE rebuilt from LICENSES/MPL-2.0.txt"
  else
    flag "LICENSES/MPL-2.0.txt absent — cannot rebuild LICENSE without fabricating licence text. Fix by hand."
  fi
  rm -f LICENSES/PMPL-1.0-or-later.txt; git rm -q --cached LICENSES/PMPL-1.0-or-later.txt 2>/dev/null || true
  printf 'version = 1\n\n[[annotations]]\npath = "**"\nprecedence = "aggregate"\nSPDX-FileCopyrightText = "2024-2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>"\nSPDX-License-Identifier = "MPL-2.0"\n' > REUSE.toml
  ok "SPDX headers → MPL-2.0; REUSE.toml written"
  # 1c. broad SAFE prose normalisation — every one of these is a licence
  #     reference that must now read MPL-2.0; none changes meaning.
  git ls-files -z | xargs -0 grep -lZ "PMPL\|Palimpsest" 2>/dev/null | xargs -0 -r sed -i \
    -e 's/PMPL-1\.0-or-later/MPL-2.0/g' \
    -e 's/Palimpsest License (MPL-2.0)/Mozilla Public License 2.0/g' \
    -e 's/Palimpsest License/Mozilla Public License 2.0/g' \
    -e 's/the Palimpsest/the Mozilla Public License 2.0/g' \
    -e 's/Palimpsest/Mozilla Public License 2.0/g' \
    -e 's#https://github.com/hyperpolymath/palimpsest-license#https://www.mozilla.org/MPL/2.0/#g' \
    -e 's/License-PMPL--1\.0-blue/License-MPL--2.0-blue/g' \
    -e 's/license = with licenses; \[ mit \];.*/license = with licenses; [ mpl20 ];  # MPL-2.0/' \
    -e 's/"Add PMPL-1\.0 license"/"Add MPL-2.0 license"/g' \
    -e "s/'SPDX\\\\|License\\\\|MIT\\\\|Apache\\\\|PMPL\\\\|MPL'/'SPDX\\\\|License\\\\|MIT\\\\|Apache\\\\|MPL'/g" \
    -e "s/'PMPL\\\\|MPL\\\\|MIT\\\\|Apache\\\\|LGPL'/'MPL\\\\|MIT\\\\|Apache\\\\|LGPL'/g" \
    -e '/preserve Emotional Lineage per PMPL/d' \
    -e '/automatic legal fallback until PMPL is formally recognised/d' \
    -e '/^.*\bPMPL\b.*Section [0-9].*$/d'
  # 1c-bis. ROBUST SPDX normalisation. The broad prose pass can leave or
  #  create non-canonical identifiers (`MPL-2.0 +` from `... +` tails,
  #  pre-existing `MPL-2.0-or-later`, the `PLMP-1.0-or-later` typo). reuse
  #  lint rejects these. Force every SPDX-License-Identifier value to the
  #  single canonical `MPL-2.0` (this is what makes `reuse lint` pass).
  git ls-files -z | xargs -0 -r sed -i -E \
    -e 's/(SPDX-License-Identifier:) +MPL-2\.0 \+/\1 MPL-2.0/g' \
    -e 's/(SPDX-License-Identifier:) +MPL-2\.0-or-later/\1 MPL-2.0/g' \
    -e 's/(SPDX-License-Identifier:) +P[LM]MP-1\.0-or-later/\1 MPL-2.0/g'
  # 1d. canonical NOTICE (standard MPL-2.0 notice — not fabrication, the
  #     generic form; replaces any bespoke PMPL paragraph deterministically)
  if [ -f NOTICE ]; then
    repo=$(basename "$(git rev-parse --show-toplevel)")
    cat > NOTICE <<EOF
Licensing Notice
================

$repo is authored by Jonathan D.A. Jewell (hyperpolymath)
<j.d.a.jewell@open.ac.uk> and is licensed under the Mozilla Public
License 2.0 (MPL-2.0), an OSI-approved licence.

The full licence text is in LICENSE and LICENSES/MPL-2.0.txt. Per-file
SPDX-License-Identifier headers and REUSE.toml record MPL-2.0
consistently across the repository (REUSE-compliant).
EOF
    ok "NOTICE rewritten to canonical MPL-2.0 form"
  fi
  resid=$(git ls-files | xargs grep -l "PMPL\|Palimpsest" 2>/dev/null | grep -v '\.git/' || true)
  [ -n "$resid" ] && { warn "residual PMPL/Palimpsest refs still present:"; printf '   %s\n' $resid; } || ok "0 PMPL/Palimpsest refs remain"
  [ -f README.adoc ] || [ -f README.md ] && flag "README: verify it has purpose + a run-verified usage example + prior-art (goerz/DilumAluthge asked) — content-specific, not auto-generated"
else
  ok "no PMPL references"
fi
command -v reuse >/dev/null 2>&1 && { reuse lint >/dev/null 2>&1 && ok "reuse lint passes" || warn "reuse lint not clean — run 'reuse lint' and inspect"; } || warn "reuse CLI not installed (pipx install reuse)"

say "== 2. De-LLM the package face =="
for f in 0-AI-MANIFEST.a2ml llm-warmup-dev.md llm-warmup-user.md; do
  [ -e "$f" ] && { git rm -q "$f" 2>/dev/null || rm -f "$f"; ok "removed $f"; }
done
[ -e EXPLAINME.adoc ] && flag "EXPLAINME.adoc present — do NOT auto-delete; rewrite to verified truth or remove by decision"

say "== 3. Project.toml manifest sanity (REPORT ONLY — never auto-edit deps) =="
if [ -f Project.toml ]; then
  if grep -qE '"([0-9])\1{7}-' Project.toml; then
    flag "fabricated sequential-dummy weakdep UUIDs in Project.toml — remove the phantom deps+extensions by hand (see Axiom.jl#18)"
  else ok "no obvious fabricated UUIDs"; fi
  grep -q '^license *= *"MPL-2.0"' Project.toml && ok 'Project.toml license = "MPL-2.0"' \
    || flag "Project.toml license is not MPL-2.0 — set it"
fi

say "== 4. Tests (authoritative — paste real result) =="
if [ -f Project.toml ] && command -v julia >/dev/null 2>&1; then
  if timeout 560 julia --project=. -e 'using Pkg; Pkg.test()' 2>&1 | tee /tmp/rr_test.$$ | tail -3; then
    grep -q "Testing .* tests passed" /tmp/rr_test.$$ && ok "Pkg.test() passed" || flag "Pkg.test() did NOT pass — inspect /tmp/rr_test.$$"
  else flag "Pkg.test() errored — inspect /tmp/rr_test.$$"; fi
else warn "skipped tests (no Project.toml or julia)"; fi

say "${GRN}== registry-readiness pass complete — review, then commit on this branch ==${NC}"
