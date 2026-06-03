#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# build-registry.sh — regenerate the verifiable spec registry and the
# DERIVED topology map from ground truth (the file tree + STATE.a2ml).
#
# This is the generator behind two artefacts that MUST NOT be hand-edited:
#   * .machine_readable/REGISTRY.a2ml  — the machine index of every spec,
#       its canonical home, and a content-addressed `source_hash`.
#   * TOPOLOGY.md                      — the human-readable map, derived
#       from the registry + STATE.a2ml so it can never freeze again.
#
# `source_hash` is computed from `git ls-files -s` over each home path:
# that listing already pins every tracked file's blob SHA + path, so any
# content change under a home changes its hash. Hypatia rule HYP-S006
# (registry-staleness) compares the recorded hash against a fresh compute
# and emits a `doc.drift` finding when they diverge — drift becomes a
# detected, routed event instead of something noticed 60 days later.
#
# Principles (do not violate):
#   * Honest. Only specs whose home directory exists are listed. A missing
#     home is reported to stderr, never silently invented.
#   * Deterministic + idempotent. Run twice → byte-identical output.
#   * No network, no commit, no push. Run on a branch; review the diff.
#
# Usage:  bash scripts/build-registry.sh          # write artefacts
#         bash scripts/build-registry.sh --check   # verify, non-zero on drift
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

MODE="write"
[ "${1:-}" = "--check" ] && MODE="check"

REGISTRY=".machine_readable/REGISTRY.a2ml"
TOPOLOGY="TOPOLOGY.md"
GEN_DATE="$(git log -1 --format=%cs 2>/dev/null || date -u +%Y-%m-%d)"

# ---------------------------------------------------------------------------
# Spec table — the single source of truth for what this monorepo standardises.
# Columns: id | stream | home | name | route (one-line "go here if you want X")
# stream ∈ foundation | language | protocol | governance | readiness | integration
# Add a row here when a new spec lands; the hash + topology follow automatically.
# ---------------------------------------------------------------------------
read -r -d '' SPECS <<'TSV' || true
a2ml|foundation|a2ml/|A2ML — Attested Markup Language|the typed/verified machine-readable document format
k9-svc|foundation|k9-svc/|K9 Self-Validating Components|self-validating components with embedded contracts + deploy logic
contractiles|foundation|contractiles/|Contractiles (Must/Trust/Dust/Intend)|policy-enforcement primitives the K9 layer is built from
meta-a2ml|foundation|meta-a2ml/|META.a2ml spec|architecture decisions / governance metadata format
state-a2ml|foundation|state-a2ml/|STATE.a2ml spec|project-state metadata format (drives this registry's topology)
ecosystem-a2ml|foundation|ecosystem-a2ml/|ECOSYSTEM.a2ml spec|ecosystem-positioning metadata format
agentic-a2ml|foundation|agentic-a2ml/|AGENTIC.a2ml spec|AI-agent operational gating / entropy budgets
neurosym-a2ml|foundation|neurosym-a2ml/|NEUROSYM.a2ml spec|symbolic semantics / proof obligations
playbook-a2ml|foundation|playbook-a2ml/|PLAYBOOK.a2ml spec|executable operational runbooks
anchor-a2ml|foundation|anchor-a2ml/|ANCHOR.a2ml spec|project-recalibration intervention format
0-ai-gatekeeper-protocol|protocol|0-ai-gatekeeper-protocol/|0-AI Gatekeeper Protocol|the AI-agent entry/gating protocol behind 0-AI-MANIFEST
k9-coordination-protocol|protocol|k9-coordination-protocol/|K9 Coordination Protocol|multi-agent coordination on top of K9
avow-protocol|protocol|avow-protocol/|AVOW Protocol|consent-attested messaging / origin attribution
axel-protocol|protocol|axel-protocol/|AXEL Protocol|age-gating + explicit-content enforcement
overlay-protocol|protocol|overlay-protocol/|Overlay Protocol|layered overlay composition spec
consent-aware-http|protocol|consent-aware-http/|Consent-Aware HTTP|consent headers / AI-usage boundaries for HTTP
adoption-readiness-grades|readiness|adoption-readiness-grades/|ARG — Adoption Readiness Grades|per-language adoption-maturity profile templates
foundations-readiness-grades|readiness|foundations-readiness-grades/|FRG — Foundations Readiness Grades|per-language foundational-maturity profile templates
component-readiness-grades|readiness|component-readiness-grades/|CRG — Component Readiness Grades|the X..A grading system for components
toolchain-readiness-grades|readiness|toolchain-readiness-grades/|TRG — Toolchain Readiness Grades|per-toolchain readiness profile templates
rhodium-standard-repositories|governance|rhodium-standard-repositories/|RSR — Rhodium Standard Repositories|the repository-compliance standard every repo is graded against
session-management-standards|governance|session-management-standards/|Session Management Standards|continuity / verify / handover protocols
ensaid-config|governance|ensaid-config/|ENSAID Config|the ensaid configuration standard
accessibility|governance|accessibility/|Accessibility Standard|estate accessibility requirements
publication-pre-flight|governance|publication-pre-flight/|Publication Pre-Flight|submission gate (HOL + Zenodo checklists)
release-pre-flight|governance|release-pre-flight/|Release Pre-Flight (V1 Gate)|hard v1.0.0 audit requirements
hypatia-rules|integration|hypatia-rules/|Standards Hypatia Rules|the dogfooding rules that scan THIS repo (incl. drift detection)
a2ml-templates|integration|a2ml-templates/|A2ML Templates|copy-in templates for the 7 A2ML files
TSV

# Pick the canonical human doc for a home: README.adoc > README.md > first *.adoc spec.
canonical_doc() {
  local home="$1"
  for c in "${home}README.adoc" "${home}README.md"; do
    [ -f "$c" ] && { printf '%s' "$c"; return; }
  done
  local first
  first="$(git ls-files -- "${home}" | grep -iE '\.(adoc|a2ml)$' | head -1 || true)"
  printf '%s' "${first:-${home}}"
}

# Content-addressed hash of everything tracked under a home path.
home_hash() {
  local home="$1"
  git ls-files -s -- "$home" | sha256sum | cut -d' ' -f1
}

# ---------------------------------------------------------------------------
# Emit REGISTRY.a2ml
# ---------------------------------------------------------------------------
emit_registry() {
  cat <<HEADER
# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# REGISTRY.a2ml — the verifiable index of every spec/standard in this monorepo.
#
# GENERATED FILE — DO NOT EDIT BY HAND.
# Regenerate with:  bash scripts/build-registry.sh   (or: just registry)
# Source of truth:  the SPECS table in scripts/build-registry.sh + the file tree.
#
# Each entry's \`source_hash\` is a sha256 over \`git ls-files -s <home>\`, so it
# changes whenever any tracked file under the spec's home changes. Hypatia rule
# HYP-S006 (hypatia-rules/registry-staleness.a2ml) recomputes these and emits a
# \`doc.drift\` finding (strategy :review) when a recorded hash goes stale.

[registry]
version = "1.0.0"
generated = "${GEN_DATE}"
generator = "scripts/build-registry.sh"
hash_algorithm = "sha256(git ls-files -s <home>)"
entry_count = ${ENTRY_COUNT}

[registry.streams]
foundation  = "A2ML format family + K9 + contractiles (Stream 1)"
language    = "AffineScript and language-policy specs (Stream 2)"
protocol    = "Inter-service / agent protocols"
governance  = "RSR, readiness grading, pre-flight gates, session standards"
readiness   = "ARG / FRG / CRG / TRG maturity-grading frameworks"
integration = "Registry, hypatia rules, templates — the wiring (Stream 3)"
HEADER

  while IFS='|' read -r id stream home name route; do
    [ -z "$id" ] && continue
    if [ ! -d "$home" ]; then
      echo "WARN: home missing for '$id' ($home) — skipped" >&2
      continue
    fi
    local doc hash
    doc="$(canonical_doc "$home")"
    hash="$(home_hash "$home")"
    cat <<ENTRY

[[spec]]
id = "${id}"
name = "${name}"
stream = "${stream}"
home = "${home}"
canonical_doc = "${doc}"
source_hash = "sha256:${hash}"
route = "${route}"
ENTRY
  done <<< "$SPECS"

  printf '\n### End of REGISTRY.a2ml\n'
}

# ---------------------------------------------------------------------------
# Emit TOPOLOGY.md (DERIVED from the registry + STATE.a2ml)
# ---------------------------------------------------------------------------
state_field() {
  # crude TOML-ish field reader for STATE.a2ml
  local key="$1" file=".machine_readable/6a2/STATE.a2ml"
  # take RHS of `key = value`, drop trailing inline `# comment`, trim, unquote
  grep -E "^${key}[[:space:]]*=" "$file" 2>/dev/null | head -1 \
    | sed -E 's/^[^=]*=[[:space:]]*//; s/[[:space:]]*#.*$//; s/[[:space:]]+$//; s/^"//; s/"$//'
}

emit_topology() {
  local phase maturity updated
  phase="$(state_field phase)"; maturity="$(state_field maturity)"
  updated="$(state_field last-updated)"
  cat <<HEADER
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
<!-- TOPOLOGY.md — DERIVED architecture map. Generated: ${GEN_DATE} -->
<!-- GENERATED FILE — DO NOT EDIT BY HAND. Run: just topology (scripts/build-registry.sh) -->

# Hyperpolymath Standards — Topology (derived)

> This file is **generated** from \`.machine_readable/REGISTRY.a2ml\` and
> \`.machine_readable/6a2/STATE.a2ml\` by \`scripts/build-registry.sh\`.
> It cannot freeze: every regeneration re-reads ground truth. Do not edit by hand.

- **Phase:** ${phase:-unknown}  &nbsp;|&nbsp; **Maturity:** ${maturity:-unknown}  &nbsp;|&nbsp; **STATE last-updated:** ${updated:-unknown}
- **Registry entries:** ${ENTRY_COUNT} specs across 6 streams
- **Front door:** human → [README.adoc](README.adoc); machine → [0-AI-MANIFEST.a2ml](0-AI-MANIFEST.a2ml)
- **Registry:** [.machine_readable/REGISTRY.a2ml](.machine_readable/REGISTRY.a2ml) (index + source hashes) · prose: [REGISTRY.adoc](REGISTRY.adoc)

## Specs by stream

HEADER

  for s in foundation language protocol governance readiness integration; do
    local label
    case "$s" in
      foundation) label="Foundation — A2ML family + K9 + contractiles (Stream 1)";;
      language)   label="Language — AffineScript + language policy (Stream 2)";;
      protocol)   label="Protocols";;
      governance) label="Governance — RSR, gates, session standards";;
      readiness)  label="Readiness grading — ARG / FRG / CRG / TRG";;
      integration)label="Integration — registry, hypatia rules, templates (Stream 3)";;
    esac
    # only print the section if it has rows
    if grep -q "|${s}|" <<< "$SPECS"; then
      printf '### %s\n\n' "$label"
      printf '| Spec | Home | If you want… |\n|---|---|---|\n'
      while IFS='|' read -r id stream home name route; do
        [ -z "$id" ] && continue
        [ "$stream" = "$s" ] || continue
        [ -d "$home" ] || continue
        printf '| %s | [`%s`](%s) | %s |\n' "$name" "$home" "$home" "$route"
      done <<< "$SPECS"
      printf '\n'
    fi
  done

  cat <<'FOOTER'
## How this map stays honest

```
file tree + STATE.a2ml ──► scripts/build-registry.sh ──► REGISTRY.a2ml ──► TOPOLOGY.md
                                      ▲                        │
                                      │                        ▼
                          just registry / CI            HYP-S006 (registry-staleness)
                          (registry-verify.yml)         emits doc.drift on hash mismatch
```

Regenerate after any spec change: `just registry` (writes REGISTRY.a2ml + TOPOLOGY.md).
CI (`registry-verify.yml`) runs `--check` and fails the build if either is stale.
FOOTER
}

ENTRY_COUNT="$(grep -c '^[a-z0-9]' <<< "$SPECS" || true)"

if [ "$MODE" = "check" ]; then
  tmp_r="$(mktemp)"; tmp_t="$(mktemp)"
  emit_registry > "$tmp_r"; emit_topology > "$tmp_t"
  rc=0
  if ! diff -q "$tmp_r" "$REGISTRY" >/dev/null 2>&1; then
    echo "DRIFT: $REGISTRY is stale — run 'just registry'"; rc=1; fi
  if ! diff -q "$tmp_t" "$TOPOLOGY" >/dev/null 2>&1; then
    echo "DRIFT: $TOPOLOGY is stale — run 'just registry'"; rc=1; fi
  rm -f "$tmp_r" "$tmp_t"
  [ "$rc" -eq 0 ] && echo "OK: registry + topology are in sync with the file tree."
  exit "$rc"
fi

emit_registry > "$REGISTRY"
emit_topology > "$TOPOLOGY"
echo "Wrote $REGISTRY and $TOPOLOGY ($ENTRY_COUNT specs)."
