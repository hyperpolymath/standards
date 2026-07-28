#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
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
# The registry indexes two kinds of spec:
#   * LOCAL specs — homed in this monorepo. `source_hash` is computed from
#     `git ls-files -s <home>`: that listing already pins every tracked
#     file's blob SHA + path, so any content change under a home changes
#     its hash.
#   * EXTERNAL specs — language/service-coupled specs whose source-of-truth
#     deliberately lives in ANOTHER repo (e.g. the AffineScript .affine /
#     .affex / .affmap standards, SSOT = hyperpolymath/affinescript). We
#     record a verified POINTER (canonical_url + version_pin + a recorded
#     source_hash), never a copy — duplicating the normative text would
#     create two sources of truth. The recorded source_hash is PINNED in
#     the EXTERNAL_SPECS table below (sentinel until upstream lands); the
#     OFFLINE generator emits it verbatim so `--check` stays deterministic.
#
# Hypatia rule HYP-S006 (hypatia-rules/registry-staleness.a2ml) recomputes
# LOCAL hashes from the tree and (network-capable, estate-side) re-fetches
# EXTERNAL canonical_urls, emitting a `doc.drift` finding when a recorded
# hash goes stale.
#
# Principles (do not violate):
#   * Honest. Only specs whose home directory exists are listed. A missing
#     home is reported to stderr, never silently invented.
#   * Deterministic + idempotent. Run twice → byte-identical output.
#   * No network, no commit, no push. Run on a branch; review the diff.
#     (External source_hashes are RECORDED here, never fetched at gen time.)
#
# Usage:  bash scripts/build-registry.sh          # write artefacts
#         bash scripts/build-registry.sh --check   # verify, non-zero on drift
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

MODE="write"
[ "${1:-}" = "--check" ] && MODE="check"

REGISTRY=".machine_readable/REGISTRY.a2ml"
TOPOLOGY="TOPOLOGY.md"
# Intentionally NO generation timestamp: a volatile date would make every
# regeneration differ and defeat `--check`. The content (hashes + STATE) is the
# only source of truth, so the output is a pure function of the committed tree.

# ---------------------------------------------------------------------------
# Spec table — the single source of truth for what this monorepo standardises.
# Columns: id | stream | home | name | route (one-line "go here if you want X")
# stream ∈ foundation | language | protocol | governance | readiness | integration
# Add a row here when a new spec lands; the hash + topology follow automatically.
# ---------------------------------------------------------------------------
read -r -d '' SPECS <<'TSV' || true
estate-constitution|governance|constitution/|Hyperpolymath Estate Constitution|the highest estate-level rules, authority precedence, assurance, contribution, exceptions, and known tensions
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
did-you-actually-do-that|governance|did-you-actually-do-that/|DYADT — Did-You-Actually-Do-That|post-action agent-claim verification (Tier 4 accountability)
ensaid-config|governance|ensaid-config/|ENSAID Config|the ensaid configuration standard
accessibility|governance|accessibility/|Accessibility Standard|estate accessibility requirements
publication-pre-flight|governance|publication-pre-flight/|Publication Pre-Flight|submission gate (HOL + Zenodo checklists)
release-pre-flight|governance|release-pre-flight/|Release Pre-Flight (V1 Gate)|hard v1.0.0 audit requirements
hypatia-rules|integration|hypatia-rules/|Standards Hypatia Rules|the dogfooding rules that scan THIS repo (incl. drift detection)
a2ml-templates|integration|a2ml-templates/|A2ML Templates|copy-in templates for the 7 A2ML files
TSV

# ---------------------------------------------------------------------------
# External spec table — language/service-coupled specs whose SSOT lives in
# ANOTHER repo. The registry holds a verified POINTER, never a copy.
# Columns: id | stream | spec_kind | owning_repo | canonical_url | version_pin |
#          format_version | source_hash | media_type | lineage | name | route
#   * spec_kind     ∈ language-coupled | service-coupled
#   * format_version: only for regenerable artefacts whose format version is
#       tracked INDEPENDENTLY of the language version_pin (e.g. .affex). Empty
#       otherwise. (owner directive 2026-06-03)
#   * source_hash   : RECORDED here (sentinel `PENDING-FIRST-SYNC` until the
#       upstream spec lands). NEVER fetched at generation time — HYP-S006 does
#       the network-side verification. NEVER fabricate a hash.
# Field set ratified by owner 2026-06-03: 7 base (name/canonical_url/owning_repo/
# version_pin/source_hash/conformance_level/last_synced) + provenance
# (source_hash_algo/spec_kind/media_type/lineage). conformance_level is "draft"
# and last_synced "never" until first sync, so they are emitted by the writer,
# not carried per-row. source_hash_algo is sha256 (md5/sha1 are banned).
# ---------------------------------------------------------------------------
read -r -d '' EXTERNAL_SPECS <<'TSV' || true
affine-spec|language|language-coupled|hyperpolymath/affinescript|https://github.com/hyperpolymath/affinescript/blob/main/spec/affine.adoc|v2.0.0||PENDING-FIRST-SYNC|application/vnd.affinescript.affine|affinescript:affine@2|AffineScript .affine (faces / source documents)|faces, canonical-lowering invariant, canonical islands, idiom packs, mimicry bindings, project face policy
affex-manifest|language|language-coupled|hyperpolymath/affinescript|https://github.com/hyperpolymath/affinescript/blob/main/spec/affex.adoc|v2.0.0|2|PENDING-FIRST-SYNC|application/vnd.affinescript.affex|affinescript:affex@2|AffineScript .affex (face-interop manifest)|derived regenerable manifest; declaration heads not full bodies; format_version bumps independently
affmap-provenance|language|language-coupled|hyperpolymath/affinescript|https://github.com/hyperpolymath/affinescript/blob/main/spec/affmap.adoc|v2.0.0||PENDING-FIRST-SYNC|application/vnd.affinescript.affmap|affinescript:affmap@2|AffineScript .affmap (provenance)|provenance format; own pointer for independent staleness tracking
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
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# REGISTRY.a2ml — the verifiable index of every spec/standard in this monorepo.
#
# GENERATED FILE — DO NOT EDIT BY HAND.
# Regenerate with:  bash scripts/build-registry.sh   (or: just registry)
# Source of truth:  the SPECS table in scripts/build-registry.sh + the file tree.
#
# LOCAL entries: \`source_hash\` is a sha256 over \`git ls-files -s <home>\`, so it
# changes whenever any tracked file under the spec's home changes.
# EXTERNAL entries (kind = "external"): a verified POINTER to a spec whose SSOT
# lives in another repo. \`source_hash\` is RECORDED (sentinel PENDING-FIRST-SYNC
# until upstream lands), not computed locally — the offline generator emits it
# verbatim. Hypatia rule HYP-S006 (hypatia-rules/registry-staleness.a2ml)
# recomputes LOCAL hashes and re-fetches EXTERNAL canonical_urls, emitting a
# \`doc.drift\` finding (strategy :review) when a recorded hash goes stale.

[registry]
version = "1.0.0"
generator = "scripts/build-registry.sh"
hash_algorithm = "sha256(git ls-files -s <home>)  # local; external: recorded pin"
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

  # External pointers — verified references to specs whose SSOT is another
  # repo. Emitted from the recorded pin (no home, no local hash compute).
  while IFS='|' read -r id stream spec_kind owning_repo canonical_url version_pin format_version source_hash media_type lineage name route; do
    [ -z "$id" ] && continue
    local sync_status conformance
    if [ "$source_hash" = "PENDING-FIRST-SYNC" ]; then
      sync_status="awaiting-upstream"; conformance="draft"
    else
      sync_status="verified"; conformance="normative"
    fi
    cat <<ENTRY

[[spec]]
id = "${id}"
name = "${name}"
stream = "${stream}"
kind = "external"
spec_kind = "${spec_kind}"
owning_repo = "${owning_repo}"
canonical_url = "${canonical_url}"
version_pin = "${version_pin}"
source_hash = "${source_hash}"
source_hash_algo = "sha256"
conformance_level = "${conformance}"
last_synced = "never"
sync_status = "${sync_status}"
media_type = "${media_type}"
lineage = "${lineage}"
route = "${route}"
ENTRY
    [ -n "$format_version" ] && printf 'format_version = "%s"  # tracked independently of version_pin\n' "$format_version"
  done <<< "$EXTERNAL_SPECS"

  # NB: .affcite.a2ml is an A2ML document under the CodeCite citation profile;
  # its citation contents live in that artefact inside the AffineScript repo,
  # not here and not in .affex — so it is not a separate registry pointer.

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
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- TOPOLOGY.md — DERIVED architecture map (generated from REGISTRY.a2ml + STATE.a2ml) -->
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
    # print the section if EITHER local or external specs populate this stream
    if grep -q "|${s}|" <<< "$SPECS" || grep -q "|${s}|" <<< "$EXTERNAL_SPECS"; then
      printf '### %s\n\n' "$label"
      printf '| Spec | Home | If you want… |\n|---|---|---|\n'
      while IFS='|' read -r id stream home name route; do
        [ -z "$id" ] && continue
        [ "$stream" = "$s" ] || continue
        [ -d "$home" ] || continue
        printf '| %s | [`%s`](%s) | %s |\n' "$name" "$home" "$home" "$route"
      done <<< "$SPECS"
      # external pointers in this stream (SSOT lives in another repo)
      while IFS='|' read -r id stream spec_kind owning_repo canonical_url version_pin format_version source_hash media_type lineage name route; do
        [ -z "$id" ] && continue
        [ "$stream" = "$s" ] || continue
        printf '| %s | [`%s`](%s) `@ %s` ⇗ | %s |\n' "$name" "$owning_repo" "$canonical_url" "$version_pin" "$route"
      done <<< "$EXTERNAL_SPECS"
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

ENTRY_COUNT="$(( $(grep -c '^[a-z0-9]' <<< "$SPECS" || true) + $(grep -c '^[a-z0-9]' <<< "$EXTERNAL_SPECS" || true) ))"

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
