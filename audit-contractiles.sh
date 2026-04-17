#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Systemic Contractile Audit Script
# Audits all Hyperpolymath repositories for complete contractile implementation

set -euo pipefail

echo "═══════════════════════════════════════════════════════════════════════════════"
echo "  Hyperpolymath Contractile System Audit"
echo "  $(date '+%Y-%m-%d %H:%M:%S')"
echo "════╕══════════════════════════════════════════════════════════════════════════════"
echo ""

# Repositories to audit
REPOS=(
  "/var/mnt/eclipse/repos/burble"
  "/var/mnt/eclipse/repos/panll"
  "/var/mnt/eclipse/repos/nextgen-databases"
  "/var/mnt/eclipse/repos/rescript"
  "/var/mnt/eclipse/repos/standards"
)

# Contractile types to check
# `lust` deprecated 2026-04-18 — wishes absorbed into intend/Intentfile.a2ml
CONTRACTILES=("must" "trust" "dust" "bust" "adjust" "intend")

echo "## Contractile Completeness Audit"
echo ""

for repo in "${REPOS[@]}"; do
  if [ -d "$repo" ]; then
    echo "### $(basename "$repo")"
    
    if [ -d "$repo/.machine_readable/contractiles" ]; then
      for contractile in "${CONTRACTILES[@]}"; do
        if [ -f "$repo/.machine_readable/contractiles/$contractile/Intentfile.a2ml" ] || \
           [ -f "$repo/.machine_readable/contractiles/$contractile/${contractile^}file.a2ml" ]; then
          echo "  ✅ $contractile: Present"
        else
          echo "  ❌ $contractile: MISSING"
        fi
      done
    else
      echo "  ❌ No contractiles directory"
    fi
    echo ""
  fi
done

echo "## K9-SVC Integration Audit"
echo ""

for repo in "${REPOS[@]}"; do
  if [ -d "$repo" ]; then
    if grep -q "K9-SVC\|contractile" "$repo/.github/workflows/"*.yml 2>/dev/null || \
       grep -q "K9-SVC\|contractile" "$repo/.pre-commit-config.yaml" 2>/dev/null; then
      echo "  ✅ $(basename "$repo"): K9 integrated"
    else
      echo "  ❌ $(basename "$repo"): K9 missing"
    fi
  fi
done

echo ""
echo "## Accessibility Implementation Audit"
echo ""

for repo in "${REPOS[@]}"; do
  if [ -d "$repo" ]; then
    if [ -d "$repo/server/lib/burble/accessibility" ] || \
       [ -f "$repo/.machine_readable/contractiles/adjust/Adjustfile.a2ml" ]; then
      echo "  ✅ $(basename "$repo"): Accessibility features present"
    else
      echo "  ❌ $(basename "$repo"): No accessibility implementation"
    fi
  fi
done

echo ""
echo "## Documentation Audit"
echo ""

for repo in "${REPOS[@]}"; do
  if [ -d "$repo" ]; then
    if [ -f "$repo/docs/accessibility/README.adoc" ] || \
       grep -q "accessibility" "$repo/README.adoc" 2>/dev/null; then
      echo "  ✅ $(basename "$repo"): Accessibility documented"
    else
      echo "  ❌ $(basename "$repo"): Accessibility documentation missing"
    fi
  fi
done

echo ""
echo "═══════════════════════════════════════════════════════════════════════════════"
echo "  Audit Complete"
echo "  Total repositories checked: ${#REPOS[@]}"
echo "═══════════════════════════════════════════════════════════════════════════════"
