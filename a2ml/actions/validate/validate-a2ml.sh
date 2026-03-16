#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# validate-a2ml.sh — A2ML manifest validation script
#
# Scans for .a2ml files and validates:
#   1. Required fields: agent-id or pedigree name, version
#   2. SPDX-License-Identifier header presence
#   3. Attestation block structure (if present)
#   4. Section heading syntax ([section] or ## section)
#
# Environment variables:
#   INPUT_PATH   — Directory to scan (default: .)
#   INPUT_STRICT — Promote warnings to errors (default: false)
#
# Exit codes:
#   0 — All files valid (or only warnings in non-strict mode)
#   1 — Validation errors found

set -euo pipefail

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

SCAN_PATH="${INPUT_PATH:-.}"
STRICT="${INPUT_STRICT:-false}"

# Counters
FILES_SCANNED=0
ERRORS=0
WARNINGS=0

# ---------------------------------------------------------------------------
# Helper: emit GitHub annotation
# ---------------------------------------------------------------------------
# Usage: annotate <level> <file> <line> <message>
#   level: error | warning | notice
annotate() {
    local level="$1" file="$2" line="$3" message="$4"
    echo "::${level} file=${file},line=${line}::${message}"
}

# ---------------------------------------------------------------------------
# Helper: report issue (respects strict mode)
# ---------------------------------------------------------------------------
# Usage: report_issue <severity> <file> <line> <message>
#   severity: error | warning
report_issue() {
    local severity="$1" file="$2" line="$3" message="$4"

    if [[ "$severity" == "warning" && "$STRICT" == "true" ]]; then
        severity="error"
    fi

    annotate "$severity" "$file" "$line" "$message"

    if [[ "$severity" == "error" ]]; then
        ERRORS=$((ERRORS + 1))
    else
        WARNINGS=$((WARNINGS + 1))
    fi
}

# ---------------------------------------------------------------------------
# Validator: check a single .a2ml file
# ---------------------------------------------------------------------------
validate_a2ml() {
    local file="$1"
    FILES_SCANNED=$((FILES_SCANNED + 1))

    # --- Check 1: SPDX header ---
    # The SPDX-License-Identifier should appear in the first 10 lines
    local has_spdx=false
    local line_num=0
    while IFS= read -r line; do
        line_num=$((line_num + 1))
        if [[ $line_num -gt 10 ]]; then
            break
        fi
        if [[ "$line" == *"SPDX-License-Identifier"* ]]; then
            has_spdx=true
            break
        fi
    done < "$file"

    if [[ "$has_spdx" == "false" ]]; then
        report_issue "warning" "$file" 1 \
            "Missing SPDX-License-Identifier in first 10 lines"
    fi

    # --- Check 2: Required identity fields ---
    # A2ML files must contain either:
    #   - agent-id = "..." or agent_id = "..."
    #   - pedigree block with name field
    #   - name = "..." at top level (for AI manifests)
    #   - project = "..." (for STATE.a2ml)
    local has_identity=false
    local has_version=false
    line_num=0

    while IFS= read -r line; do
        line_num=$((line_num + 1))

        # Check for identity fields (various A2ML patterns)
        if [[ "$line" =~ ^[[:space:]]*(agent[-_]id|name|project)[[:space:]]*= ]]; then
            has_identity=true
        fi
        # Check for version field
        if [[ "$line" =~ ^[[:space:]]*(version|schema_version)[[:space:]]*= ]]; then
            has_version=true
        fi
    done < "$file"

    # AI manifest files (0-AI-MANIFEST.a2ml, 0.1-AI-MANIFEST.a2ml, etc.)
    # use markdown-style headers and free text, so identity check is relaxed
    local basename
    basename="$(basename "$file")"
    local is_manifest=false
    if [[ "$basename" == *"AI-MANIFEST"* ]]; then
        is_manifest=true
    fi

    if [[ "$has_identity" == "false" && "$is_manifest" == "false" ]]; then
        report_issue "error" "$file" 1 \
            "Missing required identity field (agent-id, name, or project)"
    fi

    if [[ "$has_version" == "false" && "$is_manifest" == "false" ]]; then
        report_issue "warning" "$file" 1 \
            "Missing version or schema_version field"
    fi

    # --- Check 3: Attestation block structure ---
    # If file contains [attestation] or ## ATTESTATION, validate it has
    # required sub-fields: proof or signature
    local in_attestation=false
    local attestation_line=0
    local attestation_has_content=false
    line_num=0

    while IFS= read -r line; do
        line_num=$((line_num + 1))

        # Detect attestation section start
        if [[ "$line" =~ ^\[attestation\] ]] || [[ "$line" =~ ^##[[:space:]]+[Aa]ttestation ]] || [[ "$line" =~ ^##[[:space:]]+ATTESTATION ]]; then
            in_attestation=true
            attestation_line=$line_num
            continue
        fi

        # Detect next section (ends attestation block)
        if [[ "$in_attestation" == "true" ]]; then
            if [[ "$line" =~ ^\[.+\] ]] || [[ "$line" =~ ^##[[:space:]] ]]; then
                in_attestation=false
                continue
            fi
            # Check for content in attestation block
            if [[ "$line" =~ (proof|signature|verified|hash)[[:space:]]*= ]]; then
                attestation_has_content=true
            fi
        fi
    done < "$file"

    if [[ $attestation_line -gt 0 && "$attestation_has_content" == "false" ]]; then
        report_issue "warning" "$file" "$attestation_line" \
            "Attestation block found but missing proof/signature/hash fields"
    fi

    # --- Check 4: Section heading syntax ---
    # Validate that [section] headings are well-formed (no unclosed brackets)
    line_num=0
    while IFS= read -r line; do
        line_num=$((line_num + 1))
        # Lines starting with [ should have a matching ]
        if [[ "$line" =~ ^\[ && ! "$line" =~ ^\[.+\] ]]; then
            # Exclude markdown-style links and multi-line values
            if [[ ! "$line" =~ ^\[.*\]\( && ! "$line" =~ ^\[TODO && ! "$line" =~ ^\[YOUR ]]; then
                report_issue "warning" "$file" "$line_num" \
                    "Possibly malformed section heading: unclosed bracket"
            fi
        fi
    done < "$file"
}

# ---------------------------------------------------------------------------
# Main: discover and validate .a2ml files
# ---------------------------------------------------------------------------

echo "::group::A2ML Manifest Validation"
echo "Scanning ${SCAN_PATH} for .a2ml files..."
echo ""

# Find all .a2ml files, excluding .git directory
mapfile -t a2ml_files < <(find "$SCAN_PATH" -name '*.a2ml' -not -path '*/.git/*' -type f | sort)

if [[ ${#a2ml_files[@]} -eq 0 ]]; then
    echo "::notice::No .a2ml files found in ${SCAN_PATH}"
    echo "files_scanned=0" >> "$GITHUB_OUTPUT" 2>/dev/null || true
    echo "errors=0" >> "$GITHUB_OUTPUT" 2>/dev/null || true
    echo "warnings=0" >> "$GITHUB_OUTPUT" 2>/dev/null || true
    echo "::endgroup::"
    exit 0
fi

echo "Found ${#a2ml_files[@]} .a2ml file(s)"
echo ""

for file in "${a2ml_files[@]}"; do
    echo "  Validating: ${file}"
    validate_a2ml "$file"
done

echo ""
echo "────────────────────────────────────────"
echo "Files scanned: ${FILES_SCANNED}"
echo "Errors:        ${ERRORS}"
echo "Warnings:      ${WARNINGS}"
echo "Strict mode:   ${STRICT}"
echo "────────────────────────────────────────"

# Write outputs for GitHub Actions
{
    echo "files_scanned=${FILES_SCANNED}"
    echo "errors=${ERRORS}"
    echo "warnings=${WARNINGS}"
} >> "$GITHUB_OUTPUT" 2>/dev/null || true

echo "::endgroup::"

# Exit with failure if errors were found
if [[ $ERRORS -gt 0 ]]; then
    echo "::error::A2ML validation failed with ${ERRORS} error(s)"
    exit 1
fi

echo "A2ML validation passed."
exit 0
