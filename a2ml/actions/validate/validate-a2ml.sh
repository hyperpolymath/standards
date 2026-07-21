#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# validate-a2ml.sh — A2ML manifest validation script
#
# Scans for .a2ml files and validates:
#   1. Identity presence (warning — see below)
#   2. SPDX-License-Identifier header presence
#   3. Attestation block structure (if present)
#   4. Section heading syntax ([section] or ## section)
#
# On identity (standards#435): the A2ML SPEC's only identity requirement is
# per-record (`@record` needs author/tool/kind, SPEC §7) — there is NO
# normative file-level "must have agent-id/name" rule. An earlier version of
# this script required `agent-id|name|project =` as a hard error and flagged
# the majority of canonical estate files (scorecards identify via `spec_id`,
# contractile Xfiles via `@abstract` + filename, the six-file set via
# `[metadata]`). Identity is therefore checked as a lint WARNING against the
# real estate shapes, and skipped entirely for classes that are identity-free
# by design:
#   - AI manifests (AI-MANIFEST*.a2ml, AI.a2ml): markdown prose
#   - design-rationale/example trees (INPUT_DESIGN_TREES)
#   - templates/scaffolds: basename contains "template", or the body carries
#     {{PLACEHOLDER}} markers — a scaffold cannot validate as concrete
#
# Environment variables:
#   INPUT_PATH         — Directory to scan (default: .)
#   INPUT_STRICT       — Promote warnings to errors (default: false)
#   INPUT_DESIGN_TREES — Space-separated path fragments exempt from
#                        identity/version checks (default:
#                        "machine-readable-design/ self-validating/examples/
#                        docs/templates/")
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
DESIGN_TREES="${INPUT_DESIGN_TREES:-machine-readable-design/ self-validating/examples/ docs/templates/}"

# Outside GitHub Actions GITHUB_OUTPUT is unset; under `set -u` an unset
# expansion inside a redirection aborts the whole script (the `|| true`
# cannot catch an expansion error). Default to /dev/null for local runs.
GITHUB_OUTPUT="${GITHUB_OUTPUT:-/dev/null}"

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

    # --- Check 2: Identity presence (lint warning; see header) ---
    # Identity shapes actually used across the estate:
    #   - agent-id / agent_id / name / project / spec_id = "..." (TOML-ish)
    #   - name: "..." (colon dialect)
    #   - a [metadata] or [scorecard] section (the six-file set and
    #     scorecards: the filename + section carry the identity)
    #   - an @abstract directive (contractile Xfile dialect)
    local has_identity=false
    local has_version=false
    local has_placeholders=false
    line_num=0

    while IFS= read -r line; do
        line_num=$((line_num + 1))

        # Check for identity fields (various A2ML patterns)
        if [[ "$line" =~ ^[[:space:]]*(agent[-_]id|name|project|spec_id)[[:space:]]*= ]] \
           || [[ "$line" =~ ^[[:space:]]*name[[:space:]]*: ]] \
           || [[ "$line" =~ ^\[(metadata|scorecard)\] ]] \
           || [[ "$line" =~ ^@abstract ]]; then
            has_identity=true
        fi
        # Check for version field (either separator)
        if [[ "$line" =~ ^[[:space:]]*(version|schema_version)[[:space:]]*[=:] ]]; then
            has_version=true
        fi
        # Template placeholder marker ({{PROJECT_NAME}}, {{VERSION}}, …)
        if [[ "$line" == *"{{"*"}}"* ]]; then
            has_placeholders=true
        fi
    done < "$file"

    # Classes that are identity-free by design (see header):
    local basename
    basename="$(basename "$file")"
    local identity_exempt=false
    # AI manifests: markdown prose (0-AI-MANIFEST.a2ml, AI.a2ml, …)
    if [[ "$basename" == *"AI-MANIFEST"* || "$basename" == "AI.a2ml" ]]; then
        identity_exempt=true
    fi
    # Templates/scaffolds
    if [[ "${basename,,}" == *"template"* || "$has_placeholders" == "true" ]]; then
        identity_exempt=true
    fi
    # Design-rationale / example trees (standards#435 option a)
    local tree
    for tree in $DESIGN_TREES; do
        if [[ "$file" == *"$tree"* ]]; then
            identity_exempt=true
            break
        fi
    done

    if [[ "$has_identity" == "false" && "$identity_exempt" == "false" ]]; then
        report_issue "warning" "$file" 1 \
            "No identity found (agent-id/name/project/spec_id field, [metadata] or [scorecard] section, or @abstract directive)"
    fi

    if [[ "$has_version" == "false" && "$identity_exempt" == "false" ]]; then
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
