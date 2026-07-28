#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# validate-k9.sh — K9 configuration file validation script
#
# K9 files come in two dialects, validated differently (standards#434):
#
#   Plain dialect (.k9) — text/YAML-ish documents. The `K9!` magic line is
#   the format marker and MUST be the first non-empty line. Fields use
#   `key: value` form.
#
#   Nickel dialect (.k9.ncl) — Nickel source. A bare `K9!` line is a Nickel
#   syntax error, so the format marker is carried differently: a
#   `magic_number = "K9!"` field, a literal `K9!` preamble line (template
#   files that are preprocessed before evaluation), or by construction —
#   the file imports/merges a K9 pedigree schema (`K9Pedigree`,
#   `pedigree_schema`, or an `import ".…k9.ncl"` of a base template that
#   itself carries the magic). Library/contractile modules are that last
#   class and are first-class citizens, not violations.
#
# Checks:
#   1. Format marker (dialect-appropriate, see above)
#   2. Pedigree presence with required fields (name; version as warning)
#   3. Security level is one of: kennel, yard, hunt (case-insensitive)
#   4. Hunt-level files must have a signature or signature_required field
#   5. SPDX-License-Identifier header presence
#
# This is a LEXICAL linter (grep-grade), not a Nickel evaluator. Field
# checks are file-scope presence checks on purpose: Nickel lets authors
# factor the pedigree through `let` bindings and `&` merges, which no
# line-oriented block tracker can follow. (A previous version tracked
# brace depth to scope checks to the pedigree block; it missed every
# `let component_pedigree = {…}` factoring and miscounted its own opening
# brace. Do not reintroduce block scoping here — deep validation belongs
# to the Nickel contracts themselves.)
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

# Outside GitHub Actions GITHUB_OUTPUT is unset; under `set -u` an unset
# expansion inside a redirection aborts the whole script (the `|| true`
# cannot catch an expansion error). Default to /dev/null for local runs.
GITHUB_OUTPUT="${GITHUB_OUTPUT:-/dev/null}"

# Counters
FILES_SCANNED=0
ERRORS=0
WARNINGS=0

# Valid security levels (the leash metaphor)
VALID_LEVELS="kennel yard hunt"

# ---------------------------------------------------------------------------
# Helper: emit GitHub annotation
# ---------------------------------------------------------------------------
annotate() {
    local level="$1" file="$2" line="$3" message="$4"
    echo "::${level} file=${file},line=${line}::${message}"
}

# ---------------------------------------------------------------------------
# Helper: report issue (respects strict mode)
# ---------------------------------------------------------------------------
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
# Helper: normalise a security level string
# ---------------------------------------------------------------------------
# Strips quotes, leading/trailing whitespace, Nickel enum tick prefix.
# Handles both separators: `leash = 'Hunt` (Nickel) and `leash: hunt` (plain).
normalise_level() {
    local raw="$1"
    # Remove surrounding quotes, tick prefix ('Kennel -> Kennel), whitespace
    if [[ "$raw" == *"="* ]]; then
        raw="${raw#*=}"          # Remove everything before =
    else
        raw="${raw#*:}"          # Plain dialect: remove everything before :
    fi
    raw="${raw//\"/}"            # Remove double quotes
    raw="${raw//\'/}"            # Remove single quotes (Nickel tick)
    raw="${raw//,/}"             # Remove trailing commas
    raw="${raw%%#*}"             # Remove inline comments
    # Trim ALL leading/trailing whitespace (a single-space `%% ` pattern
    # strips only one char and let `'Kennel  # comment` survive as
    # "kennel " — an invalid-level false positive)
    raw="${raw#"${raw%%[![:space:]]*}"}"
    raw="${raw%"${raw##*[![:space:]]}"}"
    echo "${raw,,}"             # Lowercase
}

# ---------------------------------------------------------------------------
# Validator: check a single K9 file
# ---------------------------------------------------------------------------
validate_k9() {
    local file="$1"
    FILES_SCANNED=$((FILES_SCANNED + 1))

    # Dialect: .k9.ncl is Nickel source; bare .k9 is the plain dialect.
    local dialect="plain"
    if [[ "$file" == *.k9.ncl ]]; then
        dialect="ncl"
    fi

    # --- Check 1: format marker (dialect-appropriate) ---
    local first_content_line=""
    local first_content_line_num=0
    local line_num=0

    while IFS= read -r line; do
        line_num=$((line_num + 1))
        # Skip empty lines
        if [[ -z "${line// /}" ]]; then
            continue
        fi
        first_content_line="$line"
        first_content_line_num=$line_num
        break
    done < "$file"

    if [[ "$dialect" == "plain" ]]; then
        if [[ "$first_content_line" != "K9!" ]]; then
            report_issue "error" "$file" "$first_content_line_num" \
                "Missing K9! magic number. First non-empty line must be exactly 'K9!'"
        fi
    else
        # Nickel dialect: a bare K9! line is a Nickel syntax error, so the
        # marker may instead be a magic_number field or arrive by construction
        # through a pedigree-schema import/merge (library modules, #434).
        local has_marker=false
        if [[ "$first_content_line" == "K9!" ]]; then
            has_marker=true
        elif grep -Eq '^[[:space:]]*magic_number[[:space:]]*=[[:space:]]*"K9!"' "$file"; then
            has_marker=true
        elif grep -Eq '(K9Pedigree|pedigree_schema)[[:space:]]*&|&[[:space:]]*(.*\.)?(K9Pedigree|pedigree_schema)|import[[:space:]]*"[^"]*\.k9\.ncl"' "$file"; then
            has_marker=true
        fi

        if [[ "$has_marker" == "false" ]]; then
            report_issue "error" "$file" "$first_content_line_num" \
                "Missing K9 format marker. A .k9.ncl file needs a magic_number = \"K9!\" field, a K9! preamble line, or a K9 pedigree schema import/merge"
        fi
    fi

    # --- Check 2: SPDX header ---
    local has_spdx=false
    line_num=0
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

    # --- Check 3: Pedigree presence with required fields ---
    # File-scope scans by design (see header): Nickel factoring means the
    # pedigree may be `pedigree = {…}`, a let-bound `let component_pedigree
    # = {…}`, a schema merge `X.pedigree_schema & {…}` / `X.K9Pedigree &
    # {…}`, or — plain dialect — a `metadata:`/`pedigree:` YAML block.
    local has_pedigree=false
    local has_pedigree_name=false
    local has_pedigree_version=false
    local has_security_level=false
    local security_level_value=""
    local security_level_line=0
    local has_signature_field=false

    line_num=0
    while IFS= read -r line; do
        line_num=$((line_num + 1))

        # Pedigree construct, Nickel forms: direct, let-bound, schema merge
        if [[ "$line" =~ ^[[:space:]]*(let[[:space:]]+)?[A-Za-z_]*pedigree[[:space:]]*= ]] \
           || [[ "$line" =~ (K9Pedigree|pedigree_schema)[[:space:]]*\& ]] \
           || [[ "$line" =~ \&[[:space:]]*([A-Za-z_][A-Za-z0-9_]*\.)?(K9Pedigree|pedigree_schema) ]]; then
            has_pedigree=true
        fi

        # Pedigree construct, plain dialect: top-level metadata:/pedigree: block
        if [[ "$dialect" == "plain" ]] \
           && [[ "$line" =~ ^(metadata|pedigree):[[:space:]]*$ ]]; then
            has_pedigree=true
        fi

        # Required fields, either separator (= Nickel, : plain)
        if [[ "$line" =~ ^[[:space:]]*name[[:space:]]*[=:] ]]; then
            has_pedigree_name=true
        fi

        if [[ "$line" =~ ^[[:space:]]*(version|schema_version)[[:space:]]*[=:] ]]; then
            has_pedigree_version=true
        fi

        # Security level (leash field)
        if [[ "$line" =~ ^[[:space:]]*(leash|security_level)[[:space:]]*[=:] ]]; then
            has_security_level=true
            security_level_value="$(normalise_level "$line")"
            security_level_line=$line_num
        fi

        # Signature fields
        if [[ "$line" =~ ^[[:space:]]*(signature|signature_required)[[:space:]]*[=:] ]]; then
            has_signature_field=true
        fi
    done < "$file"

    if [[ "$has_pedigree" == "false" ]]; then
        report_issue "error" "$file" 1 \
            "Missing pedigree. K9 files need a pedigree section: 'pedigree = { ... }', a pedigree-schema merge, or (plain dialect) a 'metadata:' block"
    else
        if [[ "$has_pedigree_name" == "false" ]]; then
            report_issue "error" "$file" 1 \
                "Pedigree block missing 'name' field (in pedigree.metadata.name or pedigree.name)"
        fi

        if [[ "$has_pedigree_version" == "false" ]]; then
            report_issue "warning" "$file" 1 \
                "Pedigree block missing 'version' or 'schema_version' field"
        fi
    fi

    # --- Check 4: Security level validation ---
    if [[ "$has_security_level" == "true" && "$security_level_value" =~ ^\{\{.*\}\}$ ]]; then
        # Scaffold file: the level is a template placeholder to be filled at
        # instantiation time. Note it, but a template cannot validate as
        # concrete and flagging it every run just trains people to ignore
        # the gate.
        annotate "notice" "$file" "$security_level_line" \
            "Security level is a template placeholder (${security_level_value}); skipping level validation"
    elif [[ "$has_security_level" == "true" ]]; then
        local level_valid=false
        for valid in $VALID_LEVELS; do
            if [[ "$security_level_value" == "$valid" ]]; then
                level_valid=true
                break
            fi
        done

        if [[ "$level_valid" == "false" ]]; then
            report_issue "error" "$file" "$security_level_line" \
                "Invalid security level '${security_level_value}'. Must be one of: kennel, yard, hunt"
        fi
    else
        if [[ "$has_pedigree" == "true" ]]; then
            report_issue "warning" "$file" 1 \
                "No security level (leash/security_level) found in pedigree block"
        fi
    fi

    # --- Check 5: Hunt-level signature requirement ---
    if [[ "$security_level_value" == "hunt" && "$has_signature_field" == "false" ]]; then
        report_issue "error" "$file" "$security_level_line" \
            "Hunt-level K9 file must include a 'signature' or 'signature_required' field"
    fi
}

# ---------------------------------------------------------------------------
# Main: discover and validate K9 files
# ---------------------------------------------------------------------------

echo "::group::K9 Configuration Validation"
echo "Scanning ${SCAN_PATH} for K9 files (.k9, .k9.ncl)..."
echo ""

# Find all K9 files, excluding .git directory
mapfile -t k9_files < <(find "$SCAN_PATH" \( -name '*.k9' -o -name '*.k9.ncl' \) -not -path '*/.git/*' -type f | sort)

if [[ ${#k9_files[@]} -eq 0 ]]; then
    echo "::notice::No K9 files found in ${SCAN_PATH}"
    echo "files_scanned=0" >> "$GITHUB_OUTPUT" 2>/dev/null || true
    echo "errors=0" >> "$GITHUB_OUTPUT" 2>/dev/null || true
    echo "warnings=0" >> "$GITHUB_OUTPUT" 2>/dev/null || true
    echo "::endgroup::"
    exit 0
fi

echo "Found ${#k9_files[@]} K9 file(s)"
echo ""

for file in "${k9_files[@]}"; do
    echo "  Validating: ${file}"
    validate_k9 "$file"
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
    echo "::error::K9 validation failed with ${ERRORS} error(s)"
    exit 1
fi

echo "K9 validation passed."
exit 0
