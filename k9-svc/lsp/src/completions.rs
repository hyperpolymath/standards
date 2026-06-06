// SPDX-License-Identifier: MPL-2.0
//
// completions.rs — K9 completion provider
//
// Provides context-aware completions for:
//   1. Pedigree fields (name, version, description, author, spdx_license)
//   2. Security fields (trust_level, allow_network, etc.)
//   3. Trust level values ('Kennel, 'Yard, 'Hunt) after trust_level =
//   4. Recipe names (install, validate, deploy, migrate, rollback)
//   5. Nickel standard library imports (std.contract, std.string, etc.)

use tower_lsp::lsp_types::*;

/// Get the line at the given position from the document text.
///
/// Returns the line content and the character offset of the cursor within it.
fn line_at(text: &str, position: Position) -> Option<(&str, usize)> {
    let line_idx = position.line as usize;
    let col = position.character as usize;
    text.lines().nth(line_idx).map(|l| (l, col))
}

/// Determine the completion context and return appropriate items.
///
/// Analyses the current line to decide which category of completions
/// to offer: trust level values, pedigree fields, security fields,
/// recipe names, or Nickel imports.
pub fn complete(text: &str, position: Position) -> Vec<CompletionItem> {
    let (line, col) = match line_at(text, position) {
        Some(v) => v,
        None => return Vec::new(),
    };

    let prefix = &line[..col.min(line.len())];

    // Context: after trust_level = → offer trust level values.
    if prefix.contains("trust_level") && prefix.contains('=') {
        return trust_level_completions();
    }

    // Context: after "import" or starts with "import" → Nickel std imports.
    if prefix.trim_start().starts_with("import") || prefix.contains("std.") {
        return nickel_import_completions();
    }

    // Context: inside a security block (heuristic: line has security-related prefix).
    if is_in_block(text, position, "security") {
        return security_field_completions();
    }

    // Context: inside a recipes block.
    if is_in_block(text, position, "recipes") {
        return recipe_completions();
    }

    // Context: inside a metadata block or at top level → pedigree fields.
    if is_in_block(text, position, "metadata") {
        return pedigree_field_completions();
    }

    // Default: offer all top-level completions.
    let mut items = Vec::new();
    items.extend(pedigree_field_completions());
    items.extend(security_field_completions());
    items.extend(recipe_completions());
    items
}

/// Heuristic check: is the cursor inside a block named `block_name`?
///
/// Scans backwards from the cursor position looking for `block_name = {`
/// or `block_name = {\n` and checks that the brace depth indicates we
/// are still inside that block.
fn is_in_block(text: &str, position: Position, block_name: &str) -> bool {
    let lines: Vec<&str> = text.lines().collect();
    let cursor_line = position.line as usize;
    let mut depth: i32 = 0;

    // Scan backwards from cursor line.
    for i in (0..=cursor_line.min(lines.len().saturating_sub(1))).rev() {
        let line = lines[i];
        let trimmed = line.trim();

        // Skip comments.
        if trimmed.starts_with('#') {
            continue;
        }

        // Count braces (simplified — does not handle strings).
        for ch in line.chars().rev() {
            match ch {
                '}' => depth += 1,
                '{' => depth -= 1,
                _ => {}
            }
        }

        // If we found an opening brace that matches our block name, we're inside it.
        if depth < 0 && trimmed.contains(block_name) && trimmed.contains('=') {
            return true;
        }
    }

    false
}

// ─────────────────────────────────────────────────────────────────────
// Completion item builders
// ─────────────────────────────────────────────────────────────────────

/// Pedigree / metadata field completions.
fn pedigree_field_completions() -> Vec<CompletionItem> {
    vec![
        make_field_completion(
            "name",
            "Component name (lowercase, hyphenated)",
            "name = \"${1:my-component}\",",
        ),
        make_field_completion(
            "version",
            "SemVer version with optional stability suffix",
            "version = \"${1:1.0.0-alpha}\",",
        ),
        make_field_completion(
            "description",
            "Human-readable component purpose",
            "description = \"${1:A K9 component}\",",
        ),
        make_field_completion(
            "author",
            "Component author name and email",
            "author = \"${1:Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>}\",",
        ),
        make_field_completion(
            "spdx_license",
            "SPDX license identifier for the component",
            "spdx_license = \"${1:PMPL-1.0-or-later}\",",
        ),
    ]
}

/// Security field completions.
fn security_field_completions() -> Vec<CompletionItem> {
    vec![
        make_field_completion(
            "trust_level",
            "Execution permission tier: 'Kennel, 'Yard, or 'Hunt",
            "trust_level = ${1:'Yard},",
        ),
        make_field_completion(
            "allow_network",
            "Whether the component can fetch external resources",
            "allow_network = ${1:false},",
        ),
        make_field_completion(
            "allow_filesystem_write",
            "Whether the component can modify the host filesystem",
            "allow_filesystem_write = ${1:false},",
        ),
        make_field_completion(
            "allow_subprocess",
            "Whether the component can spawn child processes",
            "allow_subprocess = ${1:false},",
        ),
        make_field_completion(
            "signature",
            "Cryptographic handshake for Hunt level (required for 'Hunt)",
            "signature = \"${1}\",",
        ),
    ]
}

/// Trust level value completions.
fn trust_level_completions() -> Vec<CompletionItem> {
    vec![
        CompletionItem {
            label: "'Kennel".to_string(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Pure data — no execution, read-only, safe anywhere".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "**Kennel** — Pure Data\n\n\
                        No execution permitted. Read-only access. Safe to open \
                        on any platform including constrained environments (Edge, ASIC)."
                    .to_string(),
            })),
            insert_text: Some("'Kennel".to_string()),
            insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
            ..Default::default()
        },
        CompletionItem {
            label: "'Yard".to_string(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Validation only — Nickel evaluation, no I/O".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "**Yard** — Validation Only\n\n\
                        Nickel evaluation is permitted for contract checking. \
                        No filesystem, network, or subprocess access."
                    .to_string(),
            })),
            insert_text: Some("'Yard".to_string()),
            insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
            ..Default::default()
        },
        CompletionItem {
            label: "'Hunt".to_string(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Full execution — requires cryptographic handshake".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "**Hunt** — Full Execution\n\n\
                        Complete must-just-nickel triad execution. Requires a valid \
                        cryptographic handshake via the `signature` field. Can access \
                        network, filesystem, and spawn subprocesses."
                    .to_string(),
            })),
            insert_text: Some("'Hunt".to_string()),
            insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
            ..Default::default()
        },
    ]
}

/// Recipe name completions.
fn recipe_completions() -> Vec<CompletionItem> {
    vec![
        make_field_completion(
            "install",
            "Installation recipe — runs during component setup",
            "install = \"${1:just install}\",",
        ),
        make_field_completion(
            "validate",
            "Self-validation recipe — checks component integrity",
            "validate = \"${1:just validate}\",",
        ),
        make_field_completion(
            "deploy",
            "Deployment recipe — Podman or native deployment",
            "deploy = \"${1:just deploy}\",",
        ),
        make_field_completion(
            "migrate",
            "Version migration recipe — zero-deprecation upgrade",
            "migrate = \"${1:just migrate}\",",
        ),
        make_field_completion(
            "rollback",
            "Rollback recipe — revert to previous version",
            "rollback = \"${1:just rollback}\",",
        ),
    ]
}

/// Nickel standard library import completions.
fn nickel_import_completions() -> Vec<CompletionItem> {
    let imports = [
        ("std.contract", "Contract combinators and utilities"),
        ("std.string", "String manipulation functions"),
        ("std.number", "Numeric operations"),
        ("std.array", "Array/list operations"),
        ("std.record", "Record manipulation functions"),
        ("std.function", "Function combinators"),
        ("std.io", "I/O operations (restricted by security level)"),
        ("std.enum", "Enum tag utilities"),
    ];

    imports
        .iter()
        .map(|(name, desc)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::MODULE),
            detail: Some(desc.to_string()),
            insert_text: Some(format!("import \"{}\"", name)),
            insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
            ..Default::default()
        })
        .collect()
}

/// Helper to build a field completion item with snippet insertion.
fn make_field_completion(label: &str, detail: &str, snippet: &str) -> CompletionItem {
    CompletionItem {
        label: label.to_string(),
        kind: Some(CompletionItemKind::FIELD),
        detail: Some(detail.to_string()),
        insert_text: Some(snippet.to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        ..Default::default()
    }
}
