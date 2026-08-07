// SPDX-License-Identifier: MPL-2.0
//
// a2ml-lsp — Completion provider for A2ML documents.
//
// Provides context-aware completions:
//   1. Directive names after `@`
//   2. Section headers after `[`
//   3. Known key names inside a [section]
//   4. `@ref()` targets from declared IDs in the document
//
// Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

use std::sync::LazyLock;
use regex::Regex;
use tower_lsp::lsp_types::*;

// Literal patterns, compiled once.
//
// These were `Regex::new(<literal>).unwrap()` inside the functions below.
// An LSP recompiles those on EVERY request — every keystroke, for
// completions and diagnostics — and `unwrap` panics the handler task if a
// literal is ever malformed. `LazyLock` compiles each pattern once, and
// `expect` states the invariant: the pattern is a compile-time constant, so
// a failure is a programming error rather than a runtime condition.
static REF_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"@ref\(([^)]*)$").expect("REF_RE is a valid literal pattern"));

static ID_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"id\s*=\s*"([^"]+)""#).expect("ID_RE is a valid literal pattern"));

static SECTION_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^\[([A-Za-z][A-Za-z0-9_-]*)\]\s*$").expect("SECTION_RE is a valid literal pattern"));


// ── Directive catalogue ──────────────────────────────────────────────

/// A2ML directive with its name, brief description, and snippet insert text.
struct DirectiveInfo {
    name: &'static str,
    detail: &'static str,
    snippet: &'static str,
}

/// All core directives from the A2ML v0/v1 spec.
const DIRECTIVES: &[DirectiveInfo] = &[
    DirectiveInfo {
        name: "abstract",
        detail: "Declares the abstract / summary section",
        snippet: "abstract:\n$0\n@end",
    },
    DirectiveInfo {
        name: "opaque",
        detail: "Byte-for-byte preserved payload block",
        snippet: "opaque(lang=\"${1:text}\", id=\"${2:id}\"):\n$0\n@end",
    },
    DirectiveInfo {
        name: "fig",
        detail: "Figure block with caption and optional ID",
        snippet: "fig(id=\"${1:fig-id}\"):\n$0\n@end",
    },
    DirectiveInfo {
        name: "table",
        detail: "Table block with caption and optional ID",
        snippet: "table(id=\"${1:table-id}\"):\n$0\n@end",
    },
    DirectiveInfo {
        name: "refs",
        detail: "References / bibliography section",
        snippet: "refs:\n$0\n@end",
    },
    DirectiveInfo {
        name: "requires",
        detail: "Declares structural dependencies for attested mode",
        snippet: "requires:\n$0\n@end",
    },
    DirectiveInfo {
        name: "include",
        detail: "Include content from another A2ML file",
        snippet: "include(\"${1:path.a2ml}\")",
    },
    DirectiveInfo {
        name: "ref",
        detail: "Inline reference to a declared ID",
        snippet: "ref(${1:target-id})",
    },
    DirectiveInfo {
        name: "meta",
        detail: "Metadata directive block",
        snippet: "meta:\n$0\n@end",
    },
    DirectiveInfo {
        name: "note",
        detail: "Admonition: informational note",
        snippet: "note:\n$0\n@end",
    },
    DirectiveInfo {
        name: "warning",
        detail: "Admonition: warning callout",
        snippet: "warning:\n$0\n@end",
    },
    DirectiveInfo {
        name: "example",
        detail: "Example block",
        snippet: "example:\n$0\n@end",
    },
    DirectiveInfo {
        name: "quote",
        detail: "Block quotation",
        snippet: "quote:\n$0\n@end",
    },
    DirectiveInfo {
        name: "end",
        detail: "Closes the current directive block",
        snippet: "end",
    },
];

// ── Section catalogue ────────────────────────────────────────────────

/// Section name and its purpose (for checkpoint / metadata files).
struct SectionInfo {
    name: &'static str,
    detail: &'static str,
}

/// Known [section] header names used in A2ML checkpoint files.
const SECTIONS: &[SectionInfo] = &[
    SectionInfo { name: "metadata",                detail: "Document metadata (project, version, status)" },
    SectionInfo { name: "project-context",         detail: "Project background and goals" },
    SectionInfo { name: "dependencies",            detail: "External and internal dependencies" },
    SectionInfo { name: "current-position",        detail: "Where the project is right now" },
    SectionInfo { name: "route-to-mvp",            detail: "Steps remaining to reach MVP" },
    SectionInfo { name: "blockers-and-issues",     detail: "Active blockers and known issues" },
    SectionInfo { name: "critical-next-actions",   detail: "Immediate action items" },
    SectionInfo { name: "session-history",         detail: "Timestamped log of work sessions" },
    SectionInfo { name: "architecture-decisions",  detail: "Architecture Decision Records (ADRs)" },
    SectionInfo { name: "development-practices",   detail: "Coding standards and workflow conventions" },
    SectionInfo { name: "design-rationale",        detail: "Why decisions were made the way they were" },
    SectionInfo { name: "ecosystem",               detail: "Ecosystem position and relationships" },
    SectionInfo { name: "related-projects",        detail: "Sibling / upstream / downstream projects" },
    SectionInfo { name: "position-in-ecosystem",   detail: "Where this project fits in the larger picture" },
    SectionInfo { name: "agentic",                 detail: "AI agent interaction patterns" },
    SectionInfo { name: "neurosym",                detail: "Neurosymbolic integration configuration" },
    SectionInfo { name: "playbook",                detail: "Operational runbook and procedures" },
];

/// Known keys inside a [metadata] section.
const METADATA_KEYS: &[(&str, &str)] = &[
    ("project",      "Project name"),
    ("version",      "Semantic version string"),
    ("status",       "Project status (active, paused, archived, …)"),
    ("author",       "Primary author"),
    ("license",      "SPDX license identifier"),
    ("created",      "Creation date (ISO 8601)"),
    ("updated",      "Last update date (ISO 8601)"),
    ("description",  "One-line description"),
];

// ── Public entry point ───────────────────────────────────────────────

/// Produce completion items given the full document `text` and the cursor
/// `position`.
///
/// Dispatches to the appropriate sub-provider based on what the cursor line
/// looks like at the given column.
pub fn provide_completions(text: &str, position: Position) -> Vec<CompletionItem> {
    let lines: Vec<&str> = text.lines().collect();
    let line_idx = position.line as usize;

    // Guard: cursor beyond document range.
    if line_idx >= lines.len() {
        return vec![];
    }

    let line = lines[line_idx];
    let col = position.character as usize;
    let prefix = if col <= line.len() { &line[..col] } else { line };

    // 1. After `@` → directive completions.
    if let Some(at_pos) = prefix.rfind('@') {
        let after_at = &prefix[at_pos + 1..];
        // Only trigger when the text after @ looks like the start of a name.
        if after_at.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '-') {
            return directive_completions(after_at);
        }
    }

    // 2. After `[` at the start of a line → section header completions.
    let trimmed = prefix.trim_start();
    if trimmed.starts_with('[') && !trimmed.contains(']') {
        let after_bracket = &trimmed[1..];
        return section_completions(after_bracket);
    }

    // 3. Inside a [section], beginning of a new line → key completions.
    if trimmed.is_empty() || (!trimmed.contains('=') && !trimmed.starts_with('#') && !trimmed.starts_with('@')) {
        if let Some(section) = find_enclosing_section(&lines, line_idx) {
            return key_completions(&section, trimmed);
        }
    }

    // 4. Inside `@ref(` → ID completions.
    if prefix.contains("@ref(") {
        let ref_re = &*REF_RE;
        if let Some(caps) = ref_re.captures(prefix) {
            let Some(partial) = caps.get(1).map(|m| m.as_str()) else {
                return Vec::new();
            };
            return ref_target_completions(text, partial);
        }
    }

    vec![]
}

// ── Sub-providers ────────────────────────────────────────────────────

/// Directive name completions filtered by the partial text after `@`.
fn directive_completions(partial: &str) -> Vec<CompletionItem> {
    DIRECTIVES
        .iter()
        .filter(|d| d.name.starts_with(partial))
        .map(|d| CompletionItem {
            label: format!("@{}", d.name),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(d.detail.to_string()),
            insert_text: Some(d.snippet.to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        })
        .collect()
}

/// Section header completions filtered by the partial text after `[`.
fn section_completions(partial: &str) -> Vec<CompletionItem> {
    SECTIONS
        .iter()
        .filter(|s| s.name.starts_with(partial))
        .map(|s| CompletionItem {
            label: format!("[{}]", s.name),
            kind: Some(CompletionItemKind::MODULE),
            detail: Some(s.detail.to_string()),
            insert_text: Some(format!("{}]", s.name)),
            insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
            ..Default::default()
        })
        .collect()
}

/// Key name completions inside a section block.
fn key_completions(section_name: &str, partial: &str) -> Vec<CompletionItem> {
    let keys: &[(&str, &str)] = match section_name {
        "metadata" => METADATA_KEYS,
        // Other sections could have their own key catalogues in future.
        _ => return vec![],
    };

    keys.iter()
        .filter(|(k, _)| k.starts_with(partial))
        .map(|(k, desc)| CompletionItem {
            label: k.to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some(desc.to_string()),
            insert_text: Some(format!("{} = \"$1\"", k)),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        })
        .collect()
}

/// `@ref()` target completions from IDs declared in the document.
fn ref_target_completions(text: &str, partial: &str) -> Vec<CompletionItem> {
    let id_re = &*ID_RE;
    id_re
        .captures_iter(text)
        // filter_map, not map: a capture that did not participate is skipped
        // rather than panicking the completion handler.
        .filter_map(|cap| cap.get(1).map(|m| m.as_str().to_string()))
        .filter(|id| id.starts_with(partial))
        .map(|id| CompletionItem {
            label: id.clone(),
            kind: Some(CompletionItemKind::REFERENCE),
            detail: Some("Document ID".to_string()),
            insert_text: Some(id),
            insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
            ..Default::default()
        })
        .collect()
}

// ── Helpers ──────────────────────────────────────────────────────────

/// Walk backwards from `line_idx` to find the nearest `[section]` header.
///
/// Returns `None` if the cursor is not inside a section block.
fn find_enclosing_section(lines: &[&str], line_idx: usize) -> Option<String> {
    let section_re = &*SECTION_RE;
    for i in (0..line_idx).rev() {
        let trimmed = lines[i].trim();

        // Stop if we hit a heading or directive — we've left any section scope.
        if trimmed.starts_with('#') || trimmed.starts_with('@') {
            return None;
        }

        if let Some(caps) = section_re.captures(trimmed) {
            return caps.get(1).map(|m| m.as_str().to_string());
        }
    }

    None
}
