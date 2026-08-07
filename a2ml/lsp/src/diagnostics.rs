// SPDX-License-Identifier: MPL-2.0
//
// a2ml-lsp — Diagnostic checks for A2ML documents.
//
// Each public function in this module inspects the document text and returns a
// Vec<Diagnostic>.  `run_all_checks` composes them into a single pass.
//
// Checks implemented:
//   1. Missing SPDX header                    → Warning
//   2. Unclosed @directive…@end blocks         → Error
//   3. Mismatched directive nesting            → Error
//   4. Invalid @ref() targets                  → Warning
//   5. Missing required metadata fields        → Info
//   6. Duplicate section headers               → Warning
//   7. Malformed key = "value" lines           → Error
//   8. TOML-like [section] validation          → Info
//
// Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

use std::sync::LazyLock;
use regex::Regex;
use tower_lsp::lsp_types::*;

// Literal patterns, compiled once.
//
// These were built by calling Regex::new on a literal and unwrapping the
// result, inside the functions below.
// An LSP recompiles those on EVERY request — every keystroke, for
// completions and diagnostics — and `unwrap` panics the handler task if a
// literal is ever malformed. `LazyLock` compiles each pattern once, and
// `expect` states the invariant: the pattern is a compile-time constant, so
// a failure is a programming error rather than a runtime condition.
static OPEN_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^@([A-Za-z][A-Za-z0-9_-]*)(?:\([^)]*\))?:\s*$").expect("OPEN_RE is a valid literal pattern"));

static CLOSE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^@end\s*$").expect("CLOSE_RE is a valid literal pattern"));

static ID_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"id\s*=\s*"([^"]+)""#).expect("ID_RE is a valid literal pattern"));

static REF_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"@ref\(([^)]+)\)").expect("REF_RE is a valid literal pattern"));

static HEADING_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(#{1,5})\s+(.+)$").expect("HEADING_RE is a valid literal pattern"));

static SECTION_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^\[([A-Za-z][A-Za-z0-9_-]*)\]\s*$").expect("SECTION_RE is a valid literal pattern"));


// ── Known values ─────────────────────────────────────────────────────

/// Core A2ML directive names recognised by the spec (v0 + v1).
///
/// Used by `check_unclosed_directives` to distinguish known directives from
/// user-defined ones (future: emit a hint for unrecognised directive names).
#[allow(dead_code)]
const KNOWN_DIRECTIVES: &[&str] = &[
    "abstract", "opaque", "fig", "table", "refs", "requires", "include",
    "ref", "meta", "note", "warning", "example", "quote",
];

/// Section header names commonly found in A2ML checkpoint / metadata files.
const KNOWN_SECTIONS: &[&str] = &[
    "metadata", "project-context", "dependencies", "current-position",
    "route-to-mvp", "blockers-and-issues", "critical-next-actions",
    "session-history", "architecture-decisions", "development-practices",
    "design-rationale", "ecosystem", "related-projects", "position-in-ecosystem",
    "agentic", "neurosym", "playbook",
];

/// Required metadata keys for checkpoint-style A2ML files.
const REQUIRED_METADATA_KEYS: &[&str] = &["project", "version", "status"];

// ── Public entry point ───────────────────────────────────────────────

/// Run every diagnostic check against `text` and return a merged list.
///
/// Each check is independent; we concatenate their results so the editor
/// receives them all at once.
pub fn run_all_checks(text: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    diags.extend(check_spdx_header(text));
    diags.extend(check_unclosed_directives(text));
    diags.extend(check_invalid_refs(text));
    diags.extend(check_required_metadata(text));
    diags.extend(check_duplicate_sections(text));
    diags.extend(check_malformed_kv_lines(text));
    diags.extend(check_toml_sections(text));

    diags
}

// ── Individual checks ────────────────────────────────────────────────

/// 1. Missing SPDX header.
///
/// Warns when the first non-blank, non-comment line of the document does not
/// contain an `SPDX-License-Identifier:` string.
fn check_spdx_header(text: &str) -> Vec<Diagnostic> {
    // Look for SPDX anywhere in the first 5 lines (comments, blank lines OK).
    let has_spdx = text
        .lines()
        .take(5)
        .any(|line| line.contains("SPDX-License-Identifier:"));

    if has_spdx {
        return vec![];
    }

    vec![Diagnostic {
        range: Range::new(Position::new(0, 0), Position::new(0, 1)),
        severity: Some(DiagnosticSeverity::WARNING),
        code: Some(NumberOrString::String("missing-spdx".into())),
        source: Some("a2ml".into()),
        message: "Missing SPDX-License-Identifier header in the first 5 lines".into(),
        ..Default::default()
    }]
}

/// 2 + 3. Unclosed and mis-nested `@directive:` … `@end` blocks.
///
/// We maintain a simple stack of open directives.  An `@end` pops the stack;
/// reaching EOF with a non-empty stack means unclosed directives.
fn check_unclosed_directives(text: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    // Regex for `@name:` or `@name(…):` at the start of a line.
    let open_re = &*OPEN_RE;
    let close_re = &*CLOSE_RE;
    /// A directive that we opened but haven't yet closed.
    struct OpenDirective {
        name: String,
        line: u32,
    }

    let mut stack: Vec<OpenDirective> = Vec::new();

    for (line_idx, line) in text.lines().enumerate() {
        let trimmed = line.trim();

        if let Some(caps) = open_re.captures(trimmed) {
            let Some(name) = caps.get(1).map(|m| m.as_str().to_string()) else {
                continue;
            };
            stack.push(OpenDirective {
                name,
                line: line_idx as u32,
            });
        } else if close_re.is_match(trimmed) {
            if stack.is_empty() {
                // @end without a matching opener → mismatch error.
                diags.push(Diagnostic {
                    range: line_range(line_idx as u32, line),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("mismatched-end".into())),
                    source: Some("a2ml".into()),
                    message: "@end without a matching @directive: opener".into(),
                    ..Default::default()
                });
            } else {
                stack.pop();
            }
        }
    }

    // Anything left on the stack is unclosed.
    for open in &stack {
        diags.push(Diagnostic {
            range: Range::new(
                Position::new(open.line, 0),
                Position::new(open.line, open.name.len() as u32 + 2),
            ),
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("unclosed-directive".into())),
            source: Some("a2ml".into()),
            message: format!("Unclosed directive @{}: — missing @end", open.name),
            ..Default::default()
        });
    }

    diags
}

/// 4. Invalid `@ref()` targets.
///
/// Collects every ID declared via `id="…"` attributes or headings, then checks
/// that every `@ref(target)` points to a known ID.
fn check_invalid_refs(text: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    // Collect declared IDs.
    //   - Directive attributes: id="some-id"
    //   - Heading-derived slugs are out of scope for now; we only check explicit
    //     `id=` attributes and opaque(id="…") forms.
    let id_re = &*ID_RE;
    let declared_ids: Vec<String> = id_re
        .captures_iter(text)
        // filter_map, not map: a capture that did not participate is skipped
        // rather than panicking the diagnostics pass.
        .filter_map(|cap| cap.get(1).map(|m| m.as_str().to_string()))
        .collect();

    // Find all @ref(…) invocations.
    let ref_re = &*REF_RE;
    for (line_idx, line) in text.lines().enumerate() {
        for cap in ref_re.captures_iter(line) {
            let (Some(t), Some(whole)) = (cap.get(1), cap.get(0)) else {
                continue;
            };
            let target = t.as_str();
            let start_col = whole.start() as u32;
            let end_col = whole.end() as u32;

            if !declared_ids.iter().any(|id| id == target) {
                diags.push(Diagnostic {
                    range: Range::new(
                        Position::new(line_idx as u32, start_col),
                        Position::new(line_idx as u32, end_col),
                    ),
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(NumberOrString::String("unresolved-ref".into())),
                    source: Some("a2ml".into()),
                    message: format!(
                        "Unresolved reference @ref({}) — no id=\"{}\" found in document",
                        target, target
                    ),
                    ..Default::default()
                });
            }
        }
    }

    diags
}

/// 5. Missing required metadata fields in checkpoint-style files.
///
/// If the document contains a `[metadata]` section, we expect to find
/// `project`, `version`, and `status` keys within it.
fn check_required_metadata(text: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    // Find the [metadata] section.
    let mut in_metadata = false;
    let mut metadata_line: u32 = 0;
    let mut found_keys: Vec<String> = Vec::new();

    for (line_idx, line) in text.lines().enumerate() {
        let trimmed = line.trim();

        if trimmed == "[metadata]" {
            in_metadata = true;
            metadata_line = line_idx as u32;
            continue;
        }

        // Another section header ends [metadata].
        if in_metadata && trimmed.starts_with('[') && trimmed.ends_with(']') {
            break;
        }

        if in_metadata {
            // Extract key from `key = …` lines.
            if let Some(eq_pos) = trimmed.find('=') {
                let key = trimmed[..eq_pos].trim().to_string();
                if !key.is_empty() {
                    found_keys.push(key);
                }
            }
        }
    }

    // Only emit diagnostics if [metadata] was found at all.
    if in_metadata {
        for &required in REQUIRED_METADATA_KEYS {
            if !found_keys.iter().any(|k| k == required) {
                diags.push(Diagnostic {
                    range: Range::new(
                        Position::new(metadata_line, 0),
                        Position::new(metadata_line, "[metadata]".len() as u32),
                    ),
                    severity: Some(DiagnosticSeverity::INFORMATION),
                    code: Some(NumberOrString::String("missing-metadata-field".into())),
                    source: Some("a2ml".into()),
                    message: format!(
                        "Missing required metadata field '{}' in [metadata] section",
                        required
                    ),
                    ..Default::default()
                });
            }
        }
    }

    diags
}

/// 6. Duplicate section headers.
///
/// Detects repeated `## Heading` lines at the same level and text.  Only exact
/// duplicates (case-sensitive) are flagged.
fn check_duplicate_sections(text: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    /// Location and content of a heading we have already seen.
    struct SeenHeading {
        text: String,
        line: u32,
    }

    let mut seen: Vec<SeenHeading> = Vec::new();

    let heading_re = &*HEADING_RE;
    for (line_idx, line) in text.lines().enumerate() {
        if let Some(caps) = heading_re.captures(line) {
            let (Some(lvl), Some(ttl)) = (caps.get(1), caps.get(2)) else {
                continue;
            };
            let level = lvl.as_str();
            let title = ttl.as_str().trim();
            let key = format!("{}:{}", level.len(), title);

            if let Some(prev) = seen.iter().find(|s| s.text == key) {
                diags.push(Diagnostic {
                    range: line_range(line_idx as u32, line),
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(NumberOrString::String("duplicate-heading".into())),
                    source: Some("a2ml".into()),
                    message: format!(
                        "Duplicate heading '{}' (first defined on line {})",
                        title,
                        prev.line + 1,
                    ),
                    ..Default::default()
                });
            } else {
                seen.push(SeenHeading {
                    text: key,
                    line: line_idx as u32,
                });
            }
        }
    }

    diags
}

/// 7. Malformed `key = "value"` lines.
///
/// Inside `[section]` blocks we expect either blank lines, comments, or
/// `key = value` pairs.  This check catches unclosed quotes and missing `=`.
fn check_malformed_kv_lines(text: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut in_section = false;

    let section_re = &*SECTION_RE;
    for (line_idx, line) in text.lines().enumerate() {
        let trimmed = line.trim();

        // Track whether we are inside a [section].
        if section_re.is_match(trimmed) {
            in_section = true;
            continue;
        }

        // A heading or directive leaves the section context.
        if trimmed.starts_with('#') || trimmed.starts_with('@') {
            in_section = false;
            continue;
        }

        if !in_section || trimmed.is_empty() || trimmed.starts_with("//") {
            continue;
        }

        // Inside a section: expect `key = value`.
        if !trimmed.contains('=') {
            diags.push(Diagnostic {
                range: line_range(line_idx as u32, line),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("malformed-kv".into())),
                source: Some("a2ml".into()),
                message: "Expected key = value pair inside [section] block".into(),
                ..Default::default()
            });
            continue;
        }

        // Check for unclosed quotes in the value part.
        if let Some(eq_pos) = trimmed.find('=') {
            let value_part = trimmed[eq_pos + 1..].trim();
            if value_part.starts_with('"') && !value_part.ends_with('"') {
                diags.push(Diagnostic {
                    range: line_range(line_idx as u32, line),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("unclosed-quote".into())),
                    source: Some("a2ml".into()),
                    message: "Unclosed double quote in value".into(),
                    ..Default::default()
                });
            } else if value_part.starts_with('\'') && !value_part.ends_with('\'') {
                diags.push(Diagnostic {
                    range: line_range(line_idx as u32, line),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("unclosed-quote".into())),
                    source: Some("a2ml".into()),
                    message: "Unclosed single quote in value".into(),
                    ..Default::default()
                });
            }
        }
    }

    diags
}

/// 8. TOML-like `[section]` validation.
///
/// Emits an info diagnostic when a `[section]` header uses a name not in the
/// known list.  This helps authors discover typos early.
fn check_toml_sections(text: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let section_re = &*SECTION_RE;
    for (line_idx, line) in text.lines().enumerate() {
        let trimmed = line.trim();
        if let Some(caps) = section_re.captures(trimmed) {
            // `continue`, not unwrap: a panic aborts diagnostics for the
            // whole document, so one odd line would blank the editor's problem
            // list instead of producing one fewer entry.
            let Some(name) = caps.get(1).map(|m| m.as_str()) else {
                continue;
            };
            if !KNOWN_SECTIONS.contains(&name) {
                diags.push(Diagnostic {
                    range: line_range(line_idx as u32, line),
                    severity: Some(DiagnosticSeverity::INFORMATION),
                    code: Some(NumberOrString::String("unknown-section".into())),
                    source: Some("a2ml".into()),
                    message: format!(
                        "Unknown section [{}] — known sections: {}",
                        name,
                        KNOWN_SECTIONS.join(", "),
                    ),
                    ..Default::default()
                });
            }
        }
    }

    diags
}

// ── Helpers ──────────────────────────────────────────────────────────

/// Build a `Range` spanning an entire line.
fn line_range(line: u32, text: &str) -> Range {
    Range::new(
        Position::new(line, 0),
        Position::new(line, text.len() as u32),
    )
}

// ── Tests ────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: count diagnostics with a given code.
    fn count_with_code(diags: &[Diagnostic], code: &str) -> usize {
        diags
            .iter()
            .filter(|d| {
                matches!(&d.code, Some(NumberOrString::String(c)) if c == code)
            })
            .count()
    }

    #[test]
    fn spdx_present_no_warning() {
        let text = "// SPDX-License-Identifier: AGPL-3.0-or-later\n# Title\n";
        let diags = run_all_checks(text);
        assert_eq!(count_with_code(&diags, "missing-spdx"), 0);
    }

    #[test]
    fn spdx_missing_warns() {
        let text = "# Title\nSome content\n";
        let diags = run_all_checks(text);
        assert_eq!(count_with_code(&diags, "missing-spdx"), 1);
    }

    #[test]
    fn unclosed_directive_errors() {
        let text = "@abstract:\nSome text\n";
        let diags = run_all_checks(text);
        assert_eq!(count_with_code(&diags, "unclosed-directive"), 1);
    }

    #[test]
    fn matched_directive_no_error() {
        let text = "@abstract:\nSome text\n@end\n";
        let diags = run_all_checks(text);
        assert_eq!(count_with_code(&diags, "unclosed-directive"), 0);
        assert_eq!(count_with_code(&diags, "mismatched-end"), 0);
    }

    #[test]
    fn orphan_end_errors() {
        let text = "Some text\n@end\n";
        let diags = run_all_checks(text);
        assert_eq!(count_with_code(&diags, "mismatched-end"), 1);
    }

    #[test]
    fn unresolved_ref_warns() {
        let text = "See @ref(missing-id) for details.\n";
        let diags = run_all_checks(text);
        assert_eq!(count_with_code(&diags, "unresolved-ref"), 1);
    }

    #[test]
    fn resolved_ref_no_warning() {
        let text = "@opaque(id=\"my-id\"):\ncode\n@end\nSee @ref(my-id).\n";
        let diags = run_all_checks(text);
        assert_eq!(count_with_code(&diags, "unresolved-ref"), 0);
    }

    #[test]
    fn duplicate_heading_warns() {
        let text = "## Overview\nText\n## Overview\nMore text\n";
        let diags = run_all_checks(text);
        assert_eq!(count_with_code(&diags, "duplicate-heading"), 1);
    }

    #[test]
    fn missing_metadata_fields() {
        let text = "[metadata]\nproject = \"test\"\n";
        let diags = run_all_checks(text);
        // version and status are missing.
        assert_eq!(count_with_code(&diags, "missing-metadata-field"), 2);
    }

    #[test]
    fn unknown_section_info() {
        let text = "[banana]\nkey = \"val\"\n";
        let diags = run_all_checks(text);
        assert_eq!(count_with_code(&diags, "unknown-section"), 1);
    }

    #[test]
    fn malformed_kv_missing_equals() {
        let text = "[metadata]\nthis is not a key-value\n";
        let diags = run_all_checks(text);
        assert_eq!(count_with_code(&diags, "malformed-kv"), 1);
    }

    #[test]
    fn unclosed_quote_errors() {
        let text = "[metadata]\nproject = \"unterminated\n";
        let diags = run_all_checks(text);
        assert_eq!(count_with_code(&diags, "unclosed-quote"), 1);
    }
}
