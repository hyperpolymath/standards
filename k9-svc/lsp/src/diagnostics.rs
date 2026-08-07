// SPDX-License-Identifier: MPL-2.0
//
// diagnostics.rs — K9 diagnostic checks (linting)
//
// Implements 10 diagnostic rules for K9 Self-Validating Component files:
//
//  1. Missing K9! magic number               → warning  (K9-D001)
//  2. Missing SPDX header                    → warning  (K9-D002)
//  3. Invalid security level                 → error    (K9-D003)
//  4. Security level mismatch                → warning  (K9-D004)
//  5. Missing pedigree fields (name/version) → error    (K9-D005)
//  6. Unclosed Nickel records                → error    (K9-D006)
//  7. Invalid contract annotations           → warning  (K9-D007)
//  8. Non-standard recipe tool references    → info     (K9-D008)
//  9. Hunt without allow_subprocess=true     → warning  (K9-D009)
// 10. Deprecated Kennel YAML syntax          → info     (K9-D010)

use std::sync::LazyLock;
use regex::Regex;
use tower_lsp::lsp_types::*;

// Literal patterns, compiled once.
//
// These were built by calling Regex::new on a literal and unwrapping the
// result, inside the functions below.
// An LSP recompiles those on EVERY request — every keystroke — and `unwrap`
// panics the handler task if a literal is ever malformed. `LazyLock` compiles
// each pattern once, and `expect` states the invariant: the pattern is a
// compile-time constant, so a failure is a programming error, not a runtime
// condition.
static TRUST_LEVEL_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"trust_level\s*=\s*'(\w+)").expect("TRUST_LEVEL_RE is a valid literal pattern"));

static ENUM_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\[\|\s*((?:'[A-Za-z]+\s*,?\s*)*)\|]").expect("ENUM_RE is a valid literal pattern"));

static VARIANT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\|\s+([a-zA-Z_]\w*)").expect("VARIANT_RE is a valid literal pattern"));

static RECIPE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"(install|validate|deploy|migrate|rollback)\s*=\s*"([^"]*)""#).expect("RECIPE_RE is a valid literal pattern"));

static YAML_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^\s*([a-z_]+)\s*:\s+\S").expect("YAML_RE is a valid literal pattern"));


/// Source identifier attached to all K9 diagnostics.
const SOURCE: &str = "k9-lsp";

/// Run all diagnostic checks against the given document text.
///
/// Returns a vector of LSP `Diagnostic` items ready to be published.
pub fn diagnose(text: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let lines: Vec<&str> = text.lines().collect();

    check_magic_number(text, &lines, &mut diags);
    check_spdx_header(text, &lines, &mut diags);
    check_invalid_security_level(text, &lines, &mut diags);
    check_security_level_mismatch(text, &lines, &mut diags);
    check_missing_pedigree_fields(text, &lines, &mut diags);
    check_unclosed_records(text, &lines, &mut diags);
    check_invalid_contract_annotations(text, &lines, &mut diags);
    check_nonstandard_recipe_tools(text, &lines, &mut diags);
    check_hunt_without_subprocess(text, &lines, &mut diags);
    check_deprecated_kennel_yaml(text, &lines, &mut diags);

    diags
}

// ─────────────────────────────────────────────────────────────────────
// D001: Missing K9! magic number
// ─────────────────────────────────────────────────────────────────────

/// Every .k9 file should begin with the `K9!` magic number (possibly after
/// comments/SPDX headers). Warns if it is absent anywhere in the file.
fn check_magic_number(text: &str, _lines: &[&str], diags: &mut Vec<Diagnostic>) {
    if !text.contains("K9!") && !text.contains("magic_number") {
        diags.push(Diagnostic {
            range: Range::new(Position::new(0, 0), Position::new(0, 0)),
            severity: Some(DiagnosticSeverity::WARNING),
            code: Some(NumberOrString::String("K9-D001".to_string())),
            source: Some(SOURCE.to_string()),
            message: "Missing K9! magic number. K9 files should contain the \
                      magic number 'K9!' or reference magic_number in their pedigree."
                .to_string(),
            ..Default::default()
        });
    }
}

// ─────────────────────────────────────────────────────────────────────
// D002: Missing SPDX header
// ─────────────────────────────────────────────────────────────────────

/// Checks that the file contains an SPDX-License-Identifier comment.
fn check_spdx_header(_text: &str, lines: &[&str], diags: &mut Vec<Diagnostic>) {
    // Look in the first 10 lines for an SPDX header.
    let has_spdx = lines
        .iter()
        .take(10)
        .any(|l| l.contains("SPDX-License-Identifier"));

    if !has_spdx {
        diags.push(Diagnostic {
            range: Range::new(Position::new(0, 0), Position::new(0, 0)),
            severity: Some(DiagnosticSeverity::WARNING),
            code: Some(NumberOrString::String("K9-D002".to_string())),
            source: Some(SOURCE.to_string()),
            message: "Missing SPDX-License-Identifier header. Add a comment like: \
                      # SPDX-License-Identifier: AGPL-3.0-or-later"
                .to_string(),
            ..Default::default()
        });
    }
}

// ─────────────────────────────────────────────────────────────────────
// D003: Invalid security level
// ─────────────────────────────────────────────────────────────────────

/// Security levels must be one of 'Kennel, 'Yard, or 'Hunt.
/// Flags any trust_level assignment using an unrecognised value.
fn check_invalid_security_level(text: &str, lines: &[&str], diags: &mut Vec<Diagnostic>) {
    let re = &*TRUST_LEVEL_RE;
    for (i, line) in lines.iter().enumerate() {
        if let Some(caps) = re.captures(line) {
            // One binding, not three lookups; `continue` rather than unwrap so a
            // malformed line costs one diagnostic instead of the whole document.
            let Some(m1) = caps.get(1) else { continue; };
            let level = m1.as_str();
            if !matches!(level, "Kennel" | "Yard" | "Hunt") {
                let start = m1.start() as u32;
                let end = m1.end() as u32;
                diags.push(Diagnostic {
                    range: Range::new(
                        Position::new(i as u32, start),
                        Position::new(i as u32, end),
                    ),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("K9-D003".to_string())),
                    source: Some(SOURCE.to_string()),
                    message: format!(
                        "Invalid security level '{}'. Must be 'Kennel, 'Yard, or 'Hunt.",
                        level
                    ),
                    ..Default::default()
                });
            }
        }
    }
    // Also check the enum-style definition for invalid variants.
    let enum_re = &*ENUM_RE;
    // This intentionally does not flag the SecurityLevel type definition itself,
    // only trust_level assignments.
    let _ = (text, enum_re); // suppress unused warning; reserved for future expansion
}

// ─────────────────────────────────────────────────────────────────────
// D004: Security level mismatch
// ─────────────────────────────────────────────────────────────────────

/// Hunt level requires a signature field. Warn if trust_level is 'Hunt
/// but there is no signature field anywhere in the document.
fn check_security_level_mismatch(text: &str, lines: &[&str], diags: &mut Vec<Diagnostic>) {
    let has_hunt = lines
        .iter()
        .any(|l| l.contains("trust_level") && l.contains("'Hunt"));

    if has_hunt && !text.contains("signature") {
        // Find the line with trust_level = 'Hunt for accurate positioning.
        for (i, line) in lines.iter().enumerate() {
            if line.contains("trust_level") && line.contains("'Hunt") {
                diags.push(Diagnostic {
                    range: Range::new(Position::new(i as u32, 0), Position::new(i as u32, line.len() as u32)),
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(NumberOrString::String("K9-D004".to_string())),
                    source: Some(SOURCE.to_string()),
                    message: "Security level mismatch: 'Hunt requires a 'signature' field \
                              for cryptographic handshake verification."
                        .to_string(),
                    ..Default::default()
                });
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// D005: Missing pedigree fields (name, version required)
// ─────────────────────────────────────────────────────────────────────

/// The `name` and `version` fields are mandatory in a K9 pedigree.
/// Flags if either is missing from the document.
fn check_missing_pedigree_fields(_text: &str, lines: &[&str], diags: &mut Vec<Diagnostic>) {
    let has_name = lines.iter().any(|l| {
        let trimmed = l.trim();
        trimmed.starts_with("name") && trimmed.contains('=')
    });
    let has_version = lines.iter().any(|l| {
        let trimmed = l.trim();
        trimmed.starts_with("version") && trimmed.contains('=')
    });

    // Only flag if the file looks like a pedigree (contains metadata or K9Pedigree).
    let looks_like_pedigree = lines
        .iter()
        .any(|l| l.contains("metadata") || l.contains("K9Pedigree") || l.contains("pedigree"));

    if looks_like_pedigree {
        if !has_name {
            diags.push(Diagnostic {
                range: Range::new(Position::new(0, 0), Position::new(0, 0)),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("K9-D005".to_string())),
                source: Some(SOURCE.to_string()),
                message: "Missing required pedigree field 'name'. Every K9 component \
                          must declare a name in its metadata."
                    .to_string(),
                ..Default::default()
            });
        }
        if !has_version {
            diags.push(Diagnostic {
                range: Range::new(Position::new(0, 0), Position::new(0, 0)),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("K9-D005".to_string())),
                source: Some(SOURCE.to_string()),
                message: "Missing required pedigree field 'version'. Every K9 component \
                          must declare a version in its metadata."
                    .to_string(),
                ..Default::default()
            });
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// D006: Unclosed Nickel records
// ─────────────────────────────────────────────────────────────────────

/// Counts `{` and `}` to detect unclosed records. Ignores braces inside
/// strings and comments.
fn check_unclosed_records(_text: &str, lines: &[&str], diags: &mut Vec<Diagnostic>) {
    let mut depth: i32 = 0;
    let mut open_positions: Vec<(u32, u32)> = Vec::new();

    for (i, line) in lines.iter().enumerate() {
        // Skip comment lines entirely.
        let trimmed = line.trim();
        if trimmed.starts_with('#') {
            continue;
        }

        let mut in_string = false;
        for (j, ch) in line.chars().enumerate() {
            match ch {
                '"' => in_string = !in_string,
                '#' if !in_string => break, // rest of line is a comment
                '{' if !in_string => {
                    depth += 1;
                    open_positions.push((i as u32, j as u32));
                }
                '}' if !in_string => {
                    depth -= 1;
                    if depth >= 0 {
                        open_positions.pop();
                    }
                }
                _ => {}
            }
        }
    }

    if depth > 0 {
        // Report unclosed brace(s) at the position of the last unmatched '{'.
        for &(line_num, col) in &open_positions {
            diags.push(Diagnostic {
                range: Range::new(
                    Position::new(line_num, col),
                    Position::new(line_num, col + 1),
                ),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("K9-D006".to_string())),
                source: Some(SOURCE.to_string()),
                message: "Unclosed Nickel record: '{' without matching '}'.".to_string(),
                ..Default::default()
            });
        }
    } else if depth < 0 {
        // More closing braces than opening — report on the last line.
        let last_line = if lines.is_empty() { 0 } else { (lines.len() - 1) as u32 };
        diags.push(Diagnostic {
            range: Range::new(Position::new(last_line, 0), Position::new(last_line, 0)),
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String("K9-D006".to_string())),
            source: Some(SOURCE.to_string()),
            message: "Extra closing '}' without matching '{'.".to_string(),
            ..Default::default()
        });
    }
}

// ─────────────────────────────────────────────────────────────────────
// D007: Invalid contract annotations
// ─────────────────────────────────────────────────────────────────────

/// Nickel contract annotations use `| Type` syntax. Flags `|` followed
/// by something that does not look like a valid type (e.g. starts with
/// a lowercase letter that is not a known built-in).
fn check_invalid_contract_annotations(_text: &str, lines: &[&str], diags: &mut Vec<Diagnostic>) {
    // Known Nickel built-in contract types and K9-specific types.
    let known_types = [
        "String", "Number", "Bool", "Array", "Dyn",
        "Metadata", "Target", "Security", "Validation", "Recipes",
        "K9Pedigree", "SecurityLevel", "Architecture",
        "default", "optional", "force", "priority", "doc",
    ];

    let re = &*VARIANT_RE;
    for (i, line) in lines.iter().enumerate() {
        // Skip comment lines.
        if line.trim().starts_with('#') {
            continue;
        }
        // Skip lines that look like enum definitions [| ... |].
        if line.contains("[|") || line.contains("|]") {
            continue;
        }

        for caps in re.captures_iter(line) {
            // One binding, not three lookups; `continue` rather than unwrap so a
            // malformed line costs one diagnostic instead of the whole document.
            let Some(m1) = caps.get(1) else { continue; };
            let type_name = m1.as_str();
            // Valid if it starts with uppercase (user-defined type) or is a known name.
            let is_valid = type_name.chars().next().is_some_and(|c| c.is_uppercase())
                || known_types.contains(&type_name);

            if !is_valid {
                let start = m1.start() as u32;
                let end = m1.end() as u32;
                diags.push(Diagnostic {
                    range: Range::new(
                        Position::new(i as u32, start),
                        Position::new(i as u32, end),
                    ),
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(NumberOrString::String("K9-D007".to_string())),
                    source: Some(SOURCE.to_string()),
                    message: format!(
                        "Possibly invalid contract annotation '| {}'. Expected a type \
                         name (e.g. String, Bool) or contract (e.g. default, optional).",
                        type_name
                    ),
                    ..Default::default()
                });
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// D008: Non-standard recipe tool references
// ─────────────────────────────────────────────────────────────────────

/// Recipe fields (install, validate, deploy, migrate) should reference
/// standard K9 tools (just, nickel, podman, echo, etc.). Flags references
/// to non-standard executables as informational.
fn check_nonstandard_recipe_tools(_text: &str, lines: &[&str], diags: &mut Vec<Diagnostic>) {
    let recipe_re = &*RECIPE_RE;
    let standard_tools = [
        "just", "nickel", "podman", "echo", "sh", "bash", "curl", "wget",
        "tar", "cp", "mv", "rm", "mkdir", "chmod", "chown", "cat", "grep",
        "sed", "awk", "test", "true", "false", "exit", "command",
    ];

    for (i, line) in lines.iter().enumerate() {
        if let Some(caps) = recipe_re.captures(line) {
            // One binding, not three lookups; `continue` rather than unwrap so a
            // malformed line costs one diagnostic instead of the whole document.
            let Some(m2) = caps.get(2) else { continue; };
            let command_str = m2.as_str();
            // Extract the first word (the tool being invoked).
            let first_word = command_str.split_whitespace().next().unwrap_or("");
            // Strip any leading path.
            let tool = first_word.rsplit('/').next().unwrap_or(first_word);

            if !tool.is_empty() && !standard_tools.contains(&tool) {
                let start = m2.start() as u32;
                let end = m2.end() as u32;
                diags.push(Diagnostic {
                    range: Range::new(
                        Position::new(i as u32, start),
                        Position::new(i as u32, end),
                    ),
                    severity: Some(DiagnosticSeverity::INFORMATION),
                    code: Some(NumberOrString::String("K9-D008".to_string())),
                    source: Some(SOURCE.to_string()),
                    message: format!(
                        "Recipe references non-standard tool '{}'. Standard tools include: \
                         just, nickel, podman, echo, sh.",
                        tool
                    ),
                    ..Default::default()
                });
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// D009: Hunt-level without allow_subprocess=true
// ─────────────────────────────────────────────────────────────────────

/// 'Hunt level grants full triad execution, which typically requires
/// subprocess spawning. Warns if trust_level is 'Hunt but
/// allow_subprocess is not explicitly set to true.
fn check_hunt_without_subprocess(text: &str, lines: &[&str], diags: &mut Vec<Diagnostic>) {
    let has_hunt = lines
        .iter()
        .any(|l| l.contains("trust_level") && l.contains("'Hunt"));

    let has_subprocess_true = text.contains("allow_subprocess = true")
        || text.contains("allow_subprocess= true")
        || text.contains("allow_subprocess =true")
        || text.contains("allow_subprocess=true");

    if has_hunt && !has_subprocess_true {
        for (i, line) in lines.iter().enumerate() {
            if line.contains("trust_level") && line.contains("'Hunt") {
                diags.push(Diagnostic {
                    range: Range::new(
                        Position::new(i as u32, 0),
                        Position::new(i as u32, line.len() as u32),
                    ),
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(NumberOrString::String("K9-D009".to_string())),
                    source: Some(SOURCE.to_string()),
                    message: "Inconsistent security: 'Hunt level typically requires \
                              allow_subprocess = true for full triad execution."
                        .to_string(),
                    ..Default::default()
                });
                break;
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// D010: Deprecated Kennel YAML syntax suggestion
// ─────────────────────────────────────────────────────────────────────

/// Detects YAML-like syntax (key: value instead of key = value) which
/// suggests the author may be using deprecated Kennel YAML style.
fn check_deprecated_kennel_yaml(_text: &str, lines: &[&str], diags: &mut Vec<Diagnostic>) {
    // Match lines that look like YAML key-value pairs (word: value)
    // but are NOT inside strings and NOT comment lines and NOT Nickel
    // record type annotations.
    let yaml_re = &*YAML_RE;
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        // Skip comments, empty lines, and AsciiDoc-style headers.
        if trimmed.is_empty() || trimmed.starts_with('#') || trimmed.starts_with('=') {
            continue;
        }
        // Skip lines inside strings (heuristic: skip if line has quotes before colon).
        if trimmed.starts_with('"') {
            continue;
        }

        if yaml_re.is_match(line) {
            let col_start = line.find(':').unwrap_or(0) as u32;
            diags.push(Diagnostic {
                range: Range::new(
                    Position::new(i as u32, col_start),
                    Position::new(i as u32, col_start + 1),
                ),
                severity: Some(DiagnosticSeverity::INFORMATION),
                code: Some(NumberOrString::String("K9-D010".to_string())),
                source: Some(SOURCE.to_string()),
                message: "Possible deprecated Kennel YAML syntax. K9 uses Nickel syntax: \
                          use '=' for assignment instead of ':'."
                    .to_string(),
                ..Default::default()
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_missing_magic_number() {
        let text = "# just a comment\nname = \"test\"\n";
        let diags = diagnose(text);
        assert!(diags.iter().any(|d| d.code == Some(NumberOrString::String("K9-D001".to_string()))));
    }

    #[test]
    fn test_has_magic_number_via_field() {
        let text = "# SPDX-License-Identifier: AGPL-3.0-or-later\nmagic_number = \"K9!\"\n";
        let diags = diagnose(text);
        assert!(!diags.iter().any(|d| d.code == Some(NumberOrString::String("K9-D001".to_string()))));
    }

    #[test]
    fn test_missing_spdx() {
        let text = "K9!\nname = \"test\"\n";
        let diags = diagnose(text);
        assert!(diags.iter().any(|d| d.code == Some(NumberOrString::String("K9-D002".to_string()))));
    }

    #[test]
    fn test_invalid_security_level() {
        let text = "trust_level = 'Cage\n";
        let diags = diagnose(text);
        assert!(diags.iter().any(|d| d.code == Some(NumberOrString::String("K9-D003".to_string()))));
    }

    #[test]
    fn test_valid_security_level() {
        let text = "trust_level = 'Yard\n";
        let diags = diagnose(text);
        assert!(!diags.iter().any(|d| d.code == Some(NumberOrString::String("K9-D003".to_string()))));
    }

    #[test]
    fn test_hunt_without_signature() {
        let text = "trust_level = 'Hunt\nallow_subprocess = true\n";
        let diags = diagnose(text);
        assert!(diags.iter().any(|d| d.code == Some(NumberOrString::String("K9-D004".to_string()))));
    }

    #[test]
    fn test_unclosed_record() {
        let text = "metadata = {\n  name = \"test\"\n";
        let diags = diagnose(text);
        assert!(diags.iter().any(|d| d.code == Some(NumberOrString::String("K9-D006".to_string()))));
    }

    #[test]
    fn test_balanced_records() {
        let text = "metadata = {\n  name = \"test\"\n}\n";
        let diags = diagnose(text);
        assert!(!diags.iter().any(|d| d.code == Some(NumberOrString::String("K9-D006".to_string()))));
    }

    #[test]
    fn test_hunt_without_subprocess() {
        let text = "trust_level = 'Hunt\nsignature = \"abc\"\nallow_subprocess = false\n";
        let diags = diagnose(text);
        assert!(diags.iter().any(|d| d.code == Some(NumberOrString::String("K9-D009".to_string()))));
    }
}
