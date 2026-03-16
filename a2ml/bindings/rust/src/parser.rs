// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for crates.io)

//! Line-by-line parser for A2ML documents.
//!
//! The parser processes input one line at a time, accumulating blocks and
//! inline content.  It recognises headings, directives (`@`-prefixed),
//! attestation blocks (`!attest`), fenced code blocks, block quotes,
//! thematic breaks, and paragraphs.

use std::path::Path;

use crate::error::{A2mlError, Result};
use crate::types::*;

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Parse an A2ML document from a string.
///
/// # Errors
///
/// Returns [`A2mlError::ParseError`] if the input contains malformed
/// directives, attestation blocks, or unclosed fenced code blocks.
///
/// # Examples
///
/// ```
/// use a2ml::parser::parse;
///
/// let doc = parse("# Hello\n\nA paragraph.").unwrap();
/// assert_eq!(doc.title.as_deref(), Some("Hello"));
/// assert_eq!(doc.blocks.len(), 2);
/// ```
pub fn parse(input: &str) -> Result<Document> {
    let mut state = ParserState::new();
    let lines: Vec<&str> = input.lines().collect();

    let mut i = 0;
    while i < lines.len() {
        let line = lines[i];
        let line_num = i + 1; // 1-based

        // ----- fenced code block -----
        if state.in_code_block {
            if line.trim_start().starts_with("```") || line.trim_start().starts_with("~~~") {
                state.flush_code_block();
            } else {
                state.code_buf.push_str(line);
                state.code_buf.push('\n');
            }
            i += 1;
            continue;
        }

        let trimmed = line.trim();

        // blank line => flush current paragraph
        if trimmed.is_empty() {
            state.flush_paragraph();
            i += 1;
            continue;
        }

        // fenced code block start
        if trimmed.starts_with("```") || trimmed.starts_with("~~~") {
            state.flush_paragraph();
            let lang = trimmed.trim_start_matches('`').trim_start_matches('~').trim();
            state.code_lang = if lang.is_empty() {
                None
            } else {
                Some(lang.to_string())
            };
            state.in_code_block = true;
            state.code_buf.clear();
            i += 1;
            continue;
        }

        // thematic break (---, ***, ___)
        if is_thematic_break(trimmed) {
            state.flush_paragraph();
            state.blocks.push(Block::ThematicBreak);
            i += 1;
            continue;
        }

        // heading
        if trimmed.starts_with('#') {
            state.flush_paragraph();
            let (level, text) = parse_heading(trimmed);
            let inlines = parse_inlines(text);

            // Capture the first H1 as the document title.
            if level == 1 && state.title.is_none() {
                state.title = Some(text.to_string());
            }

            state.blocks.push(Block::Heading {
                level,
                content: inlines,
            });
            i += 1;
            continue;
        }

        // directive (@name value)
        if trimmed.starts_with('@') {
            state.flush_paragraph();
            let directive = parse_directive(trimmed, line_num)?;
            state.directives.push(directive.clone());
            state.blocks.push(Block::Directive(directive));
            i += 1;
            continue;
        }

        // attestation (!attest ...)
        if trimmed.starts_with("!attest") {
            state.flush_paragraph();
            let attestation = parse_attestation(trimmed, line_num)?;
            state.attestations.push(attestation.clone());
            state.blocks.push(Block::Attestation(attestation));
            i += 1;
            continue;
        }

        // block quote (> ...)
        if trimmed.starts_with('>') {
            state.flush_paragraph();
            let quote_text = trimmed[1..].trim_start();
            let inner_doc = parse(quote_text)?;
            state
                .blocks
                .push(Block::BlockQuote(inner_doc.blocks));
            i += 1;
            continue;
        }

        // list item (- or * or 1.)
        if is_list_start(trimmed) {
            state.flush_paragraph();
            let (ordered, item_text) = parse_list_marker(trimmed);
            let item_inlines = parse_inlines(item_text);
            let item_block = vec![Block::Paragraph(item_inlines)];

            // Try to coalesce with a preceding list of the same type.
            if let Some(Block::List {
                ordered: prev_ordered,
                items,
            }) = state.blocks.last_mut()
            {
                if *prev_ordered == ordered {
                    items.push(item_block);
                    i += 1;
                    continue;
                }
            }

            state.blocks.push(Block::List {
                ordered,
                items: vec![item_block],
            });
            i += 1;
            continue;
        }

        // default: accumulate paragraph text
        state.para_buf.push_str(trimmed);
        state.para_buf.push(' ');
        i += 1;
    }

    // Flush any remaining content.
    if state.in_code_block {
        return Err(A2mlError::parse(
            lines.len(),
            1,
            "unterminated fenced code block",
        ));
    }
    state.flush_paragraph();

    Ok(Document {
        title: state.title,
        directives: state.directives,
        blocks: state.blocks,
        attestations: state.attestations,
    })
}

/// Parse an A2ML document from a file on disk.
///
/// # Errors
///
/// Returns [`A2mlError::Io`] if the file cannot be read, or a parse error
/// if the content is malformed.
pub fn parse_file(path: impl AsRef<Path>) -> Result<Document> {
    let content = std::fs::read_to_string(path)?;
    parse(&content)
}

// ---------------------------------------------------------------------------
// Internal parser state
// ---------------------------------------------------------------------------

/// Accumulator for the line-by-line parser.
struct ParserState {
    title: Option<String>,
    directives: Vec<Directive>,
    attestations: Vec<Attestation>,
    blocks: Vec<Block>,
    para_buf: String,
    in_code_block: bool,
    code_lang: Option<String>,
    code_buf: String,
}

impl ParserState {
    fn new() -> Self {
        Self {
            title: None,
            directives: Vec::new(),
            attestations: Vec::new(),
            blocks: Vec::new(),
            para_buf: String::new(),
            in_code_block: false,
            code_lang: None,
            code_buf: String::new(),
        }
    }

    /// Flush accumulated paragraph text into a `Block::Paragraph`.
    fn flush_paragraph(&mut self) {
        let text = self.para_buf.trim().to_string();
        if !text.is_empty() {
            let inlines = parse_inlines(&text);
            self.blocks.push(Block::Paragraph(inlines));
        }
        self.para_buf.clear();
    }

    /// Flush accumulated code block text into a `Block::CodeBlock`.
    fn flush_code_block(&mut self) {
        // Remove trailing newline if present.
        if self.code_buf.ends_with('\n') {
            self.code_buf.pop();
        }
        self.blocks.push(Block::CodeBlock {
            language: self.code_lang.take(),
            content: std::mem::take(&mut self.code_buf),
        });
        self.in_code_block = false;
    }
}

// ---------------------------------------------------------------------------
// Inline parser
// ---------------------------------------------------------------------------

/// Parse a string into a sequence of inline elements.
///
/// Recognises `**bold**`, `*italic*`, `` `code` ``, and `[text](url)` links.
fn parse_inlines(input: &str) -> Vec<Inline> {
    let mut result = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut text_buf = String::new();

    while i < chars.len() {
        // Inline code: `...`
        if chars[i] == '`' {
            if !text_buf.is_empty() {
                result.push(Inline::Text(std::mem::take(&mut text_buf)));
            }
            i += 1;
            let mut code = String::new();
            while i < chars.len() && chars[i] != '`' {
                code.push(chars[i]);
                i += 1;
            }
            if i < chars.len() {
                i += 1; // skip closing `
            }
            result.push(Inline::Code(code));
            continue;
        }

        // Bold: **...**
        if i + 1 < chars.len() && chars[i] == '*' && chars[i + 1] == '*' {
            if !text_buf.is_empty() {
                result.push(Inline::Text(std::mem::take(&mut text_buf)));
            }
            i += 2;
            let mut inner = String::new();
            while i + 1 < chars.len() && !(chars[i] == '*' && chars[i + 1] == '*') {
                inner.push(chars[i]);
                i += 1;
            }
            if i + 1 < chars.len() {
                i += 2; // skip closing **
            }
            result.push(Inline::Strong(parse_inlines(&inner)));
            continue;
        }

        // Emphasis: *...*
        if chars[i] == '*' {
            if !text_buf.is_empty() {
                result.push(Inline::Text(std::mem::take(&mut text_buf)));
            }
            i += 1;
            let mut inner = String::new();
            while i < chars.len() && chars[i] != '*' {
                inner.push(chars[i]);
                i += 1;
            }
            if i < chars.len() {
                i += 1; // skip closing *
            }
            result.push(Inline::Emphasis(parse_inlines(&inner)));
            continue;
        }

        // Link: [text](url)
        if chars[i] == '[' {
            if !text_buf.is_empty() {
                result.push(Inline::Text(std::mem::take(&mut text_buf)));
            }
            i += 1;
            let mut link_text = String::new();
            while i < chars.len() && chars[i] != ']' {
                link_text.push(chars[i]);
                i += 1;
            }
            if i < chars.len() {
                i += 1; // skip ]
            }
            if i < chars.len() && chars[i] == '(' {
                i += 1;
                let mut url = String::new();
                while i < chars.len() && chars[i] != ')' {
                    url.push(chars[i]);
                    i += 1;
                }
                if i < chars.len() {
                    i += 1; // skip )
                }
                result.push(Inline::Link {
                    content: parse_inlines(&link_text),
                    url,
                });
            } else {
                // Not a valid link — emit as text.
                text_buf.push('[');
                text_buf.push_str(&link_text);
                text_buf.push(']');
            }
            continue;
        }

        text_buf.push(chars[i]);
        i += 1;
    }

    if !text_buf.is_empty() {
        result.push(Inline::Text(text_buf));
    }
    result
}

// ---------------------------------------------------------------------------
// Line-level helpers
// ---------------------------------------------------------------------------

/// Parse a heading line (e.g. `## Foo`) into its level and text content.
fn parse_heading(line: &str) -> (u8, &str) {
    let level = line.chars().take_while(|&c| c == '#').count() as u8;
    let text = line[level as usize..].trim();
    (level, text)
}

/// Parse a directive line (e.g. `@version 1.0`) into a [`Directive`].
fn parse_directive(line: &str, line_num: usize) -> Result<Directive> {
    let without_at = line[1..].trim();
    let (name, value) = match without_at.split_once(char::is_whitespace) {
        Some((n, v)) => (n.trim(), v.trim()),
        None => (without_at, ""),
    };

    if name.is_empty() {
        return Err(A2mlError::parse(line_num, 1, "empty directive name"));
    }

    Ok(Directive::new(name, value))
}

/// Parse an attestation line (e.g. `!attest identity=Alice role=author trust=reviewed`).
fn parse_attestation(line: &str, line_num: usize) -> Result<Attestation> {
    let after_keyword = line.strip_prefix("!attest").unwrap_or(line).trim();
    let mut identity = None;
    let mut role = None;
    let mut trust_level = None;
    let mut timestamp = None;
    let mut note = None;

    for token in after_keyword.split_whitespace() {
        if let Some((key, val)) = token.split_once('=') {
            match key {
                "identity" => identity = Some(val.to_string()),
                "role" => role = Some(val.to_string()),
                "trust" => {
                    trust_level = TrustLevel::from_str(val);
                    if trust_level.is_none() {
                        return Err(A2mlError::parse(
                            line_num,
                            1,
                            format!("unknown trust level: {}", val),
                        ));
                    }
                }
                "timestamp" | "ts" => timestamp = Some(val.to_string()),
                "note" => note = Some(val.to_string()),
                _ => {
                    // Ignore unknown attestation keys for forward compatibility.
                }
            }
        }
    }

    let identity = identity.ok_or_else(|| {
        A2mlError::parse(line_num, 1, "attestation missing required 'identity' field")
    })?;
    let role = role.ok_or_else(|| {
        A2mlError::parse(line_num, 1, "attestation missing required 'role' field")
    })?;
    let trust_level = trust_level.ok_or_else(|| {
        A2mlError::parse(line_num, 1, "attestation missing required 'trust' field")
    })?;

    let mut att = Attestation::new(identity, role, trust_level);
    att.timestamp = timestamp;
    att.note = note;
    Ok(att)
}

/// Check whether a line is a thematic break (`---`, `***`, `___`).
fn is_thematic_break(line: &str) -> bool {
    let trimmed = line.trim();
    if trimmed.len() < 3 {
        return false;
    }
    let first = trimmed.chars().next().unwrap();
    (first == '-' || first == '*' || first == '_') && trimmed.chars().all(|c| c == first)
}

/// Check whether a line starts a list item.
fn is_list_start(line: &str) -> bool {
    line.starts_with("- ")
        || line.starts_with("* ")
        || (line.len() >= 3
            && line.as_bytes()[0].is_ascii_digit()
            && (line.contains(". ") || line.contains(") ")))
}

/// Parse the list marker from a line, returning (ordered, item_text).
fn parse_list_marker(line: &str) -> (bool, &str) {
    if line.starts_with("- ") || line.starts_with("* ") {
        (false, &line[2..])
    } else if let Some(pos) = line.find(". ") {
        let prefix = &line[..pos];
        if prefix.chars().all(|c| c.is_ascii_digit()) {
            (true, &line[pos + 2..])
        } else {
            (false, line)
        }
    } else if let Some(pos) = line.find(") ") {
        let prefix = &line[..pos];
        if prefix.chars().all(|c| c.is_ascii_digit()) {
            (true, &line[pos + 2..])
        } else {
            (false, line)
        }
    } else {
        (false, line)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty_document() {
        let doc = parse("").unwrap();
        assert!(doc.title.is_none());
        assert!(doc.blocks.is_empty());
    }

    #[test]
    fn parse_heading_and_paragraph() {
        let doc = parse("# Title\n\nSome text here.").unwrap();
        assert_eq!(doc.title.as_deref(), Some("Title"));
        assert_eq!(doc.blocks.len(), 2);
    }

    #[test]
    fn parse_directive() {
        let doc = parse("@version 1.0").unwrap();
        assert_eq!(doc.directives.len(), 1);
        assert_eq!(doc.directives[0].name, "version");
        assert_eq!(doc.directives[0].value, "1.0");
    }

    #[test]
    fn parse_attestation_block() {
        let input = "!attest identity=Alice role=author trust=reviewed";
        let doc = parse(input).unwrap();
        assert_eq!(doc.attestations.len(), 1);
        assert_eq!(doc.attestations[0].identity, "Alice");
        assert_eq!(doc.attestations[0].trust_level, TrustLevel::Reviewed);
    }

    #[test]
    fn parse_code_block() {
        let input = "```rust\nfn main() {}\n```";
        let doc = parse(input).unwrap();
        assert_eq!(doc.blocks.len(), 1);
        if let Block::CodeBlock { language, content } = &doc.blocks[0] {
            assert_eq!(language.as_deref(), Some("rust"));
            assert_eq!(content, "fn main() {}");
        } else {
            panic!("expected CodeBlock");
        }
    }

    #[test]
    fn unterminated_code_block_errors() {
        let input = "```\nsome code";
        assert!(parse(input).is_err());
    }

    #[test]
    fn parse_thematic_break() {
        let doc = parse("---").unwrap();
        assert_eq!(doc.blocks.len(), 1);
        assert_eq!(doc.blocks[0], Block::ThematicBreak);
    }

    #[test]
    fn parse_inline_formatting() {
        let inlines = parse_inlines("hello **bold** and *italic* and `code`");
        // Should contain: Text, Strong, Text, Emphasis, Text, Code
        assert!(inlines.len() >= 5);
    }
}
