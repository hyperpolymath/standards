// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for crates.io)

//! Renderer that serialises A2ML data types back to A2ML text format.
//!
//! The [`render`] function takes a [`Document`] and produces a string in the
//! canonical A2ML text representation, suitable for writing to a `.a2ml` file.

use crate::error::Result;
use crate::types::*;

/// Render a [`Document`] to its canonical A2ML text representation.
///
/// # Errors
///
/// Returns [`A2mlError::RenderError`] if any element cannot be serialised
/// (currently infallible, but the signature allows future extension).
///
/// # Examples
///
/// ```
/// use a2ml::types::{Document, Block, Inline};
/// use a2ml::renderer::render;
///
/// let mut doc = Document::new();
/// doc.blocks.push(Block::Heading {
///     level: 1,
///     content: vec![Inline::Text("Hello".into())],
/// });
/// let output = render(&doc).unwrap();
/// assert!(output.contains("# Hello"));
/// ```
pub fn render(doc: &Document) -> Result<String> {
    let mut out = String::new();

    // Render top-level directives first.
    for directive in &doc.directives {
        render_directive(directive, &mut out);
        out.push('\n');
    }

    // Separate directives from body with a blank line if both are present.
    if !doc.directives.is_empty() && !doc.blocks.is_empty() {
        out.push('\n');
    }

    // Render body blocks.
    let block_count = doc.blocks.len();
    for (i, block) in doc.blocks.iter().enumerate() {
        render_block(block, &mut out);
        // Add a blank line between blocks (but not after the last).
        if i + 1 < block_count {
            out.push('\n');
        }
    }

    // Render trailing attestations that are not already inline.
    if !doc.attestations.is_empty() {
        if !out.is_empty() && !out.ends_with("\n\n") {
            out.push('\n');
        }
        for att in &doc.attestations {
            render_attestation(att, &mut out);
            out.push('\n');
        }
    }

    Ok(out)
}

// ---------------------------------------------------------------------------
// Block rendering
// ---------------------------------------------------------------------------

/// Render a single block element to the output buffer.
fn render_block(block: &Block, out: &mut String) {
    match block {
        Block::Heading { level, content } => {
            for _ in 0..*level {
                out.push('#');
            }
            out.push(' ');
            render_inlines(content, out);
            out.push('\n');
        }

        Block::Paragraph(inlines) => {
            render_inlines(inlines, out);
            out.push('\n');
        }

        Block::CodeBlock { language, content } => {
            out.push_str("```");
            if let Some(lang) = language {
                out.push_str(lang);
            }
            out.push('\n');
            out.push_str(content);
            out.push_str("\n```\n");
        }

        Block::Directive(d) => {
            render_directive(d, out);
            out.push('\n');
        }

        Block::Attestation(a) => {
            render_attestation(a, out);
            out.push('\n');
        }

        Block::ThematicBreak => {
            out.push_str("---\n");
        }

        Block::BlockQuote(inner) => {
            for inner_block in inner {
                out.push_str("> ");
                let mut buf = String::new();
                render_block(inner_block, &mut buf);
                // Indent continuation lines of multi-line blocks.
                for (i, line) in buf.lines().enumerate() {
                    if i > 0 {
                        out.push_str("> ");
                    }
                    out.push_str(line);
                    out.push('\n');
                }
            }
        }

        Block::List { ordered, items } => {
            for (idx, item) in items.iter().enumerate() {
                if *ordered {
                    out.push_str(&format!("{}. ", idx + 1));
                } else {
                    out.push_str("- ");
                }
                for (j, block) in item.iter().enumerate() {
                    let mut buf = String::new();
                    render_block(block, &mut buf);
                    if j == 0 {
                        // First block shares the line with the marker.
                        out.push_str(buf.trim_end());
                        out.push('\n');
                    } else {
                        // Subsequent blocks are indented.
                        for line in buf.lines() {
                            out.push_str("  ");
                            out.push_str(line);
                            out.push('\n');
                        }
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Inline rendering
// ---------------------------------------------------------------------------

/// Render a sequence of inline elements to the output buffer.
fn render_inlines(inlines: &[Inline], out: &mut String) {
    for inline in inlines {
        match inline {
            Inline::Text(t) => out.push_str(t),
            Inline::Emphasis(inner) => {
                out.push('*');
                render_inlines(inner, out);
                out.push('*');
            }
            Inline::Strong(inner) => {
                out.push_str("**");
                render_inlines(inner, out);
                out.push_str("**");
            }
            Inline::Code(c) => {
                out.push('`');
                out.push_str(c);
                out.push('`');
            }
            Inline::Link { content, url } => {
                out.push('[');
                render_inlines(content, out);
                out.push_str("](");
                out.push_str(url);
                out.push(')');
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Directive and attestation rendering
// ---------------------------------------------------------------------------

/// Render a directive to the output buffer (without trailing newline).
fn render_directive(d: &Directive, out: &mut String) {
    out.push('@');
    out.push_str(&d.name);
    if !d.value.is_empty() {
        out.push(' ');
        out.push_str(&d.value);
    }
    for (k, v) in &d.attributes {
        out.push(' ');
        out.push_str(k);
        out.push('=');
        out.push_str(v);
    }
}

/// Render an attestation to the output buffer (without trailing newline).
fn render_attestation(a: &Attestation, out: &mut String) {
    out.push_str("!attest");
    out.push_str(&format!(" identity={}", a.identity));
    out.push_str(&format!(" role={}", a.role));
    out.push_str(&format!(" trust={}", a.trust_level));
    if let Some(ts) = &a.timestamp {
        out.push_str(&format!(" timestamp={}", ts));
    }
    if let Some(note) = &a.note {
        out.push_str(&format!(" note={}", note));
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_heading() {
        let mut doc = Document::new();
        doc.blocks.push(Block::Heading {
            level: 2,
            content: vec![Inline::Text("Test".into())],
        });
        let rendered = render(&doc).unwrap();
        assert!(rendered.contains("## Test"));
    }

    #[test]
    fn round_trip_directive() {
        let mut doc = Document::new();
        doc.directives.push(Directive::new("version", "1.0"));
        doc.blocks
            .push(Block::Directive(Directive::new("version", "1.0")));
        let rendered = render(&doc).unwrap();
        assert!(rendered.contains("@version 1.0"));
    }

    #[test]
    fn round_trip_code_block() {
        let mut doc = Document::new();
        doc.blocks.push(Block::CodeBlock {
            language: Some("rust".into()),
            content: "fn main() {}".into(),
        });
        let rendered = render(&doc).unwrap();
        assert!(rendered.contains("```rust"));
        assert!(rendered.contains("fn main() {}"));
    }
}
