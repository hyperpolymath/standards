// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)
//
// a2ml_gleam/renderer — Render an A2ML Document AST back to A2ML text.
//
// Produces A2ML-formatted output from a parsed Document structure,
// suitable for round-tripping through parse -> modify -> render.

import gleam/int
import gleam/list
import gleam/string

import a2ml_gleam/types.{
  type Attestation, type Block, type Document, type Inline, type TrustLevel,
  AttestationBlock, Automated, BlockQuote, CodeBlock, DirectiveBlock, Emphasis,
  Heading, Link, ListBlock, Paragraph, Reviewed, Strong, Text, ThematicBreak,
  Unverified, Verified,
}

/// Render a Document to A2ML-formatted text.
///
/// Produces a complete document string with title, blocks, and
/// appropriate blank-line separation.
pub fn render(doc: Document) -> String {
  let parts = []

  // Render title if present.
  let parts = case doc.title {
    Ok(title) -> list.append(parts, ["# " <> title, ""])
    Error(_) -> parts
  }

  // Render each block.
  let block_strings =
    doc.blocks
    |> list.map(render_block)
    |> list.intersperse("")

  let parts = list.append(parts, block_strings)

  string.join(parts, "\n")
  |> string.trim
  |> fn(s) { s <> "\n" }
}

/// Render a single Block to A2ML text.
pub fn render_block(block: Block) -> String {
  case block {
    Heading(level: level, content: inlines) -> {
      let hashes = string.repeat("#", level)
      hashes <> " " <> render_inlines(inlines)
    }

    Paragraph(content: inlines) -> render_inlines(inlines)

    CodeBlock(language: lang, content: content) -> {
      let opener = case lang {
        Ok(l) -> "```" <> l
        Error(_) -> "```"
      }
      opener <> "\n" <> content <> "\n```"
    }

    DirectiveBlock(directive: d) -> {
      let base = "@" <> d.name <> " " <> d.value
      case d.attributes {
        [] -> base
        attrs -> {
          let attr_strs =
            attrs
            |> list.map(fn(pair) { pair.0 <> "=" <> pair.1 })
          base <> " [" <> string.join(attr_strs, ", ") <> "]"
        }
      }
    }

    AttestationBlock(attestation: a) -> render_attestation(a)

    ThematicBreak -> "---"

    BlockQuote(children: blocks) -> {
      blocks
      |> list.map(fn(b) {
        render_block(b)
        |> string.split("\n")
        |> list.map(fn(line) { "> " <> line })
        |> string.join("\n")
      })
      |> string.join("\n>\n")
    }

    ListBlock(ordered: ordered, items: items) -> {
      items
      |> list.index_map(fn(item, idx) {
        let prefix = case ordered {
          True -> int.to_string(idx + 1) <> ". "
          False -> "- "
        }
        let item_text =
          item
          |> list.map(render_block)
          |> string.join("\n")
        prefix <> item_text
      })
      |> string.join("\n")
    }
  }
}

/// Render an Attestation to its A2ML block format.
pub fn render_attestation(attestation: Attestation) -> String {
  let lines = [
    "!attest",
    "  identity: " <> attestation.identity,
    "  role: " <> attestation.role,
    "  trust-level: " <> render_trust_level(attestation.trust_level),
  ]

  let lines = case attestation.timestamp {
    Ok(ts) -> list.append(lines, ["  timestamp: " <> ts])
    Error(_) -> lines
  }

  let lines = case attestation.note {
    Ok(n) -> list.append(lines, ["  note: " <> n])
    Error(_) -> lines
  }

  string.join(lines, "\n")
}

/// Render a TrustLevel to its canonical string representation.
pub fn render_trust_level(level: TrustLevel) -> String {
  case level {
    Unverified -> "unverified"
    Automated -> "automated"
    Reviewed -> "reviewed"
    Verified -> "verified"
  }
}

/// Render a list of Inline elements to text.
pub fn render_inlines(inlines: List(Inline)) -> String {
  inlines
  |> list.map(render_inline)
  |> string.join("")
}

/// Render a single Inline element to text.
fn render_inline(inline: Inline) -> String {
  case inline {
    Text(value: v) -> v
    Emphasis(children: c) -> "*" <> render_inlines(c) <> "*"
    Strong(children: c) -> "**" <> render_inlines(c) <> "**"
    types.Code(value: v) -> "`" <> v <> "`"
    Link(content: c, url: u) -> "[" <> render_inlines(c) <> "](" <> u <> ")"
  }
}

/// Convenience: render a Document to a String, ignoring the Result wrapper
/// from render (which always succeeds).
pub fn to_string(doc: Document) -> String {
  render(doc)
}
