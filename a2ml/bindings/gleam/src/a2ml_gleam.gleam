// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)
//
// a2ml_gleam — A2ML (AI Attestation Markup Language) parser and renderer.
//
// Provides types, parsing, and rendering for A2ML documents — a lightweight
// markup format designed for AI-generated content with provenance tracking
// through attestation chains and trust levels.
//
// ## Example
//
// ```gleam
// import a2ml_gleam
// import a2ml_gleam/parser
// import a2ml_gleam/renderer
//
// let input = "# My Document\n\n@version 1.0\n\nHello, A2ML!"
// let assert Ok(doc) = parser.parse(input)
// let output = renderer.render(doc)
// ```

import a2ml_gleam/parser
import a2ml_gleam/renderer
import a2ml_gleam/types.{type Document}

/// Parse an A2ML-formatted string into a Document.
///
/// Re-exports `parser.parse` for convenience.
pub fn parse(input: String) -> Result(Document, parser.ParseError) {
  parser.parse(input)
}

/// Render a Document back to A2ML-formatted text.
///
/// Re-exports `renderer.render` for convenience.
pub fn render(doc: Document) -> String {
  renderer.render(doc)
}
