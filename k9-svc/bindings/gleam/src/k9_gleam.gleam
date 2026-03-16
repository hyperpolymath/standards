// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)
//
// k9_gleam — K9 (Self-Validating Components) parser and renderer.
//
// Provides types, parsing, and rendering for K9 component specifications.
// K9 is a format for self-validating software components with built-in
// security levels (Kennel/Yard/Hunt), pedigree metadata, and lifecycle
// recipes.
//
// ## Example
//
// ```gleam
// import k9_gleam
// import k9_gleam/parser
// import k9_gleam/renderer
//
// let input = "pedigree:\n  name: hello-k9\n  version: 1.0.0\n  description: A greeting\n\nsecurity:\n  level: kennel"
// let assert Ok(component) = parser.parse(input)
// let output = renderer.render(component)
// ```

import k9_gleam/parser
import k9_gleam/renderer
import k9_gleam/types.{type Component}

/// Parse a .k9 file string into a Component.
///
/// Re-exports `parser.parse` for convenience.
pub fn parse(input: String) -> Result(Component, parser.ParseError) {
  parser.parse(input)
}

/// Render a Component back to .k9 format text.
///
/// Re-exports `renderer.render` for convenience.
pub fn render(component: Component) -> String {
  renderer.render(component)
}
