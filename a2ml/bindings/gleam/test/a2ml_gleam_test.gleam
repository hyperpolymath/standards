// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

import a2ml_gleam/parser
import a2ml_gleam/renderer
import a2ml_gleam/types.{
  Attestation, AttestationBlock, Automated, DirectiveBlock, Directive,
  Heading, Paragraph, Reviewed, Text, Unverified, Verified,
}
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

// ---------------------------------------------------------------------------
// Parser tests
// ---------------------------------------------------------------------------

pub fn parse_empty_input_test() {
  let result = parser.parse("")
  assert result == Error(parser.EmptyInput)
}

pub fn parse_title_only_test() {
  let assert Ok(doc) = parser.parse("# Hello A2ML")
  assert doc.title == Ok("Hello A2ML")
}

pub fn parse_heading_levels_test() {
  let assert Ok(doc) = parser.parse("## Second Level\n\n### Third Level")
  let assert [Heading(level: 2, content: _), Heading(level: 3, content: _)] =
    doc.blocks
}

pub fn parse_paragraph_test() {
  let assert Ok(doc) = parser.parse("This is a simple paragraph.")
  let assert [Paragraph(content: [Text(value: "This is a simple paragraph.")])] =
    doc.blocks
}

pub fn parse_directive_test() {
  let assert Ok(doc) = parser.parse("@version 1.0")
  let assert [DirectiveBlock(directive: Directive(name: "version", value: "1.0", attributes: []))] =
    doc.blocks
  let assert [Directive(name: "version", value: "1.0", attributes: [])] =
    doc.directives
}

pub fn parse_trust_level_test() {
  let assert Ok(Unverified) = parser.parse_trust_level("unverified")
  let assert Ok(Automated) = parser.parse_trust_level("Automated")
  let assert Ok(Reviewed) = parser.parse_trust_level("REVIEWED")
  let assert Ok(Verified) = parser.parse_trust_level("verified")
  let assert Error(parser.UnknownTrustLevel("unknown")) =
    parser.parse_trust_level("unknown")
}

pub fn parse_attestation_test() {
  let input =
    "!attest\n  identity: Alice\n  role: reviewer\n  trust-level: reviewed"
  let assert Ok(doc) = parser.parse(input)
  let assert [AttestationBlock(attestation: a)] = doc.blocks
  assert a.identity == "Alice"
  assert a.role == "reviewer"
  assert a.trust_level == Reviewed
}

pub fn parse_full_document_test() {
  let input =
    "# Test Doc\n\n@version 2.0\n\nA paragraph here.\n\n!attest\n  identity: Bob\n  role: author\n  trust-level: verified"
  let assert Ok(doc) = parser.parse(input)
  assert doc.title == Ok("Test Doc")
  let assert [Directive(name: "version", ..)] = doc.directives
  let assert [Attestation(identity: "Bob", ..)] = doc.attestations
}

// ---------------------------------------------------------------------------
// Renderer tests
// ---------------------------------------------------------------------------

pub fn render_trust_level_test() {
  assert renderer.render_trust_level(Unverified) == "unverified"
  assert renderer.render_trust_level(Automated) == "automated"
  assert renderer.render_trust_level(Reviewed) == "reviewed"
  assert renderer.render_trust_level(Verified) == "verified"
}

pub fn render_attestation_test() {
  let a =
    Attestation(
      identity: "Claude",
      role: "agent",
      trust_level: Automated,
      timestamp: Ok("2026-03-16T12:00:00Z"),
      note: Error(Nil),
    )
  let rendered = renderer.render_attestation(a)
  assert rendered
    == "!attest\n  identity: Claude\n  role: agent\n  trust-level: automated\n  timestamp: 2026-03-16T12:00:00Z"
}

pub fn render_roundtrip_test() {
  let input = "# Round Trip\n\n@version 1.0\n\nHello, world!"
  let assert Ok(doc) = parser.parse(input)
  let output = renderer.render(doc)
  // Re-parse the output to verify structural equivalence.
  let assert Ok(doc2) = parser.parse(output)
  assert doc.title == doc2.title
}

// ---------------------------------------------------------------------------
// Manifest tests
// ---------------------------------------------------------------------------

pub fn extract_manifest_test() {
  let input = "# Manifest Test\n\n@version 3.0\n\n@author Alice"
  let assert Ok(doc) = parser.parse(input)
  let manifest = parser.extract_manifest(doc)
  assert manifest.version == Ok("3.0")
  assert manifest.title == Ok("Manifest Test")
}
