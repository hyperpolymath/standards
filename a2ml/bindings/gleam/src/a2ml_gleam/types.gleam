// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)
//
// a2ml_gleam/types — Core data types for A2ML documents.
//
// An A2ML document consists of a sequence of Block elements, each of which
// may contain Inline content.  Directive blocks provide machine-readable
// metadata, and Attestation records capture the provenance chain for
// AI-generated or human-reviewed content.

/// A complete A2ML document, containing metadata and a sequence of blocks.
pub type Document {
  Document(
    /// Optional document title (extracted from a leading `# title` line).
    title: Result(String, Nil),
    /// Top-level directives that apply to the whole document.
    directives: List(Directive),
    /// The ordered sequence of content blocks that make up the document body.
    blocks: List(Block),
    /// Attestation chain recording authorship and review provenance.
    attestations: List(Attestation),
  )
}

/// A block-level element in an A2ML document.
///
/// Blocks are separated by blank lines in the source text.
pub type Block {
  /// A heading with a depth (1 = `#`, 2 = `##`, etc.) and inline content.
  Heading(level: Int, content: List(Inline))
  /// A paragraph of inline content.
  Paragraph(content: List(Inline))
  /// A fenced or indented code block with an optional language tag.
  CodeBlock(language: Result(String, Nil), content: String)
  /// A directive block (starts with `@`).
  DirectiveBlock(directive: Directive)
  /// An attestation block (starts with `!attest`).
  AttestationBlock(attestation: Attestation)
  /// A horizontal rule / thematic break.
  ThematicBreak
  /// A block quotation containing nested blocks.
  BlockQuote(children: List(Block))
  /// An unordered or ordered list.
  ListBlock(ordered: Bool, items: List(List(Block)))
}

/// An inline-level element within a block.
pub type Inline {
  /// Plain, unformatted text.
  Text(value: String)
  /// Emphasised text (typically rendered as *italic*).
  Emphasis(children: List(Inline))
  /// Strongly emphasised text (typically rendered as **bold**).
  Strong(children: List(Inline))
  /// Inline code span.
  Code(value: String)
  /// A hyperlink with display content and a target URL.
  Link(content: List(Inline), url: String)
}

/// A machine-readable directive that provides metadata or instructions.
///
/// Directives begin with `@` in the source text, e.g.
/// `@version 1.0` or `@require trust-level:high`.
pub type Directive {
  Directive(
    /// The directive name (the identifier immediately after `@`).
    name: String,
    /// The directive value or argument string.
    value: String,
    /// Optional key-value attributes attached to the directive.
    attributes: List(#(String, String)),
  )
}

/// An attestation record capturing who produced or reviewed content.
///
/// Attestation blocks start with `!attest` and record the identity,
/// role, trust level, and optional timestamp of an author or reviewer.
pub type Attestation {
  Attestation(
    /// The identity of the attester (person, tool, or agent name).
    identity: String,
    /// The role of the attester (e.g. "author", "reviewer", "agent").
    role: String,
    /// The trust level assigned to this attestation.
    trust_level: TrustLevel,
    /// An optional ISO-8601 timestamp for when the attestation was made.
    timestamp: Result(String, Nil),
    /// Optional free-form notes or justification.
    note: Result(String, Nil),
  )
}

/// The degree of trust associated with an attestation.
///
/// Trust levels form a simple ordered scale from unverified content
/// through to formally verified proofs.
pub type TrustLevel {
  /// Content with no verification or review.
  Unverified
  /// Content reviewed by an automated tool or linter.
  Automated
  /// Content reviewed by a human.
  Reviewed
  /// Content that has been formally verified or proven.
  Verified
}

/// A high-level manifest extracted from a parsed A2ML document.
///
/// Collects the directives and attestations into a single structure
/// for convenient programmatic access.
pub type Manifest {
  Manifest(
    /// The document version, if declared via `@version`.
    version: Result(String, Nil),
    /// The document title, if present.
    title: Result(String, Nil),
    /// All directives found in the document.
    directives: List(Directive),
    /// All attestations found in the document.
    attestations: List(Attestation),
  )
}
