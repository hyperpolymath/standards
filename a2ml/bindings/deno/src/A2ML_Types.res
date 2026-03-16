// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// A2ML_Types — Core data types for A2ML (Attested Markup Language) documents.
//
// Defines the abstract syntax tree for A2ML documents including document
// structure, block-level elements, inline formatting, directives, and
// attestation provenance records with trust levels.

// ---------------------------------------------------------------------------
// Trust levels
// ---------------------------------------------------------------------------

/// The degree of trust associated with an attestation.
/// Forms an ordered scale from unverified content through to formally verified.
type trustLevel =
  | Unverified
  | Automated
  | Reviewed
  | Verified

/// Parse a trust level from its canonical string representation.
/// Recognised values (case-insensitive): "unverified", "automated",
/// "reviewed", "verified".
let trustLevelFromString = (s: string): option<trustLevel> => {
  switch s->String.toLowerCase {
  | "unverified" => Some(Unverified)
  | "automated" => Some(Automated)
  | "reviewed" => Some(Reviewed)
  | "verified" => Some(Verified)
  | _ => None
  }
}

/// Return the canonical string representation of a trust level.
let trustLevelToString = (level: trustLevel): string => {
  switch level {
  | Unverified => "unverified"
  | Automated => "automated"
  | Reviewed => "reviewed"
  | Verified => "verified"
  }
}

// ---------------------------------------------------------------------------
// Inline-level elements
// ---------------------------------------------------------------------------

/// An inline-level element within a block.
type rec inline =
  | Text(string)
  | Emphasis(array<inline>)
  | Strong(array<inline>)
  | Code(string)
  | Link({content: array<inline>, url: string})
  | InlineRef(string)

// ---------------------------------------------------------------------------
// Directives
// ---------------------------------------------------------------------------

/// A machine-readable directive that provides metadata or instructions.
/// Directives begin with `@` in the source text, e.g.
/// `@version 1.0` or `@require trust-level:high`.
type directive = {
  name: string,
  value: string,
  attributes: array<(string, string)>,
}

/// Create a simple directive with a name and value, and no attributes.
let makeDirective = (name: string, value: string): directive => {
  name,
  value,
  attributes: [],
}

// ---------------------------------------------------------------------------
// Attestations
// ---------------------------------------------------------------------------

/// An attestation record capturing who produced or reviewed content.
/// Attestation blocks start with `!attest` and record identity,
/// role, trust level, and optional timestamp of an author or reviewer.
type attestation = {
  identity: string,
  role: string,
  trustLevel: trustLevel,
  timestamp: option<string>,
  note: option<string>,
}

/// Create a new attestation with the minimum required fields.
let makeAttestation = (
  ~identity: string,
  ~role: string,
  ~trustLevel: trustLevel,
): attestation => {
  identity,
  role,
  trustLevel,
  timestamp: None,
  note: None,
}

// ---------------------------------------------------------------------------
// Block-level elements
// ---------------------------------------------------------------------------

/// A block-level element in an A2ML document.
/// Blocks are separated by blank lines in the source text.
type rec block =
  | Heading({level: int, content: array<inline>})
  | Paragraph(array<inline>)
  | CodeBlock({language: option<string>, content: string})
  | DirectiveBlock(directive)
  | AttestationBlock(attestation)
  | ThematicBreak
  | BlockQuote(array<block>)
  | BulletList(array<array<inline>>)
  | BlankLine

// ---------------------------------------------------------------------------
// Top-level document
// ---------------------------------------------------------------------------

/// A complete A2ML document, containing metadata and a sequence of blocks.
type document = {
  title: option<string>,
  directives: array<directive>,
  blocks: array<block>,
  attestations: array<attestation>,
}

/// Create a new, empty document with no title or content.
let emptyDocument = (): document => {
  title: None,
  directives: [],
  blocks: [],
  attestations: [],
}

// ---------------------------------------------------------------------------
// Manifest (convenience aggregate)
// ---------------------------------------------------------------------------

/// A high-level manifest extracted from a parsed A2ML document.
/// Collects directives and attestations for convenient programmatic access.
type manifest = {
  version: option<string>,
  title: option<string>,
  directives: array<directive>,
  attestations: array<attestation>,
}

/// Extract a manifest from a parsed document.
let manifestFromDocument = (doc: document): manifest => {
  let version =
    doc.directives
    ->Array.find(d => d.name == "version")
    ->Option.map(d => d.value)

  {
    version,
    title: doc.title,
    directives: doc.directives,
    attestations: doc.attestations,
  }
}

// ---------------------------------------------------------------------------
// Parse errors
// ---------------------------------------------------------------------------

/// Errors that can occur during A2ML parsing.
type parseError =
  | UnterminatedDirective({line: int, name: string})
  | InvalidHeadingLevel({line: int, level: int})
  | UnexpectedToken({line: int, token: string})
  | EmptyDocument

/// Format a parse error as a diagnostic string.
let parseErrorToString = (err: parseError): string => {
  switch err {
  | UnterminatedDirective({line, name}) =>
    `error[A2ML]: line ${line->Int.toString}: unterminated directive @${name}`
  | InvalidHeadingLevel({line, level}) =>
    `error[A2ML]: line ${line->Int.toString}: invalid heading level ${level->Int.toString} (must be 1-5)`
  | UnexpectedToken({line, token}) =>
    `error[A2ML]: line ${line->Int.toString}: unexpected token "${token}"`
  | EmptyDocument => "error[A2ML]: document is empty"
  }
}
