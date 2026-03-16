// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// A2ML — Main module for the A2ML (Attested Markup Language) parser library.
//
// Re-exports the core types, parser, and renderer for convenient access.
// This module serves as the primary entry point for library consumers.
//
// ## Usage
//
// ```rescript
// open A2ML
//
// let result = A2ML_Parser.parseA2ML("# Hello\n\nSome text.")
// switch result {
// | Ok(doc) => Console.log(A2ML_Renderer.renderA2ML(doc))
// | Error(err) => Console.error(A2ML_Types.parseErrorToString(err))
// }
// ```

// Re-export types for convenience
type trustLevel = A2ML_Types.trustLevel
type inline = A2ML_Types.inline
type directive = A2ML_Types.directive
type attestation = A2ML_Types.attestation
type block = A2ML_Types.block
type document = A2ML_Types.document
type manifest = A2ML_Types.manifest
type parseError = A2ML_Types.parseError

/// Parse an A2ML document from a string.
let parse = A2ML_Parser.parseA2ML

/// Parse an A2ML document from a file path.
let parseFile = A2ML_Parser.parseA2MLFile

/// Render an A2ML document to text.
let render = A2ML_Renderer.renderA2ML

/// Render a single block to text.
let renderBlock = A2ML_Renderer.renderBlock

/// Render a single inline element to text.
let renderInline = A2ML_Renderer.renderInline

/// Create an empty document.
let emptyDocument = A2ML_Types.emptyDocument

/// Create a simple directive.
let makeDirective = A2ML_Types.makeDirective

/// Create an attestation.
let makeAttestation = A2ML_Types.makeAttestation

/// Extract a manifest from a document.
let manifestFromDocument = A2ML_Types.manifestFromDocument

/// Format a parse error as a diagnostic string.
let parseErrorToString = A2ML_Types.parseErrorToString

/// Parse a trust level from a string.
let trustLevelFromString = A2ML_Types.trustLevelFromString

/// Convert a trust level to its canonical string.
let trustLevelToString = A2ML_Types.trustLevelToString
