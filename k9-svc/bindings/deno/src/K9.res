// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// K9 — Main module for the K9 (Self-Validating Components) parser library.
//
// Re-exports the core types, parser, and renderer for convenient access.
// This module serves as the primary entry point for library consumers.
//
// ## Usage
//
// ```rescript
// open K9
//
// let result = K9_Parser.parseK9("K9!\n---\nmetadata:\n  name: hello\n  ...")
// switch result {
// | Ok(component) => Console.log(K9_Renderer.renderK9(component))
// | Error(err) => Console.error(K9_Types.parseErrorToString(err))
// }
// ```

// Re-export types for convenience
type securityLevel = K9_Types.securityLevel
type pedigree = K9_Types.pedigree
type securityPolicy = K9_Types.securityPolicy
type target = K9_Types.target
type recipes = K9_Types.recipes
type validation = K9_Types.validation
type contractClause = K9_Types.contractClause
type contract = K9_Types.contract
type component = K9_Types.component
type parseError = K9_Types.parseError
type k9Format = K9_Parser.k9Format

/// Parse a K9 component specification from a string.
let parse = K9_Parser.parseK9

/// Parse a K9 component specification from a file path.
let parseFile = K9_Parser.parseK9File

/// Render a K9 component to the .k9 YAML-like format.
let render = K9_Renderer.renderK9

/// Render a security level to its canonical string.
let renderSecurityLevel = K9_Renderer.renderSecurityLevel

/// Detect the format of a K9 file (YAML or Nickel).
let detectFormat = K9_Parser.detectFormat

/// Create a minimal component.
let makeComponent = K9_Types.makeComponent

/// Create a pedigree with minimum required fields.
let makePedigree = K9_Types.makePedigree

/// Create a default security policy for a given level.
let defaultSecurityPolicy = K9_Types.defaultSecurityPolicy

/// Create an empty recipes collection.
let emptyRecipes = K9_Types.emptyRecipes

/// Parse a security level from a string.
let securityLevelFromString = K9_Types.securityLevelFromString

/// Convert a security level to its canonical string.
let securityLevelToString = K9_Types.securityLevelToString

/// Format a parse error as a diagnostic string.
let parseErrorToString = K9_Types.parseErrorToString
