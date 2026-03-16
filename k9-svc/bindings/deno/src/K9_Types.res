// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// K9_Types — Core data types for K9 (Self-Validating Components).
//
// Defines the abstract syntax tree for K9 component specifications,
// including pedigree metadata, security levels (Kennel/Yard/Hunt),
// target platform constraints, lifecycle recipes, validation blocks,
// and contract clauses.

// ---------------------------------------------------------------------------
// Security levels
// ---------------------------------------------------------------------------

/// K9 security levels forming a trust hierarchy.
///
/// - Kennel: Pure data, no execution, safe anywhere.
/// - Yard:   Controlled execution, limited permissions.
/// - Hunt:   Full execution with explicit authorisation required.
type securityLevel =
  | Kennel
  | Yard
  | Hunt

/// Parse a security level from its canonical string representation.
/// Recognised values (case-insensitive): "kennel", "yard", "hunt".
/// Also accepts tick-prefixed forms: "'Kennel", "'Yard", "'Hunt".
let securityLevelFromString = (s: string): option<securityLevel> => {
  let normalized = s->String.trim->String.toLowerCase
  // Strip leading tick if present (e.g., "'kennel" -> "kennel")
  let cleaned = if normalized->String.startsWith("'") {
    normalized->String.sliceToEnd(~start=1)
  } else {
    normalized
  }
  switch cleaned {
  | "kennel" => Some(Kennel)
  | "yard" => Some(Yard)
  | "hunt" => Some(Hunt)
  | _ => None
  }
}

/// Return the canonical string representation of a security level.
/// Uses the tick-prefixed form matching the K9 spec (e.g., "'Kennel").
let securityLevelToString = (level: securityLevel): string => {
  switch level {
  | Kennel => "'Kennel"
  | Yard => "'Yard"
  | Hunt => "'Hunt"
  }
}

// ---------------------------------------------------------------------------
// Pedigree metadata
// ---------------------------------------------------------------------------

/// Pedigree: identity and provenance metadata for a K9 component.
type pedigree = {
  name: string,
  version: string,
  description: string,
  author: option<string>,
  license: option<string>,
}

/// Create a pedigree with the minimum required fields.
let makePedigree = (
  ~name: string,
  ~version: string,
  ~description: string,
): pedigree => {
  name,
  version,
  description,
  author: None,
  license: None,
}

// ---------------------------------------------------------------------------
// Security policy
// ---------------------------------------------------------------------------

/// Security policy combining the level with specific permission flags.
type securityPolicy = {
  level: securityLevel,
  allowNetwork: bool,
  allowFsWrite: bool,
  allowSubprocess: bool,
}

/// Create a default security policy for the given level.
/// Kennel: all permissions denied.
/// Yard: network allowed, filesystem write and subprocess denied.
/// Hunt: all permissions allowed.
let defaultSecurityPolicy = (level: securityLevel): securityPolicy => {
  switch level {
  | Kennel => {level, allowNetwork: false, allowFsWrite: false, allowSubprocess: false}
  | Yard => {level, allowNetwork: true, allowFsWrite: false, allowSubprocess: false}
  | Hunt => {level, allowNetwork: true, allowFsWrite: true, allowSubprocess: true}
  }
}

// ---------------------------------------------------------------------------
// Target platform
// ---------------------------------------------------------------------------

/// Target platform constraints for a K9 component.
type target = {
  os: option<string>,
  isEdge: bool,
  requiresPodman: bool,
  memory: option<string>,
}

// ---------------------------------------------------------------------------
// Recipes
// ---------------------------------------------------------------------------

/// Collection of standard lifecycle recipes for a K9 component.
type recipes = {
  install: option<string>,
  validate: option<string>,
  deploy: option<string>,
  migrate: option<string>,
  custom: array<(string, string)>,
}

/// Create an empty recipes collection.
let emptyRecipes = (): recipes => {
  install: None,
  validate: None,
  deploy: None,
  migrate: None,
  custom: [],
}

// ---------------------------------------------------------------------------
// Validation
// ---------------------------------------------------------------------------

/// Self-validation block for a K9 component.
type validation = {
  checksum: string,
  pedigreeVersion: string,
  huntAuthorized: bool,
}

// ---------------------------------------------------------------------------
// Contract
// ---------------------------------------------------------------------------

/// A single clause within a K9 contract.
type contractClause = {
  clauseType: string,
  predicate: string,
  verified: bool,
}

/// A contract attached to a K9 component (from the contractile system).
type contract = {
  name: string,
  clauses: array<contractClause>,
}

// ---------------------------------------------------------------------------
// Component (top-level AST node)
// ---------------------------------------------------------------------------

/// A K9 self-validating component. This is the top-level AST node
/// representing a complete .k9 specification file.
type component = {
  pedigree: pedigree,
  security: securityPolicy,
  target: option<target>,
  recipes: option<recipes>,
  validation: option<validation>,
  content: array<(string, string)>,
  tags: array<string>,
}

/// Create a minimal component with the given pedigree and security level.
let makeComponent = (
  ~pedigree: pedigree,
  ~securityLevel: securityLevel,
): component => {
  pedigree,
  security: defaultSecurityPolicy(securityLevel),
  target: None,
  recipes: None,
  validation: None,
  content: [],
  tags: [],
}

// ---------------------------------------------------------------------------
// Parse errors
// ---------------------------------------------------------------------------

/// Errors that can occur during K9 parsing.
type parseError =
  | MissingMagicNumber
  | MissingPedigree(string)
  | InvalidSecurityLevel(string)
  | UnexpectedToken({line: int, token: string})
  | EmptyDocument

/// Format a parse error as a diagnostic string.
let parseErrorToString = (err: parseError): string => {
  switch err {
  | MissingMagicNumber => "error[K9]: missing K9! magic number at start of file"
  | MissingPedigree(field) => `error[K9]: missing required pedigree field "${field}"`
  | InvalidSecurityLevel(level) => `error[K9]: invalid security level "${level}"`
  | UnexpectedToken({line, token}) =>
    `error[K9]: line ${line->Int.toString}: unexpected token "${token}"`
  | EmptyDocument => "error[K9]: document is empty"
  }
}
