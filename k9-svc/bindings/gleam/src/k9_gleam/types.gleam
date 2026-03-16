// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)
//
// k9_gleam/types — Core data types for K9 (Self-Validating Components).
//
// Defines the abstract syntax tree for K9 component specifications, including
// pedigree metadata, security levels, target platforms, recipes, and contracts.

import gleam/dict.{type Dict}

/// A K9 self-validating component. This is the top-level AST node.
pub type Component {
  Component(
    /// Identity and provenance metadata.
    pedigree: Pedigree,
    /// Security level and permission flags.
    security: SecurityPolicy,
    /// Optional target platform constraints.
    target: Result(Target, Nil),
    /// Optional build/deploy/validate recipes.
    recipes: Result(Recipes, Nil),
    /// Optional self-validation block (checksum, pedigree version).
    validation: Result(Validation, Nil),
    /// Additional content key-value pairs.
    content: Dict(String, String),
    /// Tags for categorisation.
    tags: List(String),
  )
}

/// Pedigree: identity and provenance metadata for a K9 component.
pub type Pedigree {
  Pedigree(
    /// Component name (e.g., "hello-k9").
    name: String,
    /// Semantic version string.
    version: String,
    /// Human-readable description.
    description: String,
    /// Author identity.
    author: Result(String, Nil),
    /// SPDX license identifier.
    license: Result(String, Nil),
  )
}

/// K9 security levels forming a trust hierarchy.
///
/// - `Kennel` — Pure data, no execution, safe anywhere.
/// - `Yard`   — Controlled execution, limited permissions.
/// - `Hunt`   — Full execution with explicit authorisation required.
pub type SecurityLevel {
  /// Pure data only. No code execution. Safe to open anywhere.
  Kennel
  /// Controlled execution with limited permissions.
  Yard
  /// Full execution. Requires explicit authorisation.
  Hunt
}

/// Security policy combining the level with specific permission flags.
pub type SecurityPolicy {
  SecurityPolicy(
    /// The trust level.
    level: SecurityLevel,
    /// Whether the component may access the network.
    allow_network: Bool,
    /// Whether the component may write to the filesystem.
    allow_fs_write: Bool,
    /// Whether the component may spawn subprocesses.
    allow_subprocess: Bool,
  )
}

/// Target platform constraints.
pub type Target {
  Target(
    /// Target operating system (e.g., "Linux", "Darwin").
    os: Result(String, Nil),
    /// Whether this targets edge/embedded environments.
    is_edge: Bool,
    /// Whether Podman container runtime is required.
    requires_podman: Bool,
    /// Memory constraint (e.g., "512M", "2G").
    memory: Result(String, Nil),
  )
}

/// Collection of standard lifecycle recipes.
pub type Recipes {
  Recipes(
    /// Installation command.
    install: Result(String, Nil),
    /// Validation / typecheck command.
    validate: Result(String, Nil),
    /// Deployment command.
    deploy: Result(String, Nil),
    /// Migration command.
    migrate: Result(String, Nil),
    /// Additional named recipes.
    custom: Dict(String, String),
  )
}

/// Named recipe for lifecycle operations.
pub type Recipe {
  Recipe(
    /// Recipe identifier (e.g., "install", "validate").
    name: String,
    /// Shell command to execute.
    command: String,
  )
}

/// Self-validation block.
pub type Validation {
  Validation(
    /// SHA-256 (or other) checksum of the component.
    checksum: String,
    /// Version of the pedigree schema used.
    pedigree_version: String,
    /// Whether Hunt-level execution has been explicitly authorised.
    hunt_authorized: Bool,
  )
}

/// A contract attached to a K9 component (from the contractile system).
pub type Contract {
  Contract(
    /// Contract identifier.
    name: String,
    /// Individual clauses in the contract.
    clauses: List(ContractClause),
  )
}

/// A single clause within a K9 contract.
pub type ContractClause {
  ContractClause(
    /// Clause type: "must", "trust", "dust", "intend", "k9".
    clause_type: String,
    /// The predicate or assertion text.
    predicate: String,
    /// Whether this clause has been verified.
    verified: Bool,
  )
}
