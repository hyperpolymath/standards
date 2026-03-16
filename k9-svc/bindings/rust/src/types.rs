// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for crates.io)

//! Core data types for K9 self-validating configuration documents.
//!
//! A K9 document declares software [`Component`]s with their [`Pedigree`]
//! (provenance), [`SecurityLevel`], [`Recipe`] (build instructions), and
//! [`Contract`] (runtime invariants).

use serde::{Deserialize, Serialize};

// ---------------------------------------------------------------------------
// Component — the top-level unit
// ---------------------------------------------------------------------------

/// A software component declared in a K9 configuration file.
///
/// Each component has a name, version, pedigree (provenance chain),
/// security classification, optional build recipe, and optional contracts.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Component {
    /// The unique name of this component (e.g. `"a2ml-parser"`).
    pub name: String,

    /// The semantic version string (e.g. `"0.1.0"`).
    pub version: String,

    /// A short human-readable description of the component's purpose.
    pub description: Option<String>,

    /// Provenance information for this component.
    pub pedigree: Pedigree,

    /// The security classification level.
    pub security_level: SecurityLevel,

    /// Build or assembly instructions for the component.
    pub recipe: Option<Recipe>,

    /// Runtime contracts (invariants) this component must satisfy.
    pub contracts: Vec<Contract>,

    /// Arbitrary key-value metadata.
    pub metadata: Vec<(String, String)>,
}

impl Component {
    /// Create a new component with the minimum required fields.
    pub fn new(
        name: impl Into<String>,
        version: impl Into<String>,
        pedigree: Pedigree,
        security_level: SecurityLevel,
    ) -> Self {
        Self {
            name: name.into(),
            version: version.into(),
            description: None,
            pedigree,
            security_level,
            recipe: None,
            contracts: Vec::new(),
            metadata: Vec::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Pedigree — provenance chain
// ---------------------------------------------------------------------------

/// Provenance information recording how a component was produced.
///
/// Tracks the origin (source forge/URL), author, and an optional chain
/// of upstream sources for auditability.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Pedigree {
    /// The origin URL or forge identifier (e.g. `"https://github.com/foo/bar"`).
    pub origin: String,

    /// The author or maintainer identity.
    pub author: String,

    /// The licence SPDX identifier (e.g. `"MPL-2.0"`).
    pub license: Option<String>,

    /// An optional commit hash or tag pinning this pedigree to a specific revision.
    pub commit: Option<String>,

    /// Upstream pedigrees this component was derived from.
    pub upstream: Vec<Pedigree>,
}

impl Pedigree {
    /// Create a new pedigree with origin and author.
    pub fn new(origin: impl Into<String>, author: impl Into<String>) -> Self {
        Self {
            origin: origin.into(),
            author: author.into(),
            license: None,
            commit: None,
            upstream: Vec::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// SecurityLevel — kennel / yard / hunt
// ---------------------------------------------------------------------------

/// The security classification of a component.
///
/// K9 uses a three-tier model inspired by working-dog access zones:
///
/// - **Kennel** — internal only, highest trust, lowest exposure.
/// - **Yard** — semi-trusted, accessible within a bounded perimeter.
/// - **Hunt** — fully exposed, untrusted environment, maximum hardening.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum SecurityLevel {
    /// Internal-only component; highest trust, lowest exposure.
    Kennel,
    /// Semi-trusted component within a bounded perimeter.
    Yard,
    /// Fully exposed component requiring maximum hardening.
    Hunt,
}

impl SecurityLevel {
    /// Parse a security level from its canonical string representation.
    ///
    /// Recognised values (case-insensitive): `"kennel"`, `"yard"`, `"hunt"`.
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "kennel" => Some(Self::Kennel),
            "yard" => Some(Self::Yard),
            "hunt" => Some(Self::Hunt),
            _ => None,
        }
    }

    /// Return the canonical string representation of this security level.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Kennel => "kennel",
            Self::Yard => "yard",
            Self::Hunt => "hunt",
        }
    }
}

impl std::fmt::Display for SecurityLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

// ---------------------------------------------------------------------------
// Recipe — build instructions
// ---------------------------------------------------------------------------

/// Build or assembly instructions for a component.
///
/// A recipe describes how to produce the component from source, including
/// build commands, required tools, and output artefacts.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Recipe {
    /// The build tool or system (e.g. `"cargo"`, `"just"`, `"deno"`).
    pub tool: String,

    /// The build command(s) to execute.
    pub command: String,

    /// Required tool versions or prerequisites.
    pub requires: Vec<String>,

    /// Output artefact paths or names.
    pub outputs: Vec<String>,
}

impl Recipe {
    /// Create a new recipe with a tool and command.
    pub fn new(tool: impl Into<String>, command: impl Into<String>) -> Self {
        Self {
            tool: tool.into(),
            command: command.into(),
            requires: Vec::new(),
            outputs: Vec::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Contract — runtime invariants
// ---------------------------------------------------------------------------

/// A runtime contract (invariant) that a component must satisfy.
///
/// Contracts are self-validating checks that can be executed at build time,
/// deploy time, or runtime to verify component correctness.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Contract {
    /// A short identifier for this contract (e.g. `"no-unsafe"`).
    pub name: String,

    /// A human-readable description of what the contract checks.
    pub description: String,

    /// The check expression or command that validates the contract.
    pub check: String,

    /// The severity if the contract is violated: `"error"`, `"warning"`, or `"info"`.
    pub severity: String,
}

impl Contract {
    /// Create a new contract.
    pub fn new(
        name: impl Into<String>,
        description: impl Into<String>,
        check: impl Into<String>,
    ) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            check: check.into(),
            severity: "error".to_string(),
        }
    }
}
