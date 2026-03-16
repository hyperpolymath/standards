// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for crates.io)

//! Core data types for A2ML documents.
//!
//! An A2ML document consists of a sequence of [`Block`] elements, each of
//! which may contain [`Inline`] content.  [`Directive`] blocks provide
//! machine-readable metadata, and [`Attestation`] records capture the
//! provenance chain for AI-generated or human-reviewed content.

use serde::{Deserialize, Serialize};

// ---------------------------------------------------------------------------
// Top-level document
// ---------------------------------------------------------------------------

/// A complete A2ML document, containing metadata and a sequence of blocks.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Document {
    /// Optional document title (extracted from a leading `# title` line).
    pub title: Option<String>,

    /// Top-level directives that apply to the whole document.
    pub directives: Vec<Directive>,

    /// The ordered sequence of content blocks that make up the document body.
    pub blocks: Vec<Block>,

    /// Attestation chain recording authorship and review provenance.
    pub attestations: Vec<Attestation>,
}

impl Document {
    /// Create a new, empty document with no title or content.
    pub fn new() -> Self {
        Self {
            title: None,
            directives: Vec::new(),
            blocks: Vec::new(),
            attestations: Vec::new(),
        }
    }
}

impl Default for Document {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Block-level elements
// ---------------------------------------------------------------------------

/// A block-level element in an A2ML document.
///
/// Blocks are separated by blank lines in the source text.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Block {
    /// A heading with a depth (1 = `#`, 2 = `##`, etc.) and inline content.
    Heading {
        /// Heading depth, starting at 1.
        level: u8,
        /// The inline content of the heading.
        content: Vec<Inline>,
    },

    /// A paragraph of inline content.
    Paragraph(Vec<Inline>),

    /// A fenced or indented code block with an optional language tag.
    CodeBlock {
        /// The language identifier (e.g. `"rust"`), if any.
        language: Option<String>,
        /// The raw code content, preserving internal whitespace.
        content: String,
    },

    /// A directive block (starts with `@`).
    Directive(Directive),

    /// An attestation block (starts with `!attest`).
    Attestation(Attestation),

    /// A horizontal rule / thematic break.
    ThematicBreak,

    /// A block quotation containing nested blocks.
    BlockQuote(Vec<Block>),

    /// An unordered or ordered list.
    List {
        /// Whether the list is ordered (numbered).
        ordered: bool,
        /// The items in the list; each item is a sequence of blocks.
        items: Vec<Vec<Block>>,
    },
}

// ---------------------------------------------------------------------------
// Inline-level elements
// ---------------------------------------------------------------------------

/// An inline-level element within a block.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Inline {
    /// Plain, unformatted text.
    Text(String),

    /// Emphasised text (typically rendered as *italic*).
    Emphasis(Vec<Inline>),

    /// Strongly emphasised text (typically rendered as **bold**).
    Strong(Vec<Inline>),

    /// Inline code span.
    Code(String),

    /// A hyperlink with display content and a target URL.
    Link {
        /// The visible link text.
        content: Vec<Inline>,
        /// The link target URL or path.
        url: String,
    },
}

// ---------------------------------------------------------------------------
// Directives
// ---------------------------------------------------------------------------

/// A machine-readable directive that provides metadata or instructions.
///
/// Directives begin with `@` in the source text, e.g.
/// `@version 1.0` or `@require trust-level:high`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Directive {
    /// The directive name (the identifier immediately after `@`).
    pub name: String,

    /// The directive value or argument string.
    pub value: String,

    /// Optional key-value attributes attached to the directive.
    pub attributes: Vec<(String, String)>,
}

impl Directive {
    /// Create a simple directive with a name and value, and no attributes.
    pub fn new(name: impl Into<String>, value: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            value: value.into(),
            attributes: Vec::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Attestations
// ---------------------------------------------------------------------------

/// An attestation record capturing who produced or reviewed content.
///
/// Attestation blocks start with `!attest` and record the identity,
/// role, trust level, and optional timestamp of an author or reviewer.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Attestation {
    /// The identity of the attester (person, tool, or agent name).
    pub identity: String,

    /// The role of the attester (e.g. `"author"`, `"reviewer"`, `"agent"`).
    pub role: String,

    /// The trust level assigned to this attestation.
    pub trust_level: TrustLevel,

    /// An optional ISO-8601 timestamp for when the attestation was made.
    pub timestamp: Option<String>,

    /// Optional free-form notes or justification.
    pub note: Option<String>,
}

impl Attestation {
    /// Create a new attestation with the minimum required fields.
    pub fn new(
        identity: impl Into<String>,
        role: impl Into<String>,
        trust_level: TrustLevel,
    ) -> Self {
        Self {
            identity: identity.into(),
            role: role.into(),
            trust_level,
            timestamp: None,
            note: None,
        }
    }
}

// ---------------------------------------------------------------------------
// Trust levels
// ---------------------------------------------------------------------------

/// The degree of trust associated with an attestation.
///
/// Trust levels form a simple ordered scale from unverified content
/// through to formally verified proofs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum TrustLevel {
    /// Content with no verification or review.
    Unverified,
    /// Content reviewed by an automated tool or linter.
    Automated,
    /// Content reviewed by a human.
    Reviewed,
    /// Content that has been formally verified or proven.
    Verified,
}

impl TrustLevel {
    /// Parse a trust level from its canonical string representation.
    ///
    /// Recognised values (case-insensitive): `"unverified"`, `"automated"`,
    /// `"reviewed"`, `"verified"`.
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "unverified" => Some(Self::Unverified),
            "automated" => Some(Self::Automated),
            "reviewed" => Some(Self::Reviewed),
            "verified" => Some(Self::Verified),
            _ => None,
        }
    }

    /// Return the canonical string representation of this trust level.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Unverified => "unverified",
            Self::Automated => "automated",
            Self::Reviewed => "reviewed",
            Self::Verified => "verified",
        }
    }
}

impl std::fmt::Display for TrustLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

// ---------------------------------------------------------------------------
// Manifest (convenience aggregate)
// ---------------------------------------------------------------------------

/// A high-level manifest extracted from a parsed A2ML document.
///
/// This collects the directives and attestations into a single structure
/// for convenient programmatic access.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Manifest {
    /// The document version, if declared via `@version`.
    pub version: Option<String>,

    /// The document title, if present.
    pub title: Option<String>,

    /// All directives found in the document.
    pub directives: Vec<Directive>,

    /// All attestations found in the document.
    pub attestations: Vec<Attestation>,
}

impl Manifest {
    /// Extract a manifest from a parsed [`Document`].
    pub fn from_document(doc: &Document) -> Self {
        let version = doc
            .directives
            .iter()
            .find(|d| d.name == "version")
            .map(|d| d.value.clone());

        Self {
            version,
            title: doc.title.clone(),
            directives: doc.directives.clone(),
            attestations: doc.attestations.clone(),
        }
    }
}
