// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for crates.io)

//! # a2ml
//!
//! Parser and renderer for **A2ML** (Attested Markup Language).
//!
//! A2ML is a lightweight markup format designed for AI-agent communication
//! that carries built-in attestation metadata, enabling provenance tracking
//! and trust-level annotations on document content.
//!
//! ## Quick start
//!
//! ```
//! use a2ml::parser::parse;
//! use a2ml::renderer::render;
//!
//! let input = "# Hello\n\n@version 1.0\n\nA paragraph.";
//! let doc = parse(input).unwrap();
//! let output = render(&doc).unwrap();
//! ```
//!
//! ## Modules
//!
//! - [`types`] — Core data structures (`Document`, `Block`, `Inline`, etc.)
//! - [`parser`] — Parse A2ML text into a `Document`
//! - [`renderer`] — Render a `Document` back to A2ML text
//! - [`error`] — Error types

pub mod error;
pub mod parser;
pub mod renderer;
pub mod types;

// Re-export the most commonly used items at the crate root for convenience.
pub use error::A2mlError;
pub use types::{Attestation, Block, Directive, Document, Inline, Manifest, TrustLevel};
