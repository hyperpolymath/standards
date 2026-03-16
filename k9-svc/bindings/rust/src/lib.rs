// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for crates.io)

//! # k9-svc
//!
//! Parser and renderer for **K9** self-validating configuration files.
//!
//! K9 is a configuration format designed for declaring software components
//! with built-in provenance tracking (pedigree), security classification,
//! build recipes, and runtime contracts.  It supports both a YAML-like
//! plain-text format (`.k9`) and a Nickel-based format (`.k9.ncl`) for
//! advanced type-checked configurations.
//!
//! ## Quick start
//!
//! ```
//! use k9_svc::parser::parse;
//! use k9_svc::renderer::render;
//!
//! let input = r#"component: my-svc
//!   version: 0.1.0
//!   pedigree:
//!     origin: https://github.com/example/svc
//!     author: Alice
//!   security: kennel
//! "#;
//!
//! let components = parse(input).unwrap();
//! let output = render(&components).unwrap();
//! ```
//!
//! ## Modules
//!
//! - [`types`] — Core data structures (`Component`, `Pedigree`, `SecurityLevel`, etc.)
//! - [`parser`] — Parse K9 text into components (detects Nickel format)
//! - [`renderer`] — Render components back to K9 text
//! - [`error`] — Error types

pub mod error;
pub mod parser;
pub mod renderer;
pub mod types;

// Re-export the most commonly used items at the crate root for convenience.
pub use error::K9Error;
pub use types::{Component, Contract, Pedigree, Recipe, SecurityLevel};
