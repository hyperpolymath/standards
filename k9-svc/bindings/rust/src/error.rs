// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for crates.io)

//! Error types for K9 parsing and rendering.
//!
//! Provides structured error reporting with line/column information
//! for parse failures, Nickel format detection, and I/O error wrapping.

use thiserror::Error;

/// Errors that can occur during K9 parsing, rendering, or file I/O.
#[derive(Error, Debug)]
pub enum K9Error {
    /// A syntax or structural error encountered during parsing.
    #[error("parse error at line {line}, column {column}: {message}")]
    ParseError {
        /// The 1-based line number where the error was detected.
        line: usize,
        /// The 1-based column number where the error was detected.
        column: usize,
        /// A human-readable description of the parse failure.
        message: String,
    },

    /// The file appears to be Nickel-format K9 (`.k9.ncl`), which requires
    /// the Nickel evaluator and is not handled by this parser.
    #[error("Nickel K9 format detected: {0} — use a Nickel evaluator to process .k9.ncl files")]
    NickelFormat(String),

    /// An unknown or unsupported security level was encountered.
    #[error("unknown security level: {0}")]
    UnknownSecurityLevel(String),

    /// An I/O error occurred while reading or writing a file.
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    /// A rendering error occurred while producing K9 output.
    #[error("render error: {0}")]
    RenderError(String),
}

/// Convenience alias for results that may produce a [`K9Error`].
pub type Result<T> = std::result::Result<T, K9Error>;

impl K9Error {
    /// Create a new parse error at the given location.
    ///
    /// # Arguments
    ///
    /// * `line` - The 1-based line number.
    /// * `column` - The 1-based column number.
    /// * `message` - A description of what went wrong.
    pub fn parse(line: usize, column: usize, message: impl Into<String>) -> Self {
        Self::ParseError {
            line,
            column,
            message: message.into(),
        }
    }

    /// Format the error as a diagnostic string suitable for terminal output.
    pub fn diagnostic(&self) -> String {
        match self {
            Self::ParseError {
                line,
                column,
                message,
            } => {
                format!("error[K9]: {}:{}: {}", line, column, message)
            }
            other => format!("error[K9]: {}", other),
        }
    }
}
