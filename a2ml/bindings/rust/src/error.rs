// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for crates.io)

//! Error types for A2ML parsing and rendering.
//!
//! Provides structured error reporting with line/column information
//! for parse failures, and I/O error wrapping for file operations.

use thiserror::Error;

/// Errors that can occur during A2ML parsing, rendering, or file I/O.
#[derive(Error, Debug)]
pub enum A2mlError {
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

    /// An unexpected or unrecognised directive was encountered.
    #[error("unknown directive: {0}")]
    UnknownDirective(String),

    /// An I/O error occurred while reading or writing a file.
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    /// A rendering error occurred while producing A2ML output.
    #[error("render error: {0}")]
    RenderError(String),
}

/// Convenience alias for results that may produce an [`A2mlError`].
pub type Result<T> = std::result::Result<T, A2mlError>;

impl A2mlError {
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
}

// Display is automatically derived by thiserror, but we implement
// a source-location formatter for convenient diagnostics.
impl A2mlError {
    /// Format the error as a diagnostic string suitable for terminal output.
    ///
    /// For parse errors this includes the file location; for other variants
    /// it delegates to the standard `Display` implementation.
    pub fn diagnostic(&self) -> String {
        match self {
            Self::ParseError {
                line,
                column,
                message,
            } => {
                format!("error[A2ML]: {}:{}: {}", line, column, message)
            }
            other => format!("error[A2ML]: {}", other),
        }
    }
}
