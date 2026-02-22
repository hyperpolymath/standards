// SPDX-License-Identifier: PMPL-1.0-or-later
//! k9-validate: Standalone validator for K9 contractiles

use anyhow::{Context, Result};
use clap::Parser;
use regex::Regex;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

#[derive(Parser, Debug)]
#[command(name = "k9-validate")]
#[command(about = "Validate K9 contractiles", long_about = None)]
struct Args {
    /// File or directory to validate
    #[arg(default_value = ".")]
    path: PathBuf,

    /// Strict validation (fail on warnings)
    #[arg(short, long)]
    strict: bool,

    /// Check cryptographic signatures
    #[arg(long)]
    check_signatures: bool,

    /// Custom schema file
    #[arg(long)]
    schema: Option<PathBuf>,

    /// Output format (text or json)
    #[arg(short, long, default_value = "text")]
    format: String,
}

#[derive(Debug)]
struct ValidationResult {
    file: PathBuf,
    errors: Vec<ValidationError>,
    warnings: Vec<ValidationWarning>,
}

#[derive(Debug)]
struct ValidationError {
    line: Option<usize>,
    column: Option<usize>,
    message: String,
    error_type: ErrorType,
}

#[derive(Debug)]
enum ErrorType {
    SyntaxError,
    SchemaError,
    SecurityError,
    SignatureError,
}

#[derive(Debug)]
struct ValidationWarning {
    line: Option<usize>,
    message: String,
}

fn main() {
    let args = Args::parse();

    let exit_code = match run_validation(&args) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error: {:#}", e);
            3 // File not found or other error
        }
    };

    process::exit(exit_code);
}

fn run_validation(args: &Args) -> Result<i32> {
    println!("╔══════════════════════════════════════════════════════════╗");
    println!("║         K9 Validate - Contractile Validator             ║");
    println!("╚══════════════════════════════════════════════════════════╝");
    println!();

    let files = collect_k9_files(&args.path)?;

    if files.is_empty() {
        println!("No K9 contractile files found.");
        return Ok(0);
    }

    println!("Found {} K9 contractile(s)", files.len());
    println!();

    let mut all_valid = true;
    let mut total_errors = 0;
    let mut total_warnings = 0;

    for file in &files {
        let result = validate_file(file, args)?;

        total_errors += result.errors.len();
        total_warnings += result.warnings.len();

        if !result.errors.is_empty() {
            all_valid = false;
        }

        print_validation_result(&result, args)?;
    }

    println!();
    println!("═══════════════════════════════════════════════════════════");
    println!("Summary:");
    println!("  Files validated: {}", files.len());
    println!("  Errors: {}", total_errors);
    println!("  Warnings: {}", total_warnings);
    println!();

    let exit_code = if !all_valid {
        println!("✗ Validation FAILED");
        1 // Validation errors
    } else if total_warnings > 0 && args.strict {
        println!("✗ Validation FAILED (strict mode, warnings present)");
        1 // Strict mode, warnings treated as errors
    } else if total_warnings > 0 {
        println!("⚠  Validation PASSED with warnings");
        0 // Valid with warnings
    } else {
        println!("✓ Validation PASSED");
        0 // Valid
    };

    Ok(exit_code)
}

fn collect_k9_files(path: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    if path.is_file() {
        if path.extension().and_then(|s| s.to_str()) == Some("ncl") {
            files.push(path.to_path_buf());
        }
    } else if path.is_dir() {
        for entry in walkdir::WalkDir::new(path)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            if path.is_file() {
                if let Some(file_name) = path.file_name().and_then(|s| s.to_str()) {
                    if file_name.ends_with(".k9.ncl") {
                        files.push(path.to_path_buf());
                    }
                }
            }
        }
    }

    Ok(files)
}

fn validate_file(file: &Path, args: &Args) -> Result<ValidationResult> {
    let content = fs::read_to_string(file)
        .with_context(|| format!("Failed to read file: {}", file.display()))?;

    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Check for SPDX license header
    if !content.contains("SPDX-License-Identifier") {
        warnings.push(ValidationWarning {
            line: Some(1),
            message: "Missing SPDX license identifier".to_string(),
        });
    }

    // Validate must-just-nickel structure
    let has_must = content.contains("must = {");
    let has_just = content.contains("just = {");
    let has_nickel = content.contains("nickel = {");

    if !has_must {
        errors.push(ValidationError {
            line: None,
            column: None,
            message: "Missing 'must' section (required for K9 contractile)".to_string(),
            error_type: ErrorType::SchemaError,
        });
    }

    if !has_just {
        errors.push(ValidationError {
            line: None,
            column: None,
            message: "Missing 'just' section (required for K9 contractile)".to_string(),
            error_type: ErrorType::SchemaError,
        });
    }

    if !has_nickel {
        warnings.push(ValidationWarning {
            line: None,
            message: "'nickel' section recommended but not required".to_string(),
        });
    }

    // Validate metadata
    if !content.contains("metadata = {") {
        warnings.push(ValidationWarning {
            line: None,
            message: "Missing 'metadata' section (recommended)".to_string(),
        });
    }

    // Check for security level
    let security_levels = ["kennel", "yard", "hunt"];
    let has_security_level = security_levels.iter().any(|level| {
        content.contains(&format!("security_level = \"{}\"", level))
    });

    if !has_security_level {
        warnings.push(ValidationWarning {
            line: None,
            message: "Security level not specified (kennel, yard, or hunt)".to_string(),
        });
    }

    // Validate bash in must.check
    if let Some(must_section) = extract_section(&content, "must") {
        if must_section.contains("check = ''") || must_section.contains("check = \"\"") {
            if !must_section.contains("#!/usr/bin/env bash")
                && !must_section.contains("#!/bin/bash") {
                warnings.push(ValidationWarning {
                    line: None,
                    message: "must.check should start with shebang (#!/usr/bin/env bash)".to_string(),
                });
            }

            // Check for exit codes
            if !must_section.contains("exit 0") && !must_section.contains("exit 1") {
                warnings.push(ValidationWarning {
                    line: None,
                    message: "must.check should explicitly exit with 0 (satisfied) or non-zero (not satisfied)".to_string(),
                });
            }
        }
    }

    // Check for signature if check-signatures enabled
    if args.check_signatures {
        if !content.contains("signature =") {
            errors.push(ValidationError {
                line: None,
                column: None,
                message: "No cryptographic signature found (required with --check-signatures)".to_string(),
                error_type: ErrorType::SignatureError,
            });
        }
    }

    // Check for dangerous patterns in hunt-level contractiles
    if content.contains("security_level = \"hunt\"") {
        let dangerous_patterns = vec![
            ("rm -rf /", "Dangerous: recursive removal from root"),
            ("sudo rm -rf", "Dangerous: privileged recursive removal"),
            ("> /dev/sda", "Dangerous: writing to disk device"),
            ("dd if=/dev/zero", "Dangerous: disk wiping operation"),
        ];

        for (pattern, desc) in dangerous_patterns {
            if content.contains(pattern) {
                errors.push(ValidationError {
                    line: None,
                    column: None,
                    message: format!("{}: found '{}'", desc, pattern),
                    error_type: ErrorType::SecurityError,
                });
            }
        }
    }

    Ok(ValidationResult {
        file: file.to_path_buf(),
        errors,
        warnings,
    })
}

fn extract_section(content: &str, section_name: &str) -> Option<String> {
    let pattern = format!(r"{}\s*=\s*\{{", section_name);
    let re = Regex::new(&pattern).ok()?;

    let start = re.find(content)?.start();
    let after_start = &content[start..];

    // Find matching closing brace (simplified, doesn't handle nested braces perfectly)
    let mut depth = 0;
    let mut chars = after_start.char_indices();

    while let Some((i, ch)) = chars.next() {
        if ch == '{' {
            depth += 1;
        } else if ch == '}' {
            depth -= 1;
            if depth == 0 {
                return Some(after_start[..=i].to_string());
            }
        }
    }

    None
}

fn print_validation_result(result: &ValidationResult, args: &Args) -> Result<()> {
    if args.format == "json" {
        // TODO: JSON output
        return Ok(());
    }

    // Text output
    let status = if result.errors.is_empty() {
        if result.warnings.is_empty() {
            "✓"
        } else {
            "⚠"
        }
    } else {
        "✗"
    };

    println!("{} {}", status, result.file.display());

    for error in &result.errors {
        let location = if let (Some(line), Some(col)) = (error.line, error.column) {
            format!("  Line {}, Column {}: ", line, col)
        } else if let Some(line) = error.line {
            format!("  Line {}: ", line)
        } else {
            "  ".to_string()
        };

        println!("  ✗ ERROR: {}{}", location, error.message);
    }

    for warning in &result.warnings {
        let location = if let Some(line) = warning.line {
            format!("  Line {}: ", line)
        } else {
            "  ".to_string()
        };

        println!("  ⚠ WARNING: {}{}", location, warning.message);
    }

    if !result.errors.is_empty() || !result.warnings.is_empty() {
        println!();
    }

    Ok(())
}
