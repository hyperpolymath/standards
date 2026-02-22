// SPDX-License-Identifier: PMPL-1.0-or-later

//! AI Manifest parsing and validation

use anyhow::{Context, Result};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::Path;

/// Parsed AI manifest
#[derive(Debug, Clone)]
pub struct Manifest {
    /// SHA-256 hash of manifest content
    pub hash: String,
    /// Canonical locations extracted from manifest
    pub canonical_locations: CanonicalLocations,
    /// Critical invariants
    pub invariants: Vec<String>,
}

/// Canonical file locations declared in manifest
#[derive(Debug, Clone)]
pub struct CanonicalLocations {
    /// Location for SCM files (e.g., ".machine_readable/")
    pub scm_files: String,
    /// Location for bot directives (e.g., ".bot_directives/")
    pub bot_directives: String,
    /// Agent-specific instruction files
    pub agent_instructions: Vec<String>,
}

impl Default for CanonicalLocations {
    fn default() -> Self {
        Self {
            scm_files: ".machine_readable/".to_string(),
            bot_directives: ".bot_directives/".to_string(),
            agent_instructions: vec![
                ".claude/CLAUDE.md".to_string(),
                "AI.a2ml".to_string(),
                "0-AI-MANIFEST.a2ml".to_string(),
            ],
        }
    }
}

/// Find and parse manifest in a repository
pub fn find_and_parse_manifest(repo_path: &Path) -> Result<Manifest> {
    // Try manifest names in order of preference
    let manifest_names = ["0-AI-MANIFEST.a2ml", "AI.a2ml", "!AI.a2ml"];

    for name in &manifest_names {
        let manifest_path = repo_path.join(name);
        if manifest_path.exists() {
            return parse_manifest(&manifest_path);
        }
    }

    anyhow::bail!(
        "No AI manifest found in {:?}. Expected one of: {}",
        repo_path,
        manifest_names.join(", ")
    )
}

/// Parse an AI manifest file
pub fn parse_manifest(path: &Path) -> Result<Manifest> {
    let content = fs::read_to_string(path)
        .with_context(|| format!("Failed to read manifest: {:?}", path))?;

    // Compute SHA-256 hash
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    let hash = format!("{:x}", hasher.finalize());

    // Extract canonical locations
    let canonical_locations = extract_canonical_locations(&content);

    // Extract invariants
    let invariants = extract_invariants(&content);

    Ok(Manifest {
        hash,
        canonical_locations,
        invariants,
    })
}

/// Extract canonical locations from manifest content
fn extract_canonical_locations(content: &str) -> CanonicalLocations {
    let mut locations = CanonicalLocations::default();

    // Look for SCM files location
    for line in content.lines() {
        let line_lower = line.to_lowercase();

        // Match patterns like "SCM files ... .machine_readable/"
        if line_lower.contains("scm") && line_lower.contains("machine_readable") {
            if let Some(path) = extract_path(line) {
                if path.contains("machine_readable") {
                    locations.scm_files = path;
                }
            }
        }

        // Match patterns like "Bot directives ... .bot_directives/"
        if line_lower.contains("bot") && line_lower.contains("directive") {
            if let Some(path) = extract_path(line) {
                if path.contains("bot_directive") {
                    locations.bot_directives = path;
                }
            }
        }
    }

    locations
}

/// Extract invariants from manifest content
fn extract_invariants(content: &str) -> Vec<String> {
    let mut invariants = Vec::new();
    let mut in_invariants_section = false;

    for line in content.lines() {
        let line_trimmed = line.trim();

        // Detect start of CORE INVARIANTS section
        if line_trimmed.to_lowercase().contains("core invariant") {
            in_invariants_section = true;
            continue;
        }

        // Detect end of section (next ## heading)
        if in_invariants_section && line_trimmed.starts_with("##") {
            break;
        }

        // Extract numbered invariants
        if in_invariants_section {
            // Match patterns like "1. **No SCM duplication** - description"
            if let Some(stripped) = line_trimmed.strip_prefix(char::is_numeric) {
                if stripped.trim_start().starts_with('.') {
                    let invariant = stripped
                        .trim_start_matches('.')
                        .trim()
                        .trim_start_matches("**")
                        .split("**")
                        .next()
                        .unwrap_or("")
                        .trim()
                        .to_string();

                    if !invariant.is_empty() {
                        invariants.push(invariant);
                    }
                }
            }
        }
    }

    // If no invariants found, add defaults
    if invariants.is_empty() {
        invariants.push("no_scm_duplication".to_string());
        invariants.push("single_source_of_truth".to_string());
    }

    invariants
}

/// Extract path from a line (looks for backtick-enclosed paths)
fn extract_path(line: &str) -> Option<String> {
    // Look for paths in backticks like `.machine_readable/`
    if let Some(start) = line.find('`') {
        if let Some(end) = line[start + 1..].find('`') {
            let path = line[start + 1..start + 1 + end].to_string();
            return Some(path);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_canonical_locations() {
        let content = r#"
## CANONICAL LOCATIONS

SCM files MUST be in `.machine_readable/` directory ONLY.
Bot directives go in `.bot_directives/`.
        "#;

        let locations = extract_canonical_locations(content);
        assert_eq!(locations.scm_files, ".machine_readable/");
        assert_eq!(locations.bot_directives, ".bot_directives/");
    }

    #[test]
    fn test_extract_invariants() {
        let content = r#"
## CORE INVARIANTS

1. **No SCM duplication** - Root must NOT contain STATE.scm
2. **Single source of truth** - .machine_readable/ is authoritative

## NEXT SECTION
        "#;

        let invariants = extract_invariants(content);
        assert_eq!(invariants.len(), 2);
        assert_eq!(invariants[0], "No SCM duplication");
        assert_eq!(invariants[1], "Single source of truth");
    }
}
