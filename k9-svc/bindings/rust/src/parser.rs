// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for crates.io)

//! Line-by-line parser for K9 self-validating configuration files.
//!
//! K9 uses a YAML-like syntax with indentation-based nesting.  This parser
//! handles `.k9` files directly and detects `.k9.ncl` (Nickel) files,
//! returning an appropriate error for the latter since they require the
//! Nickel evaluator.
//!
//! ## K9 syntax overview
//!
//! ```text
//! component: my-service
//!   version: 0.1.0
//!   description: An example service
//!   pedigree:
//!     origin: https://github.com/example/my-service
//!     author: Alice
//!     license: MPL-2.0
//!   security: yard
//!   recipe:
//!     tool: cargo
//!     command: cargo build --release
//!   contract: no-unsafe
//!     description: No unsafe code allowed
//!     check: cargo clippy -- -D unsafe-code
//!     severity: error
//! ```

use std::path::Path;

use crate::error::{K9Error, Result};
use crate::types::*;

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Parse a K9 configuration document from a string.
///
/// # Errors
///
/// Returns [`K9Error::ParseError`] if the input contains malformed entries
/// or missing required fields.  Returns [`K9Error::NickelFormat`] if the
/// input appears to be Nickel-format K9.
///
/// # Examples
///
/// ```
/// use k9_svc::parser::parse;
///
/// let input = r#"component: my-svc
///   version: 0.1.0
///   pedigree:
///     origin: https://github.com/example/svc
///     author: Alice
///   security: kennel
/// "#;
/// let components = parse(input).unwrap();
/// assert_eq!(components.len(), 1);
/// assert_eq!(components[0].name, "my-svc");
/// ```
pub fn parse(input: &str) -> Result<Vec<Component>> {
    // Detect Nickel format.
    if is_nickel_format(input) {
        return Err(K9Error::NickelFormat(
            "input contains Nickel syntax (let, in, {, })".to_string(),
        ));
    }

    let lines: Vec<&str> = input.lines().collect();
    let mut components = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i];
        let trimmed = line.trim();

        // Skip blank lines and comments.
        if trimmed.is_empty() || trimmed.starts_with('#') {
            i += 1;
            continue;
        }

        // Top-level component declaration.
        if trimmed.starts_with("component:") {
            let name = trimmed
                .strip_prefix("component:")
                .unwrap()
                .trim()
                .to_string();
            if name.is_empty() {
                return Err(K9Error::parse(i + 1, 1, "component name is empty"));
            }
            i += 1;
            let (component, next_i) = parse_component_body(&name, &lines, i)?;
            components.push(component);
            i = next_i;
            continue;
        }

        // Unknown top-level key.
        return Err(K9Error::parse(
            i + 1,
            1,
            format!("unexpected top-level key: {}", trimmed),
        ));
    }

    Ok(components)
}

/// Parse a K9 configuration document from a file on disk.
///
/// Detects `.k9.ncl` files by extension and returns [`K9Error::NickelFormat`]
/// with guidance to use a Nickel evaluator.
///
/// # Errors
///
/// Returns [`K9Error::Io`] if the file cannot be read, [`K9Error::NickelFormat`]
/// if the file is Nickel-format, or a parse error if the content is malformed.
pub fn parse_file(path: impl AsRef<Path>) -> Result<Vec<Component>> {
    let path = path.as_ref();

    // Detect Nickel files by extension.
    if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
        if name.ends_with(".k9.ncl") {
            return Err(K9Error::NickelFormat(format!(
                "file '{}' has .k9.ncl extension — use a Nickel evaluator",
                path.display()
            )));
        }
    }

    let content = std::fs::read_to_string(path)?;
    parse(&content)
}

// ---------------------------------------------------------------------------
// Internal: component body parser
// ---------------------------------------------------------------------------

/// Parse the indented body of a component declaration, returning the
/// component and the line index after the body.
fn parse_component_body(name: &str, lines: &[&str], start: usize) -> Result<(Component, usize)> {
    let mut version: Option<String> = None;
    let mut description: Option<String> = None;
    let mut pedigree: Option<Pedigree> = None;
    let mut security_level: Option<SecurityLevel> = None;
    let mut recipe: Option<Recipe> = None;
    let mut contracts: Vec<Contract> = Vec::new();
    let mut metadata: Vec<(String, String)> = Vec::new();

    let mut i = start;

    while i < lines.len() {
        let line = lines[i];

        // A non-indented, non-blank line signals the end of this component.
        if !line.starts_with(' ') && !line.starts_with('\t') && !line.trim().is_empty() {
            break;
        }

        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            i += 1;
            continue;
        }

        if let Some((key, value)) = split_kv(trimmed) {
            match key {
                "version" => {
                    version = Some(value.to_string());
                    i += 1;
                }
                "description" => {
                    description = Some(value.to_string());
                    i += 1;
                }
                "security" => {
                    security_level = Some(
                        SecurityLevel::from_str(value).ok_or_else(|| {
                            K9Error::UnknownSecurityLevel(value.to_string())
                        })?,
                    );
                    i += 1;
                }
                "pedigree" => {
                    i += 1;
                    let (ped, next_i) = parse_pedigree(lines, i)?;
                    pedigree = Some(ped);
                    i = next_i;
                }
                "recipe" => {
                    i += 1;
                    let (rec, next_i) = parse_recipe(lines, i)?;
                    recipe = Some(rec);
                    i = next_i;
                }
                "contract" => {
                    let contract_name = value.to_string();
                    i += 1;
                    let (contract, next_i) = parse_contract(&contract_name, lines, i)?;
                    contracts.push(contract);
                    i = next_i;
                }
                _ => {
                    metadata.push((key.to_string(), value.to_string()));
                    i += 1;
                }
            }
        } else {
            i += 1;
        }
    }

    let version = version.ok_or_else(|| {
        K9Error::parse(start, 1, format!("component '{}' missing 'version'", name))
    })?;
    let pedigree = pedigree.ok_or_else(|| {
        K9Error::parse(start, 1, format!("component '{}' missing 'pedigree'", name))
    })?;
    let security_level = security_level.ok_or_else(|| {
        K9Error::parse(start, 1, format!("component '{}' missing 'security'", name))
    })?;

    let mut comp = Component::new(name, version, pedigree, security_level);
    comp.description = description;
    comp.recipe = recipe;
    comp.contracts = contracts;
    comp.metadata = metadata;

    Ok((comp, i))
}

/// Parse a pedigree sub-block.
fn parse_pedigree(lines: &[&str], start: usize) -> Result<(Pedigree, usize)> {
    let mut origin: Option<String> = None;
    let mut author: Option<String> = None;
    let mut license: Option<String> = None;
    let mut commit: Option<String> = None;
    let mut i = start;

    let base_indent = if i < lines.len() {
        indent_level(lines[i])
    } else {
        0
    };

    while i < lines.len() {
        let line = lines[i];
        let trimmed = line.trim();

        if trimmed.is_empty() {
            i += 1;
            continue;
        }

        if indent_level(line) < base_indent {
            break;
        }

        if let Some((key, value)) = split_kv(trimmed) {
            match key {
                "origin" => origin = Some(value.to_string()),
                "author" => author = Some(value.to_string()),
                "license" => license = Some(value.to_string()),
                "commit" => commit = Some(value.to_string()),
                _ => {} // Ignore unknown pedigree fields for forward compatibility.
            }
        }
        i += 1;
    }

    let origin = origin.ok_or_else(|| K9Error::parse(start, 1, "pedigree missing 'origin'"))?;
    let author = author.ok_or_else(|| K9Error::parse(start, 1, "pedigree missing 'author'"))?;

    let mut ped = Pedigree::new(origin, author);
    ped.license = license;
    ped.commit = commit;
    Ok((ped, i))
}

/// Parse a recipe sub-block.
fn parse_recipe(lines: &[&str], start: usize) -> Result<(Recipe, usize)> {
    let mut tool: Option<String> = None;
    let mut command: Option<String> = None;
    let mut requires: Vec<String> = Vec::new();
    let mut outputs: Vec<String> = Vec::new();
    let mut i = start;

    let base_indent = if i < lines.len() {
        indent_level(lines[i])
    } else {
        0
    };

    while i < lines.len() {
        let line = lines[i];
        let trimmed = line.trim();

        if trimmed.is_empty() {
            i += 1;
            continue;
        }

        if indent_level(line) < base_indent {
            break;
        }

        if let Some((key, value)) = split_kv(trimmed) {
            match key {
                "tool" => tool = Some(value.to_string()),
                "command" => command = Some(value.to_string()),
                "requires" => requires.push(value.to_string()),
                "output" | "outputs" => outputs.push(value.to_string()),
                _ => {}
            }
        }
        i += 1;
    }

    let tool = tool.ok_or_else(|| K9Error::parse(start, 1, "recipe missing 'tool'"))?;
    let command =
        command.ok_or_else(|| K9Error::parse(start, 1, "recipe missing 'command'"))?;

    let mut rec = Recipe::new(tool, command);
    rec.requires = requires;
    rec.outputs = outputs;
    Ok((rec, i))
}

/// Parse a contract sub-block.
fn parse_contract(name: &str, lines: &[&str], start: usize) -> Result<(Contract, usize)> {
    let mut description: Option<String> = None;
    let mut check: Option<String> = None;
    let mut severity = "error".to_string();
    let mut i = start;

    let base_indent = if i < lines.len() {
        indent_level(lines[i])
    } else {
        0
    };

    while i < lines.len() {
        let line = lines[i];
        let trimmed = line.trim();

        if trimmed.is_empty() {
            i += 1;
            continue;
        }

        if indent_level(line) < base_indent {
            break;
        }

        if let Some((key, value)) = split_kv(trimmed) {
            match key {
                "description" => description = Some(value.to_string()),
                "check" => check = Some(value.to_string()),
                "severity" => severity = value.to_string(),
                _ => {}
            }
        }
        i += 1;
    }

    let description = description.unwrap_or_default();
    let check =
        check.ok_or_else(|| K9Error::parse(start, 1, format!("contract '{}' missing 'check'", name)))?;

    let mut contract = Contract::new(name, description, check);
    contract.severity = severity;
    Ok((contract, i))
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Split a `key: value` line into its components.
fn split_kv(line: &str) -> Option<(&str, &str)> {
    let (key, rest) = line.split_once(':')?;
    Some((key.trim(), rest.trim()))
}

/// Count the leading whitespace (spaces) of a line.
fn indent_level(line: &str) -> usize {
    line.len() - line.trim_start().len()
}

/// Heuristic check for Nickel-format K9 content.
fn is_nickel_format(input: &str) -> bool {
    // Nickel files typically start with `let` bindings or contain `{` at the
    // top level outside of string values.
    let first_significant = input
        .lines()
        .map(|l| l.trim())
        .find(|l| !l.is_empty() && !l.starts_with('#'));

    if let Some(line) = first_significant {
        line.starts_with("let ") || line.starts_with('{')
    } else {
        false
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty_input() {
        let components = parse("").unwrap();
        assert!(components.is_empty());
    }

    #[test]
    fn parse_minimal_component() {
        let input = r#"component: test-svc
  version: 1.0.0
  pedigree:
    origin: https://github.com/example/test
    author: Alice
  security: kennel
"#;
        let components = parse(input).unwrap();
        assert_eq!(components.len(), 1);
        assert_eq!(components[0].name, "test-svc");
        assert_eq!(components[0].version, "1.0.0");
        assert_eq!(components[0].security_level, SecurityLevel::Kennel);
        assert_eq!(components[0].pedigree.author, "Alice");
    }

    #[test]
    fn parse_with_recipe_and_contract() {
        let input = r#"component: my-lib
  version: 0.2.0
  pedigree:
    origin: https://github.com/example/lib
    author: Bob
    license: MPL-2.0
  security: yard
  recipe:
    tool: cargo
    command: cargo build --release
  contract: no-unsafe
    description: Forbid unsafe blocks
    check: cargo clippy -- -D unsafe-code
    severity: error
"#;
        let components = parse(input).unwrap();
        assert_eq!(components.len(), 1);
        let c = &components[0];
        assert!(c.recipe.is_some());
        assert_eq!(c.recipe.as_ref().unwrap().tool, "cargo");
        assert_eq!(c.contracts.len(), 1);
        assert_eq!(c.contracts[0].name, "no-unsafe");
    }

    #[test]
    fn detect_nickel_format() {
        let input = "let config = { name = \"test\" } in config";
        let result = parse(input);
        assert!(matches!(result, Err(K9Error::NickelFormat(_))));
    }

    #[test]
    fn missing_required_field_errors() {
        let input = r#"component: incomplete
  version: 1.0.0
  security: hunt
"#;
        // Missing pedigree.
        assert!(parse(input).is_err());
    }

    #[test]
    fn unknown_security_level_errors() {
        let input = r#"component: bad
  version: 1.0.0
  pedigree:
    origin: https://example.com
    author: X
  security: fortress
"#;
        let result = parse(input);
        assert!(matches!(result, Err(K9Error::UnknownSecurityLevel(_))));
    }
}
