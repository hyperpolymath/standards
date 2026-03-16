// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for crates.io)

//! Renderer that serialises K9 data types back to K9 text format.
//!
//! The [`render`] function takes a slice of [`Component`]s and produces a
//! string in the canonical K9 text representation, suitable for writing
//! to a `.k9` file.

use crate::error::Result;
use crate::types::*;

/// Render a list of [`Component`]s to their canonical K9 text representation.
///
/// # Errors
///
/// Returns [`K9Error::RenderError`] if any element cannot be serialised
/// (currently infallible, but the signature allows future extension).
///
/// # Examples
///
/// ```
/// use k9_svc::types::*;
/// use k9_svc::renderer::render;
///
/// let comp = Component::new(
///     "example",
///     "1.0.0",
///     Pedigree::new("https://github.com/foo/bar", "Alice"),
///     SecurityLevel::Kennel,
/// );
/// let output = render(&[comp]).unwrap();
/// assert!(output.contains("component: example"));
/// ```
pub fn render(components: &[Component]) -> Result<String> {
    let mut out = String::new();

    for (idx, comp) in components.iter().enumerate() {
        render_component(comp, &mut out);
        if idx + 1 < components.len() {
            out.push('\n');
        }
    }

    Ok(out)
}

// ---------------------------------------------------------------------------
// Component rendering
// ---------------------------------------------------------------------------

/// Render a single component to the output buffer.
fn render_component(comp: &Component, out: &mut String) {
    out.push_str(&format!("component: {}\n", comp.name));
    out.push_str(&format!("  version: {}\n", comp.version));

    if let Some(desc) = &comp.description {
        out.push_str(&format!("  description: {}\n", desc));
    }

    // Pedigree.
    render_pedigree(&comp.pedigree, out);

    // Security level.
    out.push_str(&format!("  security: {}\n", comp.security_level));

    // Recipe.
    if let Some(recipe) = &comp.recipe {
        render_recipe(recipe, out);
    }

    // Contracts.
    for contract in &comp.contracts {
        render_contract(contract, out);
    }

    // Metadata.
    for (key, value) in &comp.metadata {
        out.push_str(&format!("  {}: {}\n", key, value));
    }
}

/// Render pedigree sub-block.
fn render_pedigree(ped: &Pedigree, out: &mut String) {
    out.push_str("  pedigree:\n");
    out.push_str(&format!("    origin: {}\n", ped.origin));
    out.push_str(&format!("    author: {}\n", ped.author));
    if let Some(license) = &ped.license {
        out.push_str(&format!("    license: {}\n", license));
    }
    if let Some(commit) = &ped.commit {
        out.push_str(&format!("    commit: {}\n", commit));
    }
}

/// Render recipe sub-block.
fn render_recipe(recipe: &Recipe, out: &mut String) {
    out.push_str("  recipe:\n");
    out.push_str(&format!("    tool: {}\n", recipe.tool));
    out.push_str(&format!("    command: {}\n", recipe.command));
    for req in &recipe.requires {
        out.push_str(&format!("    requires: {}\n", req));
    }
    for output_name in &recipe.outputs {
        out.push_str(&format!("    output: {}\n", output_name));
    }
}

/// Render contract sub-block.
fn render_contract(contract: &Contract, out: &mut String) {
    out.push_str(&format!("  contract: {}\n", contract.name));
    out.push_str(&format!("    description: {}\n", contract.description));
    out.push_str(&format!("    check: {}\n", contract.check));
    out.push_str(&format!("    severity: {}\n", contract.severity));
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn render_minimal_component() {
        let comp = Component::new(
            "test-svc",
            "0.1.0",
            Pedigree::new("https://github.com/example/test", "Alice"),
            SecurityLevel::Yard,
        );
        let output = render(&[comp]).unwrap();
        assert!(output.contains("component: test-svc"));
        assert!(output.contains("  version: 0.1.0"));
        assert!(output.contains("  security: yard"));
        assert!(output.contains("    origin: https://github.com/example/test"));
    }

    #[test]
    fn render_with_recipe() {
        let mut comp = Component::new(
            "builder",
            "1.0.0",
            Pedigree::new("https://example.com", "Bob"),
            SecurityLevel::Hunt,
        );
        comp.recipe = Some(Recipe::new("cargo", "cargo build --release"));
        let output = render(&[comp]).unwrap();
        assert!(output.contains("  recipe:"));
        assert!(output.contains("    tool: cargo"));
    }

    #[test]
    fn render_multiple_components() {
        let c1 = Component::new(
            "alpha",
            "1.0.0",
            Pedigree::new("https://example.com/a", "A"),
            SecurityLevel::Kennel,
        );
        let c2 = Component::new(
            "beta",
            "2.0.0",
            Pedigree::new("https://example.com/b", "B"),
            SecurityLevel::Yard,
        );
        let output = render(&[c1, c2]).unwrap();
        assert!(output.contains("component: alpha"));
        assert!(output.contains("component: beta"));
    }
}
