// SPDX-License-Identifier: MPL-2.0
//
// hover.rs — K9 hover documentation provider
//
// Provides contextual documentation when the user hovers over:
//   1. Security levels ('Kennel, 'Yard, 'Hunt) — what each permits
//   2. Pedigree fields (name, version, description, etc.)
//   3. Contract type names (String, Bool, SecurityLevel, etc.)

use tower_lsp::lsp_types::*;

/// Extract the word under the cursor at the given position.
///
/// A "word" here includes alphanumeric characters, underscores, and a
/// leading apostrophe (for Nickel enum tags like 'Kennel).
fn word_at(text: &str, position: Position) -> Option<String> {
    let line = text.lines().nth(position.line as usize)?;
    let col = position.character as usize;

    if col > line.len() {
        return None;
    }

    let chars: Vec<char> = line.chars().collect();

    // Find word boundaries around the cursor.
    let mut start = col;
    while start > 0 {
        let prev = chars[start - 1];
        if prev.is_alphanumeric() || prev == '_' || prev == '\'' {
            start -= 1;
        } else {
            break;
        }
    }

    let mut end = col;
    while end < chars.len() {
        let ch = chars[end];
        if ch.is_alphanumeric() || ch == '_' {
            end += 1;
        } else {
            break;
        }
    }

    if start == end {
        return None;
    }

    Some(chars[start..end].iter().collect())
}

/// Return hover documentation for the word at the given position.
///
/// Matches against known K9 security levels, pedigree fields, and
/// contract types to provide relevant documentation.
pub fn hover_at(text: &str, position: Position) -> Option<Hover> {
    let word = word_at(text, position)?;
    let docs = match word.as_str() {
        // ─── Security Levels ─────────────────────────────────────
        "'Kennel" | "Kennel" => Some((
            "Security Level: Kennel (Pure Data)",
            "**`'Kennel`** — Pure Data\n\n\
             The safest execution level. No code execution is permitted.\n\n\
             | Permission | Allowed |\n\
             |---|---|\n\
             | Read data | Yes |\n\
             | Nickel evaluation | No |\n\
             | Filesystem write | No |\n\
             | Network access | No |\n\
             | Subprocess spawning | No |\n\n\
             Use `'Kennel` for configuration files, data manifests, and any \
             component that should be safe to open on constrained environments \
             (Edge devices, ASICs).\n\n\
             _See: SPEC.adoc § Security Model — The Leash System_",
        )),
        "'Yard" | "Yard" => Some((
            "Security Level: Yard (Validation Only)",
            "**`'Yard`** — Validation Only\n\n\
             Permits Nickel contract evaluation for type checking and validation, \
             but no side effects.\n\n\
             | Permission | Allowed |\n\
             |---|---|\n\
             | Read data | Yes |\n\
             | Nickel evaluation | Yes |\n\
             | Filesystem write | No |\n\
             | Network access | No |\n\
             | Subprocess spawning | No |\n\n\
             Use `'Yard` for components that need self-validation via Nickel \
             contracts but should not perform I/O operations. This is the default \
             security level.\n\n\
             _See: SPEC.adoc § Security Model — The Leash System_",
        )),
        "'Hunt" | "Hunt" => Some((
            "Security Level: Hunt (Full Execution)",
            "**`'Hunt`** — Full Execution\n\n\
             Grants complete must-just-nickel triad execution. **Requires a valid \
             cryptographic handshake** via the `signature` field.\n\n\
             | Permission | Allowed |\n\
             |---|---|\n\
             | Read data | Yes |\n\
             | Nickel evaluation | Yes |\n\
             | Filesystem write | Yes (if allow_filesystem_write = true) |\n\
             | Network access | Yes (if allow_network = true) |\n\
             | Subprocess spawning | Yes (if allow_subprocess = true) |\n\n\
             Use `'Hunt` for deployment components that need to execute Just \
             recipes, spawn containers, or modify the filesystem. Always pair \
             with a `signature` field and explicit permission flags.\n\n\
             _See: SPEC.adoc § Security Model — The Leash System_",
        )),

        // ─── Pedigree Fields ─────────────────────────────────────
        "name" => Some((
            "Pedigree Field: name",
            "**`name`** — Component Name\n\n\
             The unique identifier for this K9 component. Must be lowercase \
             and hyphenated (e.g. `my-component`).\n\n\
             **Required:** Yes\n\
             **Type:** `String`\n\n\
             _See: pedigree.ncl § Metadata_",
        )),
        "version" => Some((
            "Pedigree Field: version",
            "**`version`** — Component Version\n\n\
             SemVer version string with optional stability suffix \
             (e.g. `1.0.0-alpha`, `2.3.1`).\n\n\
             **Required:** Yes (defaults to `1.0.0-alpha`)\n\
             **Type:** `String`\n\n\
             _See: pedigree.ncl § Metadata_",
        )),
        "description" => Some((
            "Pedigree Field: description",
            "**`description`** — Component Description\n\n\
             Human-readable purpose of this component.\n\n\
             **Required:** No (optional)\n\
             **Type:** `String`\n\n\
             _See: pedigree.ncl § Metadata_",
        )),
        "breed" => Some((
            "Pedigree Field: breed",
            "**`breed`** — MIME Type Identifier\n\n\
             The MIME type for this component. Defaults to \
             `application/vnd.k9+nickel`.\n\n\
             **Required:** No (has default)\n\
             **Type:** `String`\n\n\
             _See: pedigree.ncl § Metadata_",
        )),
        "magic_number" => Some((
            "Pedigree Field: magic_number",
            "**`magic_number`** — Binary Signature\n\n\
             The magic number for kernel-level file identification. \
             Must be `K9!` (bytes `0x4B 0x39 0x21`).\n\n\
             **Required:** No (defaults to `K9!`)\n\
             **Type:** `String`\n\n\
             _See: SPEC.adoc § L1: The Scent (Identity)_",
        )),
        "trust_level" => Some((
            "Security Field: trust_level",
            "**`trust_level`** — Execution Permission Tier\n\n\
             Controls what operations this component is allowed to perform.\n\n\
             | Value | Meaning |\n\
             |---|---|\n\
             | `'Kennel` | Pure data, no execution |\n\
             | `'Yard` | Nickel evaluation only (default) |\n\
             | `'Hunt` | Full triad execution (requires signature) |\n\n\
             **Type:** `SecurityLevel` (`[| 'Kennel, 'Yard, 'Hunt |]`)\n\n\
             _See: pedigree.ncl § Security_",
        )),
        "allow_network" => Some((
            "Security Field: allow_network",
            "**`allow_network`** — Network Access Permission\n\n\
             Whether the component can fetch external resources. \
             Only meaningful at `'Hunt` level.\n\n\
             **Default:** `false`\n\
             **Type:** `Bool`\n\n\
             _See: pedigree.ncl § Security_",
        )),
        "allow_filesystem_write" => Some((
            "Security Field: allow_filesystem_write",
            "**`allow_filesystem_write`** — Filesystem Write Permission\n\n\
             Whether the component can modify the host filesystem. \
             Only meaningful at `'Hunt` level.\n\n\
             **Default:** `false`\n\
             **Type:** `Bool`\n\n\
             _See: pedigree.ncl § Security_",
        )),
        "allow_subprocess" => Some((
            "Security Field: allow_subprocess",
            "**`allow_subprocess`** — Subprocess Spawning Permission\n\n\
             Whether the component can spawn child processes. \
             Typically required for `'Hunt` level components that execute \
             Just recipes or Podman containers.\n\n\
             **Default:** `false`\n\
             **Type:** `Bool`\n\n\
             _See: pedigree.ncl § Security_",
        )),
        "signature" => Some((
            "Security Field: signature",
            "**`signature`** — Cryptographic Handshake\n\n\
             Required for `'Hunt` level components. Contains a cryptographic \
             signature used to verify the component's authenticity before \
             granting full execution permissions.\n\n\
             **Required:** Yes (for `'Hunt` level)\n\
             **Type:** `String` (optional)\n\n\
             _See: pedigree.ncl § Security_",
        )),
        "checksum" => Some((
            "Validation Field: checksum",
            "**`checksum`** — Payload Integrity Hash\n\n\
             SHA256 hash of the component payload for integrity verification.\n\n\
             **Required:** Yes\n\
             **Type:** `String`\n\n\
             _See: pedigree.ncl § Validation_",
        )),
        "hunt_authorized" => Some((
            "Validation Field: hunt_authorized",
            "**`hunt_authorized`** — Hunt Execution Authorization\n\n\
             Computed field indicating whether this component's `'Hunt` level \
             execution has been authorized via a valid signature.\n\n\
             **Default:** `false`\n\
             **Type:** `Bool`\n\n\
             _See: pedigree.ncl § Validation_",
        )),

        // ─── Contract Types ─────────────────────────────────────
        "SecurityLevel" => Some((
            "Contract: SecurityLevel",
            "**`SecurityLevel`** — Enum Contract\n\n\
             ```nickel\n\
             let SecurityLevel = [| 'Kennel, 'Yard, 'Hunt |] in\n\
             ```\n\n\
             Defines the three execution permission tiers for K9 components.\n\n\
             _See: pedigree.ncl_",
        )),
        "Architecture" => Some((
            "Contract: Architecture",
            "**`Architecture`** — Enum Contract\n\n\
             ```nickel\n\
             let Architecture = [| 'Linux, 'Minix, 'MacOS, 'Android, 'PC, 'ASIC, 'Unknown |] in\n\
             ```\n\n\
             Defines the target deployment architectures supported by K9.\n\n\
             _See: pedigree.ncl_",
        )),
        "K9Pedigree" => Some((
            "Contract: K9Pedigree",
            "**`K9Pedigree`** — Root Contract\n\n\
             ```nickel\n\
             K9Pedigree = {\n\
             \x20 metadata | Metadata,\n\
             \x20 target | Target,\n\
             \x20 security | Security,\n\
             \x20 validation | Validation,\n\
             \x20 recipes | Recipes,\n\
             }\n\
             ```\n\n\
             The top-level contract that every `.k9` component must satisfy. \
             Combines all five pedigree sections.\n\n\
             _See: pedigree.ncl § K9Pedigree_",
        )),
        "Metadata" => Some((
            "Contract: Metadata",
            "**`Metadata`** — Identity Section\n\n\
             Contains component identification fields: `name`, `version`, \
             `breed`, `magic_number`, and `description`.\n\n\
             _See: pedigree.ncl § Metadata_",
        )),
        "Target" => Some((
            "Contract: Target",
            "**`Target`** — Environment Section\n\n\
             Specifies the deployment target: `os` (Architecture), `is_edge`, \
             `requires_podman`, and `min_memory_mb`.\n\n\
             _See: pedigree.ncl § Target_",
        )),
        "Security" => Some((
            "Contract: Security",
            "**`Security`** — Leash Section\n\n\
             Defines the security posture: `trust_level`, `allow_network`, \
             `allow_filesystem_write`, `allow_subprocess`, and `signature`.\n\n\
             _See: pedigree.ncl § Security_",
        )),
        "Validation" => Some((
            "Contract: Validation",
            "**`Validation`** — Self-Check Section\n\n\
             Contains integrity verification fields: `checksum`, \
             `pedigree_version`, and `hunt_authorized`.\n\n\
             _See: pedigree.ncl § Validation_",
        )),
        "Recipes" => Some((
            "Contract: Recipes",
            "**`Recipes`** — Deployment Section\n\n\
             Defines Just recipe commands: `install`, `validate`, `deploy`, \
             and `migrate`.\n\n\
             _See: pedigree.ncl § Recipes_",
        )),

        _ => None,
    };

    docs.map(|(label, content)| Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("### {}\n\n{}", label, content),
        }),
        range: None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hover_kennel() {
        let text = "trust_level = 'Kennel\n";
        let hover = hover_at(text, Position::new(0, 16));
        assert!(hover.is_some());
    }

    #[test]
    fn test_hover_yard() {
        let text = "trust_level = 'Yard\n";
        let hover = hover_at(text, Position::new(0, 16));
        assert!(hover.is_some());
    }

    #[test]
    fn test_hover_hunt() {
        let text = "trust_level = 'Hunt\n";
        let hover = hover_at(text, Position::new(0, 16));
        assert!(hover.is_some());
    }

    #[test]
    fn test_hover_pedigree_field() {
        let text = "name = \"hello\"\n";
        let hover = hover_at(text, Position::new(0, 2));
        assert!(hover.is_some());
    }

    #[test]
    fn test_hover_contract_type() {
        let text = "metadata | Metadata,\n";
        let hover = hover_at(text, Position::new(0, 14));
        assert!(hover.is_some());
    }

    #[test]
    fn test_hover_unknown_word() {
        let text = "foobar = 42\n";
        let hover = hover_at(text, Position::new(0, 3));
        assert!(hover.is_none());
    }

    #[test]
    fn test_word_at_basic() {
        let text = "trust_level = 'Hunt\n";
        assert_eq!(word_at(text, Position::new(0, 3)), Some("trust_level".to_string()));
        assert_eq!(word_at(text, Position::new(0, 16)), Some("'Hunt".to_string()));
    }
}
