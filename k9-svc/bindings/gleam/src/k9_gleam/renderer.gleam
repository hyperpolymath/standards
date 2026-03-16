// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)
//
// k9_gleam/renderer — Render a K9 Component AST back to .k9 text.
//
// Produces K9-formatted output (YAML-like at Kennel level) from a
// parsed Component structure.

import gleam/dict
import gleam/list
import gleam/string

import k9_gleam/types.{
  type Component, type SecurityLevel, type SecurityPolicy, Hunt, Kennel, Yard,
}

/// Render a Component to K9-formatted text.
///
/// Produces a complete .k9 file string with pedigree, security,
/// and optional target/recipes/validation sections.
pub fn render(component: Component) -> String {
  let parts = []

  // Pedigree section.
  let parts = list.append(parts, render_pedigree(component))

  // Security section.
  let parts = list.append(parts, ["", ..render_security(component.security)])

  // Target section (optional).
  let parts = case component.target {
    Ok(target) -> list.append(parts, ["", ..render_target(target)])
    Error(_) -> parts
  }

  // Recipes section (optional).
  let parts = case component.recipes {
    Ok(recipes) -> list.append(parts, ["", ..render_recipes(recipes)])
    Error(_) -> parts
  }

  // Validation section (optional).
  let parts = case component.validation {
    Ok(v) -> list.append(parts, ["", ..render_validation(v)])
    Error(_) -> parts
  }

  // Tags (top-level).
  let parts = case component.tags {
    [] -> parts
    tags ->
      list.append(parts, ["", "tags: " <> string.join(tags, ", ")])
  }

  string.join(parts, "\n") <> "\n"
}

/// Render a SecurityLevel to its canonical string representation.
pub fn render_security_level(level: SecurityLevel) -> String {
  case level {
    Kennel -> "kennel"
    Yard -> "yard"
    Hunt -> "hunt"
  }
}

// ---------------------------------------------------------------------------
// Internal section renderers
// ---------------------------------------------------------------------------

/// Render the pedigree section.
fn render_pedigree(component: Component) -> List(String) {
  let p = component.pedigree
  let lines = [
    "pedigree:",
    "  name: " <> p.name,
    "  version: " <> p.version,
    "  description: " <> p.description,
  ]

  let lines = case p.author {
    Ok(a) -> list.append(lines, ["  author: " <> a])
    Error(_) -> lines
  }

  let lines = case p.license {
    Ok(l) -> list.append(lines, ["  license: " <> l])
    Error(_) -> lines
  }

  lines
}

/// Render the security section.
fn render_security(security: SecurityPolicy) -> List(String) {
  [
    "security:",
    "  level: " <> render_security_level(security.level),
    "  allow-network: " <> bool_to_string(security.allow_network),
    "  allow-fs-write: " <> bool_to_string(security.allow_fs_write),
    "  allow-subprocess: " <> bool_to_string(security.allow_subprocess),
  ]
}

/// Render the target section.
fn render_target(target: types.Target) -> List(String) {
  let lines = ["target:"]

  let lines = case target.os {
    Ok(os) -> list.append(lines, ["  os: " <> os])
    Error(_) -> lines
  }

  let lines =
    list.append(lines, [
      "  edge: " <> bool_to_string(target.is_edge),
      "  requires-podman: " <> bool_to_string(target.requires_podman),
    ])

  let lines = case target.memory {
    Ok(m) -> list.append(lines, ["  memory: " <> m])
    Error(_) -> lines
  }

  lines
}

/// Render the recipes section.
fn render_recipes(recipes: types.Recipes) -> List(String) {
  let lines = ["recipes:"]

  let lines = case recipes.install {
    Ok(c) -> list.append(lines, ["  install: " <> c])
    Error(_) -> lines
  }

  let lines = case recipes.validate {
    Ok(c) -> list.append(lines, ["  validate: " <> c])
    Error(_) -> lines
  }

  let lines = case recipes.deploy {
    Ok(c) -> list.append(lines, ["  deploy: " <> c])
    Error(_) -> lines
  }

  let lines = case recipes.migrate {
    Ok(c) -> list.append(lines, ["  migrate: " <> c])
    Error(_) -> lines
  }

  // Custom recipes.
  let custom_entries = dict.to_list(recipes.custom)
  let lines =
    list.fold(custom_entries, lines, fn(acc, entry) {
      list.append(acc, ["  " <> entry.0 <> ": " <> entry.1])
    })

  lines
}

/// Render the validation section.
fn render_validation(v: types.Validation) -> List(String) {
  [
    "validation:",
    "  checksum: " <> v.checksum,
    "  pedigree-version: " <> v.pedigree_version,
    "  hunt-authorized: " <> bool_to_string(v.hunt_authorized),
  ]
}

/// Convert a Bool to a lowercase string.
fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}
