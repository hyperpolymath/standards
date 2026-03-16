// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// K9_Renderer — Render K9 AST back to K9 surface syntax.
//
// Converts the typed AST from K9_Types into the YAML-like .k9 format,
// including the K9! magic number, pedigree, security, target, recipes,
// validation, and tags sections.

open K9_Types

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Render a boolean as lowercase text ("true"/"false").
let renderBool = (b: bool): string => {
  if b {
    "true"
  } else {
    "false"
  }
}

/// Render an optional field. Returns an array containing one line if
/// the value is Some, or an empty array if None.
let renderOptional = (key: string, value: option<string>): array<string> => {
  switch value {
  | Some(v) => [key ++ ": " ++ v]
  | None => []
  }
}

// ---------------------------------------------------------------------------
// Section renderers
// ---------------------------------------------------------------------------

/// Render the pedigree/metadata section.
let renderPedigreeSection = (ped: pedigree): array<string> => {
  Array.concat(
    [
      "metadata:",
      "  name: " ++ ped.name,
      "  version: " ++ ped.version,
      "  description: " ++ ped.description,
    ],
    Array.concat(
      renderOptional("  author", ped.author),
      renderOptional("  license", ped.license),
    ),
  )
}

/// Render the security section.
let renderSecuritySection = (sec: securityPolicy): array<string> => {
  [
    "",
    "security:",
    "  trust_level: " ++ securityLevelToString(sec.level),
    "  allow_network: " ++ renderBool(sec.allowNetwork),
    "  allow_filesystem_write: " ++ renderBool(sec.allowFsWrite),
    "  allow_subprocess: " ++ renderBool(sec.allowSubprocess),
  ]
}

/// Render the target section if present.
let renderTargetSection = (tgt: option<target>): array<string> => {
  switch tgt {
  | None => []
  | Some(t) =>
    Array.concat(
      Array.concat(["", "target:"], renderOptional("  os", t.os)),
      Array.concat(
        [
          "  is_edge: " ++ renderBool(t.isEdge),
          "  requires_podman: " ++ renderBool(t.requiresPodman),
        ],
        renderOptional("  memory", t.memory),
      ),
    )
  }
}

/// Render the recipes section if present.
let renderRecipesSection = (rec_: option<recipes>): array<string> => {
  switch rec_ {
  | None => []
  | Some(r) =>
    let lines = ["", "recipes:"]
    let standard = Array.concat(
      Array.concat(
        renderOptional("  install", r.install),
        renderOptional("  validate", r.validate),
      ),
      Array.concat(
        renderOptional("  deploy", r.deploy),
        renderOptional("  migrate", r.migrate),
      ),
    )
    let customLines = r.custom->Array.map(((k, v)) => "  " ++ k ++ ": " ++ v)
    Array.concat(lines, Array.concat(standard, customLines))
  }
}

/// Render the validation section if present.
let renderValidationSection = (val_: option<validation>): array<string> => {
  switch val_ {
  | None => []
  | Some(v) => [
      "",
      "validation:",
      "  checksum: " ++ v.checksum,
      "  pedigree_version: " ++ v.pedigreeVersion,
      "  hunt_authorized: " ++ renderBool(v.huntAuthorized),
    ]
  }
}

/// Render the tags section if non-empty.
let renderTagsSection = (tags: array<string>): array<string> => {
  if tags->Array.length == 0 {
    []
  } else {
    let header = ["", "tags:"]
    let items = tags->Array.map(t => "  - " ++ t)
    Array.concat(header, items)
  }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Render a complete K9 component to the .k9 YAML-like format.
///
/// ### Example
/// ```
/// let text = renderK9(component)
/// // "K9!\n---\nmetadata:\n  name: hello-k9\n  ..."
/// ```
let renderK9 = (c: component): string => {
  let lines = Array.concat(
    ["K9!", "---"],
    Array.concat(
      renderPedigreeSection(c.pedigree),
      Array.concat(
        renderSecuritySection(c.security),
        Array.concat(
          renderTargetSection(c.target),
          Array.concat(
            renderRecipesSection(c.recipes),
            Array.concat(renderValidationSection(c.validation), renderTagsSection(c.tags)),
          ),
        ),
      ),
    ),
  )

  // Filter out empty strings that would create unwanted blank lines at the end
  let filtered = lines->Array.filter(l => l->String.length > 0 || l == "")
  filtered->Array.join("\n") ++ "\n"
}

/// Render a security level to its canonical text representation.
let renderSecurityLevel = securityLevelToString
