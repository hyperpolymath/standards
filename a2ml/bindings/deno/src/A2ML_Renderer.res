// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// A2ML_Renderer — Render A2ML AST back to A2ML surface syntax.
//
// Converts the typed AST from A2ML_Types into A2ML text format,
// preserving structure and formatting conventions. Produces output
// compatible with the A2ML parser for round-trip fidelity.

open A2ML_Types

// ---------------------------------------------------------------------------
// Inline rendering
// ---------------------------------------------------------------------------

/// Render a single inline element to A2ML text.
let rec renderInline = (inl: inline): string => {
  switch inl {
  | Text(t) => t
  | Emphasis(children) => "*" ++ renderInlines(children) ++ "*"
  | Strong(children) => "**" ++ renderInlines(children) ++ "**"
  | Code(c) => "`" ++ c ++ "`"
  | Link({content, url}) => "[" ++ renderInlines(content) ++ "](" ++ url ++ ")"
  | InlineRef(refId) => "@ref(" ++ refId ++ ")"
  }
}

/// Render a list of inline elements to text.
and renderInlines = (inlines: array<inline>): string => {
  inlines->Array.map(renderInline)->Array.join("")
}

// ---------------------------------------------------------------------------
// Directive rendering
// ---------------------------------------------------------------------------

/// Render a directive to A2ML surface syntax.
/// Single-line directives use `@name: value` format.
/// Multi-line directives use `@name:\n...\n@end` format.
let renderDirective = (dir: directive): string => {
  let attrStr = if dir.attributes->Array.length > 0 {
    let pairs =
      dir.attributes
      ->Array.map(((k, v)) => k ++ "=" ++ v)
      ->Array.join(", ")
    "(" ++ pairs ++ ")"
  } else {
    ""
  }

  let hasNewlines = dir.value->String.includes("\n")
  if hasNewlines {
    "@" ++ dir.name ++ attrStr ++ ":\n" ++ dir.value ++ "\n@end"
  } else {
    "@" ++ dir.name ++ attrStr ++ ": " ++ dir.value
  }
}

// ---------------------------------------------------------------------------
// Attestation rendering
// ---------------------------------------------------------------------------

/// Render an attestation block to A2ML surface syntax.
let renderAttestation = (att: attestation): string => {
  let lines = [
    "!attest",
    "identity: " ++ att.identity,
    "role: " ++ att.role,
    "trust-level: " ++ trustLevelToString(att.trustLevel),
  ]

  switch att.timestamp {
  | Some(ts) => lines->Array.push("timestamp: " ++ ts)->ignore
  | None => ()
  }

  switch att.note {
  | Some(n) => lines->Array.push("note: " ++ n)->ignore
  | None => ()
  }

  lines->Array.push("!end")->ignore
  lines->Array.join("\n")
}

// ---------------------------------------------------------------------------
// Block rendering
// ---------------------------------------------------------------------------

/// Render a single block to A2ML text.
let rec renderBlock = (blk: block): string => {
  switch blk {
  | Heading({level, content}) =>
    let hashes = Array.make(~length=level, "#")->Array.join("")
    hashes ++ " " ++ renderInlines(content)
  | Paragraph(inlines) => renderInlines(inlines)
  | CodeBlock({language, content}) =>
    let langTag = switch language {
    | Some(l) => l
    | None => ""
    }
    "```" ++ langTag ++ "\n" ++ content ++ "\n```"
  | DirectiveBlock(dir) => renderDirective(dir)
  | AttestationBlock(att) => renderAttestation(att)
  | ThematicBreak => "---"
  | BlockQuote(blocks) =>
    blocks->Array.map(b => "> " ++ renderBlock(b))->Array.join("\n")
  | BulletList(items) =>
    items->Array.map(inlines => "- " ++ renderInlines(inlines))->Array.join("\n")
  | BlankLine => ""
  }
}

// ---------------------------------------------------------------------------
// Document rendering
// ---------------------------------------------------------------------------

/// Render a complete A2ML document to text.
///
/// ### Example
/// ```
/// let doc = { title: Some("Hello"), directives: [], blocks: [...], attestations: [] }
/// let text = renderA2ML(doc)
/// ```
let renderA2ML = (doc: document): string => {
  doc.blocks->Array.map(renderBlock)->Array.join("\n") ++ "\n"
}
