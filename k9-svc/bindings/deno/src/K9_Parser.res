// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// K9_Parser — Parser for K9 self-validating component specifications.
//
// Parses the YAML-like .k9 format into the typed AST defined in K9_Types.
// The parser is line-oriented and extracts:
//   - Magic number (K9!)
//   - Pedigree metadata (name, version, description, author, license)
//   - Security level (Kennel/Yard/Hunt) with permission flags
//   - Target platform constraints
//   - Recipes and validation blocks
//   - Tags

open K9_Types

// ---------------------------------------------------------------------------
// Parsing helpers
// ---------------------------------------------------------------------------

/// Extract a key-value pair from a "  key: value" line.
/// Returns None if the line does not match the expected format.
let parseKeyValue = (line: string): option<(string, string)> => {
  let trimmed = line->String.trim
  let colonIdx = trimmed->String.indexOf(":")
  if colonIdx >= 0 {
    let key = trimmed->String.slice(~start=0, ~end=colonIdx)->String.trim
    let value = trimmed->String.sliceToEnd(~start=colonIdx + 1)->String.trim
    Some((key, value))
  } else {
    None
  }
}

/// Parse a boolean from a string ("true"/"false").
let parseBool = (s: string): bool => {
  s->String.trim->String.toLowerCase == "true"
}

/// Internal state for the line-oriented K9 parser.
type parserState = {
  mutable lineIndex: int,
  lines: array<string>,
}

/// Advance past blank lines and separator lines (---).
let skipBlanksAndSeparators = (state: parserState): unit => {
  let done = ref(false)
  while state.lineIndex < state.lines->Array.length && !done.contents {
    let line = state.lines->Array.getUnsafe(state.lineIndex)->String.trim
    if line->String.length == 0 || line == "---" {
      state.lineIndex = state.lineIndex + 1
    } else {
      done := true
    }
  }
}

/// Check if the current line matches a section header (e.g., "metadata:").
let isSectionHeader = (line: string, section: string): bool => {
  line->String.trim == section ++ ":"
}

// ---------------------------------------------------------------------------
// Section parsers
// ---------------------------------------------------------------------------

/// Parse the metadata/pedigree section.
/// Reads indented key-value pairs until a non-indented line or new section.
let parsePedigreeSection = (state: parserState): pedigree => {
  // Skip the "metadata:" header line
  state.lineIndex = state.lineIndex + 1

  let name = ref("")
  let version = ref("")
  let description = ref("")
  let author = ref(None)
  let license = ref(None)
  let done = ref(false)

  while state.lineIndex < state.lines->Array.length && !done.contents {
    let line = state.lines->Array.getUnsafe(state.lineIndex)
    let trimmed = line->String.trim
    // Indented lines belong to this section
    if line->String.startsWith("  ") && trimmed->String.length > 0 {
      switch parseKeyValue(trimmed) {
      | Some(("name", v)) => name := v
      | Some(("version", v)) => version := v
      | Some(("description", v)) => description := v
      | Some(("author", v)) => author := Some(v)
      | Some(("license", v)) => license := Some(v)
      | _ => () // Ignore unknown fields
      }
      state.lineIndex = state.lineIndex + 1
    } else {
      done := true
    }
  }

  {
    name: name.contents,
    version: version.contents,
    description: description.contents,
    author: author.contents,
    license: license.contents,
  }
}

/// Parse the security section.
let parseSecuritySection = (state: parserState): securityPolicy => {
  // Skip the "security:" header line
  state.lineIndex = state.lineIndex + 1

  let level = ref(Kennel)
  let allowNetwork = ref(false)
  let allowFsWrite = ref(false)
  let allowSubprocess = ref(false)
  let done = ref(false)

  while state.lineIndex < state.lines->Array.length && !done.contents {
    let line = state.lines->Array.getUnsafe(state.lineIndex)
    let trimmed = line->String.trim
    if line->String.startsWith("  ") && trimmed->String.length > 0 {
      switch parseKeyValue(trimmed) {
      | Some(("trust_level", v)) =>
        switch securityLevelFromString(v) {
        | Some(lvl) => level := lvl
        | None => () // Keep default
        }
      | Some(("allow_network", v)) => allowNetwork := parseBool(v)
      | Some(("allow_filesystem_write", v)) => allowFsWrite := parseBool(v)
      | Some(("allow_subprocess", v)) => allowSubprocess := parseBool(v)
      | _ => ()
      }
      state.lineIndex = state.lineIndex + 1
    } else {
      done := true
    }
  }

  {
    level: level.contents,
    allowNetwork: allowNetwork.contents,
    allowFsWrite: allowFsWrite.contents,
    allowSubprocess: allowSubprocess.contents,
  }
}

/// Parse the target section.
let parseTargetSection = (state: parserState): target => {
  // Skip the "target:" header line
  state.lineIndex = state.lineIndex + 1

  let os = ref(None)
  let isEdge = ref(false)
  let requiresPodman = ref(false)
  let memory = ref(None)
  let done = ref(false)

  while state.lineIndex < state.lines->Array.length && !done.contents {
    let line = state.lines->Array.getUnsafe(state.lineIndex)
    let trimmed = line->String.trim
    if line->String.startsWith("  ") && trimmed->String.length > 0 {
      switch parseKeyValue(trimmed) {
      | Some(("os", v)) => os := Some(v)
      | Some(("is_edge", v)) => isEdge := parseBool(v)
      | Some(("requires_podman", v)) => requiresPodman := parseBool(v)
      | Some(("memory", v)) => memory := Some(v)
      | _ => ()
      }
      state.lineIndex = state.lineIndex + 1
    } else {
      done := true
    }
  }

  {
    os: os.contents,
    isEdge: isEdge.contents,
    requiresPodman: requiresPodman.contents,
    memory: memory.contents,
  }
}

/// Parse the recipes section.
let parseRecipesSection = (state: parserState): recipes => {
  // Skip the "recipes:" header line
  state.lineIndex = state.lineIndex + 1

  let install = ref(None)
  let validate = ref(None)
  let deploy = ref(None)
  let migrate = ref(None)
  let custom = []
  let done = ref(false)

  while state.lineIndex < state.lines->Array.length && !done.contents {
    let line = state.lines->Array.getUnsafe(state.lineIndex)
    let trimmed = line->String.trim
    if line->String.startsWith("  ") && trimmed->String.length > 0 {
      switch parseKeyValue(trimmed) {
      | Some(("install", v)) => install := Some(v)
      | Some(("validate", v)) => validate := Some(v)
      | Some(("deploy", v)) => deploy := Some(v)
      | Some(("migrate", v)) => migrate := Some(v)
      | Some((k, v)) => custom->Array.push((k, v))->ignore
      | None => ()
      }
      state.lineIndex = state.lineIndex + 1
    } else {
      done := true
    }
  }

  {
    install: install.contents,
    validate: validate.contents,
    deploy: deploy.contents,
    migrate: migrate.contents,
    custom,
  }
}

/// Parse the validation section.
let parseValidationSection = (state: parserState): validation => {
  // Skip the "validation:" header line
  state.lineIndex = state.lineIndex + 1

  let checksum = ref("")
  let pedigreeVersion = ref("")
  let huntAuthorized = ref(false)
  let done = ref(false)

  while state.lineIndex < state.lines->Array.length && !done.contents {
    let line = state.lines->Array.getUnsafe(state.lineIndex)
    let trimmed = line->String.trim
    if line->String.startsWith("  ") && trimmed->String.length > 0 {
      switch parseKeyValue(trimmed) {
      | Some(("checksum", v)) => checksum := v
      | Some(("pedigree_version", v)) => pedigreeVersion := v
      | Some(("hunt_authorized", v)) => huntAuthorized := parseBool(v)
      | _ => ()
      }
      state.lineIndex = state.lineIndex + 1
    } else {
      done := true
    }
  }

  {
    checksum: checksum.contents,
    pedigreeVersion: pedigreeVersion.contents,
    huntAuthorized: huntAuthorized.contents,
  }
}

/// Parse the tags section.
let parseTagsSection = (state: parserState): array<string> => {
  // Skip the "tags:" header line
  state.lineIndex = state.lineIndex + 1

  let tags = []
  let done = ref(false)

  while state.lineIndex < state.lines->Array.length && !done.contents {
    let line = state.lines->Array.getUnsafe(state.lineIndex)
    let trimmed = line->String.trim
    if trimmed->String.startsWith("- ") {
      let tag = trimmed->String.sliceToEnd(~start=2)->String.trim
      tags->Array.push(tag)->ignore
      state.lineIndex = state.lineIndex + 1
    } else if line->String.startsWith("  ") && trimmed->String.length > 0 {
      // Also handle indented non-dash entries
      tags->Array.push(trimmed)->ignore
      state.lineIndex = state.lineIndex + 1
    } else {
      done := true
    }
  }

  tags
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Parse a K9 component specification from a string.
///
/// The input must start with the K9! magic number.
/// Returns either a parseError or the parsed component.
///
/// ### Example
/// ```
/// let result = parseK9("K9!\n---\nmetadata:\n  name: hello-k9\n  ...")
/// ```
let parseK9 = (input: string): result<component, parseError> => {
  let trimmed = input->String.trim
  if trimmed->String.length == 0 {
    Error(EmptyDocument)
  } else {
    let lines = input->String.split("\n")
    let state: parserState = {lineIndex: 0, lines}

    // Check for K9! magic number
    skipBlanksAndSeparators(state)
    if state.lineIndex >= lines->Array.length {
      Error(EmptyDocument)
    } else {
      let firstLine = lines->Array.getUnsafe(state.lineIndex)->String.trim
      if firstLine != "K9!" {
        Error(MissingMagicNumber)
      } else {
        state.lineIndex = state.lineIndex + 1
        skipBlanksAndSeparators(state)

        // Parse sections in order
        let pedigreeRef = ref(makePedigree(~name="", ~version="", ~description=""))
        let securityRef = ref(defaultSecurityPolicy(Kennel))
        let targetRef = ref(None)
        let recipesRef = ref(None)
        let validationRef = ref(None)
        let tagsRef = ref([])
        let contentRef = ref([])

        while state.lineIndex < lines->Array.length {
          let line = lines->Array.getUnsafe(state.lineIndex)->String.trim

          if line->String.length == 0 || line == "---" {
            state.lineIndex = state.lineIndex + 1
          } else if isSectionHeader(line, "metadata") {
            pedigreeRef := parsePedigreeSection(state)
          } else if isSectionHeader(line, "security") {
            securityRef := parseSecuritySection(state)
          } else if isSectionHeader(line, "target") {
            targetRef := Some(parseTargetSection(state))
          } else if isSectionHeader(line, "recipes") {
            recipesRef := Some(parseRecipesSection(state))
          } else if isSectionHeader(line, "validation") {
            validationRef := Some(parseValidationSection(state))
          } else if isSectionHeader(line, "tags") {
            tagsRef := parseTagsSection(state)
          } else {
            // Unknown key-value pair at root level — store as content
            switch parseKeyValue(line) {
            | Some((k, v)) =>
              contentRef.contents->Array.push((k, v))->ignore
            | None => ()
            }
            state.lineIndex = state.lineIndex + 1
          }
        }

        // Validate required pedigree fields
        let ped = pedigreeRef.contents
        if ped.name->String.length == 0 {
          Error(MissingPedigree("name"))
        } else if ped.version->String.length == 0 {
          Error(MissingPedigree("version"))
        } else if ped.description->String.length == 0 {
          Error(MissingPedigree("description"))
        } else {
          Ok({
            pedigree: ped,
            security: securityRef.contents,
            target: targetRef.contents,
            recipes: recipesRef.contents,
            validation: validationRef.contents,
            content: contentRef.contents,
            tags: tagsRef.contents,
          })
        }
      }
    }
  }
}

/// Parse a K9 component specification from a file path.
/// Uses Node.js fs.readFileSync for Deno compatibility.
@module("node:fs")
external readFileSync: (string, string) => string = "readFileSync"

let parseK9File = (path: string): result<component, parseError> => {
  let content = readFileSync(path, "utf-8")
  parseK9(content)
}

// ---------------------------------------------------------------------------
// Format detection
// ---------------------------------------------------------------------------

/// K9 file format variants.
type k9Format =
  | K9Yaml
  | K9Nickel

/// Detect whether a K9 file is YAML-like (.k9) or Nickel (.k9.ncl).
let detectFormat = (input: string): k9Format => {
  let trimmed = input->String.trim
  if trimmed->String.startsWith("K9!") {
    K9Yaml
  } else {
    K9Nickel
  }
}
