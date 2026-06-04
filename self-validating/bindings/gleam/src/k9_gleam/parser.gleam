// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)
//
// k9_gleam/parser — K9 component specification parser.
//
// Parses .k9 files (YAML-like at Kennel level) into the Component AST.
// K9 files use a simple key-value format with sections denoted by
// indentation and section headers ending with a colon.

import gleam/dict
import gleam/list
import gleam/result
import gleam/string

import k9_gleam/types.{
  type Component, type Pedigree, type Recipes, type SecurityLevel,
  type SecurityPolicy, type Target, type Validation, Component, Hunt, Kennel,
  Pedigree, Recipes, SecurityPolicy, Target, Validation, Yard,
}

/// Error type for parse failures.
pub type ParseError {
  /// The input was empty or contained only whitespace.
  EmptyInput
  /// A syntax error was encountered at the given line number.
  SyntaxError(line: Int, message: String)
  /// An unknown security level string was encountered.
  UnknownSecurityLevel(value: String)
  /// A required field is missing.
  MissingField(field: String)
}

/// Parse a .k9 file string into a Component.
///
/// Returns `Ok(Component)` on success or `Error(ParseError)` on failure.
pub fn parse(input: String) -> Result(Component, ParseError) {
  let trimmed = string.trim(input)
  case trimmed {
    "" -> Error(EmptyInput)
    _ -> parse_component(trimmed)
  }
}

/// Parse a security level from its canonical string representation.
///
/// Recognised values (case-insensitive): "kennel", "yard", "hunt".
pub fn parse_security_level(input: String) -> Result(SecurityLevel, ParseError) {
  case string.lowercase(string.trim(input)) {
    "kennel" -> Ok(Kennel)
    "yard" -> Ok(Yard)
    "hunt" -> Ok(Hunt)
    other -> Error(UnknownSecurityLevel(other))
  }
}

// ---------------------------------------------------------------------------
// Internal parsing
// ---------------------------------------------------------------------------

/// Parse a complete component from non-empty trimmed input.
fn parse_component(input: String) -> Result(Component, ParseError) {
  let lines = string.split(input, "\n")
  let sections = split_sections(lines)

  // Extract the pedigree section (required).
  let pedigree_fields = find_section(sections, "pedigree")
  use pedigree <- result.try(parse_pedigree(pedigree_fields))

  // Extract security section (required).
  let security_fields = find_section(sections, "security")
  use security <- result.try(parse_security(security_fields))

  // Extract optional sections.
  let target = parse_target(find_section(sections, "target"))
  let recipes = parse_recipes(find_section(sections, "recipes"))
  let validation = parse_validation(find_section(sections, "validation"))

  // Extract tags — may appear at top level or within the last section
  // because "tags: a, b" is parsed as a field (not a section header).
  let tags =
    sections
    |> list.flat_map(fn(section) {
      case find_field(section.1, "tags") {
        "" -> []
        tag_str ->
          tag_str
          |> string.split(",")
          |> list.map(string.trim)
          |> list.filter(fn(t) { t != "" })
      }
    })

  Ok(Component(
    pedigree: pedigree,
    security: security,
    target: target,
    recipes: recipes,
    validation: validation,
    content: dict.new(),
    tags: tags,
  ))
}

/// Split input lines into named sections.
/// Each section is a tuple of (section_name, field_lines).
fn split_sections(
  lines: List(String),
) -> List(#(String, List(#(String, String)))) {
  split_sections_acc(lines, "", [], [])
}

fn split_sections_acc(
  lines: List(String),
  current_section: String,
  current_fields: List(#(String, String)),
  acc: List(#(String, List(#(String, String)))),
) -> List(#(String, List(#(String, String)))) {
  case lines {
    [] -> {
      // Flush remaining section.
      let acc = case current_fields {
        [] -> acc
        _ -> [#(current_section, list.reverse(current_fields)), ..acc]
      }
      list.reverse(acc)
    }
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      case trimmed {
        // Skip blank lines and comments.
        "" -> split_sections_acc(rest, current_section, current_fields, acc)
        _ ->
          case string.starts_with(trimmed, "#") {
            True ->
              split_sections_acc(rest, current_section, current_fields, acc)
            False ->
              case is_section_header(line, trimmed) {
                // New section header (non-indented, ends with ":").
                True -> {
                  let section_name =
                    string.lowercase(
                      string.trim(string.drop_end(trimmed, 1)),
                    )
                  // Flush previous section.
                  let acc = case current_fields {
                    [] -> acc
                    _ -> [
                      #(current_section, list.reverse(current_fields)),
                      ..acc
                    ]
                  }
                  split_sections_acc(rest, section_name, [], acc)
                }
                // Field line (indented key: value).
                False -> {
                  case string.split_once(trimmed, ":") {
                    Ok(#(key, value)) -> {
                      let field = #(
                        string.lowercase(string.trim(key)),
                        string.trim(value),
                      )
                      split_sections_acc(
                        rest,
                        current_section,
                        [field, ..current_fields],
                        acc,
                      )
                    }
                    Error(_) ->
                      split_sections_acc(
                        rest,
                        current_section,
                        current_fields,
                        acc,
                      )
                  }
                }
              }
          }
      }
    }
  }
}

/// Check if a line is a section header (not indented, ends with ":").
fn is_section_header(raw_line: String, trimmed: String) -> Bool {
  case string.ends_with(trimmed, ":") {
    False -> False
    True -> {
      // Must not be indented.
      case
        string.starts_with(raw_line, " ")
        || string.starts_with(raw_line, "\t")
      {
        True -> False
        False -> {
          // Must not contain ":" in the middle (i.e., it's just "word:").
          let without_colon = string.drop_end(trimmed, 1)
          case string.contains(without_colon, ":") {
            True -> False
            False -> True
          }
        }
      }
    }
  }
}

/// Find a section by name, returning its fields or an empty list.
fn find_section(
  sections: List(#(String, List(#(String, String)))),
  name: String,
) -> List(#(String, String)) {
  sections
  |> list.find(fn(s) { s.0 == name })
  |> result.map(fn(s) { s.1 })
  |> result.unwrap([])
}

/// Find a field value by key in a list of fields.
fn find_field(fields: List(#(String, String)), key: String) -> String {
  fields
  |> list.find(fn(pair) { pair.0 == key })
  |> result.map(fn(pair) { pair.1 })
  |> result.unwrap("")
}

/// Parse the pedigree section into a Pedigree.
fn parse_pedigree(
  fields: List(#(String, String)),
) -> Result(Pedigree, ParseError) {
  let name = find_field(fields, "name")
  case name {
    "" -> Error(MissingField("pedigree.name"))
    _ -> {
      let version = case find_field(fields, "version") {
        "" -> "0.1.0"
        v -> v
      }
      let description = find_field(fields, "description")
      let author = case find_field(fields, "author") {
        "" -> Error(Nil)
        a -> Ok(a)
      }
      let license = case find_field(fields, "license") {
        "" -> Error(Nil)
        l -> Ok(l)
      }
      Ok(Pedigree(
        name: name,
        version: version,
        description: description,
        author: author,
        license: license,
      ))
    }
  }
}

/// Parse the security section into a SecurityPolicy.
fn parse_security(
  fields: List(#(String, String)),
) -> Result(SecurityPolicy, ParseError) {
  let level_str = find_field(fields, "level")
  use level <- result.try(case level_str {
    "" -> Ok(Kennel)
    s -> parse_security_level(s)
  })

  let allow_network = parse_bool_field(fields, "allow-network")
  let allow_fs_write = parse_bool_field(fields, "allow-fs-write")
  let allow_subprocess = parse_bool_field(fields, "allow-subprocess")

  Ok(SecurityPolicy(
    level: level,
    allow_network: allow_network,
    allow_fs_write: allow_fs_write,
    allow_subprocess: allow_subprocess,
  ))
}

/// Parse a boolean field, defaulting to False.
fn parse_bool_field(fields: List(#(String, String)), key: String) -> Bool {
  case string.lowercase(find_field(fields, key)) {
    "true" | "yes" -> True
    _ -> False
  }
}

/// Parse the optional target section.
fn parse_target(fields: List(#(String, String))) -> Result(Target, Nil) {
  case fields {
    [] -> Error(Nil)
    _ ->
      Ok(Target(
        os: case find_field(fields, "os") {
          "" -> Error(Nil)
          o -> Ok(o)
        },
        is_edge: parse_bool_field(fields, "edge"),
        requires_podman: parse_bool_field(fields, "requires-podman"),
        memory: case find_field(fields, "memory") {
          "" -> Error(Nil)
          m -> Ok(m)
        },
      ))
  }
}

/// Parse the optional recipes section.
fn parse_recipes(fields: List(#(String, String))) -> Result(Recipes, Nil) {
  case fields {
    [] -> Error(Nil)
    _ -> {
      let standard_keys = ["install", "validate", "deploy", "migrate"]
      let custom =
        fields
        |> list.filter(fn(pair) {
          !list.contains(standard_keys, pair.0) && pair.1 != ""
        })
        |> dict.from_list

      Ok(Recipes(
        install: case find_field(fields, "install") {
          "" -> Error(Nil)
          c -> Ok(c)
        },
        validate: case find_field(fields, "validate") {
          "" -> Error(Nil)
          c -> Ok(c)
        },
        deploy: case find_field(fields, "deploy") {
          "" -> Error(Nil)
          c -> Ok(c)
        },
        migrate: case find_field(fields, "migrate") {
          "" -> Error(Nil)
          c -> Ok(c)
        },
        custom: custom,
      ))
    }
  }
}

/// Parse the optional validation section.
fn parse_validation(
  fields: List(#(String, String)),
) -> Result(Validation, Nil) {
  case fields {
    [] -> Error(Nil)
    _ -> {
      let checksum = find_field(fields, "checksum")
      case checksum {
        "" -> Error(Nil)
        _ ->
          Ok(Validation(
            checksum: checksum,
            pedigree_version: case find_field(fields, "pedigree-version") {
              "" -> "1.0"
              v -> v
            },
            hunt_authorized: parse_bool_field(fields, "hunt-authorized"),
          ))
      }
    }
  }
}
