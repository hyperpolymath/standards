// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)
//
// a2ml_gleam/parser — A2ML document parser.
//
// Parses A2ML-formatted text into a structured Document AST.
// A2ML syntax is similar to Markdown with extensions for directives (@)
// and attestation blocks (!attest).

import gleam/list
import gleam/result
import gleam/string

import a2ml_gleam/types.{
  type Attestation, type Block, type Directive, type Document, type Inline,
  type Manifest, type TrustLevel, Attestation, AttestationBlock, Automated,
  BlockQuote, CodeBlock, Directive, DirectiveBlock, Document, Emphasis, Heading,
  Link, ListBlock, Manifest, Paragraph, Reviewed, Strong, Text, ThematicBreak,
  Unverified, Verified,
}

/// Error type for parse failures.
pub type ParseError {
  /// The input was empty or contained only whitespace.
  EmptyInput
  /// A syntax error was encountered at the given line number.
  SyntaxError(line: Int, message: String)
  /// An unknown trust level string was encountered.
  UnknownTrustLevel(value: String)
}

/// Parse an A2ML-formatted string into a Document.
///
/// Returns `Ok(Document)` on success or `Error(ParseError)` on failure.
pub fn parse(input: String) -> Result(Document, ParseError) {
  let trimmed = string.trim(input)
  case trimmed {
    "" -> Error(EmptyInput)
    _ -> Ok(parse_document(trimmed))
  }
}

/// Parse a trust level from its canonical string representation.
///
/// Recognised values (case-insensitive): "unverified", "automated",
/// "reviewed", "verified".
pub fn parse_trust_level(input: String) -> Result(TrustLevel, ParseError) {
  case string.lowercase(string.trim(input)) {
    "unverified" -> Ok(Unverified)
    "automated" -> Ok(Automated)
    "reviewed" -> Ok(Reviewed)
    "verified" -> Ok(Verified)
    other -> Error(UnknownTrustLevel(other))
  }
}

/// Extract a Manifest from a parsed Document.
///
/// Collects the version directive (if any), title, directives, and
/// attestations into a convenient summary structure.
pub fn extract_manifest(doc: Document) -> Manifest {
  let version =
    doc.directives
    |> list.find(fn(d: Directive) { d.name == "version" })
    |> result.map(fn(d: Directive) { d.value })

  Manifest(
    version: version,
    title: doc.title,
    directives: doc.directives,
    attestations: doc.attestations,
  )
}

// ---------------------------------------------------------------------------
// Internal parsing helpers
// ---------------------------------------------------------------------------

/// Parse a full document from non-empty trimmed input.
fn parse_document(input: String) -> Document {
  let lines = string.split(input, "\n")
  let #(title, rest_lines) = extract_title(lines)
  let blocks = parse_blocks(rest_lines, [])

  // Separate directives and attestations from blocks.
  let directives = extract_directives(blocks)
  let attestations = extract_attestations(blocks)

  Document(
    title: title,
    directives: directives,
    blocks: blocks,
    attestations: attestations,
  )
}

/// Extract the title from the first line if it starts with "# ".
fn extract_title(lines: List(String)) -> #(Result(String, Nil), List(String)) {
  case lines {
    [first, ..rest] -> {
      let trimmed = string.trim(first)
      case string.starts_with(trimmed, "# ") {
        True -> #(Ok(string.drop_start(trimmed, 2)), rest)
        False -> #(Error(Nil), lines)
      }
    }
    [] -> #(Error(Nil), [])
  }
}

/// Parse lines into a list of Block elements.
fn parse_blocks(lines: List(String), acc: List(Block)) -> List(Block) {
  case lines {
    [] -> list.reverse(acc)
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      case trimmed {
        // Skip blank lines between blocks.
        "" -> parse_blocks(rest, acc)

        // Thematic break: three or more dashes.
        "---" | "----" | "-----" ->
          parse_blocks(rest, [ThematicBreak, ..acc])

        _ -> {
          case string.starts_with(trimmed, "```") {
            // Code block.
            True -> {
              let lang = string.trim(string.drop_start(trimmed, 3))
              let lang_result = case lang {
                "" -> Error(Nil)
                l -> Ok(l)
              }
              let #(code_lines, remaining) = collect_code_block(rest, [])
              let content = string.join(code_lines, "\n")
              let block = CodeBlock(language: lang_result, content: content)
              parse_blocks(remaining, [block, ..acc])
            }
            False ->
              case string.starts_with(trimmed, "@") {
                // Directive block.
                True -> {
                  let directive = parse_directive_line(trimmed)
                  parse_blocks(rest, [DirectiveBlock(directive), ..acc])
                }
                False ->
                  case string.starts_with(trimmed, "!attest") {
                    // Attestation block.
                    True -> {
                      let #(attest_lines, remaining) =
                        collect_indented_block(rest, [])
                      let attestation =
                        parse_attestation_block([trimmed, ..attest_lines])
                      parse_blocks(remaining, [
                        AttestationBlock(attestation),
                        ..acc
                      ])
                    }
                    False ->
                      case string.starts_with(trimmed, "#") {
                        // Heading.
                        True -> {
                          let #(level, text) = parse_heading_line(trimmed)
                          let inlines = parse_inlines(text)
                          parse_blocks(rest, [
                            Heading(level: level, content: inlines),
                            ..acc
                          ])
                        }
                        False ->
                          case string.starts_with(trimmed, ">") {
                            // Block quote.
                            True -> {
                              let #(quote_lines, remaining) =
                                collect_prefixed_block(lines, ">", [])
                              let stripped =
                                list.map(quote_lines, fn(l) {
                                  let t = string.trim(l)
                                  case string.starts_with(t, "> ") {
                                    True -> string.drop_start(t, 2)
                                    False ->
                                      case string.starts_with(t, ">") {
                                        True -> string.drop_start(t, 1)
                                        False -> t
                                      }
                                  }
                                })
                              let inner_blocks = parse_blocks(stripped, [])
                              parse_blocks(remaining, [
                                BlockQuote(inner_blocks),
                                ..acc
                              ])
                            }
                            False ->
                              case
                                string.starts_with(trimmed, "- ")
                                || string.starts_with(trimmed, "* ")
                              {
                                // Unordered list.
                                True -> {
                                  let #(list_lines, remaining) =
                                    collect_list_block(lines, [])
                                  let items = parse_list_items(list_lines, False)
                                  parse_blocks(remaining, [
                                    ListBlock(ordered: False, items: items),
                                    ..acc
                                  ])
                                }
                                False ->
                                  case is_ordered_list_start(trimmed) {
                                    // Ordered list.
                                    True -> {
                                      let #(list_lines, remaining) =
                                        collect_list_block(lines, [])
                                      let items =
                                        parse_list_items(list_lines, True)
                                      parse_blocks(remaining, [
                                        ListBlock(ordered: True, items: items),
                                        ..acc
                                      ])
                                    }
                                    // Paragraph (default).
                                    False -> {
                                      let #(para_lines, remaining) =
                                        collect_paragraph(lines, [])
                                      let text = string.join(para_lines, " ")
                                      let inlines = parse_inlines(text)
                                      parse_blocks(remaining, [
                                        Paragraph(inlines),
                                        ..acc
                                      ])
                                    }
                                  }
                              }
                          }
                      }
                  }
              }
          }
        }
      }
    }
  }
}

/// Collect lines until a closing ``` fence is found.
fn collect_code_block(
  lines: List(String),
  acc: List(String),
) -> #(List(String), List(String)) {
  case lines {
    [] -> #(list.reverse(acc), [])
    [line, ..rest] -> {
      case string.starts_with(string.trim(line), "```") {
        True -> #(list.reverse(acc), rest)
        False -> collect_code_block(rest, [line, ..acc])
      }
    }
  }
}

/// Collect lines that are indented (continuation of a block).
fn collect_indented_block(
  lines: List(String),
  acc: List(String),
) -> #(List(String), List(String)) {
  case lines {
    [] -> #(list.reverse(acc), [])
    [line, ..rest] -> {
      case string.starts_with(line, "  ") || string.starts_with(line, "\t") {
        True -> collect_indented_block(rest, [string.trim(line), ..acc])
        False -> #(list.reverse(acc), lines)
      }
    }
  }
}

/// Collect lines starting with a given prefix (e.g. ">").
fn collect_prefixed_block(
  lines: List(String),
  prefix: String,
  acc: List(String),
) -> #(List(String), List(String)) {
  case lines {
    [] -> #(list.reverse(acc), [])
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      case string.starts_with(trimmed, prefix) {
        True -> collect_prefixed_block(rest, prefix, [line, ..acc])
        False -> #(list.reverse(acc), lines)
      }
    }
  }
}

/// Collect contiguous list lines (starting with - / * or indented continuations).
fn collect_list_block(
  lines: List(String),
  acc: List(String),
) -> #(List(String), List(String)) {
  case lines {
    [] -> #(list.reverse(acc), [])
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      case trimmed {
        "" -> #(list.reverse(acc), rest)
        _ ->
          case
            string.starts_with(trimmed, "- ")
            || string.starts_with(trimmed, "* ")
            || is_ordered_list_start(trimmed)
            || string.starts_with(line, "  ")
            || string.starts_with(line, "\t")
          {
            True -> collect_list_block(rest, [line, ..acc])
            False -> #(list.reverse(acc), lines)
          }
      }
    }
  }
}

/// Collect a paragraph (non-blank, non-special lines).
fn collect_paragraph(
  lines: List(String),
  acc: List(String),
) -> #(List(String), List(String)) {
  case lines {
    [] -> #(list.reverse(acc), [])
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      case trimmed {
        "" -> #(list.reverse(acc), rest)
        _ ->
          case
            string.starts_with(trimmed, "#")
            || string.starts_with(trimmed, "@")
            || string.starts_with(trimmed, "!attest")
            || string.starts_with(trimmed, "```")
            || string.starts_with(trimmed, ">")
            || trimmed == "---"
            || trimmed == "----"
            || trimmed == "-----"
          {
            True -> #(list.reverse(acc), lines)
            False -> collect_paragraph(rest, [trimmed, ..acc])
          }
      }
    }
  }
}

/// Parse a heading line like "## Some Title" into (level, text).
fn parse_heading_line(line: String) -> #(Int, String) {
  let level = count_leading_hashes(line, 0)
  let text = string.trim(string.drop_start(line, level))
  #(level, text)
}

/// Count leading '#' characters.
fn count_leading_hashes(s: String, n: Int) -> Int {
  case string.starts_with(s, "#") {
    True -> count_leading_hashes(string.drop_start(s, 1), n + 1)
    False -> n
  }
}

/// Check if a line starts with a number followed by ". ".
fn is_ordered_list_start(line: String) -> Bool {
  case string.split_once(line, ". ") {
    Ok(#(prefix, _)) -> string.length(prefix) > 0 && is_all_digits(prefix)
    Error(_) -> False
  }
}

/// Check if a string is all ASCII digits.
fn is_all_digits(s: String) -> Bool {
  s
  |> string.to_graphemes
  |> list.all(fn(c) {
    c == "0"
    || c == "1"
    || c == "2"
    || c == "3"
    || c == "4"
    || c == "5"
    || c == "6"
    || c == "7"
    || c == "8"
    || c == "9"
  })
}

/// Parse list items from collected list lines.
fn parse_list_items(
  lines: List(String),
  _ordered: Bool,
) -> List(List(Block)) {
  // Split on lines that start a new item.
  split_list_items(lines, [])
  |> list.map(fn(item_text) {
    let text = string.join(item_text, " ")
    let inlines = parse_inlines(text)
    [Paragraph(inlines)]
  })
}

/// Split list lines into groups, one per item.
fn split_list_items(
  lines: List(String),
  acc: List(List(String)),
) -> List(List(String)) {
  case lines {
    [] -> list.reverse(acc)
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      let item_text = strip_list_marker(trimmed)
      // Check if this is a new item marker.
      case
        string.starts_with(trimmed, "- ")
        || string.starts_with(trimmed, "* ")
        || is_ordered_list_start(trimmed)
      {
        True -> split_list_items(rest, [[item_text], ..acc])
        False ->
          // Continuation line — append to current item.
          case acc {
            [current, ..rest_items] ->
              split_list_items(rest, [
                list.append(current, [trimmed]),
                ..rest_items
              ])
            [] -> split_list_items(rest, [[trimmed]])
          }
      }
    }
  }
}

/// Strip the list marker prefix ("- ", "* ", "1. ") from a line.
fn strip_list_marker(line: String) -> String {
  case string.starts_with(line, "- ") || string.starts_with(line, "* ") {
    True -> string.drop_start(line, 2)
    False ->
      case string.split_once(line, ". ") {
        Ok(#(prefix, rest)) ->
          case is_all_digits(prefix) {
            True -> rest
            False -> line
          }
        Error(_) -> line
      }
  }
}

/// Parse a directive line like "@version 1.0" into a Directive.
fn parse_directive_line(line: String) -> Directive {
  let without_at = string.drop_start(string.trim(line), 1)
  case string.split_once(without_at, " ") {
    Ok(#(name, value)) ->
      Directive(
        name: string.trim(name),
        value: string.trim(value),
        attributes: [],
      )
    Error(_) ->
      Directive(name: string.trim(without_at), value: "", attributes: [])
  }
}

/// Parse an attestation block from its collected lines.
fn parse_attestation_block(lines: List(String)) -> Attestation {
  let fields = parse_key_value_lines(lines)
  let identity = find_field(fields, "identity")
  let role = find_field(fields, "role")
  let trust_str = find_field(fields, "trust-level")
  let trust_level = case parse_trust_level(trust_str) {
    Ok(tl) -> tl
    Error(_) -> Unverified
  }
  let timestamp = case find_field(fields, "timestamp") {
    "" -> Error(Nil)
    ts -> Ok(ts)
  }
  let note = case find_field(fields, "note") {
    "" -> Error(Nil)
    n -> Ok(n)
  }

  Attestation(
    identity: identity,
    role: role,
    trust_level: trust_level,
    timestamp: timestamp,
    note: note,
  )
}

/// Parse key-value pairs from indented lines (e.g. "  identity: Alice").
fn parse_key_value_lines(lines: List(String)) -> List(#(String, String)) {
  lines
  |> list.filter_map(fn(line) {
    let trimmed = string.trim(line)
    case string.split_once(trimmed, ":") {
      Ok(#(key, value)) ->
        Ok(#(string.lowercase(string.trim(key)), string.trim(value)))
      Error(_) -> Error(Nil)
    }
  })
}

/// Find a field value by key, returning "" if not found.
fn find_field(fields: List(#(String, String)), key: String) -> String {
  fields
  |> list.find(fn(pair) { pair.0 == key })
  |> result.map(fn(pair) { pair.1 })
  |> result.unwrap("")
}

/// Parse inline elements from a text string.
///
/// Handles: **bold**, *italic*, `code`, and [text](url) links.
pub fn parse_inlines(input: String) -> List(Inline) {
  parse_inlines_acc(input, [])
  |> list.reverse
}

/// Internal inline parser accumulating results.
fn parse_inlines_acc(input: String, acc: List(Inline)) -> List(Inline) {
  case input {
    "" -> acc
    _ -> {
      case string.starts_with(input, "**") {
        True -> {
          let rest = string.drop_start(input, 2)
          case string.split_once(rest, "**") {
            Ok(#(inner, after)) -> {
              let children = parse_inlines(inner)
              parse_inlines_acc(after, [Strong(children), ..acc])
            }
            Error(_) -> {
              // No closing **, treat as text.
              let #(text, remaining) = take_until_special(input, "")
              parse_inlines_acc(remaining, [Text(text), ..acc])
            }
          }
        }
        False ->
          case string.starts_with(input, "*") {
            True -> {
              let rest = string.drop_start(input, 1)
              case string.split_once(rest, "*") {
                Ok(#(inner, after)) -> {
                  let children = parse_inlines(inner)
                  parse_inlines_acc(after, [Emphasis(children), ..acc])
                }
                Error(_) -> {
                  let #(text, remaining) = take_until_special(input, "")
                  parse_inlines_acc(remaining, [Text(text), ..acc])
                }
              }
            }
            False ->
              case string.starts_with(input, "`") {
                True -> {
                  let rest = string.drop_start(input, 1)
                  case string.split_once(rest, "`") {
                    Ok(#(code, after)) ->
                      parse_inlines_acc(after, [
                        types.Code(value: code),
                        ..acc
                      ])
                    Error(_) -> {
                      let #(text, remaining) = take_until_special(input, "")
                      parse_inlines_acc(remaining, [Text(text), ..acc])
                    }
                  }
                }
                False ->
                  case string.starts_with(input, "[") {
                    True -> {
                      let rest = string.drop_start(input, 1)
                      case string.split_once(rest, "](") {
                        Ok(#(link_text, after_bracket)) ->
                          case string.split_once(after_bracket, ")") {
                            Ok(#(url, after)) -> {
                              let content = parse_inlines(link_text)
                              parse_inlines_acc(after, [
                                Link(content: content, url: url),
                                ..acc
                              ])
                            }
                            Error(_) -> {
                              let #(text, remaining) =
                                take_until_special(input, "")
                              parse_inlines_acc(remaining, [Text(text), ..acc])
                            }
                          }
                        Error(_) -> {
                          let #(text, remaining) =
                            take_until_special(input, "")
                          parse_inlines_acc(remaining, [Text(text), ..acc])
                        }
                      }
                    }
                    False -> {
                      // Plain text until the next special character.
                      let #(text, remaining) = take_until_special(input, "")
                      parse_inlines_acc(remaining, [Text(text), ..acc])
                    }
                  }
              }
          }
      }
    }
  }
}

/// Take text characters until a special inline character is reached.
fn take_until_special(input: String, acc: String) -> #(String, String) {
  case input {
    "" -> #(acc, "")
    _ -> {
      case
        string.starts_with(input, "*")
        || string.starts_with(input, "`")
        || string.starts_with(input, "[")
      {
        True ->
          case acc {
            "" -> {
              // If we're at a special char but haven't accumulated anything,
              // take one character literally to avoid infinite loops.
              let char = string.slice(input, 0, 1)
              #(char, string.drop_start(input, 1))
            }
            _ -> #(acc, input)
          }
        False -> {
          let char = string.slice(input, 0, 1)
          take_until_special(string.drop_start(input, 1), acc <> char)
        }
      }
    }
  }
}

/// Extract all Directive blocks from a list of blocks.
fn extract_directives(blocks: List(Block)) -> List(Directive) {
  blocks
  |> list.filter_map(fn(block) {
    case block {
      DirectiveBlock(d) -> Ok(d)
      _ -> Error(Nil)
    }
  })
}

/// Extract all Attestation blocks from a list of blocks.
fn extract_attestations(blocks: List(Block)) -> List(Attestation) {
  blocks
  |> list.filter_map(fn(block) {
    case block {
      AttestationBlock(a) -> Ok(a)
      _ -> Error(Nil)
    }
  })
}
