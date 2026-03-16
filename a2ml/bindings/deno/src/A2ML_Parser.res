// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// A2ML_Parser — Parser for A2ML (Attested Markup Language) documents.
//
// Parses the A2ML surface syntax into the typed AST defined in A2ML_Types.
// The parser is line-oriented and processes:
//   - Headings (# through #####)
//   - Directive blocks (@name(attrs): ... @end)
//   - Attestation blocks (!attest ... !end)
//   - Inline formatting (**bold**, *italic*, `code`, [link](url), @ref(id))
//   - Bullet lists (- item)
//   - Code blocks (``` fenced blocks)

open A2ML_Types

// ---------------------------------------------------------------------------
// Inline parsing helpers
// ---------------------------------------------------------------------------

/// Parse a single line of text into inline elements.
/// Handles **bold**, *italic*, `code`, [text](url), and @ref(id).
let parseInlines = (text: string): array<inline> => {
  let result = []
  let len = text->String.length
  let i = ref(0)
  let buf = ref("")

  // Flush accumulated plain text into the result array
  let flushBuf = () => {
    if buf.contents->String.length > 0 {
      result->Array.push(Text(buf.contents))->ignore
      buf := ""
    }
  }

  while i.contents < len {
    let ch = text->String.charAt(i.contents)
    let remaining = text->String.sliceToEnd(~start=i.contents)

    // **bold**
    if remaining->String.startsWith("**") {
      flushBuf()
      let closeIdx = text->String.indexOfFrom("**", i.contents + 2)
      if closeIdx >= 0 {
        let inner = text->String.slice(~start=i.contents + 2, ~end=closeIdx)
        result->Array.push(Strong([Text(inner)]))->ignore
        i := closeIdx + 2
      } else {
        buf := buf.contents ++ ch
        i := i.contents + 1
      }
    }
    // *italic*
    else if ch == "*" && !(remaining->String.startsWith("**")) {
      flushBuf()
      let closeIdx = text->String.indexOfFrom("*", i.contents + 1)
      if closeIdx >= 0 {
        let inner = text->String.slice(~start=i.contents + 1, ~end=closeIdx)
        result->Array.push(Emphasis([Text(inner)]))->ignore
        i := closeIdx + 1
      } else {
        buf := buf.contents ++ ch
        i := i.contents + 1
      }
    }
    // `code`
    else if ch == "`" {
      flushBuf()
      let closeIdx = text->String.indexOfFrom("`", i.contents + 1)
      if closeIdx >= 0 {
        let inner = text->String.slice(~start=i.contents + 1, ~end=closeIdx)
        result->Array.push(Code(inner))->ignore
        i := closeIdx + 1
      } else {
        buf := buf.contents ++ ch
        i := i.contents + 1
      }
    }
    // [text](url)
    else if ch == "[" {
      flushBuf()
      let closeBracket = text->String.indexOfFrom("]", i.contents + 1)
      if closeBracket >= 0 {
        let afterBracket = text->String.charAt(closeBracket + 1)
        if afterBracket == "(" {
          let closeParen = text->String.indexOfFrom(")", closeBracket + 2)
          if closeParen >= 0 {
            let linkText = text->String.slice(~start=i.contents + 1, ~end=closeBracket)
            let linkUrl = text->String.slice(~start=closeBracket + 2, ~end=closeParen)
            result->Array.push(Link({content: [Text(linkText)], url: linkUrl}))->ignore
            i := closeParen + 1
          } else {
            buf := buf.contents ++ ch
            i := i.contents + 1
          }
        } else {
          buf := buf.contents ++ ch
          i := i.contents + 1
        }
      } else {
        buf := buf.contents ++ ch
        i := i.contents + 1
      }
    }
    // @ref(id)
    else if remaining->String.startsWith("@ref(") {
      flushBuf()
      let closeParen = text->String.indexOfFrom(")", i.contents + 5)
      if closeParen >= 0 {
        let refId = text->String.slice(~start=i.contents + 5, ~end=closeParen)
        result->Array.push(InlineRef(refId))->ignore
        i := closeParen + 1
      } else {
        buf := buf.contents ++ ch
        i := i.contents + 1
      }
    }
    // Plain text
    else {
      buf := buf.contents ++ ch
      i := i.contents + 1
    }
  }

  flushBuf()
  result
}

// ---------------------------------------------------------------------------
// Directive attribute parsing
// ---------------------------------------------------------------------------

/// Parse directive attributes from a parenthesised string like "(key=val, key2=val2)".
let parseAttributes = (attrStr: string): array<(string, string)> => {
  if attrStr->String.length == 0 {
    []
  } else {
    attrStr
    ->String.split(",")
    ->Array.filterMap(pair => {
      let trimmed = pair->String.trim
      let eqIdx = trimmed->String.indexOf("=")
      if eqIdx >= 0 {
        let key = trimmed->String.slice(~start=0, ~end=eqIdx)->String.trim
        let value = trimmed->String.sliceToEnd(~start=eqIdx + 1)->String.trim
        Some((key, value))
      } else {
        None
      }
    })
  }
}

// ---------------------------------------------------------------------------
// Block-level parser
// ---------------------------------------------------------------------------

/// Internal state for the line-oriented parser.
type parserState = {
  mutable lineIndex: int,
  lines: array<string>,
  blocks: array<block>,
  directives: array<directive>,
  attestations: array<attestation>,
  mutable title: option<string>,
}

/// Count the number of leading '#' characters on a line.
let countHashes = (line: string): int => {
  let count = ref(0)
  let len = line->String.length
  while count.contents < len && line->String.charAt(count.contents) == "#" {
    count := count.contents + 1
  }
  count.contents
}

/// Parse a directive block starting with @name or @name(attrs):
/// Reads lines until @end is encountered.
let parseDirectiveBlock = (state: parserState): result<directive, parseError> => {
  let startLine = state.lineIndex
  let line = state.lines->Array.getUnsafe(startLine)->String.trim

  // Extract directive name and optional attributes
  // Formats: @name: body  or  @name(attrs): body  or  @name:\n multi-line \n @end
  let afterAt = line->String.sliceToEnd(~start=1)

  // Check for parenthesised attributes
  let (name, attributes) = {
    let parenIdx = afterAt->String.indexOf("(")
    if parenIdx >= 0 {
      let closeParenIdx = afterAt->String.indexOf(")")
      if closeParenIdx > parenIdx {
        let dirName = afterAt->String.slice(~start=0, ~end=parenIdx)->String.trim
        let attrStr = afterAt->String.slice(~start=parenIdx + 1, ~end=closeParenIdx)
        (dirName, parseAttributes(attrStr))
      } else {
        let colonIdx = afterAt->String.indexOf(":")
        let dirName = if colonIdx >= 0 {
          afterAt->String.slice(~start=0, ~end=colonIdx)->String.trim
        } else {
          afterAt->String.trim
        }
        (dirName, [])
      }
    } else {
      let colonIdx = afterAt->String.indexOf(":")
      let dirName = if colonIdx >= 0 {
        afterAt->String.slice(~start=0, ~end=colonIdx)->String.trim
      } else {
        afterAt->String.trim
      }
      (dirName, [])
    }
  }

  // Extract inline body (text after the colon on the same line)
  let colonIdx = line->String.indexOf(":")
  let inlineBody = if colonIdx >= 0 {
    line->String.sliceToEnd(~start=colonIdx + 1)->String.trim
  } else {
    ""
  }

  // Check if this is a single-line directive (no @end needed)
  if inlineBody->String.length > 0 {
    state.lineIndex = state.lineIndex + 1
    Ok({name, value: inlineBody, attributes})
  } else {
    // Multi-line directive: read until @end
    state.lineIndex = state.lineIndex + 1
    let bodyLines = []
    let found = ref(false)
    while state.lineIndex < state.lines->Array.length && !found.contents {
      let currentLine = state.lines->Array.getUnsafe(state.lineIndex)
      if currentLine->String.trim == "@end" {
        found := true
        state.lineIndex = state.lineIndex + 1
      } else {
        bodyLines->Array.push(currentLine)->ignore
        state.lineIndex = state.lineIndex + 1
      }
    }
    if found.contents {
      Ok({name, value: bodyLines->Array.join("\n"), attributes})
    } else {
      Error(UnterminatedDirective({line: startLine + 1, name}))
    }
  }
}

/// Parse an attestation block starting with !attest.
/// Format:
///   !attest
///   identity: <name>
///   role: <role>
///   trust-level: <level>
///   timestamp: <iso8601>  (optional)
///   note: <text>          (optional)
///   !end
let parseAttestationBlock = (state: parserState): result<attestation, parseError> => {
  let startLine = state.lineIndex
  state.lineIndex = state.lineIndex + 1

  let identity = ref("")
  let role = ref("")
  let trustLvl = ref(Unverified)
  let timestamp = ref(None)
  let note = ref(None)
  let found = ref(false)

  while state.lineIndex < state.lines->Array.length && !found.contents {
    let currentLine = state.lines->Array.getUnsafe(state.lineIndex)->String.trim
    if currentLine == "!end" {
      found := true
      state.lineIndex = state.lineIndex + 1
    } else {
      let colonIdx = currentLine->String.indexOf(":")
      if colonIdx >= 0 {
        let key = currentLine->String.slice(~start=0, ~end=colonIdx)->String.trim
        let value = currentLine->String.sliceToEnd(~start=colonIdx + 1)->String.trim
        switch key {
        | "identity" => identity := value
        | "role" => role := value
        | "trust-level" =>
          switch trustLevelFromString(value) {
          | Some(lvl) => trustLvl := lvl
          | None => () // Default to Unverified if unrecognised
          }
        | "timestamp" => timestamp := Some(value)
        | "note" => note := Some(value)
        | _ => () // Ignore unknown fields
        }
      }
      state.lineIndex = state.lineIndex + 1
    }
  }

  if found.contents {
    Ok({
      identity: identity.contents,
      role: role.contents,
      trustLevel: trustLvl.contents,
      timestamp: timestamp.contents,
      note: note.contents,
    })
  } else {
    Error(UnexpectedToken({line: startLine + 1, token: "unterminated !attest block"}))
  }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Parse an A2ML document from a string.
///
/// Returns either a parseError or the parsed document.
///
/// ### Example
/// ```
/// let result = parseA2ML("# Hello\n\nSome text.\n")
/// ```
let parseA2ML = (input: string): result<document, parseError> => {
  let trimmed = input->String.trim
  if trimmed->String.length == 0 {
    Error(EmptyDocument)
  } else {
    let lines = input->String.split("\n")
    let state: parserState = {
      lineIndex: 0,
      lines,
      blocks: [],
      directives: [],
      attestations: [],
      title: None,
    }

    let error = ref(None)

    while state.lineIndex < lines->Array.length && error.contents->Option.isNone {
      let line = lines->Array.getUnsafe(state.lineIndex)
      let trimmedLine = line->String.trim

      // Blank line
      if trimmedLine->String.length == 0 {
        state.blocks->Array.push(BlankLine)->ignore
        state.lineIndex = state.lineIndex + 1
      }
      // Thematic break (--- or ***)
      else if trimmedLine == "---" || trimmedLine == "***" || trimmedLine == "___" {
        state.blocks->Array.push(ThematicBreak)->ignore
        state.lineIndex = state.lineIndex + 1
      }
      // Fenced code block (```)
      else if trimmedLine->String.startsWith("```") {
        let lang = trimmedLine->String.sliceToEnd(~start=3)->String.trim
        let language = if lang->String.length > 0 {
          Some(lang)
        } else {
          None
        }
        state.lineIndex = state.lineIndex + 1
        let codeLines = []
        let closed = ref(false)
        while state.lineIndex < lines->Array.length && !closed.contents {
          let codeLine = lines->Array.getUnsafe(state.lineIndex)
          if codeLine->String.trim->String.startsWith("```") {
            closed := true
            state.lineIndex = state.lineIndex + 1
          } else {
            codeLines->Array.push(codeLine)->ignore
            state.lineIndex = state.lineIndex + 1
          }
        }
        state.blocks
        ->Array.push(CodeBlock({language, content: codeLines->Array.join("\n")}))
        ->ignore
      }
      // Heading (# through #####)
      else if trimmedLine->String.startsWith("#") {
        let level = countHashes(trimmedLine)
        if level >= 1 && level <= 5 {
          let headingText = trimmedLine->String.sliceToEnd(~start=level)->String.trim
          let inlines = parseInlines(headingText)
          // Extract title from first H1 heading
          if level == 1 && state.title->Option.isNone {
            state.title = Some(headingText)
          }
          state.blocks->Array.push(Heading({level, content: inlines}))->ignore
          state.lineIndex = state.lineIndex + 1
        } else {
          error := Some(InvalidHeadingLevel({line: state.lineIndex + 1, level}))
        }
      }
      // Directive block (@name...)
      else if trimmedLine->String.startsWith("@") && trimmedLine != "@end" {
        switch parseDirectiveBlock(state) {
        | Ok(dir) =>
          state.directives->Array.push(dir)->ignore
          state.blocks->Array.push(DirectiveBlock(dir))->ignore
        | Error(err) => error := Some(err)
        }
      }
      // Attestation block (!attest)
      else if trimmedLine->String.startsWith("!attest") {
        switch parseAttestationBlock(state) {
        | Ok(att) =>
          state.attestations->Array.push(att)->ignore
          state.blocks->Array.push(AttestationBlock(att))->ignore
        | Error(err) => error := Some(err)
        }
      }
      // Block quote (> ...)
      else if trimmedLine->String.startsWith("> ") {
        let quoteLines = []
        let done = ref(false)
        while state.lineIndex < lines->Array.length && !done.contents {
          let ql = lines->Array.getUnsafe(state.lineIndex)->String.trim
          if ql->String.startsWith("> ") {
            quoteLines->Array.push(ql->String.sliceToEnd(~start=2))->ignore
            state.lineIndex = state.lineIndex + 1
          } else {
            done := true
          }
        }
        let quoteText = quoteLines->Array.join("\n")
        state.blocks
        ->Array.push(BlockQuote([Paragraph(parseInlines(quoteText))]))
        ->ignore
      }
      // Bullet list (- item)
      else if trimmedLine->String.startsWith("- ") || trimmedLine->String.startsWith("* ") {
        let items = []
        let done = ref(false)
        while state.lineIndex < lines->Array.length && !done.contents {
          let listLine = lines->Array.getUnsafe(state.lineIndex)->String.trim
          if listLine->String.startsWith("- ") || listLine->String.startsWith("* ") {
            let itemText = listLine->String.sliceToEnd(~start=2)->String.trim
            items->Array.push(parseInlines(itemText))->ignore
            state.lineIndex = state.lineIndex + 1
          } else {
            done := true
          }
        }
        state.blocks->Array.push(BulletList(items))->ignore
      }
      // Paragraph (default)
      else {
        let paraLines = []
        let done = ref(false)
        while state.lineIndex < lines->Array.length && !done.contents {
          let pl = lines->Array.getUnsafe(state.lineIndex)->String.trim
          if (
            pl->String.length > 0 &&
            !(pl->String.startsWith("#")) &&
            !(pl->String.startsWith("@")) &&
            !(pl->String.startsWith("!attest")) &&
            !(pl->String.startsWith("```")) &&
            !(pl->String.startsWith("- ")) &&
            !(pl->String.startsWith("* ")) &&
            !(pl->String.startsWith("> ")) &&
            pl != "---" &&
            pl != "***" &&
            pl != "___"
          ) {
            paraLines->Array.push(pl)->ignore
            state.lineIndex = state.lineIndex + 1
          } else {
            done := true
          }
        }
        let paraText = paraLines->Array.join(" ")
        state.blocks->Array.push(Paragraph(parseInlines(paraText)))->ignore
      }
    }

    switch error.contents {
    | Some(err) => Error(err)
    | None =>
      Ok({
        title: state.title,
        directives: state.directives,
        blocks: state.blocks,
        attestations: state.attestations,
      })
    }
  }
}

/// Parse an A2ML document from a file path (Deno-compatible).
/// Uses Deno.readTextFile under the hood.
/// Returns a Promise resolving to Result<document, parseError>.
@module("node:fs")
external readFileSync: (string, string) => string = "readFileSync"

let parseA2MLFile = (path: string): result<document, parseError> => {
  let content = readFileSync(path, "utf-8")
  parseA2ML(content)
}
