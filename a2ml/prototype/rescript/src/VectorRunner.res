// SPDX-License-Identifier: PMPL-1.0-or-later

// Minimal test vector runner for Module 0.

open Compat

module Fs = {
  @module("fs")
  external readdirSync: string => array<string> = "readdirSync"

  @module("fs")
  external readFileSync: (string, string) => string = "readFileSync"

  @module("fs")
  external existsSync: string => bool = "existsSync"
}

@send external includes: (string, string) => bool = "includes"

let regexReplace = (s: string, pattern: string, replacement: string): string => {
  %raw(`s.replace(new RegExp(pattern, 'g'), replacement)`)
}

let listVectors = (): array<(string, string)> => {
let files = Fs.readdirSync("tests/vectors")
files
->arrayKeep(file => endsWith(file, ".a2ml"))
->arrayMap(file => {
    let expected = replace(file, ".a2ml", ".expected")
    ("tests/vectors/" ++ file, "tests/vectors/" ++ expected)
  })
}

let parseExpected = (text: string): option<string> => {
  let lines = split(text, "\n")
  let errorLine = lines->arrayKeep(line => startsWith(line, "ERROR:"))
  if arrayLength(errorLine) > 0 {
    Some(trim(sliceToEnd(arrayGetExn(errorLine, 0), ~from=6)))
  } else {
    None
  }
}

let normalizeHtml = (html: string): string => {
  // Collapse whitespace for stable comparison.
  let normalized = regexReplace(html, "\\s+", " ")
  trim(normalized)
}

let run = (): int => {
  let vectors = listVectors()
  let failures = arrayMake(0, "")

  vectors->arrayForEach(((inputPath, expectedPath)) => {
    let input = Fs.readFileSync(inputPath, "utf8")
    let expected = Fs.readFileSync(expectedPath, "utf8")

    let doc = A2ml.parse(input)
    let errors = A2ml.validateChecked(doc)
    let expectedError = parseExpected(expected)

    switch expectedError {
    | None =>
        if arrayLength(errors) > 0 {
          failures->arrayPush(inputPath ++ ": expected ok, got error")
        }
    | Some(msg) =>
        if arrayLength(errors) == 0 {
          failures->arrayPush(inputPath ++ ": expected error, got ok")
        } else {
          let first = arrayGetExn(errors, 0)
  if !includes(first.msg, msg) {
            failures->arrayPush(inputPath ++ ": error mismatch")
          }
        }
    }

let htmlExpectedPath = replace(inputPath, ".a2ml", ".html.expected")
    if Fs.existsSync(htmlExpectedPath) {
      let actualHtml = A2ml.renderHtml(doc)->normalizeHtml
      let expectedHtml = Fs.readFileSync(htmlExpectedPath, "utf8")->normalizeHtml
      if actualHtml != expectedHtml {
        failures->arrayPush(inputPath ++ ": html mismatch")
      }
    }
  })

  if arrayLength(failures) == 0 {
  consoleLog("All vectors passed")
    0
  } else {
  failures->arrayForEach(msg => consoleLog(msg))
    1
  }
}
