// SPDX-License-Identifier: PMPL-1.0-or-later
open Compat

module Fs = {
  @module("fs")
  external readFileSync: (string, string) => string = "readFileSync"
}

@val external argv: array<string> = "process.argv"

let _ = {
  // Usage: node DumpAst.bs.js path/to/file.a2ml
  let args = argv
  if arrayLength(args) < 3 {
    consoleLog("Usage: dump-ast <path>")
  } else {
    let path = args->arrayGetExn(2)
    let input = Fs.readFileSync(path, "utf8")
    let doc = A2ml.parse(input)
    let json = Json.docToJson(doc)
    consoleLog(json)
  }
}
