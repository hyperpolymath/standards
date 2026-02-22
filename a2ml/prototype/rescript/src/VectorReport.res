// SPDX-License-Identifier: PMPL-1.0-or-later

open Compat

module Fs = {
  @module("fs")
  external readdirSync: string => array<string> = "readdirSync"

  @module("fs")
  external readFileSync: (string, string) => string = "readFileSync"

  @module("fs")
  external writeFileSync: (string, string) => unit = "writeFileSync"

  @module("fs")
  external existsSync: string => bool = "existsSync"
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

let run = (): unit => {
  let vectors = listVectors()
  let report = arrayMake(0, "")

  vectors->arrayForEach(((inputPath, expectedPath)) => {
    let input = Fs.readFileSync(inputPath, "utf8")
    let doc = A2ml.parse(input)
    let errors = A2ml.validateChecked(doc)
    let status = if arrayLength(errors) == 0 { "PASS" } else { "FAIL" }
    report->arrayPush(`{"file": "${inputPath}", "status": "${status}"}`)
  })

  let json = "[" ++ arrayJoin(report, ",") ++ "]"
  Fs.writeFileSync("build/vector-report.json", json)
  consoleLog("Report written to build/vector-report.json")
}
