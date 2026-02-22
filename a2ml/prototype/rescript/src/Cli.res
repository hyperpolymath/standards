// SPDX-License-Identifier: PMPL-1.0-or-later
open Compat

let hasDeno = %raw(`typeof Deno !== "undefined"`)

@val external argv: array<string> = "process.argv"

let readText = (path: string): string => {
  let _ = path
  %raw(`(typeof Deno !== "undefined")
    ? Deno.readTextFileSync(path)
    : require("fs").readFileSync(path, "utf8")`)
}

let writeText = (path: string, text: string): unit => {
  let _ = path
  let _ = text
  %raw(`(typeof Deno !== "undefined")
    ? Deno.writeTextFileSync(path, text)
    : require("fs").writeFileSync(path, text, "utf8")`)
}

let readStdin = (): string => {
  %raw(`(typeof Deno !== "undefined")
    ? new TextDecoder().decode(Deno.readAllSync(Deno.stdin))
    : require("fs").readFileSync(0, "utf8")`)
}

let exit = (code: int): unit => {
  let _ = code
  %raw(`(typeof Deno !== "undefined") ? Deno.exit(code) : process.exit(code)`)
}

let helpText = "A2ML CLI (prototype)\n\n" ++
  "Usage:\n  a2ml <render|validate|ast> <file...|-> [options]\n\n" ++
  "Commands:\n  render    Render HTML to stdout (or --out)\n" ++
  "  validate  Validate in checked mode, exit 2 on errors\n" ++
  "  ast       Output JSON surface AST\n\n" ++
  "Options:\n  --mode <lax|checked>   Parse mode (default: lax)\n" ++
  "  --out <path>           Write output to file\n" ++
  "  --concat               Concatenate outputs when multiple inputs\n" ++
  "  --stdin                Read input from stdin (equivalent to '-')\n" ++
  "  -h, --help             Show this help\n\n" ++
  "Notes:\n  * Use '-' as a filename to read from stdin.\n" ++
  "  * For multiple files, --concat joins outputs in order.\n"

let usage = (): unit => {
  consoleLog(helpText)
  exit(1)
}

let getArg = (args: array<string>, name: string): option<string> => {
  let rec loop = i =>
    if i >= arrayLength(args) {
      None
    } else if arrayGetExn(args, i) == name {
      if i + 1 < arrayLength(args) {
        Some(arrayGetExn(args, i + 1))
      } else {
        None
      }
    } else {
      loop(i + 1)
    }
  loop(0)
}

let hasFlag = (args: array<string>, name: string): bool => {
  arrayLength(arrayFilter(args, arg => arg == name)) > 0
}

let collectInputs = (args: array<string>): array<string> => {
  let inputs = arrayMake(0, "")
  let rec loop = i =>
    if i >= arrayLength(args) {
      inputs
    } else {
      let arg = arrayGetExn(args, i)
      if startsWith(arg, "-") {
        inputs
      } else {
        inputs->arrayPush(arg)
        loop(i + 1)
      }
    }
  loop(3)
}

let readInput = (path: string): string => {
  if path == "-" { readStdin() } else { readText(path) }
}

let renderDoc = (input: string, mode: A2ml.parseMode): string => {
  let doc = A2ml.parse(~mode, input)
  A2ml.renderHtml(doc)
}

let validateDoc = (input: string, mode: A2ml.parseMode): array<A2ml.parseError> => {
  let doc = A2ml.parse(~mode, input)
  if mode == A2ml.Checked { A2ml.validateChecked(doc) } else { [] }
}

let astDoc = (input: string, mode: A2ml.parseMode): string => {
  let doc = A2ml.parse(~mode, input)
  Json.docToJson(doc)
}

let _ = {
  let args = argv
  if hasFlag(args, "-h") || hasFlag(args, "--help") { consoleLog(helpText); exit(0) }
  if arrayLength(args) < 3 { usage() }

  let command = arrayGetExn(args, 2)
  let inputs = collectInputs(args)
  let readFromStdin = hasFlag(args, "--stdin")

  let mode = switch getArg(args, "--mode") {
  | Some("checked") => A2ml.Checked
  | _ => A2ml.Lax
  }

  let outPath = getArg(args, "--out")
  let concat = hasFlag(args, "--concat")

  let sources = if readFromStdin { ["-"] } else { inputs }
  if arrayLength(sources) == 0 { usage() }

  switch command {
  | "render" =>
      let outputs = sources->arrayMap(src => renderDoc(readInput(src), mode))
      let result =
        if concat { outputs->arrayJoinWith("\n", s => s) } else { outputs->arrayJoinWith("\n\n", s => s) }
      switch outPath {
      | Some(p) => writeText(p, result)
      | None => consoleLog(result)
      }
  | "validate" =>
      let allErrors = sources
        ->arrayMap(src => validateDoc(readInput(src), mode))
        ->arrayReduce([], (acc, errs) => arrayConcat(acc, errs))
      if arrayLength(allErrors) == 0 {
        consoleLog("ok")
      } else {
        allErrors->arrayForEach(e => consoleLog(`${intToString(e.line)}: ${e.msg}`))
        exit(2)
      }
  | "ast" =>
      let outputs = sources->arrayMap(src => astDoc(readInput(src), mode))
      let result = outputs->arrayJoinWith("\n", s => s)
      switch outPath {
      | Some(p) => writeText(p, result)
      | None => consoleLog(result)
      }
  | _ => usage()
  }
}
