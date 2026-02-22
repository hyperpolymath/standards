// SPDX-License-Identifier: PMPL-1.0-or-later

// Minimal Module-0 parser and checked validator for web rendering demos.
// This is intentionally small but deterministic.

// Use external bindings for JavaScript String and Array methods (ReScript v12 compatible)
@get external length: string => int = "length"
@send external slice: (string, ~start: int, ~end: int) => string = "slice"
@send external sliceToEnd: (string, ~from: int) => string = "slice"
@send external trim: string => string = "trim"
@send external indexOf: (string, string) => int = "indexOf"
@send external split: (string, string) => array<string> = "split"
@send external startsWith: (string, string) => bool = "startsWith"
@send external endsWith: (string, string) => bool = "endsWith"

// Int methods
@val external intToString: int => string = "String"

// Array methods
@get external arrayLength: array<'a> => int = "length"
@send external arrayPush: (array<'a>, 'a) => unit = "push"
@send external arrayConcat: (array<'a>, array<'a>) => array<'a> = "concat"
@send external arrayReverse: array<'a> => array<'a> = "reverse"
@send external arrayMap: (array<'a>, 'a => 'b) => array<'b> = "map"
@send external arrayFilter: (array<'a>, 'a => bool) => array<'a> = "filter"
@send external arrayReduceNative: (array<'a>, ('b, 'a) => 'b, 'b) => 'b = "reduce"

let arrayReduce = (arr: array<'a>, init: 'b, fn: ('b, 'a) => 'b): 'b => {
  arrayReduceNative(arr, fn, init)
}
@send external arrayJoin: (array<'a>, string) => string = "join"
@send external arrayForEach: (array<'a>, 'a => unit) => unit = "forEach"
let arrayForEachWithIndex = (arr: array<'a>, fn: (int, 'a) => unit): unit => {
  let idx = ref(0)
  arr->arrayForEach(item => {
    fn(idx.contents, item)
    idx.contents = idx.contents + 1
  })
}
@get_index external arrayGet: (array<'a>, int) => 'a = ""

// Helper functions
let arrayKeepMap = (arr: array<'a>, fn: 'a => option<'b>): array<'b> => {
  arr->arrayMap(fn)->arrayFilter(x => switch x { | None => false | Some(_) => true })->arrayMap(x => switch x { | Some(v) => v | None => assert(false) })
}

let arrayGetExn = (arr: array<'a>, idx: int): 'a => {
  arrayGet(arr, idx)
}

let arrayJoinWith = (arr: array<'a>, sep: string, fn: 'a => string): string => {
  arr->arrayMap(fn)->arrayJoin(sep)
}

let arrayMake = (_size: int, _default: 'a): array<'a> => {
  []
}

type attrs = array<(string, string)>

type inline =
  | Text(string)
  | Emph(string)
  | Strong(string)
  | Link(string, string)

type rec block =
  | Heading(int, string)
  | Paragraph(array<inline>)
  | List(array<array<inline>>)
  | Directive(string, attrs, array<block>)

type doc = array<block>

type parseMode =
  | Lax
  | Checked

type parseError = {
  line: int,
  msg: string,
}

let isChar = (s: string, i: int, ch: string): bool => {
  i >= 0 && i < length(s) && slice(s, ~start=i, ~end=i + 1) == ch
}

let indexOfOpt = (s: string, sub: string): option<int> => {
  let idx = indexOf(s, sub)
  if idx < 0 { None } else { Some(idx) }
}

let indexFromOpt = (s: string, start: int, sub: string): option<int> => {
  // Js.String2 doesn't have indexOfFrom, so slice and search
  let sliced = sliceToEnd(s, ~from=start)
  let idx = indexOf(sliced, sub)
  if idx < 0 { None } else { Some(idx + start) }
}

let isHeading = (line: string): option<(int, string)> => {
  let trimmed = trim(line)
  let rec countHashes = (i, count) =>
    if i >= length(trimmed) {count} else {
      if isChar(trimmed, i, "#") {countHashes(i + 1, count + 1)} else {count}
    }
  let hcount = countHashes(0, 0)
  if hcount > 0 && hcount <= 5 {
    let text = trim(sliceToEnd(trimmed, ~from=hcount))
    Some((hcount, text))
  } else {
    None
  }
}

let parseAttrs = (line: string): attrs => {
  // Parse "name(a=b,c=d)" into [("a","b"),("c","d")]
  let start = switch indexOfOpt(line, "(") {
  | None => -1
  | Some(idx) => idx
  }
  let end_ = switch indexOfOpt(line, ")") {
  | None => -1
  | Some(idx) => idx
  }
  if start == -1 || end_ == -1 || end_ < start {
    []
  } else {
    let inner = slice(line, ~start=start + 1, ~end=end_)
    let parts = split(inner, ",")
    parts
    ->arrayKeepMap(part => {
        let kv = split(trim(part), "=")
        if arrayLength(kv) == 2 {
          let key = kv->arrayGetExn(0)->trim
          let value = kv->arrayGetExn(1)->trim
          let unquoted =
            if (startsWith(value, "\"") && endsWith(value, "\"")) ||
                (startsWith(value, "'") && endsWith(value, "'")) {
              slice(value, ~start=1, ~end=length(value) - 1)
            } else {
              value
            }
          Some((key, unquoted))
        } else {
          None
        }
      })
  }
}

let isDirectiveStart = (line: string): bool => {
  let trimmed = trim(line)
  startsWith(trimmed, "@") && endsWith(trimmed, ":")
}

let parseInline = (text: string): array<inline> => {
  // Simple, non-nested parser for strong/emph/link in one pass.
  let rec loop = (i, acc) =>
    if i >= length(text) {
      arrayReverse(acc)
    } else if i + 1 < length(text) && slice(text, ~start=i, ~end=i + 2) == "**" {
      let close = indexFromOpt(text, i + 2, "**")
      switch close {
      | None => loop(i + 2, arrayConcat([Text("**")], acc))
      | Some(j) =>
        let content = slice(text, ~start=i + 2, ~end=j)
        loop(j + 2, arrayConcat([Strong(content)], acc))
      }
    } else if isChar(text, i, "*") {
      let close = indexFromOpt(text, i + 1, "*")
      switch close {
      | None => loop(i + 1, arrayConcat([Text("*")], acc))
      | Some(j) =>
        let content = slice(text, ~start=i + 1, ~end=j)
        loop(j + 1, arrayConcat([Emph(content)], acc))
      }
    } else if isChar(text, i, "[") {
      let closeText = indexFromOpt(text, i + 1, "]")
      switch closeText {
      | None => loop(i + 1, arrayConcat([Text("[")], acc))
      | Some(j) =>
        if j + 1 < length(text) && isChar(text, j + 1, "(") {
          let closeUrl = indexFromOpt(text, j + 2, ")")
          switch closeUrl {
          | None => loop(i + 1, arrayConcat([Text("[")], acc))
          | Some(k) =>
            let label = slice(text, ~start=i + 1, ~end=j)
            let url = slice(text, ~start=j + 2, ~end=k)
            loop(k + 1, arrayConcat([Link(label, url)], acc))
          }
        } else {
          loop(i + 1, arrayConcat([Text("[")], acc))
        }
      }
    } else {
      let nextSpecial = ["*", "["]
        ->arrayKeepMap(ch => indexFromOpt(text, i, ch))
      let next =
        if arrayLength(nextSpecial) == 0 {
          length(text)
        } else {
          arrayReduce(nextSpecial, length(text), (a, b) => if b < a {b} else {a})
        }
      let chunk = slice(text, ~start=i, ~end=next)
      loop(next, arrayConcat([Text(chunk)], acc))
    }
  loop(0, [])
}

let parseDirectiveHeader = (line: string): (string, string) => {
  let trimmed = trim(line)
  let withoutAt = sliceToEnd(trimmed, ~from=1)
  let body =
    if endsWith(withoutAt, ":") {
      slice(withoutAt, ~start=0, ~end=length(withoutAt) - 1)
    } else {
      withoutAt
    }
  let nameOnly = switch indexOfOpt(body, "(") {
    | None => body
    | Some(idx) => slice(body, ~start=0, ~end=idx)
  }
  (nameOnly, body)
}

let parseDirectiveLines = (
  lines: array<string>,
  startIndex: int,
  parseLine: string => option<block>,
): (array<block>, int) => {
  let blocks = arrayMake(0, Paragraph([]))
  let rec loop = i =>
    if i >= arrayLength(lines) {
      (blocks, i)
    } else {
      let line = arrayGetExn(lines, i)
      if trim(line) == "@end" {
        (blocks, i + 1)
      } else {
        switch parseLine(line) {
        | Some(block) => blocks->arrayPush(block)
        | None => ()
        }
        loop(i + 1)
      }
    }
  loop(startIndex)
}

let rec parseBlocks = (lines: array<string>, startIndex: int, stopAtEnd: bool): (array<block>, int) => {
  let blocks = arrayMake(0, Paragraph([]))

  let rec loop = i => {
    if i >= arrayLength(lines) {
      (blocks, i)
    } else {
      let line = arrayGetExn(lines, i)
      if stopAtEnd && trim(line) == "@end" {
        (blocks, i + 1)
      } else if trim(line) == "" {
        loop(i + 1)
      } else {
        switch isHeading(line) {
        | Some((level, text)) => {
            blocks->arrayPush(Heading(level, text))
            loop(i + 1)
          }
        | None =>
          if isDirectiveStart(line) {
            let (nameOnly, body) = parseDirectiveHeader(line)
            let attrs = parseAttrs(body)
            if nameOnly == "opaque" {
              let rec collectRaw = (j, acc) =>
                if j >= arrayLength(lines) {
                  (j, acc)
                } else {
                  let rawLine = arrayGetExn(lines, j)
                  if trim(rawLine) == "@end" {
                    (j + 1, acc)
                  } else {
                    collectRaw(j + 1, arrayConcat(acc, [rawLine]))
                  }
                }
              let (nextIndex, rawLines) = collectRaw(i + 1, [])
              let rawText = rawLines->arrayJoinWith("\n", s => s)
              blocks->arrayPush(Directive(nameOnly, attrs, [Paragraph([Text(rawText)])]))
              loop(nextIndex)
            } else if nameOnly == "refs" {
              let (refBlocks, nextIndex) =
                parseDirectiveLines(
                  lines,
                  i + 1,
                  refLine => {
                    let trimmed = trim(refLine)
                    if trimmed == "" {
                      None
                    } else {
                      Some(Paragraph(parseInline(trimmed)))
                    }
                  },
                )
              blocks->arrayPush(Directive(nameOnly, attrs, refBlocks))
              loop(nextIndex)
            } else {
              let (innerBlocks, nextIndex) = parseBlocks(lines, i + 1, true)
              blocks->arrayPush(Directive(nameOnly, attrs, innerBlocks))
              loop(nextIndex)
            }
          } else if startsWith(trim(line), "-") {
            let rec collect = (j, acc) =>
              if j >= arrayLength(lines) { (j, acc) } else {
                let l = trim(arrayGetExn(lines, j))
                if startsWith(l, "-") {
                  let item = trim(sliceToEnd(l, ~from=1))
                  collect(j + 1, arrayConcat(acc, [parseInline(item)]))
                } else {
                  (j, acc)
                }
              }
            let (nextIndex, items) = collect(i, [])
            blocks->arrayPush(List(items))
            loop(nextIndex)
          } else {
            // Multi-line paragraph: continue until blank or structural block
            let rec collect = (j, acc) =>
              if j >= arrayLength(lines) { (j, acc) } else {
                let l = arrayGetExn(lines, j)
                if trim(l) == "" ||
                   (stopAtEnd && trim(l) == "@end") ||
                   isDirectiveStart(l) ||
                   startsWith(trim(l), "-") ||
                   isHeading(l) != None {
                  (j, acc)
                } else {
                  collect(j + 1, arrayConcat(acc, [trim(l)]))
                }
              }
            let (nextIndex, parts) = collect(i, [])
            let text = parts->arrayJoinWith(" ", s => s)
            blocks->arrayPush(Paragraph(parseInline(text)))
            loop(nextIndex)
          }
        }
      }
    }
  }

  loop(startIndex)
}

let parse = (~mode: parseMode=Lax, input: string): doc => {
  let _ = mode
  let lines = split(input, "\n")
  let (blocks, _index) = parseBlocks(lines, 0, false)
  blocks
}

let renderInline = (parts: array<inline>): string => {
  parts
  ->arrayMap(part =>
      switch part {
      | Text(t) => t
      | Emph(t) => "<em>" ++ t ++ "</em>"
      | Strong(t) => "<strong>" ++ t ++ "</strong>"
      | Link(label, url) => "<a href=\"" ++ url ++ "\">" ++ label ++ "</a>"
      }
    )
  ->arrayJoinWith("", s => s)
}

let rec renderBlocks = (blocks: array<block>): string => {
  blocks
  ->arrayMap(block =>
      switch block {
      | Heading(level, text) =>
          "<h" ++ intToString(level) ++ ">" ++ text ++ "</h" ++ intToString(level) ++ ">"
      | Paragraph(parts) => "<p>" ++ renderInline(parts) ++ "</p>"
      | List(items) =>
          let lis =
            items
            ->arrayMap(item => "<li>" ++ renderInline(item) ++ "</li>")
            ->arrayJoinWith("", s => s)
          "<ul>" ++ lis ++ "</ul>"
      | Directive(name, attrs, body) =>
          let content = renderBlocks(body)
          let attrsString = attrs
            ->arrayMap(((k, v)) => k ++ "=\"" ++ v ++ "\"")
            ->arrayJoinWith(" ", s => s)
          let dataAttr = if attrsString == "" {""} else {" " ++ attrsString}
          "<div data-a2ml=\"" ++ name ++ "\"" ++ dataAttr ++ ">" ++ content ++ "</div>"
      }
    )
  ->arrayJoinWith("\n", s => s)
}

let renderHtml = (doc: doc): string => {
  renderBlocks(doc)
}

// Simple set implementation for strings
type stringSet = array<string>
let setEmpty = (): stringSet => []
let setHas = (set: stringSet, value: string): bool => {
  set->arrayMap(x => x == value)->arrayReduce(false, (acc, x) => acc || x)
}
let setAdd = (set: stringSet, value: string): stringSet => {
  if setHas(set, value) { set } else { arrayConcat(set, [value]) }
}

let validate = (doc: doc): array<parseError> => {
  let ids = ref(setEmpty())
  let refs = arrayMake(0, ("", 0))
  let errors = arrayMake(0, {line: 0, msg: ""})

  let rec walk = (blocks: array<block>, depthLine: int) => {
    blocks->arrayForEachWithIndex((i, block) => {
      let lineNo = depthLine + i + 1
      switch block {
      | Directive(_name, attrs, body) =>
          attrs->arrayForEach(((k, v)) => {
            if k == "id" {
              if setHas(ids.contents, v) {
                errors->arrayPush({line: lineNo, msg: "duplicate id: " ++ v})
              } else {
                ids.contents = setAdd(ids.contents, v)
              }
            } else if k == "ref" {
              refs->arrayPush((v, lineNo))
            } else {
              ()
            }
          })
          walk(body, lineNo)
      | _ => ()
      }
    })
  }

  walk(doc, 0)

  refs->arrayForEach(((refId, lineNo)) => {
    if !setHas(ids.contents, refId) {
      errors->arrayPush({line: lineNo, msg: "unresolved reference \"" ++ refId ++ "\""})
    }
  })

  errors
}

let setFromArray = (arr: array<string>): stringSet => arr

let validateChecked = (doc: doc): array<parseError> => {
  let errors = validate(doc)
  let allowed = setFromArray([
    "abstract",
    "refs",
    "fig",
    "table",
    "opaque",
    "section",
    "requires",
  ])

  let rec walk = (blocks: array<block>, depthLine: int) => {
    blocks->arrayForEachWithIndex((i, block) => {
      let lineNo = depthLine + i + 1
      switch block {
      | Directive(name, _attrs, body) =>
          if !setHas(allowed, name) {
            errors->arrayPush({line: lineNo, msg: "unknown directive: " ++ name})
          }
          walk(body, lineNo)
      | _ => ()
      }
    })
  }

  walk(doc, 0)
  errors
}
