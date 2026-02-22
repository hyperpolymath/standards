// SPDX-License-Identifier: PMPL-1.0-or-later
// ReScript v12 compatibility helpers

// Console
@val external consoleLog: 'a => unit = "console.log"

// String methods
@get external length: string => int = "length"
@send external slice: (string, ~start: int, ~end: int) => string = "slice"
@send external sliceToEnd: (string, ~from: int) => string = "slice"
@send external trim: string => string = "trim"
@send external indexOf: (string, string) => int = "indexOf"
@send external split: (string, string) => array<string> = "split"
@send external startsWith: (string, string) => bool = "startsWith"
@send external endsWith: (string, string) => bool = "endsWith"
@send external replace: (string, string, string) => string = "replace"

// Array methods
@get external arrayLength: array<'a> => int = "length"
@send external arrayPush: (array<'a>, 'a) => unit = "push"
@send external arrayConcat: (array<'a>, array<'a>) => array<'a> = "concat"
@send external arrayReverse: array<'a> => array<'a> = "reverse"
@send external arrayMap: (array<'a>, 'a => 'b) => array<'b> = "map"
@send external arrayFilter: (array<'a>, 'a => bool) => array<'a> = "filter"
@send external arrayJoin: (array<'a>, string) => string = "join"
@send external arrayForEach: (array<'a>, 'a => unit) => unit = "forEach"
@get_index external arrayGet: (array<'a>, int) => 'a = ""

@send external arrayReduceNative: (array<'a>, ('b, 'a) => 'b, 'b) => 'b = "reduce"
let arrayReduce = (arr: array<'a>, init: 'b, fn: ('b, 'a) => 'b): 'b => {
  arrayReduceNative(arr, fn, init)
}

// Helper functions
let arrayKeep = arrayFilter
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

let arrayForEachWithIndex = (arr: array<'a>, fn: (int, 'a) => unit): unit => {
  let idx = ref(0)
  arr->arrayForEach(item => {
    fn(idx.contents, item)
    idx.contents = idx.contents + 1
  })
}

// Int methods
@val external intToString: int => string = "String"

// JSON
type rec jsonValue =
  | String(string)
  | Number(float)
  | Bool(bool)
  | Null
  | Array(array<jsonValue>)
  | Object(array<(string, jsonValue)>)

@val external jsonStringify: 'a => string = "JSON.stringify"
