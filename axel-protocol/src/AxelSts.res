// SPDX-License-Identifier: PMPL-1.0-or-later
// AXEL Protocol - DNS TXT Record Parser (Strict)
//
// Parses AXEL DNS TXT record payloads (RDATA only, not full RR lines).
// Fails closed on missing required fields. Does NOT default v=AXEL1.

type parseError =
  | MissingVersion
  | InvalidVersion(string)
  | MissingId
  | EmptyId
  | MalformedRecord(string)

type axelRecord = {
  version: string,
  id: string,
}

type parseResult = result<axelRecord, parseError>

// String utilities via external bindings
@send external trim: (string) => string = "trim"
@send external split: (string, string) => array<string> = "split"
@send external startsWith: (string, string) => bool = "startsWith"
@get external length: string => int = "length"
@send external slice: (array<string>, int) => array<string> = "slice"
@send external joinArray: (array<string>, string) => string = "join"

let getArrayLength: array<string> => int = %raw(`function(arr) { return arr.length }`)
let getArrayItem: (array<string>, int) => string = %raw(`function(arr, i) { return arr[i] }`)

// Parse a single key=value pair from a TXT record segment
let parseKeyValue = (segment: string): option<(string, string)> => {
  let trimmed = segment->trim
  let parts = trimmed->split("=")
  let len = getArrayLength(parts)
  if len < 2 {
    None
  } else {
    let key = getArrayItem(parts, 0)->trim
    let value = parts->slice(1)->joinArray("=")->trim
    Some((key, value))
  }
}

// Parse a TXT record payload string into an AXEL record
let parse = (payload: string): parseResult => {
  let trimmed = payload->trim

  if trimmed->length == 0 {
    Error(MalformedRecord("empty payload"))
  } else {
    let segments = trimmed->split(";")
    let version = ref(None)
    let id = ref(None)

    let segmentCount = getArrayLength(segments)
    for i in 0 to segmentCount - 1 {
      let segment = getArrayItem(segments, i)
      switch parseKeyValue(segment) {
      | Some(("v", value)) => version := Some(value)
      | Some(("id", value)) => id := Some(value)
      | _ => ()
      }
    }

    switch (version.contents, id.contents) {
    | (None, _) => Error(MissingVersion)
    | (Some(v), _) if v != "AXEL1" => Error(InvalidVersion(v))
    | (Some(_), None) => Error(MissingId)
    | (Some(_), Some(idVal)) if idVal->trim->length == 0 => Error(EmptyId)
    | (Some(v), Some(idVal)) =>
      Ok({
        version: v,
        id: idVal->trim,
      })
    }
  }
}

// Convert parse error to human-readable string
let errorToString = (err: parseError): string => {
  switch err {
  | MissingVersion => "Missing required field: v (version)"
  | InvalidVersion(v) => "Invalid version: '" ++ v ++ "' (expected 'AXEL1')"
  | MissingId => "Missing required field: id"
  | EmptyId => "Empty id field (must be non-empty)"
  | MalformedRecord(msg) => "Malformed record: " ++ msg
  }
}
