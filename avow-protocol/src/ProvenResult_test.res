// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

@module("@std/assert") external assertEquals: ('a, 'a) => unit = "assertEquals"
@module("@std/assert") external assertNotEquals: ('a, 'a) => unit = "assertNotEquals"

@val @scope("Deno") external test: (string, unit => unit) => unit = "test"

// --- fromJs tests ---

let () = test("fromJs: ok result with value converts to Ok", () => {
  let jsResult: ProvenResult.jsResult<string> = {
    ok: true,
    value: Some("hello"),
    error: None,
  }
  let result = ProvenResult.fromJs(jsResult)
  assertEquals(result, Ok("hello"))
})

let () = test("fromJs: ok result missing value converts to Error", () => {
  let jsResult: ProvenResult.jsResult<string> = {
    ok: true,
    value: None,
    error: None,
  }
  let result = ProvenResult.fromJs(jsResult)
  assertEquals(result, Error("Ok result missing value"))
})

let () = test("fromJs: error result with message converts to Error", () => {
  let jsResult: ProvenResult.jsResult<string> = {
    ok: false,
    value: None,
    error: Some("parse failed"),
  }
  let result = ProvenResult.fromJs(jsResult)
  assertEquals(result, Error("parse failed"))
})

let () = test("fromJs: error result without message converts to Unknown error", () => {
  let jsResult: ProvenResult.jsResult<string> = {
    ok: false,
    value: None,
    error: None,
  }
  let result = ProvenResult.fromJs(jsResult)
  assertEquals(result, Error("Unknown error"))
})

// --- toJs tests ---

let () = test("toJs: Ok value converts to jsResult with ok=true", () => {
  let jsResult = ProvenResult.toJs(Ok("world"))
  assertEquals(jsResult.ok, true)
  assertEquals(jsResult.value, Some("world"))
  assertEquals(jsResult.error, None)
})

let () = test("toJs: Error value converts to jsResult with ok=false", () => {
  let jsResult = ProvenResult.toJs(Error("bad input"))
  assertEquals(jsResult.ok, false)
  assertEquals(jsResult.value, None)
  assertEquals(jsResult.error, Some("bad input"))
})

// --- round-trip tests ---

let () = test("round-trip: Ok -> toJs -> fromJs preserves value", () => {
  let original: result<int, string> = Ok(42)
  let roundTripped = original->ProvenResult.toJs->ProvenResult.fromJs
  assertEquals(roundTripped, original)
})

let () = test("round-trip: Error -> toJs -> fromJs preserves error", () => {
  let original: result<int, string> = Error("not found")
  let roundTripped = original->ProvenResult.toJs->ProvenResult.fromJs
  assertEquals(roundTripped, original)
})
