// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
/**
 * Result type for proven bindings
 * Matches the JavaScript { ok: boolean, value?: T, error?: string } pattern
 */

type t<'value, 'error> = result<'value, 'error>

// JavaScript interop types
type jsResult<'value> = {
  ok: bool,
  value: option<'value>,
  error: option<string>,
}

@module("proven/result")
external okJs: 'value => jsResult<'value> = "ok"

@module("proven/result")
external errJs: string => jsResult<'never> = "err"

// Convert JavaScript result to ReScript result
let fromJs = (jsResult: jsResult<'value>): result<'value, string> => {
  if jsResult.ok {
    switch jsResult.value {
    | Some(v) => Ok(v)
    | None => Error("Ok result missing value")
    }
  } else {
    switch jsResult.error {
    | Some(e) => Error(e)
    | None => Error("Unknown error")
    }
  }
}

// Convert ReScript result to JavaScript result
let toJs = (result: result<'value, string>): jsResult<'value> => {
  switch result {
  | Ok(value) => {ok: true, value: Some(value), error: None}
  | Error(error) => {ok: false, value: None, error: Some(error)}
  }
}
