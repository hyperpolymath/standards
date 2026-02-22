// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * Vitest Bindings for ReScript
 *
 * Minimal bindings to the Vitest testing framework.
 */

@module("vitest") @val
external describe: (string, @uncurry unit => unit) => unit = "describe"

@module("vitest") @val
external test: (string, @uncurry unit => unit) => unit = "test"

@module("vitest") @val
external it: (string, @uncurry unit => unit) => unit = "it"

type expectation<'a>

@module("vitest") @val
external expect: 'a => expectation<'a> = "expect"

@send external toBe: (expectation<'a>, 'a) => unit = "toBe"
@send external toEqual: (expectation<'a>, 'a) => unit = "toEqual"
@send external toBeTruthy: expectation<'a> => unit = "toBeTruthy"
@send external toBeFalsy: expectation<'a> => unit = "toBeFalsy"
@send external toBeNull: expectation<'a> => unit = "toBeNull"
@send external toBeUndefined: expectation<'a> => unit = "toBeUndefined"
@send external toBeDefined: expectation<'a> => unit = "toBeDefined"
@send external toBeGreaterThan: (expectation<'a>, 'a) => unit = "toBeGreaterThan"
@send external toBeGreaterThanOrEqual: (expectation<'a>, 'a) => unit = "toBeGreaterThanOrEqual"
@send external toBeLessThan: (expectation<'a>, 'a) => unit = "toBeLessThan"
@send external toBeLessThanOrEqual: (expectation<'a>, 'a) => unit = "toBeLessThanOrEqual"
@send external toContain: (expectation<array<'a>>, 'a) => unit = "toContain"
@send external toHaveLength: (expectation<array<'a>>, int) => unit = "toHaveLength"
@send external toMatch: (expectation<string>, string) => unit = "toMatch"
@send external toMatchRegex: (expectation<string>, Js.Re.t) => unit = "toMatch"
@send external toThrow: expectation<unit => 'a> => unit = "toThrow"
@send external toThrowError: (expectation<unit => 'a>, string) => unit = "toThrowError"

module Expect = {
  @send external not_: expectation<'a> => expectation<'a> = "not"
}

@module("vitest") @val
external beforeAll: (@uncurry unit => unit) => unit = "beforeAll"

@module("vitest") @val
external afterAll: (@uncurry unit => unit) => unit = "afterAll"

@module("vitest") @val
external beforeEach: (@uncurry unit => unit) => unit = "beforeEach"

@module("vitest") @val
external afterEach: (@uncurry unit => unit) => unit = "afterEach"

@module("vitest") @val
external fail: unit => unit = "fail"

@module("vitest") @val
external failWithMessage: string => unit = "fail"
