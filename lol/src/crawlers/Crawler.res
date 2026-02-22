// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * Base Crawler Module
 *
 * Provides the foundational types and functions for web crawling
 * Bible corpus sources.
 */

module Types = {
  type httpMethod = GET | POST | HEAD

  type requestConfig = {
    url: string,
    method: httpMethod,
    headers: Dict.t<string>,
    timeout: int,
    retries: int,
  }

  type responseStatus =
    | Ok(int)
    | NetworkError(string)
    | Timeout
    | RateLimited
    | NotFound

  type response = {
    status: responseStatus,
    headers: Dict.t<string>,
    body: option<string>,
  }

  type crawlResult<'a> =
    | Success('a)
    | Failure(string)
    | Pending

  type crawlerState =
    | Idle
    | Crawling(string)
    | Paused
    | Stopped
    | Error(string)
}

module Config = {
  let defaultTimeout = 30000
  let defaultRetries = 3
  let defaultUserAgent = "1000Langs/0.1.0 (Parallel Corpus Crawler; +https://github.com/Hyperpolymath/1000Langs)"
  let defaultRateLimitMs = 1000

  let makeDefaultHeaders = () => {
    let headers = Dict.make()
    Dict.set(headers, "User-Agent", defaultUserAgent)
    Dict.set(headers, "Accept", "text/html,application/xhtml+xml,application/xml")
    Dict.set(headers, "Accept-Language", "en-US,en;q=0.9")
    headers
  }
}

module Request = {
  open Types

  let make = (~url, ~method=GET, ~headers=?, ~timeout=?, ~retries=?, ()) => {
    url,
    method,
    headers: headers->Option.getOr(Config.makeDefaultHeaders()),
    timeout: timeout->Option.getOr(Config.defaultTimeout),
    retries: retries->Option.getOr(Config.defaultRetries),
  }

  let withHeader = (config, key, value) => {
    Dict.set(config.headers, key, value)
    config
  }

  let methodToString = method => switch method {
    | GET => "GET"
    | POST => "POST"
    | HEAD => "HEAD"
  }
}

module RateLimiter = {
  type t = {
    mutable lastRequest: float,
    delayMs: int,
  }

  let make = (~delayMs=Config.defaultRateLimitMs, ()) => {
    lastRequest: 0.0,
    delayMs,
  }

  let canProceed = limiter => {
    let now = Date.now()
    let elapsed = now -. limiter.lastRequest
    elapsed >= Float.fromInt(limiter.delayMs)
  }

  let recordRequest = limiter => {
    limiter.lastRequest = Date.now()
  }
}

module RetryPolicy = {
  type backoffStrategy =
    | Constant(int)
    | Linear(int)
    | Exponential(int, float)

  let calculateDelay = (strategy, attempt) => switch strategy {
    | Constant(ms) => ms
    | Linear(base) => base * attempt
    | Exponential(base, factor) =>
        Float.toInt(Float.fromInt(base) *. Math.pow(factor, ~exp=Float.fromInt(attempt - 1)))
  }

  let shouldRetry = (attempt, maxRetries) => attempt < maxRetries
}

module Parser = {
  type selector =
    | Css(string)
    | XPath(string)
    | Regex(string)

  type parseResult<'a> =
    | Parsed('a)
    | ParseError(string)
    | NoMatch

  let selectorToString = selector => switch selector {
    | Css(s) => `css:${s}`
    | XPath(s) => `xpath:${s}`
    | Regex(s) => `regex:${s}`
  }
}
