// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * HTTP Fetch Module
 *
 * ReScript bindings to Deno's native fetch() API with rate limiting,
 * retry logic, and error handling for crawler use.
 */

/** Deno/Web fetch API bindings */
module Fetch = {
  type response

  @val external fetch: (string, {..}) => promise<response> = "fetch"
  @val external fetchSimple: string => promise<response> = "fetch"

  @get external status: response => int = "status"
  @get external ok: response => bool = "ok"
  @get external statusText: response => string = "statusText"
  @send external text: response => promise<string> = "text"
  @send external json: response => promise<JSON.t> = "json"
  @send external getHeader: (response, string) => option<string> = "headers.get"
}

type httpError =
  | NetworkError(string)
  | HttpError(int, string)
  | TimeoutError
  | ParseError(string)

type httpResponse = {
  status: int,
  body: string,
  headers: Dict.t<string>,
}

/** Perform a GET request with optional headers */
let get = async (url: string, ~headers: option<Dict.t<string>>=?, ()): result<
  httpResponse,
  httpError,
> => {
  try {
    let resp = switch headers {
    | Some(h) =>
      let headerDict = Dict.make()
      h->Dict.toArray->Array.forEach(((k, v)) => {
        headerDict->Dict.set(k, JSON.Encode.string(v))
      })
      await Fetch.fetch(
        url,
        {"method": "GET", "headers": JSON.Encode.object(headerDict)},
      )
    | None => await Fetch.fetchSimple(url)
    }

    let body = await Fetch.text(resp)
    let status = Fetch.status(resp)

    if Fetch.ok(resp) {
      Ok({status, body, headers: Dict.make()})
    } else if status == 429 {
      Error(HttpError(429, "Rate limited"))
    } else if status == 404 {
      Error(HttpError(404, "Not found"))
    } else {
      Error(HttpError(status, Fetch.statusText(resp)))
    }
  } catch {
  | exn =>
    let msg = switch exn {
    | Exn.Error(e) => Exn.message(e)->Option.getOr("Unknown error")
    | _ => "Unknown error"
    }
    Error(NetworkError(msg))
  }
}

/** Perform a GET request and parse response as JSON */
let getJson = async (url: string, ~headers: option<Dict.t<string>>=?, ()): result<
  JSON.t,
  httpError,
> => {
  let resp = await get(url, ~headers?, ())
  switch resp {
  | Ok({body}) =>
    try {
      Ok(JSON.parseExn(body))
    } catch {
    | _ => Error(ParseError("Invalid JSON response"))
    }
  | Error(e) => Error(e)
  }
}

/** Perform a GET request with rate limiter integration */
let getWithRateLimit = async (
  url: string,
  ~headers: option<Dict.t<string>>=?,
  ~rateLimiter: Crawler.RateLimiter.t,
  (),
): result<httpResponse, httpError> => {
  // Wait until rate limit allows
  let rec waitForLimit = async () => {
    if Crawler.RateLimiter.canProceed(rateLimiter) {
      Crawler.RateLimiter.recordRequest(rateLimiter)
    } else {
      await Promise.make((resolve, _) => {
        let _ = setTimeout(() => resolve(.), rateLimiter.delayMs)
      })
      await waitForLimit()
    }
  }
  await waitForLimit()
  await get(url, ~headers?, ())
}

/** Perform a GET request with retry logic */
let getWithRetry = async (
  url: string,
  ~headers: option<Dict.t<string>>=?,
  ~maxRetries=3,
  ~backoff=Crawler.RetryPolicy.Exponential(1000, 2.0),
  (),
): result<httpResponse, httpError> => {
  let rec attempt = async (n: int) => {
    let resp = await get(url, ~headers?, ())
    switch resp {
    | Ok(_) => resp
    | Error(HttpError(429, _)) | Error(NetworkError(_)) if n < maxRetries =>
      let delay = Crawler.RetryPolicy.calculateDelay(backoff, n + 1)
      await Promise.make((resolve, _) => {
        let _ = setTimeout(() => resolve(.), delay)
      })
      await attempt(n + 1)
    | Error(_) => resp
    }
  }
  await attempt(0)
}

/** Deno setTimeout binding */
@val external setTimeout: (unit => unit, int) => int = "setTimeout"
