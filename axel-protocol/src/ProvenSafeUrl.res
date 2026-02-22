// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
/**
 * SafeUrl - URL parsing that cannot crash
 *
 * ReScript bindings to proven's formally verified URL module
 */

open ProvenResult

// Parsed URL components
type parsedUrl = {
  protocol: string,
  host: string,
  hostname: string,
  port: string,
  pathname: string,
  search: string,
  hash: string,
  origin: string,
  href: string,
}

// JavaScript bindings to proven/safe_url
module SafeUrlJs = {
  @module("proven/safe_url") @scope("SafeUrl")
  external parse: (string, option<string>) => jsResult<parsedUrl> = "parse"

  @module("proven/safe_url") @scope("SafeUrl")
  external isValid: string => bool = "isValid"

  @module("proven/safe_url") @scope("SafeUrl")
  external getQueryParam: (string, string) => jsResult<option<string>> = "getQueryParam"

  @module("proven/safe_url") @scope("SafeUrl")
  external getQueryParams: string => jsResult<Js.Dict.t<string>> = "getQueryParams"

  @module("proven/safe_url") @scope("SafeUrl")
  external setQueryParam: (string, string, string) => jsResult<string> = "setQueryParam"

  @module("proven/safe_url") @scope("SafeUrl")
  external removeQueryParam: (string, string) => jsResult<string> = "removeQueryParam"

  @module("proven/safe_url") @scope("SafeUrl")
  external join: (string, array<string>) => jsResult<string> = "join"

  @module("proven/safe_url") @scope("SafeUrl")
  external getDomain: string => jsResult<string> = "getDomain"

  @module("proven/safe_url") @scope("SafeUrl")
  external isHttps: string => bool = "isHttps"

  @module("proven/safe_url") @scope("SafeUrl")
  external encode: string => string = "encode"

  @module("proven/safe_url") @scope("SafeUrl")
  external decode: string => jsResult<string> = "decode"

  @module("proven/safe_url") @scope("SafeUrl")
  external normalize: string => jsResult<string> = "normalize"
}

// Type-safe ReScript API
/**
 * Parse a URL string safely
 *
 * @param urlString URL to parse
 * @param base Optional base URL
 * @returns Result with parsed URL or error message
 */
let parse = (urlString: string, ~base: option<string>=?) => {
  SafeUrlJs.parse(urlString, base)->fromJs
}

/**
 * Check if string is a valid URL
 */
let isValid = SafeUrlJs.isValid

/**
 * Get query parameter from URL
 *
 * @param urlString URL string
 * @param param Parameter name
 * @returns Result with parameter value (None if not present) or error
 */
let getQueryParam = (urlString: string, param: string) => {
  SafeUrlJs.getQueryParam(urlString, param)->fromJs
}

/**
 * Get all query parameters as dictionary
 *
 * @param urlString URL string
 * @returns Result with dictionary of parameters or error
 */
let getQueryParams = (urlString: string) => {
  SafeUrlJs.getQueryParams(urlString)->fromJs
}

/**
 * Set query parameter on URL
 *
 * @param urlString URL string
 * @param param Parameter name
 * @param value Parameter value
 * @returns Result with new URL string or error
 */
let setQueryParam = (urlString: string, param: string, value: string) => {
  SafeUrlJs.setQueryParam(urlString, param, value)->fromJs
}

/**
 * Remove query parameter from URL
 *
 * @param urlString URL string
 * @param param Parameter name
 * @returns Result with new URL string or error
 */
let removeQueryParam = (urlString: string, param: string) => {
  SafeUrlJs.removeQueryParam(urlString, param)->fromJs
}

/**
 * Join URL paths safely
 *
 * @param base Base URL
 * @param paths Path segments to join
 * @returns Result with joined URL or error
 */
let join = (base: string, paths: array<string>) => {
  SafeUrlJs.join(base, paths)->fromJs
}

/**
 * Get the domain from a URL
 *
 * @param urlString URL string
 * @returns Result with domain or error
 */
let getDomain = (urlString: string) => {
  SafeUrlJs.getDomain(urlString)->fromJs
}

/**
 * Check if URL uses HTTPS
 */
let isHttps = SafeUrlJs.isHttps

/**
 * Encode URL component safely
 */
let encode = SafeUrlJs.encode

/**
 * Decode URL component safely
 *
 * @param str String to decode
 * @returns Result with decoded string or error
 */
let decode = (str: string) => {
  SafeUrlJs.decode(str)->fromJs
}

/**
 * Normalize a URL (lowercase scheme/host, remove default port)
 *
 * @param urlString URL string
 * @returns Result with normalized URL or error
 */
let normalize = (urlString: string) => {
  SafeUrlJs.normalize(urlString)->fromJs
}
