// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// SafeUrl - URL parsing that cannot crash
// ReScript bindings to proven's formally verified URL module

open ProvenResult

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

module SafeUrlJs = {
  @module("proven/safe_url") @scope("SafeUrl")
  external parse: (string, option<string>) => jsResult<parsedUrl> = "parse"

  @module("proven/safe_url") @scope("SafeUrl")
  external isValid: string => bool = "isValid"

  @module("proven/safe_url") @scope("SafeUrl")
  external getQueryParam: (string, string) => jsResult<option<string>> = "getQueryParam"

  @module("proven/safe_url") @scope("SafeUrl")
  external getQueryParams: string => jsResult<Dict.t<string>> = "getQueryParams"

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

let parse = (urlString: string, ~base: option<string>=?) => {
  SafeUrlJs.parse(urlString, base)->fromJs
}

let isValid = SafeUrlJs.isValid

let getQueryParam = (urlString: string, param: string) => {
  SafeUrlJs.getQueryParam(urlString, param)->fromJs
}

let getQueryParams = (urlString: string) => {
  SafeUrlJs.getQueryParams(urlString)->fromJs
}

let setQueryParam = (urlString: string, param: string, value: string) => {
  SafeUrlJs.setQueryParam(urlString, param, value)->fromJs
}

let removeQueryParam = (urlString: string, param: string) => {
  SafeUrlJs.removeQueryParam(urlString, param)->fromJs
}

let join = (base: string, paths: array<string>) => {
  SafeUrlJs.join(base, paths)->fromJs
}

let getDomain = (urlString: string) => {
  SafeUrlJs.getDomain(urlString)->fromJs
}

let isHttps = SafeUrlJs.isHttps

let encode = SafeUrlJs.encode

let decode = (str: string) => {
  SafeUrlJs.decode(str)->fromJs
}

let normalize = (urlString: string) => {
  SafeUrlJs.normalize(urlString)->fromJs
}
