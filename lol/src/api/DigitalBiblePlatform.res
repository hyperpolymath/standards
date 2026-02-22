// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * Digital Bible Platform API
 *
 * Official API wrapper for the Digital Bible Platform,
 * providing access to Bible translations in 1500+ languages.
 * Implements all 6 API methods with proper error handling.
 */

module Config = {
  let apiBaseUrl = "https://api.scripture.api.bible/v1"
  let cdnBaseUrl = "https://cdn.scripture.api.bible"

  type environment =
    | Production
    | Sandbox

  let getBaseUrl = env =>
    switch env {
    | Production => apiBaseUrl
    | Sandbox => "https://api-sandbox.scripture.api.bible/v1"
    }
}

module Types = {
  type language = {
    id: string,
    name: string,
    nameLocal: string,
    script: string,
    scriptDirection: [#ltr | #rtl],
  }

  type bible = {
    id: string,
    dblId: string,
    abbreviation: string,
    abbreviationLocal: string,
    name: string,
    nameLocal: string,
    description: option<string>,
    descriptionLocal: option<string>,
    language: language,
    countries: array<string>,
    type_: string,
  }

  type book = {
    id: string,
    bibleId: string,
    abbreviation: string,
    name: string,
    nameLong: string,
    chapters: array<chapter>,
  }
  and chapter = {
    id: string,
    bibleId: string,
    bookId: string,
    number: string,
    reference: string,
  }

  type verse = {
    id: string,
    orgId: string,
    bibleId: string,
    bookId: string,
    chapterId: string,
    reference: string,
    content: string,
  }

  type passage = {
    id: string,
    bibleId: string,
    orgId: string,
    reference: string,
    content: string,
    verseCount: int,
    copyright: string,
  }

  type apiResponse<'a> = {
    data: 'a,
    meta: option<{
      fums: string,
      fumsId: string,
      fumsJs: string,
    }>,
  }

  type apiError = {
    statusCode: int,
    error: string,
    message: string,
  }
}

module JsonHelpers = {
  let getString = (obj: Dict.t<JSON.t>, key: string): string =>
    switch obj->Dict.get(key) {
    | Some(String(s)) => s
    | _ => ""
    }

  let getOptString = (obj: Dict.t<JSON.t>, key: string): option<string> =>
    switch obj->Dict.get(key) {
    | Some(String(s)) => Some(s)
    | _ => None
    }

  let getInt = (obj: Dict.t<JSON.t>, key: string): int =>
    switch obj->Dict.get(key) {
    | Some(Number(n)) => Float.toInt(n)
    | _ => 0
    }

  let getStringArray = (obj: Dict.t<JSON.t>, key: string): array<string> =>
    switch obj->Dict.get(key) {
    | Some(Array(arr)) => arr->Array.filterMap(v => switch v {
      | String(s) => Some(s)
      | _ => None
      })
    | _ => []
    }

  let getDirection = (obj: Dict.t<JSON.t>, key: string): [#ltr | #rtl] =>
    switch obj->Dict.get(key) {
    | Some(String("RTL")) | Some(String("rtl")) => #rtl
    | _ => #ltr
    }
}

module Client = {
  open Types
  open JsonHelpers

  type t = {
    apiKey: string,
    environment: Config.environment,
    rateLimiter: Crawler.RateLimiter.t,
  }

  let make = (~apiKey, ~environment=Config.Production, ()) => {
    apiKey,
    environment,
    rateLimiter: Crawler.RateLimiter.make(~delayMs=100, ()),
  }

  let getAuthHeaders = client => {
    let headers = Dict.make()
    Dict.set(headers, "api-key", client.apiKey)
    Dict.set(headers, "Accept", "application/json")
    headers
  }

  let makeError = (code, msg): apiError => {
    statusCode: code,
    error: "Error",
    message: msg,
  }

  /** Generic API request helper */
  let apiRequest = async (client: t, url: string): result<JSON.t, apiError> => {
    let headers = getAuthHeaders(client)
    let resp = await Http.getWithRateLimit(url, ~headers, ~rateLimiter=client.rateLimiter, ())
    switch resp {
    | Ok({body}) =>
      try {
        let json = JSON.parseExn(body)
        switch json {
        | Object(obj) =>
          switch obj->Dict.get("data") {
          | Some(data) => Ok(data)
          | None => Ok(json)
          }
        | _ => Ok(json)
        }
      } catch {
      | _ => Error(makeError(500, "Invalid JSON response"))
      }
    | Error(Http.HttpError(code, msg)) => Error(makeError(code, msg))
    | Error(Http.NetworkError(msg)) => Error(makeError(0, msg))
    | Error(Http.TimeoutError) => Error(makeError(408, "Request timed out"))
    | Error(Http.ParseError(msg)) => Error(makeError(422, msg))
    }
  }

  let parseLanguage = (obj: Dict.t<JSON.t>): language => {
    id: getString(obj, "id"),
    name: getString(obj, "name"),
    nameLocal: getString(obj, "nameLocal"),
    script: getString(obj, "script"),
    scriptDirection: getDirection(obj, "scriptDirection"),
  }

  let parseBible = (obj: Dict.t<JSON.t>): bible => {
    let lang = switch obj->Dict.get("language") {
    | Some(Object(l)) => parseLanguage(l)
    | _ => {id: "", name: "", nameLocal: "", script: "", scriptDirection: #ltr}
    }
    {
      id: getString(obj, "id"),
      dblId: getString(obj, "dblId"),
      abbreviation: getString(obj, "abbreviation"),
      abbreviationLocal: getString(obj, "abbreviationLocal"),
      name: getString(obj, "name"),
      nameLocal: getString(obj, "nameLocal"),
      description: getOptString(obj, "description"),
      descriptionLocal: getOptString(obj, "descriptionLocal"),
      language: lang,
      countries: getStringArray(obj, "countries"),
      type_: getString(obj, "type"),
    }
  }

  /** Get list of available Bibles, optionally filtered */
  let getBibles = async (
    client: t,
    ~language: option<string>=?,
    ~abbreviation: option<string>=?,
    (),
  ): result<array<bible>, apiError> => {
    let baseUrl = Config.getBaseUrl(client.environment)
    let params = []
    switch language {
    | Some(l) => ignore(Array.concat(params, [`language=${l}`]))
    | None => ()
    }
    switch abbreviation {
    | Some(a) => ignore(Array.concat(params, [`abbreviation=${a}`]))
    | None => ()
    }
    let queryStr = if Array.length(params) > 0 {
      "?" ++ params->Array.join("&")
    } else {
      ""
    }
    let resp = await apiRequest(client, `${baseUrl}/bibles${queryStr}`)
    switch resp {
    | Ok(Array(arr)) =>
      Ok(arr->Array.filterMap(item => switch item {
      | Object(obj) => Some(parseBible(obj))
      | _ => None
      }))
    | Ok(_) => Error(makeError(500, "Unexpected response format"))
    | Error(e) => Error(e)
    }
  }

  /** Get a specific Bible by ID */
  let getBible = async (client: t, ~bibleId: string): result<bible, apiError> => {
    let baseUrl = Config.getBaseUrl(client.environment)
    let resp = await apiRequest(client, `${baseUrl}/bibles/${bibleId}`)
    switch resp {
    | Ok(Object(obj)) => Ok(parseBible(obj))
    | Ok(_) => Error(makeError(500, "Unexpected response format"))
    | Error(e) => Error(e)
    }
  }

  /** Get books for a Bible */
  let getBooks = async (client: t, ~bibleId: string): result<array<book>, apiError> => {
    let baseUrl = Config.getBaseUrl(client.environment)
    let resp = await apiRequest(client, `${baseUrl}/bibles/${bibleId}/books`)
    switch resp {
    | Ok(Array(arr)) =>
      Ok(arr->Array.filterMap(item => switch item {
      | Object(obj) =>
        let chapters = switch obj->Dict.get("chapters") {
        | Some(Array(chArr)) => chArr->Array.filterMap(ch => switch ch {
          | Object(chObj) => Some({
              id: getString(chObj, "id"),
              bibleId: getString(chObj, "bibleId"),
              bookId: getString(chObj, "bookId"),
              number: getString(chObj, "number"),
              reference: getString(chObj, "reference"),
            })
          | _ => None
          })
        | _ => []
        }
        Some({
          id: getString(obj, "id"),
          bibleId: getString(obj, "bibleId"),
          abbreviation: getString(obj, "abbreviation"),
          name: getString(obj, "name"),
          nameLong: getString(obj, "nameLong"),
          chapters,
        })
      | _ => None
      }))
    | Ok(_) => Error(makeError(500, "Unexpected response format"))
    | Error(e) => Error(e)
    }
  }

  /** Get chapters for a book */
  let getChapters = async (
    client: t,
    ~bibleId: string,
    ~bookId: string,
  ): result<array<chapter>, apiError> => {
    let baseUrl = Config.getBaseUrl(client.environment)
    let resp = await apiRequest(client, `${baseUrl}/bibles/${bibleId}/books/${bookId}/chapters`)
    switch resp {
    | Ok(Array(arr)) =>
      Ok(arr->Array.filterMap(item => switch item {
      | Object(obj) => Some({
          id: getString(obj, "id"),
          bibleId: getString(obj, "bibleId"),
          bookId: getString(obj, "bookId"),
          number: getString(obj, "number"),
          reference: getString(obj, "reference"),
        })
      | _ => None
      }))
    | Ok(_) => Error(makeError(500, "Unexpected response format"))
    | Error(e) => Error(e)
    }
  }

  /** Get verses for a chapter */
  let getVerses = async (
    client: t,
    ~bibleId: string,
    ~chapterId: string,
  ): result<array<verse>, apiError> => {
    let baseUrl = Config.getBaseUrl(client.environment)
    let resp = await apiRequest(
      client,
      `${baseUrl}/bibles/${bibleId}/chapters/${chapterId}/verses`,
    )
    switch resp {
    | Ok(Array(arr)) =>
      Ok(arr->Array.filterMap(item => switch item {
      | Object(obj) => Some({
          id: getString(obj, "id"),
          orgId: getString(obj, "orgId"),
          bibleId: getString(obj, "bibleId"),
          bookId: getString(obj, "bookId"),
          chapterId: getString(obj, "chapterId"),
          reference: getString(obj, "reference"),
          content: getString(obj, "content"),
        })
      | _ => None
      }))
    | Ok(_) => Error(makeError(500, "Unexpected response format"))
    | Error(e) => Error(e)
    }
  }

  /** Get a passage (range of verses) */
  let getPassage = async (
    client: t,
    ~bibleId: string,
    ~passageId: string,
  ): result<passage, apiError> => {
    let baseUrl = Config.getBaseUrl(client.environment)
    let resp = await apiRequest(client, `${baseUrl}/bibles/${bibleId}/passages/${passageId}`)
    switch resp {
    | Ok(Object(obj)) =>
      Ok({
        id: getString(obj, "id"),
        bibleId: getString(obj, "bibleId"),
        orgId: getString(obj, "orgId"),
        reference: getString(obj, "reference"),
        content: getString(obj, "content"),
        verseCount: getInt(obj, "verseCount"),
        copyright: getString(obj, "copyright"),
      })
    | Ok(_) => Error(makeError(500, "Unexpected response format"))
    | Error(e) => Error(e)
    }
  }
}
