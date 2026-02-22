// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * Bible.cloud Crawler
 *
 * Crawler implementation for the Digital Bible Platform (bible.cloud)
 * API-based access to Bible translations in 1500+ languages.
 */

open Crawler.Types

module Config = {
  let baseUrl = "https://api.scripture.api.bible/v1"
  let webUrl = "https://bible.cloud"
  let apiVersion = "v1"
  let rateLimitMs = 500

  type apiCredentials = {
    apiKey: string,
    apiSecret: option<string>,
  }
}

module Endpoints = {
  let bibles = () => `${Config.baseUrl}/bibles`
  let bible = bibleId => `${Config.baseUrl}/bibles/${bibleId}`
  let books = bibleId => `${Config.baseUrl}/bibles/${bibleId}/books`
  let chapters = (bibleId, bookId) =>
    `${Config.baseUrl}/bibles/${bibleId}/books/${bookId}/chapters`
  let verses = (bibleId, chapterId) =>
    `${Config.baseUrl}/bibles/${bibleId}/chapters/${chapterId}/verses`
  let verse = (bibleId, verseId) => `${Config.baseUrl}/bibles/${bibleId}/verses/${verseId}`
}

module Types = {
  type bibleInfo = {
    id: string,
    name: string,
    nameLocal: string,
    language: Lang1000.Language.t,
    description: option<string>,
    copyright: option<string>,
  }

  type bookInfo = {
    id: string,
    bibleId: string,
    abbreviation: string,
    name: string,
    nameLong: string,
  }

  type chapterInfo = {
    id: string,
    bibleId: string,
    bookId: string,
    number: string,
    reference: string,
  }
}

module Parser = {
  open Types

  /** Parse the data array from an API response JSON */
  let getDataArray = (json: JSON.t): option<array<JSON.t>> => {
    switch json {
    | Object(obj) =>
      switch obj->Dict.get("data") {
      | Some(Array(arr)) => Some(arr)
      | _ => None
      }
    | _ => None
    }
  }

  /** Extract a string field from a JSON object */
  let getString = (obj: Dict.t<JSON.t>, key: string): string => {
    switch obj->Dict.get(key) {
    | Some(String(s)) => s
    | _ => ""
    }
  }

  let getOptString = (obj: Dict.t<JSON.t>, key: string): option<string> => {
    switch obj->Dict.get(key) {
    | Some(String(s)) => Some(s)
    | _ => None
    }
  }

  let parseBibleList = (json: JSON.t): Crawler.Parser.parseResult<array<bibleInfo>> => {
    switch getDataArray(json) {
    | None => Crawler.Parser.NoMatch
    | Some(arr) =>
      let bibles = arr->Array.filterMap(item => {
        switch item {
        | Object(obj) =>
          let langObj = switch obj->Dict.get("language") {
          | Some(Object(l)) => l
          | _ => Dict.make()
          }
          Some({
            id: getString(obj, "id"),
            name: getString(obj, "name"),
            nameLocal: getString(obj, "nameLocal"),
            language: Lang1000.Language.make(
              ~code=getString(langObj, "id"),
              ~name=getString(langObj, "name"),
              ~script=getString(langObj, "script"),
              (),
            ),
            description: getOptString(obj, "description"),
            copyright: getOptString(obj, "copyright"),
          })
        | _ => None
        }
      })
      Crawler.Parser.Parsed(bibles)
    }
  }

  let parseBooks = (json: JSON.t): Crawler.Parser.parseResult<array<bookInfo>> => {
    switch getDataArray(json) {
    | None => Crawler.Parser.NoMatch
    | Some(arr) =>
      let books = arr->Array.filterMap(item => {
        switch item {
        | Object(obj) =>
          Some({
            id: getString(obj, "id"),
            bibleId: getString(obj, "bibleId"),
            abbreviation: getString(obj, "abbreviation"),
            name: getString(obj, "name"),
            nameLong: getString(obj, "nameLong"),
          })
        | _ => None
        }
      })
      Crawler.Parser.Parsed(books)
    }
  }

  let parseChapters = (json: JSON.t): Crawler.Parser.parseResult<array<chapterInfo>> => {
    switch getDataArray(json) {
    | None => Crawler.Parser.NoMatch
    | Some(arr) =>
      let chapters = arr->Array.filterMap(item => {
        switch item {
        | Object(obj) =>
          Some({
            id: getString(obj, "id"),
            bibleId: getString(obj, "bibleId"),
            bookId: getString(obj, "bookId"),
            number: getString(obj, "number"),
            reference: getString(obj, "reference"),
          })
        | _ => None
        }
      })
      Crawler.Parser.Parsed(chapters)
    }
  }

  let parseVerses = (json: JSON.t, languageCode: string): Crawler.Parser.parseResult<
    array<Lang1000.Verse.t>,
  > => {
    switch getDataArray(json) {
    | None => Crawler.Parser.NoMatch
    | Some(arr) =>
      let verses = arr->Array.filterMap(item => {
        switch item {
        | Object(obj) =>
          let ref = getString(obj, "reference")
          let content = getString(obj, "content")
          // Parse reference like "GEN.1.1" into book/chapter/verse
          let parts = ref->String.split(".")
          if Array.length(parts) >= 3 {
            let book = Array.getUnsafe(parts, 0)
            let chapter = Array.getUnsafe(parts, 1)->Int.fromString->Option.getOr(1)
            let verse = Array.getUnsafe(parts, 2)->Int.fromString->Option.getOr(1)
            Some(
              Lang1000.Verse.make(
                ~reference=Lang1000.Verse.makeReference(~book, ~chapter, ~verse),
                ~text=content->String.trim,
                ~language=languageCode,
              ),
            )
          } else {
            None
          }
        | _ => None
        }
      })
      Crawler.Parser.Parsed(verses)
    }
  }
}

module Crawler = {
  type t = {
    credentials: option<Config.apiCredentials>,
    rateLimiter: Crawler.RateLimiter.t,
    mutable state: crawlerState,
  }

  let make = (~apiKey=?, ()) => {
    credentials: apiKey->Option.map(key => {Config.apiKey: key, apiSecret: None}),
    rateLimiter: Crawler.RateLimiter.make(~delayMs=Config.rateLimitMs, ()),
    state: Idle,
  }

  let getHeaders = (crawler: t): Dict.t<string> => {
    let headers = Crawler.Config.makeDefaultHeaders()
    switch crawler.credentials {
    | Some({apiKey}) => Dict.set(headers, "api-key", apiKey)
    | None => ()
    }
    Dict.set(headers, "Accept", "application/json")
    headers
  }

  let fetchBibles = async (crawler: t): crawlResult<array<Types.bibleInfo>> => {
    crawler.state = Crawling("bibles")
    let headers = getHeaders(crawler)
    let resp = await Http.getWithRateLimit(
      Endpoints.bibles(),
      ~headers,
      ~rateLimiter=crawler.rateLimiter,
      (),
    )
    crawler.state = Idle
    switch resp {
    | Ok({body}) =>
      let json = JSON.parseExn(body)
      switch Parser.parseBibleList(json) {
      | Crawler.Parser.Parsed(bibles) => Success(bibles)
      | _ => Failure("Failed to parse bibles list")
      }
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(Http.NetworkError(msg)) => Failure(`Network error: ${msg}`)
    | Error(Http.TimeoutError) => Failure("Request timed out")
    | Error(Http.ParseError(msg)) => Failure(`Parse error: ${msg}`)
    }
  }

  let fetchBooks = async (crawler: t, bibleId: string): crawlResult<array<Types.bookInfo>> => {
    let headers = getHeaders(crawler)
    let resp = await Http.getWithRateLimit(
      Endpoints.books(bibleId),
      ~headers,
      ~rateLimiter=crawler.rateLimiter,
      (),
    )
    switch resp {
    | Ok({body}) =>
      let json = JSON.parseExn(body)
      switch Parser.parseBooks(json) {
      | Crawler.Parser.Parsed(books) => Success(books)
      | _ => Failure("Failed to parse books")
      }
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(_) => Failure("Request failed")
    }
  }

  let fetchChapter = async (
    crawler: t,
    bibleId: string,
    chapterId: string,
    languageCode: string,
  ): crawlResult<array<Lang1000.Verse.t>> => {
    let headers = getHeaders(crawler)
    let resp = await Http.getWithRateLimit(
      Endpoints.verses(bibleId, chapterId),
      ~headers,
      ~rateLimiter=crawler.rateLimiter,
      (),
    )
    switch resp {
    | Ok({body}) =>
      let json = JSON.parseExn(body)
      switch Parser.parseVerses(json, languageCode) {
      | Crawler.Parser.Parsed(verses) => Success(verses)
      | _ => Failure("Failed to parse verses")
      }
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(_) => Failure("Request failed")
    }
  }
}
