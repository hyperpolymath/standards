// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * Find.Bible Crawler
 *
 * Crawler for find.bible - API + HTML hybrid approach for
 * language discovery and per-language Bible text retrieval.
 */

open Crawler.Types

module Config = {
  let baseUrl = "https://find.bible"
  let apiUrl = "https://find.bible/api"
  let rateLimitMs = 1000
}

module Endpoints = {
  let languages = () => `${Config.apiUrl}/languages`
  let language = langCode => `${Config.apiUrl}/languages/${langCode}`
  let bibles = langCode => `${Config.apiUrl}/bibles?language=${langCode}`
  let bible = bibleId => `${Config.apiUrl}/bibles/${bibleId}`
  let text = (bibleId, book, chapter) =>
    `${Config.apiUrl}/bibles/${bibleId}/${book}/${Int.toString(chapter)}`
}

module Types = {
  type languageInfo = {
    code: string,
    name: string,
    nativeName: option<string>,
    bibleCount: int,
  }

  type bibleEntry = {
    id: string,
    title: string,
    languageCode: string,
    year: option<int>,
    copyright: option<string>,
  }
}

module Parser = {
  open Types

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

  let getOptInt = (obj: Dict.t<JSON.t>, key: string): option<int> =>
    switch obj->Dict.get(key) {
    | Some(Number(n)) => Some(Float.toInt(n))
    | _ => None
    }

  /** Parse language list from API JSON response */
  let parseLanguages = (json: JSON.t): Crawler.Parser.parseResult<array<languageInfo>> => {
    switch json {
    | Array(arr) =>
      let langs = arr->Array.filterMap(item => {
        switch item {
        | Object(obj) =>
          Some({
            code: getString(obj, "code"),
            name: getString(obj, "name"),
            nativeName: getOptString(obj, "nativeName"),
            bibleCount: getInt(obj, "bibleCount"),
          })
        | _ => None
        }
      })
      Crawler.Parser.Parsed(langs)
    | Object(obj) =>
      switch obj->Dict.get("data") {
      | Some(Array(arr)) =>
        let langs = arr->Array.filterMap(item => {
          switch item {
          | Object(o) =>
            Some({
              code: getString(o, "code"),
              name: getString(o, "name"),
              nativeName: getOptString(o, "nativeName"),
              bibleCount: getInt(o, "bibleCount"),
            })
          | _ => None
          }
        })
        Crawler.Parser.Parsed(langs)
      | _ => Crawler.Parser.NoMatch
      }
    | _ => Crawler.Parser.NoMatch
    }
  }

  /** Parse Bible entries from API JSON response */
  let parseBibles = (json: JSON.t): Crawler.Parser.parseResult<array<bibleEntry>> => {
    let parseArr = arr =>
      arr->Array.filterMap(item => {
        switch item {
        | Object(obj) =>
          Some({
            id: getString(obj, "id"),
            title: getString(obj, "title"),
            languageCode: getString(obj, "languageCode"),
            year: getOptInt(obj, "year"),
            copyright: getOptString(obj, "copyright"),
          })
        | _ => None
        }
      })

    switch json {
    | Array(arr) => Crawler.Parser.Parsed(parseArr(arr))
    | Object(obj) =>
      switch obj->Dict.get("data") {
      | Some(Array(arr)) => Crawler.Parser.Parsed(parseArr(arr))
      | _ => Crawler.Parser.NoMatch
      }
    | _ => Crawler.Parser.NoMatch
    }
  }

  /** Parse verse text from API chapter response */
  let parseVerses = (json: JSON.t, languageCode: string): Crawler.Parser.parseResult<
    array<Lang1000.Verse.t>,
  > => {
    let parseVerseArr = arr =>
      arr->Array.filterMap(item => {
        switch item {
        | Object(obj) =>
          let book = getString(obj, "book")
          let chapter = getInt(obj, "chapter")
          let verseNum = getInt(obj, "verse")
          let text = getString(obj, "text")
          if String.length(text) > 0 {
            Some(
              Lang1000.Verse.make(
                ~reference=Lang1000.Verse.makeReference(~book, ~chapter, ~verse=verseNum),
                ~text,
                ~language=languageCode,
              ),
            )
          } else {
            None
          }
        | _ => None
        }
      })

    switch json {
    | Array(arr) => Crawler.Parser.Parsed(parseVerseArr(arr))
    | Object(obj) =>
      switch obj->Dict.get("verses") {
      | Some(Array(arr)) => Crawler.Parser.Parsed(parseVerseArr(arr))
      | _ => Crawler.Parser.NoMatch
      }
    | _ => Crawler.Parser.NoMatch
    }
  }
}

module Crawler = {
  type t = {
    rateLimiter: Crawler.RateLimiter.t,
    mutable state: crawlerState,
  }

  let make = () => {
    rateLimiter: Crawler.RateLimiter.make(~delayMs=Config.rateLimitMs, ()),
    state: Idle,
  }

  /** Discover available languages */
  let fetchLanguages = async (crawler: t): crawlResult<array<Types.languageInfo>> => {
    crawler.state = Crawling("languages")
    let resp = await Http.getJson(Endpoints.languages(), ())
    crawler.state = Idle
    switch resp {
    | Ok(json) =>
      switch Parser.parseLanguages(json) {
      | Crawler.Parser.Parsed(langs) => Success(langs)
      | _ => Failure("Failed to parse language list")
      }
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(_) => Failure("Request failed")
    }
  }

  /** Fetch Bibles available for a language */
  let fetchBibles = async (
    crawler: t,
    langCode: string,
  ): crawlResult<array<Types.bibleEntry>> => {
    let resp = await Http.getWithRateLimit(
      Endpoints.bibles(langCode),
      ~rateLimiter=crawler.rateLimiter,
      (),
    )
    switch resp {
    | Ok({body}) =>
      switch Parser.parseBibles(JSON.parseExn(body)) {
      | Crawler.Parser.Parsed(bibles) => Success(bibles)
      | _ => Failure("Failed to parse bibles")
      }
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(_) => Failure("Request failed")
    }
  }

  /** Fetch chapter text for a specific Bible */
  let fetchChapter = async (
    crawler: t,
    bibleId: string,
    book: string,
    chapter: int,
    languageCode: string,
  ): crawlResult<array<Lang1000.Verse.t>> => {
    let resp = await Http.getWithRateLimit(
      Endpoints.text(bibleId, book, chapter),
      ~rateLimiter=crawler.rateLimiter,
      (),
    )
    switch resp {
    | Ok({body}) =>
      switch Parser.parseVerses(JSON.parseExn(body), languageCode) {
      | Crawler.Parser.Parsed(verses) => Success(verses)
      | _ => Failure("Failed to parse verses")
      }
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(_) => Failure("Request failed")
    }
  }
}
