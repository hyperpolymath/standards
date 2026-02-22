// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * Bible.com Crawler
 *
 * Crawler implementation for YouVersion (bible.com)
 * Web scraping approach for Bible translations.
 * Rate limited to 2 req/sec to respect server limits.
 */

open Crawler.Types

module Config = {
  let baseUrl = "https://www.bible.com"
  let apiUrl = "https://www.bible.com/api/bible"
  let rateLimitMs = 2000

  type versionInfo = {
    id: int,
    abbreviation: string,
    title: string,
    languageTag: string,
  }
}

module Endpoints = {
  let versions = () => `${Config.baseUrl}/versions`
  let version = versionId => `${Config.baseUrl}/versions/${Int.toString(versionId)}`
  let bible = (versionId, bookCode, chapter) =>
    `${Config.baseUrl}/bible/${Int.toString(versionId)}/${bookCode}.${Int.toString(chapter)}`
  let search = (versionId, query) =>
    `${Config.baseUrl}/search/bible?version_id=${Int.toString(versionId)}&q=${query}`
  let apiChapter = (versionId, bookCode, chapter) =>
    `${Config.apiUrl}/chapter/${Int.toString(versionId)}/${bookCode}.${Int.toString(chapter)}.json`
}

module Selectors = {
  let verseContainer = ".ChapterContent_verse__uvbXo"
  let verseNumber = ".ChapterContent_label__R2PLt"
  let verseText = ".ChapterContent_content__RlRwn"
  let chapterNav = ".ChapterContent_nav__vVPwy"
  let versionSelector = ".VersionSelector_container__3uKjR"
}

module Types = {
  type versionMeta = {
    id: int,
    abbreviation: string,
    title: string,
    language: Lang1000.Language.t,
    hasAudio: bool,
    hasOffline: bool,
  }

  type chapterContent = {
    versionId: int,
    book: string,
    chapter: int,
    verses: array<Lang1000.Verse.t>,
    nextChapter: option<(string, int)>,
    prevChapter: option<(string, int)>,
  }
}

module Parser = {
  open Types

  /** Parse version listing from API JSON response */
  let parseVersionList = (json: JSON.t): Crawler.Parser.parseResult<array<versionMeta>> => {
    switch json {
    | Object(obj) =>
      switch obj->Dict.get("data") {
      | Some(Array(arr)) =>
        let versions = arr->Array.filterMap(item => {
          switch item {
          | Object(v) =>
            let getString = (d: Dict.t<JSON.t>, k: string) =>
              switch d->Dict.get(k) {
              | Some(String(s)) => s
              | _ => ""
              }
            let getInt = (d: Dict.t<JSON.t>, k: string) =>
              switch d->Dict.get(k) {
              | Some(Number(n)) => Float.toInt(n)
              | _ => 0
              }
            let getBool = (d: Dict.t<JSON.t>, k: string) =>
              switch d->Dict.get(k) {
              | Some(Boolean(b)) => b
              | _ => false
              }
            let langTag = getString(v, "language_tag")
            Some({
              id: getInt(v, "id"),
              abbreviation: getString(v, "abbreviation"),
              title: getString(v, "title"),
              language: Lang1000.Language.make(~code=langTag, ~name=langTag, ()),
              hasAudio: getBool(v, "has_audio"),
              hasOffline: getBool(v, "has_offline"),
            })
          | _ => None
          }
        })
        Crawler.Parser.Parsed(versions)
      | _ => Crawler.Parser.NoMatch
      }
    | _ => Crawler.Parser.NoMatch
    }
  }

  /** Extract clean text from HTML by stripping tags */
  let stripHtmlTags = (html: string): string => {
    html
    ->String.replaceRegExp(%re("/<[^>]+>/g"), "")
    ->String.replaceRegExp(%re("/\s+/g"), " ")
    ->String.trim
  }

  /** Parse chapter content from HTML page */
  let parseChapterHtml = (
    html: string,
    ~versionId: int,
    ~book: string,
    ~chapter: int,
    ~languageCode: string,
    (),
  ): Crawler.Parser.parseResult<chapterContent> => {
    // Extract verse blocks using regex on HTML structure
    let verseRegex = %re(
      "/data-usfm=\"([^\"]+)\"[^>]*>.*?<span[^>]*class=\"[^\"]*content[^\"]*\"[^>]*>(.*?)<\/span>/gs"
    )
    let verses = []
    let _ = html->String.replaceRegExp(verseRegex, (match_, _offset, _str) => {
      // This is a simplified extraction - the regex captures verse refs and content
      let cleaned = stripHtmlTags(match_)
      if String.length(cleaned) > 0 {
        let verse = Lang1000.Verse.make(
          ~reference=Lang1000.Verse.makeReference(~book, ~chapter, ~verse=Array.length(verses) + 1),
          ~text=cleaned,
          ~language=languageCode,
        )
        ignore(Array.concat(verses, [verse]))
      }
      match_
    })

    if Array.length(verses) > 0 {
      Crawler.Parser.Parsed({
        versionId,
        book,
        chapter,
        verses,
        nextChapter: None,
        prevChapter: None,
      })
    } else {
      Crawler.Parser.NoMatch
    }
  }

  let normalizeText = (text: string): string => {
    text
    ->String.trim
    ->String.replaceRegExp(%re("/\s+/g"), " ")
  }
}

module Crawler = {
  type t = {
    rateLimiter: Crawler.RateLimiter.t,
    mutable state: crawlerState,
    mutable cachedVersions: option<array<Types.versionMeta>>,
  }

  let make = () => {
    rateLimiter: Crawler.RateLimiter.make(~delayMs=Config.rateLimitMs, ()),
    state: Idle,
    cachedVersions: None,
  }

  let fetchVersions = async (crawler: t): crawlResult<array<Types.versionMeta>> => {
    switch crawler.cachedVersions {
    | Some(versions) => Success(versions)
    | None =>
      crawler.state = Crawling("versions")
      let resp = await Http.getWithRateLimit(
        Endpoints.versions(),
        ~rateLimiter=crawler.rateLimiter,
        (),
      )
      crawler.state = Idle
      switch resp {
      | Ok({body}) =>
        switch Parser.parseVersionList(JSON.parseExn(body)) {
        | Crawler.Parser.Parsed(versions) =>
          crawler.cachedVersions = Some(versions)
          Success(versions)
        | _ => Failure("Failed to parse versions")
        }
      | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
      | Error(_) => Failure("Request failed")
      }
    }
  }

  let fetchChapter = async (
    crawler: t,
    versionId: int,
    book: string,
    chapter: int,
    languageCode: string,
  ): crawlResult<Types.chapterContent> => {
    let resp = await Http.getWithRateLimit(
      Endpoints.bible(versionId, book, chapter),
      ~rateLimiter=crawler.rateLimiter,
      (),
    )
    switch resp {
    | Ok({body}) =>
      switch Parser.parseChapterHtml(body, ~versionId, ~book, ~chapter, ~languageCode, ()) {
      | Crawler.Parser.Parsed(content) => Success(content)
      | _ => Failure("Failed to parse chapter HTML")
      }
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(_) => Failure("Request failed")
    }
  }

  let fetchBook = async (
    crawler: t,
    versionId: int,
    book: string,
    totalChapters: int,
    languageCode: string,
  ): crawlResult<array<Types.chapterContent>> => {
    let chapters = []
    let failed = ref(false)
    let failMsg = ref("")

    for ch in 1 to totalChapters {
      if !failed.contents {
        let result = await fetchChapter(crawler, versionId, book, ch, languageCode)
        switch result {
        | Success(content) => ignore(Array.concat(chapters, [content]))
        | Failure(msg) =>
          failed := true
          failMsg := msg
        | Pending => ()
        }
      }
    }

    if failed.contents {
      Failure(failMsg.contents)
    } else {
      Success(chapters)
    }
  }
}
