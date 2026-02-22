// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * eBible.org Crawler
 *
 * Crawler for ebible.org bulk Bible text collection.
 * High coverage with 1000+ languages in USFM plain text format.
 */

open Crawler.Types

module Config = {
  let baseUrl = "https://ebible.org"
  let downloadUrl = "https://ebible.org/Scriptures"
  let rateLimitMs = 1000
}

module Endpoints = {
  let translationList = () => `${Config.baseUrl}/Scriptures/`
  let translation = translationId => `${Config.downloadUrl}/${translationId}/`
  let usfmZip = translationId => `${Config.downloadUrl}/${translationId}_usfm.zip`
  let metadata = translationId => `${Config.downloadUrl}/${translationId}/copr.htm`
}

module Types = {
  type translationInfo = {
    id: string,
    language: string,
    title: string,
    copyright: option<string>,
    completeness: option<string>,
  }
}

module Parser = {
  open Types

  /** Parse translation listing from the Scriptures index page */
  let parseTranslationList = (html: string): Crawler.Parser.parseResult<
    array<translationInfo>,
  > => {
    // Extract translation entries from directory listing
    let linkRegex = %re("/href=\"([a-zA-Z0-9_-]+)\/\"[^>]*>([^<]*)</g")
    let translations = []
    let _ = html->String.replaceRegExp(linkRegex, (match_, _offset, _str) => {
      let parts = match_->String.split("\"")
      if Array.length(parts) >= 2 {
        let id = Array.getUnsafe(parts, 1)->String.replaceRegExp(%re("/\//"), "")
        if String.length(id) > 2 && !String.startsWith(id, ".") {
          ignore(
            Array.concat(
              translations,
              [
                {
                  id,
                  language: id,
                  title: id,
                  copyright: None,
                  completeness: None,
                },
              ],
            ),
          )
        }
      }
      match_
    })

    if Array.length(translations) > 0 {
      Crawler.Parser.Parsed(translations)
    } else {
      Crawler.Parser.NoMatch
    }
  }

  /** Parse USFM content to extract verses (inline, avoids cross-module coupling) */
  let parseUsfm = (usfm: string, languageCode: string): array<Lang1000.Verse.t> => {
    let verses: ref<array<Lang1000.Verse.t>> = ref([])
    let currentBook = ref("")
    let currentChapter = ref(0)

    let lines = usfm->String.split("\n")
    lines->Array.forEach(line => {
      let trimmed = String.trim(line)
      if String.startsWith(trimmed, "\\id ") {
        currentBook := String.sliceToEnd(trimmed, ~start=4)->String.split(" ")->Array.getUnsafe(0)
      }
      if String.startsWith(trimmed, "\\c ") {
        currentChapter :=
          String.sliceToEnd(trimmed, ~start=3)
          ->String.trim
          ->Int.fromString
          ->Option.getOr(0)
      }
      if String.startsWith(trimmed, "\\v ") {
        let rest = String.sliceToEnd(trimmed, ~start=3)
        let spaceIdx = String.indexOf(rest, " ")
        if spaceIdx > 0 {
          let verseNum = String.slice(rest, ~start=0, ~end=spaceIdx)->Int.fromString->Option.getOr(0)
          let text =
            String.sliceToEnd(rest, ~start=spaceIdx + 1)
            ->String.replaceRegExp(%re("/\\\\[a-z]+\s?/g"), "")
            ->String.trim
          if verseNum > 0 && String.length(text) > 0 {
            verses :=
              Array.concat(
                verses.contents,
                [
                  Lang1000.Verse.make(
                    ~reference=Lang1000.Verse.makeReference(
                      ~book=currentBook.contents,
                      ~chapter=currentChapter.contents,
                      ~verse=verseNum,
                    ),
                    ~text,
                    ~language=languageCode,
                  ),
                ],
              )
          }
        }
      }
    })
    verses.contents
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

  /** Fetch the list of available translations */
  let fetchTranslations = async (crawler: t): crawlResult<array<Types.translationInfo>> => {
    crawler.state = Crawling("translations")
    let resp = await Http.getWithRateLimit(
      Endpoints.translationList(),
      ~rateLimiter=crawler.rateLimiter,
      (),
    )
    crawler.state = Idle
    switch resp {
    | Ok({body}) =>
      switch Parser.parseTranslationList(body) {
      | Crawler.Parser.Parsed(translations) => Success(translations)
      | _ => Failure("Failed to parse translation list")
      }
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(_) => Failure("Request failed")
    }
  }

  /** Fetch and parse a specific translation's text */
  let fetchTranslation = async (
    crawler: t,
    translationId: string,
    languageCode: string,
  ): crawlResult<array<Lang1000.Verse.t>> => {
    crawler.state = Crawling(translationId)
    // Fetch the raw USFM text from the translation page
    let resp = await Http.getWithRateLimit(
      Endpoints.translation(translationId),
      ~rateLimiter=crawler.rateLimiter,
      (),
    )
    crawler.state = Idle
    switch resp {
    | Ok({body}) =>
      let verses = Parser.parseUsfm(body, languageCode)
      if Array.length(verses) > 0 {
        Success(verses)
      } else {
        Failure("No verses extracted from USFM content")
      }
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(_) => Failure("Request failed")
    }
  }
}
