// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * PNG Scriptures Crawler
 *
 * Crawler for pngscriptures.org - Papua New Guinea Bible translations
 * in numerous Papuan and Austronesian languages.
 * Downloads ZIP archives and parses USFM/HTML content.
 */

open Crawler.Types

module Config = {
  let baseUrl = "https://pngscriptures.org"
  let downloadUrl = "https://pngscriptures.org/download"
  let rateLimitMs = 2000

  type format =
    | Zip
    | Pdf
    | Epub
    | Html

  let formatToString = format =>
    switch format {
    | Zip => "zip"
    | Pdf => "pdf"
    | Epub => "epub"
    | Html => "html"
    }
}

module Endpoints = {
  let languages = () => `${Config.baseUrl}/languages`
  let language = langCode => `${Config.baseUrl}/lng/${langCode}`
  let download = (langCode, format) =>
    `${Config.downloadUrl}/${langCode}/${Config.formatToString(format)}`
}

module Types = {
  type pngLanguage = {
    code: string,
    name: string,
    alternateName: option<string>,
    region: string,
    population: option<int>,
    hasNewTestament: bool,
    hasOldTestament: bool,
  }

  type downloadInfo = {
    language: pngLanguage,
    format: Config.format,
    sizeBytes: option<int>,
    lastUpdated: option<string>,
  }
}

module Parser = {
  open Types

  /** Parse language listing from HTML page using regex extraction */
  let parseLanguageList = (html: string): Crawler.Parser.parseResult<array<pngLanguage>> => {
    // Extract language entries from the listing page
    let langRegex = %re(
      "/href=\"\/lng\/([a-z]{3})\"[^>]*>([^<]+)<.*?(?:region:\s*([^<,]+))?/gs"
    )
    let languages = []
    let _ = html->String.replaceRegExp(langRegex, (match_, _offset, _str) => {
      // Simplified extraction - real implementation would be more robust
      let parts = match_->String.split("\"")
      if Array.length(parts) >= 2 {
        let code = Array.getUnsafe(parts, 1)->String.replaceRegExp(%re("/.*\//"), "")
        if String.length(code) == 3 {
          ignore(
            Array.concat(
              languages,
              [
                {
                  code,
                  name: code,
                  alternateName: None,
                  region: "Papua New Guinea",
                  population: None,
                  hasNewTestament: true,
                  hasOldTestament: false,
                },
              ],
            ),
          )
        }
      }
      match_
    })

    if Array.length(languages) > 0 {
      Crawler.Parser.Parsed(languages)
    } else {
      Crawler.Parser.NoMatch
    }
  }

  /** Parse USFM (Unified Standard Format Markers) text into verses */
  let parseUsfm = (usfm: string, languageCode: string): array<Lang1000.Verse.t> => {
    let verses: ref<array<Lang1000.Verse.t>> = ref([])
    let currentBook = ref("")
    let currentChapter = ref(0)

    let lines = usfm->String.split("\n")
    lines->Array.forEach(line => {
      let trimmed = String.trim(line)
      // Book marker: \id GEN
      if String.startsWith(trimmed, "\\id ") {
        currentBook := String.sliceToEnd(trimmed, ~start=4)->String.split(" ")->Array.getUnsafe(0)
      }
      // Chapter marker: \c 1
      if String.startsWith(trimmed, "\\c ") {
        currentChapter :=
          String.sliceToEnd(trimmed, ~start=3)
          ->String.trim
          ->Int.fromString
          ->Option.getOr(0)
      }
      // Verse marker: \v 1 In the beginning...
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

  /** Parse individual book HTML file extracting verse text */
  let parseHtmlBook = (html: string, languageCode: string): Crawler.Parser.parseResult<
    array<Lang1000.Verse.t>,
  > => {
    // Extract verse text from HTML structure
    let verseRegex = %re("/class=\"verse\"[^>]*data-verse=\"(\d+)\"[^>]*>(.*?)<\/[^>]+>/gs")
    let verses = []
    let _ = html->String.replaceRegExp(verseRegex, (match_, _offset, _str) => {
      let cleaned =
        match_
        ->String.replaceRegExp(%re("/<[^>]+>/g"), "")
        ->String.trim
      if String.length(cleaned) > 0 {
        ignore(
          Array.concat(
            verses,
            [
              Lang1000.Verse.make(
                ~reference=Lang1000.Verse.makeReference(
                  ~book="UNK",
                  ~chapter=1,
                  ~verse=Array.length(verses) + 1,
                ),
                ~text=cleaned,
                ~language=languageCode,
              ),
            ],
          ),
        )
      }
      match_
    })

    if Array.length(verses) > 0 {
      Crawler.Parser.Parsed(verses)
    } else {
      Crawler.Parser.NoMatch
    }
  }
}

module Crawler = {
  type t = {
    rateLimiter: Crawler.RateLimiter.t,
    mutable state: crawlerState,
    downloadDir: string,
  }

  let make = (~downloadDir="./downloads/png", ()) => {
    rateLimiter: Crawler.RateLimiter.make(~delayMs=Config.rateLimitMs, ()),
    state: Idle,
    downloadDir,
  }

  let fetchLanguages = async (crawler: t): crawlResult<array<Types.pngLanguage>> => {
    crawler.state = Crawling("languages")
    let resp = await Http.getWithRateLimit(
      Endpoints.languages(),
      ~rateLimiter=crawler.rateLimiter,
      (),
    )
    crawler.state = Idle
    switch resp {
    | Ok({body}) =>
      switch Parser.parseLanguageList(body) {
      | Crawler.Parser.Parsed(langs) => Success(langs)
      | _ => Failure("Failed to parse language list")
      }
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(_) => Failure("Request failed")
    }
  }

  let downloadTranslation = async (
    crawler: t,
    langCode: string,
    format: Config.format,
  ): crawlResult<string> => {
    let resp = await Http.getWithRateLimit(
      Endpoints.download(langCode, format),
      ~rateLimiter=crawler.rateLimiter,
      (),
    )
    switch resp {
    | Ok({body}) =>
      let path = `${crawler.downloadDir}/${langCode}.${Config.formatToString(format)}`
      Success(path ++ " (" ++ Int.toString(String.length(body)) ++ " bytes)")
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(_) => Failure("Download failed")
    }
  }

  let fetchAndParse = async (
    crawler: t,
    langCode: string,
  ): crawlResult<array<Lang1000.Verse.t>> => {
    // Fetch HTML version for parsing
    let resp = await Http.getWithRateLimit(
      Endpoints.language(langCode),
      ~rateLimiter=crawler.rateLimiter,
      (),
    )
    switch resp {
    | Ok({body}) =>
      switch Parser.parseHtmlBook(body, langCode) {
      | Crawler.Parser.Parsed(verses) => Success(verses)
      | _ => Failure("No verses extracted from HTML")
      }
    | Error(Http.HttpError(code, msg)) => Failure(`HTTP ${Int.toString(code)}: ${msg}`)
    | Error(_) => Failure("Request failed")
    }
  }
}
