// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * 1000Langs - Super-Parallel Corpus Crawler
 *
 * A multilingual corpus building system supporting 1500+ languages
 * from parallel Bible translations across multiple sources.
 *
 * CLI interface for Deno runtime with VeriSimDB quality verification.
 */

module Config = {
  let version = "0.1.0"
  let name = "1000Langs"
  let description = "Super-parallel corpus crawler for multilingual NLP research"

  type source =
    | BibleCloud
    | BibleCom
    | BibleIs
    | PngScriptures
    | EBible
    | FindBible

  let allSources = [BibleCloud, BibleCom, BibleIs, PngScriptures, EBible, FindBible]

  let sourceToString = source =>
    switch source {
    | BibleCloud => "bible.cloud"
    | BibleCom => "bible.com"
    | BibleIs => "bible.is"
    | PngScriptures => "pngscriptures.org"
    | EBible => "ebible.org"
    | FindBible => "find.bible"
    }

  let sourceFromString = str =>
    switch str {
    | "bible.cloud" | "bible_cloud" | "biblecloud" => Some(BibleCloud)
    | "bible.com" | "bible_com" | "biblecom" => Some(BibleCom)
    | "bible.is" | "bible_is" | "bibleis" => Some(BibleIs)
    | "pngscriptures" | "pngscriptures.org" | "png_scriptures" => Some(PngScriptures)
    | "ebible" | "ebible.org" | "e_bible" => Some(EBible)
    | "find.bible" | "find_bible" | "findbible" => Some(FindBible)
    | _ => None
    }
}

module Language = {
  type iso639_3 = string
  type languageName = string

  type t = {
    code: iso639_3,
    name: languageName,
    family: option<string>,
    script: option<string>,
    country: option<string>,
  }

  let make = (~code, ~name, ~family=?, ~script=?, ~country=?, ()) => {
    code,
    name,
    family,
    script,
    country,
  }

  let getCode = lang => lang.code
  let getName = lang => lang.name
}

module Verse = {
  type book = string
  type chapter = int
  type verseNum = int

  type reference = {
    book: book,
    chapter: chapter,
    verse: verseNum,
  }

  type t = {
    reference: reference,
    text: string,
    language: Language.iso639_3,
  }

  let makeReference = (~book, ~chapter, ~verse) => {book, chapter, verse}

  let make = (~reference, ~text, ~language) => {reference, text, language}

  let toCanonicalId = ref =>
    `${ref.book}.${Int.toString(ref.chapter)}.${Int.toString(ref.verse)}`
}

module Corpus = {
  type alignment = {
    referenceId: string,
    translations: Dict.t<string>,
  }

  type t = {
    name: string,
    languages: array<Language.t>,
    alignments: array<alignment>,
    metadata: Dict.t<string>,
  }

  let empty = name => {
    name,
    languages: [],
    alignments: [],
    metadata: Dict.make(),
  }

  let addLanguage = (corpus, lang) => {
    ...corpus,
    languages: Array.concat(corpus.languages, [lang]),
  }

  let addAlignment = (corpus, alignment) => {
    ...corpus,
    alignments: Array.concat(corpus.alignments, [alignment]),
  }

  let languageCount = corpus => Array.length(corpus.languages)
  let alignmentCount = corpus => Array.length(corpus.alignments)
}

/** Deno CLI argument access */
@val @scope("Deno") external args: array<string> = "args"
@val @scope("Deno") external exit: int => unit = "exit"

module Cli = {
  type command =
    | Crawl({source: Config.source, lang: string, output: option<string>})
    | Verify({output: option<string>})
    | ListSources
    | Help
    | Version

  let getArg = (args: array<string>, flag: string): option<string> => {
    let idx = args->Array.findIndex(a => a == flag)
    if idx >= 0 && idx + 1 < Array.length(args) {
      Some(Array.getUnsafe(args, idx + 1))
    } else {
      None
    }
  }

  let parseArgs = (argv: array<string>): command => {
    if Array.length(argv) == 0 {
      Help
    } else {
      let cmd = Array.getUnsafe(argv, 0)
      switch cmd {
      | "crawl" =>
        let sourceStr = getArg(argv, "--source")->Option.getOr("bible.cloud")
        let lang = getArg(argv, "--lang")->Option.getOr("eng")
        let output = getArg(argv, "--output")
        switch Config.sourceFromString(sourceStr) {
        | Some(source) => Crawl({source, lang, output})
        | None =>
          Console.error(`Unknown source: ${sourceStr}`)
          Help
        }
      | "verify" =>
        let output = getArg(argv, "--output")
        Verify({output})
      | "list-sources" => ListSources
      | "version" | "--version" | "-v" => Version
      | "help" | "--help" | "-h" | _ => Help
      }
    }
  }

  let printHelp = () => {
    Console.log(`${Config.name} v${Config.version}`)
    Console.log(Config.description)
    Console.log("")
    Console.log("Usage:")
    Console.log("  1000langs crawl --source <name> --lang <code> [--output <path>]")
    Console.log("  1000langs verify [--output <path>]")
    Console.log("  1000langs list-sources")
    Console.log("  1000langs version")
    Console.log("  1000langs help")
    Console.log("")
    Console.log("Sources:")
    Config.allSources->Array.forEach(s => {
      Console.log(`  ${Config.sourceToString(s)}`)
    })
  }

  let printVersion = () => {
    Console.log(`${Config.name} v${Config.version}`)
  }

  let printSources = () => {
    Console.log("Available corpus sources:")
    Config.allSources->Array.forEach(s => {
      Console.log(`  ${Config.sourceToString(s)}`)
    })
  }

  let runVerify = async (output: option<string>) => {
    Console.log("Running corpus quality verification...")
    // Create a test corpus for demonstration
    let corpus = Corpus.empty("1000langs-corpus")
    let result = CorpusAnalyzer.analyzeFull(corpus, ())
    let outputPath = output->Option.getOr("/tmp/lol-scan.json")
    await Export.writeToFile(result, outputPath)
    Console.log(`Scan written to: ${outputPath}`)
    Console.log(
      `Weak points: ${Int.toString(Array.length(result.weak_points))}`,
    )
  }

  let runCrawl = async (source: Config.source, lang: string, output: option<string>) => {
    Console.log(
      `Crawling ${Config.sourceToString(source)} for language: ${lang}`,
    )
    let _ = output
    switch source {
    | BibleCloud =>
      Console.log("Using BibleCloud API crawler...")
      let crawler = BibleCloud.Crawler.make()
      let result = await BibleCloud.Crawler.fetchBibles(crawler)
      switch result {
      | Crawler.Types.Success(bibles) =>
        Console.log(`Found ${Int.toString(Array.length(bibles))} Bibles`)
      | Crawler.Types.Failure(msg) => Console.error(`Crawl failed: ${msg}`)
      | Crawler.Types.Pending => Console.log("Crawl pending (no API key configured)")
      }
    | BibleCom =>
      Console.log("Using BibleCom web scraper...")
      let crawler = BibleCom.Crawler.make()
      let result = await BibleCom.Crawler.fetchVersions(crawler)
      switch result {
      | Crawler.Types.Success(versions) =>
        Console.log(`Found ${Int.toString(Array.length(versions))} versions`)
      | Crawler.Types.Failure(msg) => Console.error(`Crawl failed: ${msg}`)
      | Crawler.Types.Pending => ()
      }
    | PngScriptures =>
      Console.log("Using PNG Scriptures crawler...")
      let crawler = PngScriptures.Crawler.make()
      let result = await PngScriptures.Crawler.fetchLanguages(crawler)
      switch result {
      | Crawler.Types.Success(langs) =>
        Console.log(`Found ${Int.toString(Array.length(langs))} languages`)
      | Crawler.Types.Failure(msg) => Console.error(`Crawl failed: ${msg}`)
      | Crawler.Types.Pending => ()
      }
    | EBible =>
      Console.log("Using eBible.org crawler...")
      let crawler = EBible.Crawler.make()
      let result = await EBible.Crawler.fetchTranslations(crawler)
      switch result {
      | Crawler.Types.Success(translations) =>
        Console.log(`Found ${Int.toString(Array.length(translations))} translations`)
      | Crawler.Types.Failure(msg) => Console.error(`Crawl failed: ${msg}`)
      | Crawler.Types.Pending => ()
      }
    | FindBible =>
      Console.log("Using Find.Bible crawler...")
      let crawler = FindBible.Crawler.make()
      let result = await FindBible.Crawler.fetchLanguages(crawler)
      switch result {
      | Crawler.Types.Success(langs) =>
        Console.log(`Found ${Int.toString(Array.length(langs))} languages`)
      | Crawler.Types.Failure(msg) => Console.error(`Crawl failed: ${msg}`)
      | Crawler.Types.Pending => ()
      }
    | BibleIs =>
      Console.log("BibleIs uses the Digital Bible Platform API...")
      Console.log("Configure API key via BIBLE_API_KEY environment variable")
    }
  }
}

let main = async () => {
  let cmd = Cli.parseArgs(args)
  switch cmd {
  | Cli.Help => Cli.printHelp()
  | Cli.Version => Cli.printVersion()
  | Cli.ListSources => Cli.printSources()
  | Cli.Verify({output}) => await Cli.runVerify(output)
  | Cli.Crawl({source, lang, output}) => await Cli.runCrawl(source, lang, output)
  }
}

ignore(main())
