// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * Main Module Tests
 */

open Vitest

describe("Lang1000.Config", () => {
  test("version is defined", () => {
    expect(Lang1000.Config.version)->toBe("0.1.0")
  })

  test("name is correct", () => {
    expect(Lang1000.Config.name)->toBe("1000Langs")
  })

  test("allSources contains expected sources", () => {
    let sources = Lang1000.Config.allSources
    expect(Array.length(sources))->toBe(6)
  })

  test("sourceToString converts correctly", () => {
    expect(Lang1000.Config.sourceToString(Lang1000.Config.BibleCloud))->toBe("bible.cloud")
    expect(Lang1000.Config.sourceToString(Lang1000.Config.BibleCom))->toBe("bible.com")
    expect(Lang1000.Config.sourceToString(Lang1000.Config.PngScriptures))->toBe("pngscriptures.org")
  })
})

describe("Lang1000.Language", () => {
  test("make creates a language with required fields", () => {
    let lang = Lang1000.Language.make(~code="eng", ~name="English", ())
    expect(Lang1000.Language.getCode(lang))->toBe("eng")
    expect(Lang1000.Language.getName(lang))->toBe("English")
  })

  test("make creates a language with optional fields", () => {
    let lang = Lang1000.Language.make(
      ~code="deu",
      ~name="German",
      ~family="Indo-European",
      ~script="Latin",
      ~country="Germany",
      (),
    )
    expect(lang.family)->toEqual(Some("Indo-European"))
    expect(lang.script)->toEqual(Some("Latin"))
    expect(lang.country)->toEqual(Some("Germany"))
  })
})

describe("Lang1000.Verse", () => {
  test("makeReference creates a valid reference", () => {
    let ref = Lang1000.Verse.makeReference(~book="GEN", ~chapter=1, ~verse=1)
    expect(ref.book)->toBe("GEN")
    expect(ref.chapter)->toBe(1)
    expect(ref.verse)->toBe(1)
  })

  test("toCanonicalId formats correctly", () => {
    let ref = Lang1000.Verse.makeReference(~book="GEN", ~chapter=1, ~verse=1)
    expect(Lang1000.Verse.toCanonicalId(ref))->toBe("GEN.1.1")
  })

  test("toCanonicalId handles multi-digit chapters and verses", () => {
    let ref = Lang1000.Verse.makeReference(~book="PSA", ~chapter=119, ~verse=176)
    expect(Lang1000.Verse.toCanonicalId(ref))->toBe("PSA.119.176")
  })
})

describe("Lang1000.Corpus", () => {
  test("empty creates an empty corpus", () => {
    let corpus = Lang1000.Corpus.empty("TestCorpus")
    expect(corpus.name)->toBe("TestCorpus")
    expect(Lang1000.Corpus.languageCount(corpus))->toBe(0)
    expect(Lang1000.Corpus.alignmentCount(corpus))->toBe(0)
  })

  test("addLanguage increases language count", () => {
    let corpus = Lang1000.Corpus.empty("TestCorpus")
    let lang = Lang1000.Language.make(~code="eng", ~name="English", ())
    let updated = Lang1000.Corpus.addLanguage(corpus, lang)
    expect(Lang1000.Corpus.languageCount(updated))->toBe(1)
  })

  test("addAlignment increases alignment count", () => {
    let corpus = Lang1000.Corpus.empty("TestCorpus")
    let alignment: Lang1000.Corpus.alignment = {
      referenceId: "GEN.1.1",
      translations: Dict.make(),
    }
    let updated = Lang1000.Corpus.addAlignment(corpus, alignment)
    expect(Lang1000.Corpus.alignmentCount(updated))->toBe(1)
  })
})
