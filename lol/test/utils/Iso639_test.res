// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * ISO 639 Utilities Tests
 */

open Vitest

describe("Iso639.Validation", () => {
  describe("isValidIso639_1", () => {
    test("returns true for valid 2-letter codes", () => {
      expect(Iso639.Validation.isValidIso639_1("en"))->toBe(true)
      expect(Iso639.Validation.isValidIso639_1("de"))->toBe(true)
      expect(Iso639.Validation.isValidIso639_1("zh"))->toBe(true)
    })

    test("returns false for invalid codes", () => {
      expect(Iso639.Validation.isValidIso639_1("eng"))->toBe(false)
      expect(Iso639.Validation.isValidIso639_1("e"))->toBe(false)
      expect(Iso639.Validation.isValidIso639_1("EN"))->toBe(false)
      expect(Iso639.Validation.isValidIso639_1(""))->toBe(false)
    })
  })

  describe("isValidIso639_3", () => {
    test("returns true for valid 3-letter codes", () => {
      expect(Iso639.Validation.isValidIso639_3("eng"))->toBe(true)
      expect(Iso639.Validation.isValidIso639_3("deu"))->toBe(true)
      expect(Iso639.Validation.isValidIso639_3("zho"))->toBe(true)
    })

    test("returns false for invalid codes", () => {
      expect(Iso639.Validation.isValidIso639_3("en"))->toBe(false)
      expect(Iso639.Validation.isValidIso639_3("english"))->toBe(false)
      expect(Iso639.Validation.isValidIso639_3("ENG"))->toBe(false)
      expect(Iso639.Validation.isValidIso639_3(""))->toBe(false)
    })
  })

  describe("detectCodeType", () => {
    test("detects ISO 639-1 codes", () => {
      expect(Iso639.Validation.detectCodeType("en"))->toEqual(Some(Iso639.Types.Iso639_1))
    })

    test("detects ISO 639-3 codes", () => {
      expect(Iso639.Validation.detectCodeType("eng"))->toEqual(Some(Iso639.Types.Iso639_3))
    })

    test("returns None for invalid codes", () => {
      expect(Iso639.Validation.detectCodeType("english"))->toEqual(None)
      expect(Iso639.Validation.detectCodeType(""))->toEqual(None)
    })
  })

  describe("normalize", () => {
    test("converts to lowercase", () => {
      expect(Iso639.Validation.normalize("ENG"))->toBe("eng")
      expect(Iso639.Validation.normalize("EN"))->toBe("en")
    })

    test("trims whitespace", () => {
      expect(Iso639.Validation.normalize(" eng "))->toBe("eng")
    })
  })
})

describe("Iso639.SpecialCodes", () => {
  test("isSpecial returns true for special codes", () => {
    expect(Iso639.SpecialCodes.isSpecial("und"))->toBe(true)
    expect(Iso639.SpecialCodes.isSpecial("mul"))->toBe(true)
    expect(Iso639.SpecialCodes.isSpecial("mis"))->toBe(true)
    expect(Iso639.SpecialCodes.isSpecial("zxx"))->toBe(true)
  })

  test("isSpecial returns false for regular codes", () => {
    expect(Iso639.SpecialCodes.isSpecial("eng"))->toBe(false)
    expect(Iso639.SpecialCodes.isSpecial("deu"))->toBe(false)
  })
})

describe("Iso639.Conversion", () => {
  describe("toIso639_3", () => {
    test("converts ISO 639-1 to ISO 639-3", () => {
      expect(Iso639.Conversion.toIso639_3("en"))->toEqual(Some("eng"))
      expect(Iso639.Conversion.toIso639_3("de"))->toEqual(Some("deu"))
      expect(Iso639.Conversion.toIso639_3("fr"))->toEqual(Some("fra"))
    })

    test("passes through valid ISO 639-3 codes", () => {
      expect(Iso639.Conversion.toIso639_3("eng"))->toEqual(Some("eng"))
    })

    test("returns None for unknown codes", () => {
      expect(Iso639.Conversion.toIso639_3("xx"))->toEqual(None)
    })
  })
})

describe("Iso639.Registry", () => {
  test("empty creates empty registry", () => {
    let registry = Iso639.Registry.empty()
    expect(Iso639.Registry.count(registry))->toBe(0)
  })

  test("add and findByCode works", () => {
    let registry = Iso639.Registry.empty()
    let entry: Iso639.Types.languageEntry = {
      iso639_3: "eng",
      iso639_2b: Some("eng"),
      iso639_2t: Some("eng"),
      iso639_1: Some("en"),
      scope: Iso639.Types.Individual,
      type_: Iso639.Types.Living,
      name: "English",
      comment: None,
    }
    Iso639.Registry.add(registry, entry)

    let found = Iso639.Registry.findByCode(registry, "eng")
    expect(found)->toEqual(Some(entry))

    let foundByIso1 = Iso639.Registry.findByCode(registry, "en")
    expect(foundByIso1)->toEqual(Some(entry))
  })

  test("findByName works", () => {
    let registry = Iso639.Registry.empty()
    let entry: Iso639.Types.languageEntry = {
      iso639_3: "deu",
      iso639_2b: Some("ger"),
      iso639_2t: Some("deu"),
      iso639_1: Some("de"),
      scope: Iso639.Types.Individual,
      type_: Iso639.Types.Living,
      name: "German",
      comment: None,
    }
    Iso639.Registry.add(registry, entry)

    let found = Iso639.Registry.findByName(registry, "German")
    expect(found)->toEqual(Some(entry))

    let foundLower = Iso639.Registry.findByName(registry, "german")
    expect(foundLower)->toEqual(Some(entry))
  })
})
