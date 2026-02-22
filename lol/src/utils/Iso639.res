// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * ISO 639 Language Code Utilities
 *
 * Handles ISO 639-1 (2-letter), ISO 639-2 (3-letter bibliographic/terminological),
 * and ISO 639-3 (3-letter comprehensive) language codes.
 */

module Types = {
  type iso639_1 = string  // 2-letter code (en, de, fr)
  type iso639_2b = string // 3-letter bibliographic (ger, fre)
  type iso639_2t = string // 3-letter terminological (deu, fra)
  type iso639_3 = string  // 3-letter comprehensive (eng, deu, fra)

  type codeType =
    | Iso639_1
    | Iso639_2b
    | Iso639_2t
    | Iso639_3

  type languageScope =
    | Individual    // I - Individual language
    | Macrolanguage // M - Macrolanguage
    | Special       // S - Special (mis, mul, und, zxx)

  type languageType =
    | Living      // L - Living language
    | Historical  // H - Historical language
    | Extinct     // E - Extinct language
    | Ancient     // A - Ancient language
    | Constructed // C - Constructed language

  type languageEntry = {
    iso639_3: iso639_3,
    iso639_2b: option<iso639_2b>,
    iso639_2t: option<iso639_2t>,
    iso639_1: option<iso639_1>,
    scope: languageScope,
    type_: languageType,
    name: string,
    comment: option<string>,
  }
}

module Validation = {
  open Types

  let iso639_1Pattern = %re("/^[a-z]{2}$/")
  let iso639_3Pattern = %re("/^[a-z]{3}$/")

  let isValidIso639_1 = (code: string): bool => {
    Js.Re.test_(iso639_1Pattern, code)
  }

  let isValidIso639_3 = (code: string): bool => {
    Js.Re.test_(iso639_3Pattern, code)
  }

  let detectCodeType = (code: string): option<codeType> => {
    let len = String.length(code)
    switch len {
      | 2 when isValidIso639_1(code) => Some(Iso639_1)
      | 3 when isValidIso639_3(code) => Some(Iso639_3)
      | _ => None
    }
  }

  let normalize = (code: string): string => {
    code->String.toLowerCase->String.trim
  }
}

module SpecialCodes = {
  // Special ISO 639 codes
  let undetermined = "und"      // Undetermined
  let multiple = "mul"          // Multiple languages
  let miscellaneous = "mis"     // Uncoded languages
  let noLinguistic = "zxx"      // No linguistic content

  let isSpecial = code => {
    code == undetermined ||
    code == multiple ||
    code == miscellaneous ||
    code == noLinguistic
  }
}

module Conversion = {
  open Types

  // Common ISO 639-1 to ISO 639-3 mappings
  let iso1ToIso3: Dict.t<iso639_3> = {
    let d = Dict.make()
    Dict.set(d, "en", "eng")
    Dict.set(d, "de", "deu")
    Dict.set(d, "fr", "fra")
    Dict.set(d, "es", "spa")
    Dict.set(d, "it", "ita")
    Dict.set(d, "pt", "por")
    Dict.set(d, "ru", "rus")
    Dict.set(d, "zh", "zho")
    Dict.set(d, "ja", "jpn")
    Dict.set(d, "ko", "kor")
    Dict.set(d, "ar", "ara")
    Dict.set(d, "he", "heb")
    Dict.set(d, "el", "ell")
    Dict.set(d, "la", "lat")
    d
  }

  let toIso639_3 = (code: string): option<iso639_3> => {
    let normalized = Validation.normalize(code)
    switch Validation.detectCodeType(normalized) {
      | Some(Iso639_1) => Dict.get(iso1ToIso3, normalized)
      | Some(Iso639_3) => Some(normalized)
      | _ => None
    }
  }
}

module Registry = {
  open Types

  type t = {
    byIso3: Dict.t<languageEntry>,
    byIso1: Dict.t<languageEntry>,
    byName: Dict.t<languageEntry>,
  }

  let empty = (): t => {
    byIso3: Dict.make(),
    byIso1: Dict.make(),
    byName: Dict.make(),
  }

  let add = (registry, entry: languageEntry) => {
    Dict.set(registry.byIso3, entry.iso639_3, entry)
    switch entry.iso639_1 {
      | Some(code) => Dict.set(registry.byIso1, code, entry)
      | None => ()
    }
    Dict.set(registry.byName, String.toLowerCase(entry.name), entry)
  }

  let findByCode = (registry, code: string): option<languageEntry> => {
    let normalized = Validation.normalize(code)
    switch Validation.detectCodeType(normalized) {
      | Some(Iso639_1) => Dict.get(registry.byIso1, normalized)
      | Some(Iso639_3) | Some(Iso639_2b) | Some(Iso639_2t) =>
        Dict.get(registry.byIso3, normalized)
      | None => None
    }
  }

  let findByName = (registry, name: string): option<languageEntry> => {
    Dict.get(registry.byName, String.toLowerCase(name))
  }

  let count = (registry): int => {
    Dict.keysToArray(registry.byIso3)->Array.length
  }
}
