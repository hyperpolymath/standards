// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * Corpus Analyzer
 *
 * Quality analysis for multilingual Bible corpus data. Detects weak
 * points including missing verses, encoding errors, alignment failures,
 * statistical outliers, coverage gaps, truncation, and duplicates.
 */

open VeriSimDB

module EntropyAnalysis = {
  /** Compute character-level entropy for a text */
  let characterEntropy = (text: string): float => {
    let chars = text->String.split("")
    let total = Array.length(chars)->Float.fromInt
    if total == 0.0 {
      0.0
    } else {
      let counts = Dict.make()
      chars->Array.forEach(c => {
        let current = counts->Dict.get(c)->Option.getOr(0)
        counts->Dict.set(c, current + 1)
      })
      let dist = counts->Dict.valuesToArray->Array.map(c => Float.fromInt(c) /. total)
      Statistics.Information.entropy(dist)
    }
  }

  /** Flag texts with abnormally low or high entropy as outliers */
  let detectOutliers = (
    corpus: Lang1000.Corpus.t,
    ~zThreshold=2.0,
    (),
  ): array<weakPoint> => {
    let entropies =
      corpus.alignments->Array.map(alignment => {
        let texts = alignment.translations->Dict.valuesToArray
        let avgEntropy = if Array.length(texts) == 0 {
          0.0
        } else {
          let entVals = texts->Array.map(characterEntropy)
          Statistics.Basic.mean(entVals)
        }
        (alignment.referenceId, avgEntropy)
      })

    let entropyValues = entropies->Array.map(((_, e)) => e)
    let mean = Statistics.Basic.mean(entropyValues)
    let sd = Statistics.Basic.standardDeviation(entropyValues)

    if sd == 0.0 {
      []
    } else {
      entropies->Array.filterMap(((refId, ent)) => {
        let zScore = Math.abs((ent -. mean) /. sd)
        if zScore > zThreshold {
          Some(
            makeWeakPoint(
              ~category=StatisticalOutlier,
              ~severity=Medium,
              ~location=refId,
              ~description=`Entropy z-score ${Float.toFixed(zScore, ~digits=2)} exceeds threshold ${Float.toFixed(zThreshold, ~digits=1)} (entropy=${Float.toFixed(ent, ~digits=3)})`,
              (),
            ),
          )
        } else {
          None
        }
      })
    }
  }
}

module MissingVerseDetection = {
  /** Detect verses present in reference language but missing in others */
  let detect = (
    corpus: Lang1000.Corpus.t,
    ~refLang: string,
    (),
  ): array<weakPoint> => {
    corpus.alignments->Array.filterMap(alignment => {
      let hasRef = alignment.translations->Dict.get(refLang)->Option.isSome
      if !hasRef {
        None
      } else {
        let langs = alignment.translations->Dict.keysToArray
        let missing =
          corpus.languages
          ->Array.map(l => l.code)
          ->Array.filter(code => code != refLang && !(langs->Array.includes(code)))

        if Array.length(missing) > 0 {
          let missingStr = missing->Array.join(", ")
          Some(
            makeWeakPoint(
              ~category=MissingVerse,
              ~severity=if Array.length(missing) > 5 { High } else { Low },
              ~location=alignment.referenceId,
              ~description=`Missing in ${Int.toString(Array.length(missing))} languages: ${missingStr}`,
              (),
            ),
          )
        } else {
          None
        }
      }
    })
  }
}

module EncodingDetection = {
  /** Check for common encoding errors in text */
  let hasEncodingError = (text: string): bool => {
    // Check for replacement character (U+FFFD) indicating invalid UTF-8
    String.includes(text, "\uFFFD") ||
    // Check for null bytes
    String.includes(text, "\u0000") ||
    // Check for common mojibake patterns
    String.includes(text, "\u00C3\u00A9") || // Ã© instead of é
    String.includes(text, "\u00C3\u00A0") || // Ã  instead of à
    String.includes(text, "\u00C2\u00BB") // Â» instead of »
  }

  /** Detect encoding errors across the corpus */
  let detect = (corpus: Lang1000.Corpus.t): array<weakPoint> => {
    corpus.alignments->Array.flatMap(alignment => {
      alignment.translations
      ->Dict.toArray
      ->Array.filterMap(((lang, text)) => {
        if hasEncodingError(text) {
          Some(
            makeWeakPoint(
              ~category=EncodingError,
              ~severity=Medium,
              ~location=alignment.referenceId,
              ~description=`Encoding error detected in text`,
              ~language=lang,
              (),
            ),
          )
        } else {
          None
        }
      })
    })
  }
}

module TruncationDetection = {
  /** Detect suspiciously short translations compared to reference */
  let detect = (
    corpus: Lang1000.Corpus.t,
    ~refLang: string,
    ~minRatio=0.2,
    (),
  ): array<weakPoint> => {
    corpus.alignments->Array.flatMap(alignment => {
      let refText = alignment.translations->Dict.get(refLang)
      switch refText {
      | None => []
      | Some(ref) =>
        let refLen = String.length(ref)->Float.fromInt
        if refLen < 5.0 {
          []
        } else {
          alignment.translations
          ->Dict.toArray
          ->Array.filterMap(((lang, text)) => {
            if lang == refLang {
              None
            } else {
              let textLen = String.length(text)->Float.fromInt
              let ratio = textLen /. refLen
              if ratio < minRatio && textLen > 0.0 {
                Some(
                  makeWeakPoint(
                    ~category=TruncatedContent,
                    ~severity=Medium,
                    ~location=alignment.referenceId,
                    ~description=`Text length ratio ${Float.toFixed(ratio, ~digits=2)} below threshold ${Float.toFixed(minRatio, ~digits=2)} (${Int.toString(Float.toInt(textLen))} vs ${Int.toString(Float.toInt(refLen))} chars)`,
                    ~language=lang,
                    (),
                  ),
                )
              } else {
                None
              }
            }
          })
        }
      }
    })
  }
}

module DuplicateDetection = {
  /** Detect identical text for different verses within a language */
  let detect = (corpus: Lang1000.Corpus.t): array<weakPoint> => {
    let langTexts: Dict.t<Dict.t<array<string>>> = Dict.make()

    // Group by language -> text -> [referenceIds]
    corpus.alignments->Array.forEach(alignment => {
      alignment.translations
      ->Dict.toArray
      ->Array.forEach(((lang, text)) => {
        let trimmed = String.trim(text)
        if String.length(trimmed) > 10 {
          let langDict = switch langTexts->Dict.get(lang) {
          | Some(d) => d
          | None =>
            let d = Dict.make()
            langTexts->Dict.set(lang, d)
            d
          }
          let refs = switch langDict->Dict.get(trimmed) {
          | Some(r) => r
          | None => []
          }
          langDict->Dict.set(trimmed, Array.concat(refs, [alignment.referenceId]))
        }
      })
    })

    langTexts
    ->Dict.toArray
    ->Array.flatMap(((lang, textDict)) => {
      textDict
      ->Dict.toArray
      ->Array.filterMap(((_, refs)) => {
        if Array.length(refs) > 1 {
          let refsStr = refs->Array.slice(~start=0, ~end=5)->Array.join(", ")
          Some(
            makeWeakPoint(
              ~category=DuplicateContent,
              ~severity=High,
              ~location=Array.getUnsafe(refs, 0),
              ~description=`Identical text in ${Int.toString(Array.length(refs))} verses: ${refsStr}`,
              ~language=lang,
              (),
            ),
          )
        } else {
          None
        }
      })
    })
  }
}

module CoverageAnalysis = {
  /** Detect languages present in one source but missing from others */
  let detectGaps = (
    ~sourceCoverage: array<(string, array<string>)>,
    (),
  ): array<weakPoint> => {
    // Build union of all languages
    let allLangs = Dict.make()
    sourceCoverage->Array.forEach(((source, langs)) => {
      langs->Array.forEach(lang => {
        let sources = switch allLangs->Dict.get(lang) {
        | Some(s) => s
        | None => []
        }
        allLangs->Dict.set(lang, Array.concat(sources, [source]))
      })
    })

    let totalSources = Array.length(sourceCoverage)
    allLangs
    ->Dict.toArray
    ->Array.filterMap(((lang, sources)) => {
      if Array.length(sources) < totalSources && Array.length(sources) == 1 {
        Some(
          makeWeakPoint(
            ~category=CoverageGap,
            ~severity=Low,
            ~location=lang,
            ~description=`Only available from ${Array.getUnsafe(sources, 0)}, missing from ${Int.toString(totalSources - 1)} other sources`,
            ~language=lang,
            (),
          ),
        )
      } else {
        None
      }
    })
  }
}

/** Run all analysis checks on a corpus and return a complete scan result */
let analyzeFull = (
  corpus: Lang1000.Corpus.t,
  ~refLang="eng",
  ~repo="lol",
  ~version="0.1.0",
  (),
): scanResult => {
  let weakPoints = Array.concatMany([
    EntropyAnalysis.detectOutliers(corpus, ()),
    MissingVerseDetection.detect(corpus, ~refLang, ()),
    EncodingDetection.detect(corpus),
    TruncationDetection.detect(corpus, ~refLang, ()),
    DuplicateDetection.detect(corpus),
  ])

  let totalLines =
    corpus.alignments->Array.reduce(0, (acc, a) =>
      acc + Dict.keysToArray(a.translations)->Array.length
    )

  {
    repo,
    version,
    timestamp: Date.make()->Date.toISOString,
    scanner: "lol-corpus-analyzer",
    scanner_version: "0.1.0",
    weak_points: weakPoints,
    statistics: {
      total_files: Lang1000.Corpus.languageCount(corpus),
      total_lines: totalLines,
      total_weak_points: Array.length(weakPoints),
      total_unsafe_blocks: weakPoints
        ->Array.filter(wp => Severity.toNumeric(wp.severity) >= 4)
        ->Array.length,
      files: corpus.languages->Array.map(lang => {
        let langWPs =
          weakPoints->Array.filter(wp =>
            wp.language->Option.getOr("") == lang.code
          )
        {
          file: lang.code,
          total_lines: corpus.alignments
            ->Array.filter(a => a.translations->Dict.get(lang.code)->Option.isSome)
            ->Array.length,
          weak_points: Array.length(langWPs),
          unsafe_blocks: langWPs
            ->Array.filter(wp => Severity.toNumeric(wp.severity) >= 4)
            ->Array.length,
        }
      }),
    },
  }
}
