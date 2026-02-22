// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * VeriSimDB Types
 *
 * Types matching the verisimdb-data JSON schema for corpus quality
 * verification and weak point tracking. Maps corpus-specific metrics
 * to the VeriSimDB pipeline format.
 */

module Severity = {
  type t =
    | Critical
    | High
    | Medium
    | Low
    | Info

  let toString = severity =>
    switch severity {
    | Critical => "critical"
    | High => "high"
    | Medium => "medium"
    | Low => "low"
    | Info => "info"
    }

  let toNumeric = severity =>
    switch severity {
    | Critical => 5
    | High => 4
    | Medium => 3
    | Low => 2
    | Info => 1
    }
}

module Category = {
  type t =
    | MissingVerse
    | EncodingError
    | AlignmentFailure
    | StatisticalOutlier
    | CoverageGap
    | TruncatedContent
    | DuplicateContent

  let toString = category =>
    switch category {
    | MissingVerse => "missing-verse"
    | EncodingError => "encoding-error"
    | AlignmentFailure => "alignment-failure"
    | StatisticalOutlier => "statistical-outlier"
    | CoverageGap => "coverage-gap"
    | TruncatedContent => "truncated-content"
    | DuplicateContent => "duplicate-content"
    }
}

type weakPoint = {
  category: Category.t,
  severity: Severity.t,
  location: string,
  description: string,
  context: option<string>,
  language: option<string>,
  source: option<string>,
}

type fileStatistic = {
  file: string,
  total_lines: int,
  weak_points: int,
  unsafe_blocks: int,
}

type corpusStatistics = {
  total_files: int,
  total_lines: int,
  total_weak_points: int,
  total_unsafe_blocks: int,
  files: array<fileStatistic>,
}

type scanResult = {
  repo: string,
  version: string,
  timestamp: string,
  scanner: string,
  scanner_version: string,
  weak_points: array<weakPoint>,
  statistics: corpusStatistics,
}

let makeWeakPoint = (
  ~category,
  ~severity,
  ~location,
  ~description,
  ~context=?,
  ~language=?,
  ~source=?,
  (),
) => {
  category,
  severity,
  location,
  description,
  context,
  language,
  source,
}

let emptyScanResult = (~repo, ~version) => {
  repo,
  version,
  timestamp: Date.make()->Date.toISOString,
  scanner: "lol-corpus-analyzer",
  scanner_version: "0.1.0",
  weak_points: [],
  statistics: {
    total_files: 0,
    total_lines: 0,
    total_weak_points: 0,
    total_unsafe_blocks: 0,
    files: [],
  },
}
