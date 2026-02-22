// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

/**
 * VeriSimDB JSON Export
 *
 * Serializes corpus scan results to verisimdb-data compatible JSON
 * format for ingestion into the VeriSimDB pipeline.
 */

open VeriSimDB

module Json = {
  /** Escape a string for JSON output */
  let escapeString = (s: string): string => {
    s
    ->String.replaceAll("\\", "\\\\")
    ->String.replaceAll("\"", "\\\"")
    ->String.replaceAll("\n", "\\n")
    ->String.replaceAll("\r", "\\r")
    ->String.replaceAll("\t", "\\t")
  }

  let str = (s: string): string => `"${escapeString(s)}"`
  let int = (n: int): string => Int.toString(n)
  let optStr = (o: option<string>): string =>
    switch o {
    | Some(s) => str(s)
    | None => "null"
    }
}

let weakPointToJson = (wp: weakPoint): string => {
  let fields = [
    `"category": ${Json.str(Category.toString(wp.category))}`,
    `"severity": ${Json.str(Severity.toString(wp.severity))}`,
    `"location": ${Json.str(wp.location)}`,
    `"description": ${Json.str(wp.description)}`,
    `"context": ${Json.optStr(wp.context)}`,
    `"language": ${Json.optStr(wp.language)}`,
    `"source": ${Json.optStr(wp.source)}`,
  ]
  `{${fields->Array.join(", ")}}`
}

let fileStatToJson = (fs: fileStatistic): string => {
  `{"file": ${Json.str(fs.file)}, "total_lines": ${Json.int(fs.total_lines)}, "weak_points": ${Json.int(fs.weak_points)}, "unsafe_blocks": ${Json.int(fs.unsafe_blocks)}}`
}

let statisticsToJson = (stats: corpusStatistics): string => {
  let filesJson = stats.files->Array.map(fileStatToJson)->Array.join(", ")
  `{"total_files": ${Json.int(stats.total_files)}, "total_lines": ${Json.int(stats.total_lines)}, "total_weak_points": ${Json.int(stats.total_weak_points)}, "total_unsafe_blocks": ${Json.int(stats.total_unsafe_blocks)}, "files": [${filesJson}]}`
}

/** Convert a scan result to verisimdb-data compatible JSON string */
let toJson = (result: scanResult): string => {
  let wpJson = result.weak_points->Array.map(weakPointToJson)->Array.join(",\n    ")
  let statsJson = statisticsToJson(result.statistics)

  `{
  "repo": ${Json.str(result.repo)},
  "version": ${Json.str(result.version)},
  "timestamp": ${Json.str(result.timestamp)},
  "scanner": ${Json.str(result.scanner)},
  "scanner_version": ${Json.str(result.scanner_version)},
  "weak_points": [
    ${wpJson}
  ],
  "statistics": ${statsJson}
}`
}

/** Deno.writeTextFile binding */
@val @scope("Deno")
external writeTextFile: (string, string) => promise<unit> = "writeTextFile"

/** Write scan result JSON to a file */
let writeToFile = async (result: scanResult, path: string): unit => {
  let json = toJson(result)
  await writeTextFile(path, json)
}
