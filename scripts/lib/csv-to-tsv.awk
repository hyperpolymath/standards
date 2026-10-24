# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# csv-to-tsv.awk — parse one CSV record per line into TSV.
#
# WHY THIS EXISTS
# ---------------
# Python is banned estate-wide (inline-python-allow.txt is a SHRINK-ONLY
# ledger: "Never add a line; never raise a count"), and the one thing Python
# was genuinely doing better here is its stdlib csv module. `awk -F,` is not a
# substitute: the 2026-10-24 backlog classification has 228 rows of which
# dozens carry quoted fields containing commas —
#
#     1306,"panic-attack (advisory): 10 critical + 1 high ...",Q,Quick fix,"bug,security",...
#
# so splitting on "," silently moves every later column one to the left. The
# failure is quiet and lands in the report, which is the one place a number is
# supposed to be trustworthy.
#
# WHAT IT HANDLES
#   * quoted fields, including commas and doubled quotes ("" -> ")
#   * unquoted fields, empty fields, CRLF input
#   * one record per line
#
# WHAT IT REFUSES
#   A quoted field that does not close on its line (a record containing a
#   literal newline) exits 2. Emitting it would produce a record boundary that
#   does not exist and a consumer reading line-by-line would silently split one
#   row into two — a wrong answer where the input was merely unsupported. Fail
#   closed; no file in the estate needs the other behaviour today.
#
# Usage:
#   awk -f scripts/lib/csv-to-tsv.awk FILE.csv
#   ... | awk -F'\t' -v tier=C '$3 == tier { print $1 "\t" $12 }'

{
  line = $0
  sub(/\r$/, "", line)

  out = ""
  in_quotes = 0
  n = length(line)

  for (i = 1; i <= n; i++) {
    c = substr(line, i, 1)

    if (in_quotes) {
      if (c == "\"") {
        if (substr(line, i + 1, 1) == "\"") {
          out = out "\""
          i++
        } else {
          in_quotes = 0
        }
      } else {
        out = out c
      }
      continue
    }

    if (c == "\"") {
      in_quotes = 1
    } else if (c == ",") {
      out = out "\t"
    } else {
      out = out c
    }
  }

  if (in_quotes) {
    printf "csv-to-tsv: line %d: quoted field does not close on its line (multi-line records are not supported)\n", NR > "/dev/stderr"
    exit 2
  }

  print out
}
