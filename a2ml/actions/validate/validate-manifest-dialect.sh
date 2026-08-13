#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# validate-manifest-dialect.sh — reference validator for MANIFEST-DIALECT-SPEC.adoc
#
# Checks every .a2ml file whose first non-comment line is `---` and which has a
# matching close delimiter. Files that open `---` and never close it are NOT
# this dialect (spec §8) and are skipped, not failed.
#
# Exits non-zero if any file fails, so it can gate CI.
#
# Ported from Python: estate language policy bans Python with no exceptions,
# and this was one of three files keeping standards' own governance gate red —
# a validator that blocked the pull requests it was meant to serve.
#
# ⚠ SHELL, NOT BUN. The estate's JS runtime moved from Deno to Bun, and this
# check has already been through one language change; a validator that depends
# on no runtime cannot be invalidated by the next one. awk does the whole job.
#
# ⚠ THE EXCLUSIONS ARE THE SPEC, NOT CONVENIENCE. The dialect deliberately
# forbids YAML constructs that make a manifest ambiguous to a non-YAML reader:
# anchors, aliases, explicit tags, flow mappings and single-quoted scalars.
# Removing one of these checks silently widens the dialect.
set -uo pipefail

mapfile -t FILES < <(git ls-files '*.a2ml' 2>/dev/null)
[ ${#FILES[@]} -eq 0 ] && { echo "  no .a2ml files tracked"; exit 0; }

printf '%s\n' "${FILES[@]}" | awk '
  function reset() { delete errs; nerr = 0 }
  function adderr(e) { if (!(e in errs)) { errs[e] = 1; nerr++ } }

  {
    file = $0
    # --- locate the frontmatter block -------------------------------------
    n = 0; open_at = 0; close_at = 0
    while ((getline line < file) > 0) { L[++n] = line }
    close(file)
    i = 1
    while (i <= n) {
      s = L[i]; sub(/\r$/, "", s)
      t = s; gsub(/^[ \t]+|[ \t]+$/, "", t)
      if (t != "" && t !~ /^(#|\/\/)/) break
      i++
    }
    if (i > n) { delete L; n = 0; next }
    s = L[i]; gsub(/^[ \t]+|[ \t]+$/, "", s)
    if (s != "---") { delete L; n = 0; next }
    open_at = i
    for (k = open_at + 1; k <= n; k++) {
      t = L[k]; sub(/\r$/, "", t); gsub(/^[ \t]+|[ \t]+$/, "", t)
      if (t == "---") { close_at = k; break }
    }
    if (close_at == 0) { delete L; n = 0; next }   # spec §8: not this dialect

    tot++
    reset()
    blk = -1
    for (k = open_at + 1; k < close_at; k++) {
      s = L[k]; sub(/\r$/, "", s)

      if (blk >= 0) {
        match(s, /^ */); ind = RLENGTH
        stripped = s; gsub(/^[ ]+|[ ]+$/, "", stripped)
        if (stripped != "" && ind > blk) continue     # opaque block content
        blk = -1
      }
      stripped = s; gsub(/^[ ]+|[ ]+$/, "", stripped)
      if (stripped == "" || stripped ~ /^#/) continue

      if (index(s, "\t")) adderr("tab-indent")
      if (s ~ /:[ \t]*&[A-Za-z0-9_]/ || s ~ /^[ ]*&[A-Za-z0-9_]/) adderr("excluded:anchor")
      if (s ~ /:[ \t]*\*[A-Za-z0-9_]/)                            adderr("excluded:alias")
      if (s ~ /:[ \t]*!!?[A-Za-z0-9_]/)                           adderr("excluded:tag")
      if (s ~ /:[ \t]*\{/)                                        adderr("excluded:flow-map")
      if (s ~ /:[ \t]*\047/)                                      adderr("excluded:single-quote")

      if (match(s, /^[ ]*[A-Za-z_][A-Za-z0-9_.\-]*[ ]*:([ ]|$)/)) {
        match(s, /^[ ]*/); ind = RLENGTH
        if (ind % 2) adderr("odd-indent")
        rest = s; sub(/^[ ]*[A-Za-z_][A-Za-z0-9_.\-]*[ ]*:[ ]?/, "", rest)
        gsub(/^[ ]+|[ ]+$/, "", rest)
        if (rest ~ /^[|>][-+]?$/) blk = ind
      } else if (s ~ /^[ ]*-[ ]+[^ ]/) {
        # sequence entry — fine
      } else if (s ~ /^[ ]*[|>][-+]?[ ]*$/) {
        # bare block indicator — fine
      } else {
        adderr("unparsed-line")
      }
    }
    if (nerr == 0) ok++
    else for (e in errs) { fails[e]++; if (!(e in example)) example[e] = file }
    delete L; n = 0
  }
  END {
    if (tot == 0) { print "  well-formed frontmatter files : 0"; exit 0 }
    printf "  well-formed frontmatter files : %d\n", tot
    printf "  CONFORM to the spec as written: %d  (%d%%)\n", ok, int(100 * ok / tot)
    printf "  rejected                      : %d\n", tot - ok
    for (e in fails) printf "    %-22s %d   e.g. %s\n", e, fails[e], example[e]
    exit (tot - ok == 0 ? 0 : 1)
  }
'
