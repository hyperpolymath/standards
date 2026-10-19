# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# k9-grid.jq — the conformance-family verdict grid for the K9 lane a consumer
# repository calls (docs/VERDICT-GRID-SPEC.adoc §Conformance family).
#
# This is a port of the item, state and delta rules in
# 1-formats/k9/tools/conformance-report.mjs (the DEED–K9 lane that runs in
# this repository), so the two K9 lanes grade one result the same way. Where
# the two disagree on a rule they share, the .mjs is the reference rendering
# and this file is a bug. Two rules are this lane's own, because its evidence
# differs, and are marked "(this lane)" below: the corpus exit status here is
# the raw validator's, not the ledger-aware hook's; and the caller's ledger is
# compared with the PR base, so an entry added without a Ratchet-exception
# buys no exemption. It is jq rather than JavaScript so the estate's
# authored-JavaScript surface does not grow (docs/NO-JAVASCRIPT-SOURCE-POLICY.adoc).
#
# Input (one object, assembled by scripts/k9-grid.sh):
#   { identity:  {lane, sha, run, url, pr, repo, dirty, generated, canon_ref},
#     profile:   "sha256 hex",
#     expected:  [ "path" ... ],          # every file the lane set out to check
#     results:   [ one k9-validate.sh --json record per file, or
#                  {malformed: "<line>"} for a line that was not JSON ],
#     ledger:    [ "path" ... ],          # .machine_readable/k9-contract-debt.txt
#     ledger_growth: [ "path" ... ],      # ledger entries this PR adds with no Ratchet-exception
#     changed:   [ "path" ... ],          # files this PR touches
#     base_resolved: bool,                # false when the PR range could not be diffed
#     selftest:  {code: int|null, output: "..."},
#     typecheck: {code: int|null, output: "..."},
#     corpus_code: int|null }             # the validator's own exit status
# Output: the structured report; `render` turns it into Markdown.

def item(id; status; reason; advisory):
  { id: (id | tostring), status: status, reason: (reason // "" | tostring), advisory: advisory };
def item(id; status; reason): item(id; status; reason; false);

def counts:
  { passed:         ([.[] | select(.status == "pass")] | length),
    failed:         ([.[] | select(.status == "fail")] | length),
    indeterminate:  ([.[] | select(.status == "indeterminate")] | length),
    not_applicable: ([.[] | select(.status == "not_applicable")] | length),
    advisory:       ([.[] | select(.status == "fail" and .advisory)] | length),
    warnings:       ([.[] | select(.status == "pass" and .advisory)] | length) };

# Precedence: red > orange > yellow > green; grey only when every item is grey.
def state:
  if length == 0 then "orange"
  elif any(.[]; .status == "fail" and (.advisory | not)) then "red"
  elif any(.[]; .status == "indeterminate") then "orange"
  elif any(.[]; .advisory) then "yellow"
  elif all(.[]; .status == "not_applicable") then "grey"
  else "green" end;

# `ok NAME` / `FAIL NAME` / `SKIP NAME` lines from the validator's self-test.
def assertions(output):
  [ (output // "" | gsub("\u001b\\[[0-9;]*m"; "") | split("\n")[])
    | capture("^(?<k>ok|FAIL|SKIP)\\s+(?<rest>.+)$")?
    | item(.rest; (if .k == "ok" then "pass" elif .k == "FAIL" then "fail" else "indeterminate" end);
           (if .k == "ok" then "" else .rest end)) ];

def checked_assertions(output; code):
  assertions(output) as $rows
  | $rows
  + (if ($rows | length) == 0 then [item("check execution"; "indeterminate"; "No assertion evidence was produced.")] else [] end)
  + (if code != 0 and ($rows | all(.[]; .status == "pass")) and ($rows | length) > 0
     then [item("check execution"; "indeterminate"; "Process exited \(code // "without a status") despite passing assertion lines.")] else [] end);

def selftest_items(st):
  checked_assertions(st.output; st.code)
  + (if st.code == 0 and ((st.output // "") | contains("self-test: all assertions passed") | not)
     then [item("completion"; "indeterminate"; "Missing self-test completion evidence.")] else [] end);

def typecheck_items(tc; contract):
  [ item(contract;
         (if tc.code == 0 then "pass" elif tc.code == null then "indeterminate" else "fail" end);
         (if tc.code == 0 then "" else (tc.output // "" | sub("\\s+$"; "")) end)) ];

# One live-corpus item per validator record (conformance-report.mjs corpusItems).
# A ledger entry exempts a file only when the entry was already in the ledger
# at the PR base, or the PR declares the growth (this lane): an undeclared
# addition is listed in `growth`, gets a failing row of its own, and the file
# it names is graded as if it were not in the ledger.
def corpus_items(results; expected; ledger; growth; changed):
  (ledger | map({key: ., value: true}) | from_entries) as $ledger
  | (growth | map({key: ., value: true}) | from_entries) as $growth
  | (changed | map({key: ., value: true}) | from_entries) as $changed
  | (expected | map({key: ., value: true}) | from_entries) as $expected
  | reduce results[] as $r ({seen: {}, rows: []};
      if ($r | has("malformed")) then
        .rows += [item("validator output"; "indeterminate"; "Malformed JSON result.")]
      elif (($r.file | type) != "string") or ($expected[$r.file] | not) or (.seen[$r.file] // false)
           or (($r.verdict // "") | IN("pass", "fail", "error", "routed") | not)
           or (($r.findings | type) != "array") then
        .rows += [item((if ($r.file | type) == "string" then $r.file else "validator output" end); "indeterminate"; "Unexpected, duplicate or invalid result.")]
      else
        .seen[$r.file] = true
        | ([$r.findings[] | select(.severity == "skipped")]) as $unavailable
        | ([$r.findings[] | select(.severity == "error")]) as $errors
        | ([$r.findings[] | select(.severity == "warning")]) as $warnings
        | ([$r.findings[] | "\(.rule): \(.message)"] | join("; ")) as $reason
        | (($ledger[$r.file] // false) and (($growth[$r.file] // false) | not)) as $listed
        | .rows += [
            if $r.verdict == "routed" then
              item($r.file; "not_applicable"; "Routed to another dialect; not validated as a K9 contract.")
            elif ($errors | any(.rule == "K9-N002")) and ($errors | any(.rule != "K9-N002") | not) then
              item($r.file; "indeterminate"; $reason)
            elif ($errors | length) > 0 or $r.verdict == "fail" or $r.verdict == "error" then
              item($r.file; "fail"; $reason; ($listed and (($changed[$r.file] // false) | not)))
            elif ($unavailable | length) > 0 then
              item($r.file; "indeterminate"; $reason)
            elif $listed then
              item($r.file; "fail"; "Stale debt exemption: this file now conforms.")
            else
              item($r.file; "pass"; (if ($warnings | length) > 0 then $reason else "" end); (($warnings | length) > 0))
            end ]
      end)
  | . as $acc
  | $acc.rows
    + [ expected[] | select(($acc.seen[.] // false) | not) | item(.; "indeterminate"; "Validator did not return a result for this file.") ]
    # A ledger entry that names no tracked K9 file exempts nothing and is debt
    # the ledger must shed (this lane): the shrink-only rule has no other way
    # to see it.
    + [ ledger[] | select(($expected[.] // false) | not) | item(.; "fail"; "Stale debt exemption: this path is not a tracked K9 file.") ]
    + [ growth[] | item("ledger + \(.)"; "fail"; "Added to the debt ledger in this PR with no Ratchet-exception naming the ledger; the entry exempts nothing.") ]
    + (if (expected | length) == 0 then [item("live corpus"; "not_applicable"; "No tracked K9 contracts or routing files.")] else [] end);

def titles: { typecheck: "K9 contract typecheck", selftest: "K9 validator self-test", corpus: "K9 live corpus (L0–L2)" };

def category(id; items): { id: id, title: titles[id], items: items, counts: (items | counts), state: (items | state) };

def build:
  . as $in
  | ("1-formats/k9/spec/contract/k9_contract.ncl") as $contract
  | (corpus_items($in.results // []; $in.expected // []; $in.ledger // []; $in.ledger_growth // []; $in.changed // [])
     + (if $in.base_resolved == false then [item("change scope"; "indeterminate"; "Cannot resolve the PR base/head range; debt exemptions cannot be determined.")] else [] end)) as $corpus0
  # The exit status is the raw validator's worst over the corpus, so a
  # ledgered (advisory) failure explains it as well as a blocking one does
  # (this lane; the .mjs reads the ledger-aware hook, where only a blocking
  # row can). Only an exit that no fail or indeterminate row accounts for is
  # missing evidence.
  | ($corpus0
     + (if ($in.corpus_code // 0) != 0 and ($corpus0 | any(.[]; .status == "indeterminate" or .status == "fail") | not)
        then [item("corpus gate"; "indeterminate"; "Validator exited \($in.corpus_code); inspect corpus.log.")] else [] end)) as $corpus
  | [ category("typecheck"; typecheck_items($in.typecheck // {code: null, output: ""}; $contract)),
      category("selftest"; selftest_items($in.selftest // {code: null, output: ""})),
      category("corpus"; $corpus) ] as $categories
  | ([$categories[].items[]]) as $all
  | { schema: 1, lane: ($in.identity.lane // "k9"), profile: ($in.profile // ""),
      sha: ($in.identity.sha // ""), run: ($in.identity.run // "local"), url: ($in.identity.url // ""),
      pr: ($in.identity.pr // "local"), repo: ($in.identity.repo // ""), dirty: ($in.identity.dirty // false),
      generated: ($in.identity.generated // ""), canon_ref: ($in.identity.canon_ref // ""),
      categories: $categories, state: ($all | state), counts: ($all | counts) };

# One category's movement since the previous revision of the same PR.
def delta(prev; report):
  . as $c
  | if prev == null or prev.schema != 1 or prev.profile != report.profile or prev.sha == report.sha or prev.pr != report.pr
    then "— no comparable baseline"
    else (prev.categories | map(select(.id == $c.id)) | first) as $b
    | if $b == null or ($b.counts.indeterminate > 0) or ($c.counts.indeterminate > 0) then "— indeterminate"
      elif $b.counts.not_applicable != $c.counts.not_applicable then "— scope changed"
      elif $b.counts.failed != $c.counts.failed then
        (if $c.counts.failed < $b.counts.failed then "↑ improved (\($b.counts.failed - $c.counts.failed) fewer failures)"
         else "↓ regressed (\($c.counts.failed - $b.counts.failed) more failures)" end)
      elif $c.counts.advisory < $b.counts.advisory then "↑ fewer advisories"
      elif $c.counts.advisory > $b.counts.advisory then "↓ more advisories"
      elif ($c.counts.warnings // 0) < ($b.counts.warnings // 0) then "↑ fewer warnings"
      elif ($c.counts.warnings // 0) > ($b.counts.warnings // 0) then "↓ more warnings"
      else "↔ unchanged" end end;

def state_label: {red: "🔴 failing", orange: "🟠 indeterminate", yellow: "🟡 advisory", green: "🟢 conformant", grey: "⚪ not applicable"}[.];
def colour: {red: "red", orange: "orange", yellow: "yellow", green: "brightgreen", grey: "lightgrey"}[.];
def word: {red: "failing", orange: "indeterminate", yellow: "advisory", green: "conformant", grey: "not_applicable"}[.];
def esc: tostring | gsub("&"; "&amp;") | gsub("<"; "&lt;") | gsub(">"; "&gt;") | gsub("\\|"; "&#124;")
         | gsub("`"; "&#96;") | gsub("\r?\n"; " ");
def grid_label: "K9 L0–L2";
def badge_url(st): "https://img.shields.io/badge/\(grid_label | @uri)-\(st | word | @uri)-\(st | colour)";

def render(prev):
  . as $r
  | "<!-- verdict-grid:\($r.lane) regenerated on every run -->\n"
  + "![\(grid_label) \($r.state)](\(badge_url($r.state)))\n\n"
  + "\($r.state | state_label) · commit `\($r.sha[0:12] | esc)` · "
  + (if $r.url != "" then "[run \($r.run | esc)](\($r.url))" else "local run" end)
  + (if $r.canon_ref != "" then " · validator hyperpolymath/standards@`\($r.canon_ref[0:12] | esc)`" else "" end)
  + (if $r.dirty then " · working tree modified" else "" end) + "\n\n"
  + "| Category | Status | Pass | Fail | Indeterminate | N/A | Since previous revision |\n|---|---|---:|---:|---:|---:|---|\n"
  + ([ $r.categories[]
       | "| \(.title) | \(.state | state_label) | \(.counts.passed)\(if .counts.warnings > 0 then " (\(.counts.warnings) with warnings)" else "" end) | \(.counts.failed)\(if .counts.advisory > 0 then " (\(.counts.advisory) advisory)" else "" end) | \(.counts.indeterminate) | \(.counts.not_applicable) | \(delta(prev; $r)) |\n" ] | add // "")
  + ([ $r.categories[]
       | ([.items[] | select(.status != "pass" or .advisory)]) as $f
       | select(($f | length) > 0)
       | "\n<details>\n<summary>\(.title): \($f | length) finding(s) / scope notes</summary>\n\n"
         + ([ $f[0:20][] | "- **\(.id | esc | .[0:240])** — \(.status | esc)\(if .advisory then " (advisory)" else "" end): \(.reason | esc | .[0:280])\n" ] | add // "")
         + (if ($f | length) > 20 then "- \(($f | length) - 20) more; see the structured report and logs in the artifact.\n" else "" end)
         + "\n</details>\n" ] | add // "")
  + "\nScope: K9 components L0–L2 against contract v1.0.0 at the named standards commit. L3 signature verification and routed coordination-dialect files are not certified. Green is limited to this scope. Orange is missing evidence; yellow is grandfathered debt or warnings; grey is an explicit scope exclusion. report.json, badge.json and the validator logs are in the k9-grid artifact on the linked run.\n"
  + "\n<!-- k9-baseline:\({schema: $r.schema, profile: $r.profile, pr: $r.pr, sha: $r.sha,
        categories: [$r.categories[] | {id, counts}]} | tojson | @base64) -->\n";

def badge: {schemaVersion: 1, label: grid_label, message: (.state | word | gsub("_"; " ")), color: (.state | colour)};

# Entry point: $mode selects build (input: the assembled object), render
# (input: report.json; $previous: the prior baseline or null) or badge.
if $mode == "build" then build
elif $mode == "render" then render($previous)
elif $mode == "badge" then badge
else error("unknown mode \($mode)") end
