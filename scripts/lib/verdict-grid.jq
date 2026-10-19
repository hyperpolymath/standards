# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# verdict-grid.jq — the findings-family verdict grid (docs/VERDICT-GRID-SPEC.adoc).
#
# Input (one object, assembled by scripts/verdict-grid.sh):
#   { sources:   [ {kind, name, present, valid, doc} ... ],
#     enrolment: object | null,       # .gitbot-fleet/enrolment.json, parsed
#     previous:  object | null,       # baseline embedded in the previous comment
#     identity:  {lane, sha, run, url, pr, repo, dirty, refs: {hypatia, panic, pons}},
#     profile:   "sha256 hex" }
# Output: the structured report; `render` turns it into Markdown.
#
# Every reporter is normalised to one record
#   { id, title, status: ran|not_run|error|not_applicable, reason, ref, findings }
# and every finding to { id, severity, file, line, message } with severity in
# critical | high | medium | low | info. Anything the adapter does not
# understand becomes status "error" with a reason: unreadable evidence is
# never a clean row.

def rank: {critical: 5, high: 4, medium: 3, low: 2, info: 1}[.] // 0;

def norm_sev:
  (. // "" | tostring | ascii_downcase)
  | if . == "critical" then "critical"
    elif . == "high" or . == "error" then "high"
    elif . == "medium" or . == "warn" or . == "warning" then "medium"
    elif . == "low" then "low"
    elif . == "info" or . == "informational" or . == "note" or . == "suggestion" then "info"
    else "unknown" end;

def finding(id; sev; file; line; msg):
  { id: (id // "" | tostring), severity: (sev | norm_sev),
    file: (file // "" | tostring), line: (line // 0 | tonumber? // 0),
    message: (msg // "" | tostring) };

def reporter(id; title; status; reason; ref; findings):
  { id: id, title: title, status: status, reason: (reason // ""), ref: (ref // ""), findings: findings }
  | if .status == "ran" and (.findings | any(.severity == "unknown"))
    then .status = "error" | .reason = "a finding carries a severity the grid does not recognise"
    else . end;

# A status envelope shared by panic-attack and pons:
#   {status: ran|not_run|error, tool, ref, reason, exit_code, counts, report}
def envelope(id; title; src; items):
  if src.present | not then reporter(id; title; "not_run"; "no status envelope was produced"; ""; [])
  elif src.valid | not then reporter(id; title; "error"; "the status envelope is not valid JSON"; ""; [])
  elif (src.doc | type) != "object" or (src.doc.status | type) != "string"
    then reporter(id; title; "error"; "the status envelope has no status"; ""; [])
  elif src.doc.status == "ran" then
    (try (src.doc | items) catch null) as $f
    | if $f == null then reporter(id; title; "error"; "the envelope reports a run but carries no readable report"; src.doc.ref; [])
      else reporter(id; title; "ran"; ""; src.doc.ref; $f) end
  elif src.doc.status == "not_run" or src.doc.status == "error"
    then reporter(id; title; src.doc.status; (src.doc.reason // "no reason recorded"); src.doc.ref; [])
  else reporter(id; title; "error"; "unrecognised envelope status \(src.doc.status)"; src.doc.ref; []) end;

def hypatia(src; ref):
  if src.present | not then reporter("hypatia"; "Hypatia scan"; "error"; "the scanner produced no findings array"; ref; [])
  elif src.valid | not then reporter("hypatia"; "Hypatia scan"; "error"; "the findings file is not valid JSON"; ref; [])
  elif (src.doc | type) != "array" or (src.doc | all(type == "object")) | not
    then reporter("hypatia"; "Hypatia scan"; "error"; "the findings file is not one array of objects"; ref; [])
  else reporter("hypatia"; "Hypatia scan"; "ran"; ""; ref;
    [src.doc[] | finding("\(.rule_module // "?")/\(.type // "?")"; .severity; .file; .line; .reason)]) end;

def panic(src):
  envelope("panic-attack"; "panic-attack assail"; src;
    [.report.weak_points[] | select(.suppressed != true)
     | finding(.category; .severity; .file; 0; .description)]);

def pons(src):
  envelope("pons-asinorum"; "pons asinorum"; src;
    [.report.findings[]
     | finding(.rule_id; .severity; .location.file; .location.line_start;
         "\(.message) (\(.tier // "?"), \(.evidence // "?"))")]);

def reportback(src):
  (src.name) as $id
  | if src.valid | not then reporter($id; $id; "error"; "the report-back is not valid JSON"; ""; [])
    elif (src.doc | type) != "object" then reporter($id; $id; "error"; "the report-back is not an object"; ""; [])
    else (src.doc.reporter // $id) as $rid
    | (src.doc.title // $rid) as $title
    | if src.doc.status == "ran" then
        reporter($rid; $title; "ran"; ""; src.doc.ref;
          [ (src.doc.findings // [])[] | finding(.rule_id // .id; .severity; .file; .line; .message) ])
      elif (src.doc.status | IN("not_run", "error", "not_applicable"))
        then reporter($rid; $title; src.doc.status; (src.doc.reason // "no reason recorded"); src.doc.ref; [])
      else reporter($rid; $title; "error"; "unrecognised report-back status \(src.doc.status)"; src.doc.ref; []) end
    end;

def counts:
  { critical: ([.findings[] | select(.severity == "critical")] | length),
    high:     ([.findings[] | select(.severity == "high")] | length),
    medium:   ([.findings[] | select(.severity == "medium")] | length),
    minor:    ([.findings[] | select(.severity == "low" or .severity == "info")] | length) };

def state:
  if .status == "not_applicable" then "grey"
  elif .status != "ran" then "orange"
  else ([.findings[].severity | rank] | max // 0) as $m
    | if $m >= 5 then "purple" elif $m == 4 then "red" elif $m == 3 then "yellow"
      elif $m >= 1 then "blue" else "green" end end;

# Precedence: a known critical beats missing evidence, which beats anything milder.
def overall:
  if length == 0 then "orange"
  elif all(. == "grey") then "grey"
  else (["purple", "red", "orange", "yellow", "blue", "green"] as $order
        | . as $states | $order | map(select(. as $c | $states | index($c))) | first) end;

def build:
  . as $in
  | ($in.sources | map(select(.kind == "hypatia")) | first // {present: false}) as $h
  | ($in.sources | map(select(.kind == "panic")) | first // {present: false}) as $p
  | ($in.sources | map(select(.kind == "pons")) | first // {present: false}) as $q
  | [ hypatia($h; $in.identity.refs.hypatia), panic($p), pons($q) ] as $builtin
  | [ $in.sources[] | select(.kind == "report-back") | reportback(.) ] as $backs
  | ($in.enrolment.reporters // {}) as $enrol
  | [ $enrol | to_entries[] | .key as $id | .value as $e
      | select(($backs | map(.id) | index($id)) == null)
      | if $e.enrolled == true
        then reporter($id; ($e.title // $id); "not_run"; "enrolled, but no report-back was received"; ""; [])
        else reporter($id; ($e.title // $id); "not_applicable"; ($e.reason // "not enrolled for this repository"); ""; []) end ] as $missing
  | ($builtin + $backs + $missing
     | map(. + {counts: counts, state: state})) as $reporters
  | { schema: 1, lane: ($in.identity.lane // "hypatia"), profile: $in.profile,
      sha: ($in.identity.sha // ""), run: ($in.identity.run // "local"), url: ($in.identity.url // ""),
      pr: ($in.identity.pr // "local"), repo: ($in.identity.repo // ""), dirty: ($in.identity.dirty // false),
      generated: ($in.identity.generated // ""), refs: ($in.identity.refs // {}),
      reporters: $reporters,
      state: ($reporters | map(.state) | overall),
      counts: ({findings: $reporters} | counts) };

# One row's movement since the previous revision of the same PR. `report` is
# the whole report (profile, pr, sha); `.` is the row.
def delta(prev; report):
  . as $r
  | if prev == null or prev.schema != 1 or prev.profile != report.profile or prev.pr != report.pr or prev.sha == report.sha
    then "— no comparable baseline" else . end
  | if type == "string" then . else
    (prev.reporters | map(select(.id == $r.id)) | first) as $b
    | if $b == null then "— no comparable baseline"
      elif $b.status != $r.status then "— evidence changed"
      elif $r.status == "not_applicable" then "↔ unchanged"
      elif $r.status != "ran" then "— indeterminate"
      else (($b.counts.critical + $b.counts.high) as $a | ($r.counts.critical + $r.counts.high) as $c
        | if $c < $a then "↑ improved (\($a - $c) fewer blocking)"
          elif $c > $a then "↓ regressed (\($c - $a) more blocking)"
          elif $r.counts.medium < $b.counts.medium then "↑ fewer medium"
          elif $r.counts.medium > $b.counts.medium then "↓ more medium"
          elif $r.counts.minor < $b.counts.minor then "↑ fewer minor"
          elif $r.counts.minor > $b.counts.minor then "↓ more minor"
          else "↔ unchanged" end) end end;

def state_label: {purple: "🟣 critical", red: "🔴 high", orange: "🟠 indeterminate", yellow: "🟡 medium",
            blue: "🔵 minor", green: "🟢 clean", grey: "⚪ not applicable"}[.];
def colour: {purple: "blueviolet", red: "red", orange: "orange", yellow: "yellow",
             blue: "blue", green: "brightgreen", grey: "lightgrey"}[.];
def word: {purple: "critical", red: "high", orange: "indeterminate", yellow: "medium",
           blue: "minor", green: "clean", grey: "not_applicable"}[.];
def esc: tostring | gsub("&"; "&amp;") | gsub("<"; "&lt;") | gsub(">"; "&gt;") | gsub("\\|"; "&#124;")
         | gsub("`"; "&#96;") | gsub("\r?\n"; " ");
def badge_url(lbl; st): "https://img.shields.io/badge/\(lbl | @uri)-\(st | word | @uri)-\(st | colour)";

def render(prev):
  . as $r
  | ($r.lane | if . == "hypatia" then "Hypatia grid" else . end) as $lbl
  | "<!-- verdict-grid:\($r.lane) regenerated on every run -->\n"
  + "![\($lbl) \($r.state)](\(badge_url($lbl; $r.state)))\n\n"
  + "\($r.state | state_label) · commit `\($r.sha[0:12] | esc)` · "
  + (if $r.url != "" then "[run \($r.run | esc)](\($r.url))" else "local run" end)
  + (if $r.dirty then " · working tree modified" else "" end) + "\n\n"
  + "| Reporter | Status | Critical | High | Medium | Minor | Since previous revision |\n|---|---|---:|---:|---:|---:|---|\n"
  + ([ $r.reporters[]
       | "| \(.title | esc) | \(.state | state_label) | \(.counts.critical) | \(.counts.high) | \(.counts.medium) | \(.counts.minor) | \(delta(prev; $r)) |\n" ] | add // "")
  + ([ $r.reporters[]
       | select(.status != "ran" or (.findings | length) > 0)
       | "\n<details>\n<summary>\(.title | esc): "
         + (if .status == "ran" then "\(.findings | length) finding(s)" else "\(.status | esc) — \(.reason | esc)" end)
         + "</summary>\n\n"
         + (if .status != "ran" then "- **\(.state | state_label)**: \(.reason | esc)\n" else "" end)
         + ([ .findings | sort_by(.severity | rank) | reverse | .[0:20][]
              | "- **\(.severity | esc)** `\(.id | esc)` — \(if .file != "" then "`\(.file | esc)\(if .line > 0 then ":\(.line)" else "" end)` — " else "" end)\(.message | esc | .[0:280])\n" ] | add // "")
         + (if (.findings | length) > 20 then "- \((.findings | length) - 20) more; see the structured report in the artifact.\n" else "" end)
         + "\n</details>\n" ] | add // "")
  + "\nScope: static findings reported by each row's tool at the named commit. Purple, red, yellow and blue grade the worst finding present; orange is evidence missing, malformed or a scanner that did not run; grey is an explicit scope exclusion with its reason. No row is a security certification. report.json, badge.json and the raw findings are in the verdict-grid artifact on the linked run.\n"
  + "\n<!-- verdict-grid-baseline:\({schema: $r.schema, profile: $r.profile, pr: $r.pr, sha: $r.sha,
        reporters: [$r.reporters[] | {id, status, counts}]} | tojson | @base64) -->\n";

def badge: {schemaVersion: 1, label: (.lane | if . == "hypatia" then "Hypatia grid" else . end),
            message: (.state | word | gsub("_"; " ")), color: (.state | colour)};

# Entry point: $mode selects build (input: the assembled sources object),
# render (input: report.json; $previous: the prior baseline or null) or badge.
if $mode == "build" then build
elif $mode == "render" then render($previous)
elif $mode == "badge" then badge
else error("unknown mode \($mode)") end
