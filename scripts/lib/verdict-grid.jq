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
#     profile:   "sha256 hex",
#     scope:     {mode: whole-repo|delta, changed: [paths], excluded_prefixes: [prefixes]} }
#
# SCOPE decides what the grid GRADES, and nothing else. Every finding is still
# reported in the artifact and counted in the JSON; scope only chooses which
# ones can move a row's colour. Three buckets:
#
#   graded    the finding counts toward the row state and the table columns
#   standing  outside this change (delta mode): reported, not graded
#   excluded  under a declared out-of-scope prefix (frozen archive): reported,
#             never graded, in either mode
#
# This exists because a findings lane that grades the WHOLE repository produces
# a verdict that cannot change: 57 standing findings meant every pull request,
# including a one-line docs fix, was purple. A verdict that is the same on
# every revision is a verdict nobody reads, which is the same outcome as no
# lane at all -- and worse, because it looks like one.
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

def counts_of($fs):
  { critical: ([$fs[] | select(.severity == "critical")] | length),
    high:     ([$fs[] | select(.severity == "high")] | length),
    medium:   ([$fs[] | select(.severity == "medium")] | length),
    minor:    ([$fs[] | select(.severity == "low" or .severity == "info")] | length) };

def counts: counts_of(.findings);

# A declared exclusion is a PREFIX match on the finding's path, and it applies
# in every mode: frozen history is out of scope whether or not the change
# touched it.
def under_exclusion($sc; $f):
  any((($sc.excluded_prefixes // [])[]) ; . as $x | ($f.file // "") | startswith($x));

# A finding with NO path cannot be shown to be outside the change, so it is
# graded. The alternative -- inferring "not mine" from a missing field -- would
# turn malformed evidence into silence.
def bucket($sc; $f):
  if under_exclusion($sc; $f) then "excluded"
  elif ($sc.mode // "whole-repo") != "delta" then "graded"
  elif ($f.file // "") == "" then "graded"
  elif any((($sc.changed // [])[]); . == $f.file) then "graded"
  else "standing" end;

def state:
  if .status == "not_applicable" then "grey"
  elif .status != "ran" then "orange"
  else ([.findings_graded[].severity | rank] | max // 0) as $m
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
  | ($in.scope // {mode: "whole-repo"}) as $scope
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
     | map(. + {findings_graded:   [.findings[] | select(bucket($scope; .) == "graded")],
                findings_standing: [.findings[] | select(bucket($scope; .) == "standing")],
                findings_excluded: [.findings[] | select(bucket($scope; .) == "excluded")]})
     | map(. + {counts:          (.findings_graded   | counts_of(.)),
                counts_standing: (.findings_standing | counts_of(.)),
                counts_excluded: (.findings_excluded | counts_of(.))})
     # ── Gating vs advisory ────────────────────────────────────────────────
     # A row is GATING when failing it can block a merge. Only such a row may
     # set the lane's overall state; anything else is advertised but never
     # grades the change.
     #
     # Why this is not cosmetic: panic-attack's own step in the reusable says it
     # "does not gate", yet its findings were driving the lane purple on every
     # revision. A verdict a green gate contradicts teaches readers to ignore
     # the badge, and then to ignore the gate.
     #
     # The default is `true` ONLY for the Hypatia row: it is the one producer
     # whose findings pass through .hypatia-baseline.json and the blocking
     # gate. An enrolment manifest may override either way -- `gating` is read
     # as a boolean when present, never as a truthy string.
     | map(. as $r | . + {gating: (if (($enrol[$r.id].gating | type) == "boolean")
                                   then $enrol[$r.id].gating else ($r.id == "hypatia") end)})
     | map(. + {state: state})) as $reporters
  | ([$reporters[] | select(.gating)]) as $gating_rows
  | ([$reporters[] | select(.gating | not)]) as $advisory_rows
  | { schema: 2, lane: ($in.identity.lane // "hypatia"), profile: $in.profile,
      sha: ($in.identity.sha // ""), run: ($in.identity.run // "local"), url: ($in.identity.url // ""),
      pr: ($in.identity.pr // "local"), repo: ($in.identity.repo // ""), dirty: ($in.identity.dirty // false),
      generated: ($in.identity.generated // ""), refs: ($in.identity.refs // {}),
      scope: { mode: ($scope.mode // "whole-repo"),
               changed: (($scope.changed // []) | length),
               excluded_prefixes: ($scope.excluded_prefixes // []) },
      reporters: $reporters,
      # The lane state is the worst GATING row. When a lane has no gating row
      # at all, the state falls back to every row, because "no row can block"
      # must not read as "green".
      state: (if ($gating_rows | length) == 0
              then ($reporters | map(.state) | overall)
              else ($gating_rows | map(.state) | overall) end),
      advisory: { rows: ($advisory_rows | length),
                  state: (if ($advisory_rows | length) == 0 then "none"
                          else ($advisory_rows | map(.state) | overall) end),
                  critical: ([$advisory_rows[].findings_graded[] | select(.severity == "critical")] | length),
                  high: ([$advisory_rows[].findings_graded[] | select(.severity == "high")] | length) },
      counts: ([$reporters[].findings_graded[]] | counts_of(.)) };

# One row's movement since the previous revision of the same PR. `report` is
# the whole report (profile, pr, sha); `.` is the row.
def delta(prev; report):
  . as $r
  # schema 2 added scope-aware counts. A baseline embedded by a v1 renderer
  # counted the whole repository, so comparing it with a delta-scoped count
  # would show a large fake "improvement" on the first revision after upgrade.
  # Unknown scope in the baseline is treated the same way: incomparable, said
  # out loud, never silently zero.
  | if prev == null or prev.schema != 2 or prev.profile != report.profile or prev.pr != report.pr or prev.sha == report.sha
      or (prev.scope.mode // "unknown") != (report.scope.mode // "unknown")
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
  + (if $r.dirty then " · working tree modified" else "" end)
  # The scope is stated where the verdict is stated, not only in the footer.
  # A reader must not have to reach the last line to learn that "green" meant
  # "green for this change".
  + (if ($r.scope.mode // "whole-repo") == "delta"
       then " · scope: this change (\($r.scope.changed) file(s))"
       else " · scope: whole repository" end)
  # An exclusion the reader cannot see is indistinguishable from a clean scan.
  + (if (($r.scope.excluded_prefixes // []) | length) > 0
       then " · excludes \((($r.scope.excluded_prefixes) | map("`" + . + "`") | join(", ")))"
       else "" end) + "\n\n"
  # A row's gate status is stated in the table, not inferred from the prose:
  # "purple" from a reporter that cannot block a merge and "purple" from one
  # that can are different facts, and used to look identical.
  + (if (($r.advisory.rows // 0) > 0)
       then "Advisory: \($r.advisory.rows) row(s), worst \($r.advisory.state | state_label) (\($r.advisory.critical) critical, \($r.advisory.high) high in grid scope) — reported, never grading the change.\n"
       else "" end)
  + "\n| Reporter | Gate | Status | Critical | High | Medium | Minor | Since previous revision |\n|---|---|---|---:|---:|---:|---:|---|\n"
  + ([ $r.reporters[]
       | "| \(.title | esc) | \(if .gating then "gating" else "advisory" end) | \(.state | state_label) | \(.counts.critical) | \(.counts.high) | \(.counts.medium) | \(.counts.minor) | \(delta(prev; $r)) |\n" ] | add // "")
  + ([ $r.reporters[]
       | select(.status != "ran" or (.findings | length) > 0)
       | "\n<details>\n<summary>\(.title | esc): "
         + (if .status == "ran" then "\(.findings | length) finding(s)" else "\(.status | esc) — \(.reason | esc)" end)
         + "</summary>\n\n"
         + (if .status != "ran" then "- **\(.state | state_label)**: \(.reason | esc)\n" else "" end)
         + ([ .findings_graded | sort_by(.severity | rank) | reverse | .[0:20][]
              | "- **\(.severity | esc)** `\(.id | esc)` — \(if .file != "" then "`\(.file | esc)\(if .line > 0 then ":\(.line)" else "" end)` — " else "" end)\(.message | esc | .[0:280])\n" ] | add // "")
         + (if (.findings_graded | length) > 20 then "- \((.findings_graded | length) - 20) more graded finding(s); see the structured report in the artifact.\n" else "" end)
         # Standing and excluded findings are NAMED AND COUNTED, never dropped.
         # The whole design accepts that a quieter grid can become a blind one;
         # this line is the mitigation, and it is why the numbers are here even
         # when they are large and unflattering.
         + (if (.findings_standing | length) > 0
            then "- \(.findings_standing | length) further finding(s) in files this change does not touch: reported, not graded (\(.counts_standing.critical) critical, \(.counts_standing.high) high).\n"
            else "" end)
         + (if (.findings_excluded | length) > 0
            then "- \(.findings_excluded | length) finding(s) under a declared out-of-scope prefix: reported, never graded.\n"
            else "" end)
         + "\n</details>\n" ] | add // "")
  + "\nScope: static findings reported by each row's tool at the named commit."
  + (if ($r.scope.mode // "whole-repo") == "delta"
       then " The table grades only findings in files this change touches, so the colour answers \"does this change introduce anything?\", not \"what does the whole repository still carry?\". Standing findings are counted in each accordion and in report.json; the baseline filter remains the place where acknowledged debt is recorded."
       else " The table grades every finding in the repository, so the colour will not move on a change that touches nothing a scanner flags." end)
  + " The lane state is the worst GATING row; advisory rows are listed and counted but never set it. Purple, red, yellow and blue grade the worst GRADED finding present; orange is evidence missing, malformed or a scanner that did not run; grey is an explicit scope exclusion with its reason. No row is a security certification. report.json, badge.json and the raw findings are in the verdict-grid artifact on the linked run.\n"
  # `scope` is embedded so the NEXT revision can tell whether the two counts
  # answer the same question. A baseline written before scoping existed has no
  # scope key and is therefore incomparable -- deliberately, because comparing
  # a whole-repository count with a delta count would report a large fake
  # improvement on the first revision after the upgrade.
  + "\n<!-- verdict-grid-baseline:\({schema: $r.schema, profile: $r.profile, pr: $r.pr, sha: $r.sha,
        scope: {mode: $r.scope.mode},
        reporters: [$r.reporters[] | {id, status, gating, counts}]} | tojson | @base64) -->\n";

def badge: {schemaVersion: 1, label: (.lane | if . == "hypatia" then "Hypatia grid" else . end),
            message: (.state | word | gsub("_"; " ")), color: (.state | colour)};

# Entry point: $mode selects build (input: the assembled sources object),
# render (input: report.json; $previous: the prior baseline or null) or badge.
if $mode == "build" then build
elif $mode == "render" then render($previous)
elif $mode == "badge" then badge
else error("unknown mode \($mode)") end
