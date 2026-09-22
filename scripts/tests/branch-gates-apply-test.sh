#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# branch-gates-apply-test.sh — regression test for apply-branch-gates.sh.
#
# WHAT THIS PINS, AND WHY A PASSING SUITE WOULD NOT BE ENOUGH
#   The defect this script exists to prevent is a VACUOUS GATE: a
#   required_status_checks rule carrying an EMPTY context list. Such a rule
#   reports "this branch is protected" in every summary view while requiring
#   nothing at all — strictly worse than having no rule, because it is
#   indistinguishable from a working one.
#
#   A green suite proves nothing about that. So three of the cases below are
#   MUTANTS: the applier is copied, the guard under test is deliberately
#   removed, and the suite must go RED. A mutant that survives means the
#   corresponding control is decorative.
#
# Run: bash scripts/tests/branch-gates-apply-test.sh
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
APPLIER="${BRANCH_GATES_TARGET:-$SCRIPT_DIR/../apply-branch-gates.sh}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

pass=0; fail=0
ok()   { echo "PASS: $1"; pass=$((pass+1)); }
bad()  { echo "FAIL: $1"; fail=$((fail+1)); }

# ------------------------------------------------------------------ fixtures
FIX="$WORK/fix"; mkdir -p "$FIX"
BIN="$WORK/bin"; mkdir -p "$BIN"

# A `gh` shim: maps an API path to a fixture file, honours --jq, records PUTs.
cat > "$BIN/gh" <<'SHIM'
#!/usr/bin/env bash
set -uo pipefail
[ "${1:-}" = api ] || exit 0
shift
METHOD=GET; JQF=''; APIPATH=''
while [ $# -gt 0 ]; do
  case "$1" in
    -X) shift; METHOD="$1" ;;
    --jq) shift; JQF="$1" ;;
    --input) shift; cp "$1" "$GH_FIX/LAST_PUT.json" ;;
    --paginate|--silent) : ;;
    -*) : ;;
    *) [ -n "$APIPATH" ] || APIPATH="$1" ;;
  esac
  shift
done
KEY=$(printf '%s' "$APIPATH" | tr '/?&=' '____')
if [ "$METHOD" = PUT ]; then printf '%s\n' "$APIPATH" >> "$GH_FIX/PUTS.log"; echo '{}'; exit 0; fi
F="$GH_FIX/$KEY.json"
[ -r "$F" ] || exit 1
if [ -n "$JQF" ]; then jq -r "$JQF" "$F"; else cat "$F"; fi
SHIM
chmod +x "$BIN/gh"

GATES="$WORK/gates-src.json"
cat > "$GATES" <<'G'
{ "version": 1,
  "profiles": {
    "base": { "applies_to": "every repo", "gate_workflows": ["governance.yml","codeql.yml","mirror.yml"] },
    "rust": { "detect": ["Cargo.toml"], "gate_workflows": ["rust-ci.yml"] },
    "ada":  { "detect": ["*.gpr"],      "gate_workflows": ["ada-ci.yml"] }
  },
  "never_required_workflows": ["mirror.yml"],
  "never_required_contexts": ["Allowlist Preflight","Code quality + docs"] }
G

mkfix() { printf '%s' "$2" > "$FIX/$(printf '%s' "$1" | tr '/?&=' '____').json"; }

reset_fix() { rm -f "$FIX"/*.json; cp "$GATES" "$WORK/gates.json"; : > "$FIX/PUTS.log"; }

run_applier() {           # run_applier <repo> [extra flags...]
  local repo="$1"; shift
  PATH="$BIN:$PATH" GH_FIX="$FIX" bash "${MUTANT:-$APPLIER}" \
    --gates-file "$WORK/gates.json" --repo "$repo" "$@" 2>"$WORK/err"
}

state_of() { tail -n +2 <<< "$1" | head -1 | cut -f2; }
detail_of() { tail -n +2 <<< "$1" | head -1 | cut -f3; }

# ================================================================ CASE 1
# POSITIVE CONTROL: contexts are derived from real jobs, never-required ones
# are stripped, and a never_required_workflow is not consulted at all.
reset_fix
R=acme/widget
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"},{"name":"codeql.yml"},{"name":"mirror.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"},{"name":"Cargo.toml"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/workflows/codeql.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":22}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance"},{"name":"governance / Code quality + docs"},{"name":"Allowlist Preflight"}]}'
mkfix "repos/$R/actions/runs/22/jobs?per_page=100" '{"jobs":[{"name":"CodeQL Security Analysis"}]}'
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active"},{"id":8,"target":"tag","enforcement":"active"}]'
mkfix "repos/$R/rulesets/9" '{"id":9,"name":"Base","target":"branch","enforcement":"active","conditions":{"ref_name":{"include":["~DEFAULT_BRANCH"],"exclude":[]}},"bypass_actors":[{"actor_id":5,"actor_type":"RepositoryRole","bypass_mode":"pull_request"}],"rules":[{"type":"deletion"},{"type":"required_signatures"}]}'

OUT=$(run_applier "$R")
S=$(state_of "$OUT"); D=$(detail_of "$OUT")
[ "$S" = "WOULD-GATE" ] && ok "control: state is WOULD-GATE" || bad "control: state=$S (want WOULD-GATE)"
case "$D" in *"governance / Governance"*) ok "control: derived the real job name" ;; *) bad "control: missing derived context — $D" ;; esac
case "$D" in *"CodeQL Security Analysis"*) ok "control: derived across two workflows" ;; *) bad "control: second workflow not derived" ;; esac
case "$D" in *"contexts=2"*) ok "control: exactly 2 contexts survive" ;; *) bad "control: wrong count — $D" ;; esac
case "$D" in *"excluded=["*"Code quality + docs"*) ok "control: never_required matched AFTER the ' / '" ;; *) bad "control: reusable-job-name exclusion missed — $D" ;; esac
# assert against the CONTEXT LIST only (after ":: "), not the excluded= report
CTXLIST="${D##*:: }"
case "$CTXLIST" in
  *"Allowlist Preflight"*|*"Code quality + docs"*) bad "control: a never_required context reached the rule — [$CTXLIST]" ;;
  *) ok "control: never_required contexts kept out of the rule — [$CTXLIST]" ;;
esac
[ -s "$FIX/PUTS.log" ] && bad "control: wrote WITHOUT --apply" || ok "control: report-only performed no PUT"

# ================================================================ CASE 2
# THE REFUSAL: zero derivable contexts must yield UNGATED and write nothing.
reset_fix
R=acme/norun
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[]}'
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active"}]'
mkfix "repos/$R/rulesets/9" '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'

OUT=$(run_applier "$R" --apply)
S=$(state_of "$OUT"); D=$(detail_of "$OUT")
[ "$S" = "UNGATED" ] && ok "zero contexts: state is UNGATED" || bad "zero contexts: state=$S (want UNGATED)"
case "$D" in *"no_run=[governance.yml]"*) ok "zero contexts: the un-run workflow is named" ;; *) bad "zero contexts: no_run not reported — $D" ;; esac
[ -s "$FIX/PUTS.log" ] && bad "zero contexts: PUT happened despite --apply refusal" || ok "zero contexts: no PUT even with --apply"

# ---- MUTANT A: delete the zero-context refusal. The suite MUST go red. ----
MUT="$WORK/mutant-a.sh"
sed 's|if \[ "\$NCTX" -eq 0 \]; then|if false; then|' "$APPLIER" > "$MUT"
reset_fix
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[]}'
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active"}]'
mkfix "repos/$R/rulesets/9" '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'
OUT=$(MUTANT="$MUT" run_applier "$R" --apply)
if [ "$(state_of "$OUT")" = "UNGATED" ]; then
  bad "MUTANT A SURVIVED: refusal removed yet still UNGATED — the control is decorative"
else
  ok "mutant A killed: without the refusal it becomes $(state_of "$OUT") and PUTs $(wc -l < "$FIX/PUTS.log") time(s)"
fi
if command grep -q 'required_status_checks' "$FIX/LAST_PUT.json" 2>/dev/null &&
   [ "$(jq '[.rules[]|select(.type=="required_status_checks")|.parameters.required_status_checks|length]|add // 0' "$FIX/LAST_PUT.json")" = "0" ]; then
  ok "mutant A wrote the VACUOUS empty-list rule — precisely the defect being guarded"
fi

# ================================================================ CASE 3
# EXACTNESS GUARD: a transform touching anything else must be REFUSED.
reset_fix
R=acme/widget
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance"}]}'
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active"}]'
mkfix "repos/$R/rulesets/9" '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[{"actor_id":5,"actor_type":"RepositoryRole","bypass_mode":"pull_request"}],"rules":[{"type":"deletion"},{"type":"required_signatures"}]}'

# MUTANT B: the body-builder also drops required_signatures.
MUTB="$WORK/mutant-b.sh"
python3 - "$APPLIER" "$MUTB" <<'MUTPY'
import sys
src, dst = sys.argv[1], sys.argv[2]
s = open(src).read()
old = '| .rules = ((.rules // []) | map(select(.type!="required_status_checks")))'
new = '| .rules = ((.rules // []) | map(select(.type!="required_status_checks" and .type!="required_signatures")))'
open(dst, "w").write(s.replace(old, new, 1) if old in s else s)
MUTPY
if ! diff -q "$APPLIER" "$MUTB" >/dev/null; then
  OUT=$(MUTANT="$MUTB" run_applier "$R" --apply)
  if [ "$(state_of "$OUT")" = "REFUSED" ]; then
    ok "mutant B killed: dropping required_signatures is REFUSED by the exactness guard"
  else
    bad "MUTANT B SURVIVED: required_signatures silently dropped, state=$(state_of "$OUT")"
  fi
else
  bad "mutant B was not applied — the sed pattern no longer matches the applier"
fi

# ================================================================ CASE 4
# Rulesets are ADDITIVE: two active branch rulesets must fail closed.
reset_fix
R=acme/two
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance"}]}'
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active"},{"id":10,"target":"branch","enforcement":"active"}]'
OUT=$(run_applier "$R" --apply)
[ "$(state_of "$OUT")" = "AMBIGUOUS" ] && ok "two active branch rulesets: AMBIGUOUS, fail closed" || bad "two rulesets: state=$(state_of "$OUT") (want AMBIGUOUS)"
[ -s "$FIX/PUTS.log" ] && bad "two rulesets: wrote anyway" || ok "two rulesets: no PUT"

# ================================================================ CASE 5
# No active branch ruleset: report, never create one.
reset_fix
R=acme/none
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance"}]}'
mkfix "repos/$R/rulesets" '[{"id":8,"target":"tag","enforcement":"active"},{"id":7,"target":"branch","enforcement":"disabled"}]'
OUT=$(run_applier "$R" --apply)
[ "$(state_of "$OUT")" = "NORULESET" ] && ok "no active branch ruleset: NORULESET, nothing created" || bad "no ruleset: state=$(state_of "$OUT")"

# ================================================================ CASE 6
# Profile detection: a root-level glob (*.gpr) activates the ada profile.
reset_fix
R=acme/ada
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"},{"name":"ada-ci.yml"}]'
mkfix "repos/$R/contents" '[{"name":"thing.gpr"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/workflows/ada-ci.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":33}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance"}]}'
mkfix "repos/$R/actions/runs/33/jobs?per_page=100" '{"jobs":[{"name":"Ada Build"}]}'
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active"}]'
mkfix "repos/$R/rulesets/9" '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'
OUT=$(run_applier "$R")
case "$(detail_of "$OUT")" in *"Ada Build"*) ok "profile detect: *.gpr glob activated the ada profile" ;; *) bad "profile detect: ada gate missing — $(detail_of "$OUT")" ;; esac

# ================================================================ CASE 7
# Refuse an empty target list rather than report a clean sweep over nothing.
if PATH="$BIN:$PATH" GH_FIX="$FIX" bash "$APPLIER" --gates-file "$WORK/gates.json" --skip-user >/dev/null 2>&1; then
  bad "empty target list: exited 0 instead of refusing"
else
  ok "empty target list: refused"
fi

echo
echo "passed=$pass failed=$fail"
[ "$fail" -eq 0 ]
