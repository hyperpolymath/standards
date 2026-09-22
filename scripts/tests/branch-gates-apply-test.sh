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
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"},{"id":8,"target":"tag","enforcement":"active","source_type":"Repository"}]'
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
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
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
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
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
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
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
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"},{"id":10,"target":"branch","enforcement":"active","source_type":"Repository"}]'
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
mkfix "repos/$R/rulesets" '[{"id":8,"target":"tag","enforcement":"active","source_type":"Repository"},{"id":7,"target":"branch","enforcement":"disabled","source_type":"Repository"}]'
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
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
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


# ================================================================ CASE 8
# THE FAIL-OPEN REFUSAL: a derivation fetch that FAILS must never be read as
# "this workflow contributes no contexts". Measured 2026-09-22 on
# hyperpolymath/standards: a swallowed error dropped 2 of 18 required
# contexts and the run still reported GATED. A short gate is a weak gate,
# and nothing in the output said so.
#
# Fixture shape: governance.yml resolves and yields jobs; codeql.yml resolves
# to run 22 but its JOBS fixture is ABSENT, so the shim exits non-zero --
# exactly a transient API failure.
reset_fix
R=acme/flaky
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"},{"name":"codeql.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/workflows/codeql.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":22}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance"}]}'
# NOTE: no fixture for run 22's jobs -- the shim will exit 1.
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
mkfix "repos/$R/rulesets/9" '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'

OUT=$(run_applier "$R" --apply)
S=$(state_of "$OUT"); D=$(detail_of "$OUT")
[ "$S" = "REFUSED" ] && ok "flaky derive: state is REFUSED" || bad "flaky derive: state=$S (want REFUSED)"
case "$D" in *"derive_failed=["*"codeql.yml"*) ok "flaky derive: the failed workflow is NAMED, not silently absent" ;; *) bad "flaky derive: failure not reported — $D" ;; esac
[ -s "$FIX/PUTS.log" ] && bad "flaky derive: PUT a gate built from an incomplete read" || ok "flaky derive: no PUT even with --apply"

# ---- MUTANT C: delete the incomplete-read refusal. The suite MUST go red. ----
# This is the mutant that matters: without it the applier does not error, it
# writes a SHORTER gate and calls it success.
MUTC="$WORK/mutant-c.sh"
sed 's|if \[ -n "\$DERIVEFAIL" \]; then|if false; then|' "$APPLIER" > "$MUTC"
reset_fix
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"},{"name":"codeql.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/workflows/codeql.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":22}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance"}]}'
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
mkfix "repos/$R/rulesets/9" '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'
OUT=$(MUTANT="$MUTC" run_applier "$R" --apply)
if [ "$(state_of "$OUT")" = "REFUSED" ]; then
  bad "MUTANT C SURVIVED: refusal removed yet still REFUSED — the control is decorative"
else
  ok "mutant C killed: without the refusal it becomes $(state_of "$OUT") and PUTs $(wc -l < "$FIX/PUTS.log") time(s)"
fi
if [ -r "$FIX/LAST_PUT.json" ] &&
   [ "$(jq '[.rules[]|select(.type=="required_status_checks")|.parameters.required_status_checks|length]|add // 0' "$FIX/LAST_PUT.json")" = "1" ]; then
  ok "mutant C wrote the SHORTENED gate (1 context, not 2) — the silent weakening being guarded"
else
  bad "mutant C: expected a 1-context gate from the incomplete read"
fi

# ================================================================ CASE 9
# THE SAME FAIL-OPEN, POINTING THE OTHER WAY: --require-green asked the API
# only for the NON-green jobs, so "this run is entirely green" and "this run
# was not read" were the SAME observation -- zero lines -- and zero lines was
# read as greenness. There it does not shorten the gate; it ADMITS a context
# that was never shown to be green, which is how a red check ends up required
# and every PR blocks on it.
#
# Fixture shape: derive (per_page=1) resolves run 11, which has jobs. The
# greenness probe (per_page=3) additionally resolves run 12, whose jobs
# fixture EXISTS and is EMPTY -- a successful read of nothing, which the
# missing-fixture trick cannot simulate.
reset_fix
R=acme/zerojobs
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=3" '{"workflow_runs":[{"id":11},{"id":12}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance","conclusion":"success"}]}'
mkfix "repos/$R/actions/runs/12/jobs?per_page=100" '{"jobs":[]}'
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
mkfix "repos/$R/rulesets/9" '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'

OUT=$(run_applier "$R" --apply --require-green 3)
S=$(state_of "$OUT"); D=$(detail_of "$OUT")
[ "$S" = "REFUSED" ] && ok "zero-job green probe: state is REFUSED" || bad "zero-job green probe: state=$S (want REFUSED)"
case "$D" in *"returned zero jobs"*) ok "zero-job green probe: the unread run is NAMED, not counted as green" ;; *) bad "zero-job green probe: not reported — $D" ;; esac
[ -s "$FIX/PUTS.log" ] && bad "zero-job green probe: PUT a gate whose greenness was never read" || ok "zero-job green probe: no PUT even with --apply"

# ---- MUTANT D: delete the zero-jobs guard from the GREENNESS probe only. ----
# Keyed on jobs2 so it cannot touch the derivation loop's jobs1 guard.
MUTD="$WORK/mutant-d.sh"
sed 's|if \[ ! -s "\$WORK/jobs2" \]; then|if false; then|' "$APPLIER" > "$MUTD"
if ! cmp -s "$MUTD" "$APPLIER"; then
  reset_fix
  mkfix "repos/$R" '{"default_branch":"main"}'
  mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
  mkfix "repos/$R/contents" '[{"name":"README.md"}]'
  mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
  mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=3" '{"workflow_runs":[{"id":11},{"id":12}]}'
  mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance","conclusion":"success"}]}'
  mkfix "repos/$R/actions/runs/12/jobs?per_page=100" '{"jobs":[]}'
  mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
  mkfix "repos/$R/rulesets/9" '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'
  OUT=$(MUTANT="$MUTD" run_applier "$R" --apply --require-green 3)
  if [ "$(state_of "$OUT")" = "REFUSED" ]; then
    bad "MUTANT D SURVIVED: zero-jobs guard removed yet still REFUSED — the control is decorative"
  else
    ok "mutant D killed: without the guard it becomes $(state_of "$OUT") and PUTs $(wc -l < "$FIX/PUTS.log") time(s)"
  fi
  if [ -r "$FIX/LAST_PUT.json" ] &&
     [ "$(jq -r '[.rules[]|select(.type=="required_status_checks")|.parameters.required_status_checks[].context]|join(",")' "$FIX/LAST_PUT.json")" = "governance / Governance" ]; then
    ok "mutant D REQUIRED a context whose greenness was never read — the silent admission being guarded"
  else
    bad "mutant D: expected the unverified context to be required anyway"
  fi
else
  bad "mutant D was not applied — the sed pattern no longer matches the applier"
fi

# =============================================================== CASE 10
# THE THIRD INSTANCE OF THE SAME CLASS, three lines above CASE 9's.  The
# greenness probe's RUNS query has the identical shape: a query that succeeds
# with {"workflow_runs":[]} leaves rids empty, so the per-run loop never
# executes, nothing is ever appended to "bad", and every context the workflow
# contributed is admitted as green.  Zero runs and zero BAD runs were the same
# observation.  Since the probe now iterates gatewf3 -- the workflows that DID
# contribute contexts, and therefore DID have a run -- an empty runs list here
# can only be a failed read.
reset_fix
R=acme/zeroruns
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=3" '{"workflow_runs":[]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance","conclusion":"success"}]}'
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
mkfix "repos/$R/rulesets/9" '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'

OUT=$(run_applier "$R" --apply --require-green 3)
S=$(state_of "$OUT"); D=$(detail_of "$OUT")
[ "$S" = "REFUSED" ] && ok "zero-run green probe: state is REFUSED" \
  || bad "zero-run green probe: expected REFUSED, got '$S' ($D)"
case "$D" in
  *"green-runs-query-returned-none"*) ok "zero-run green probe: the unread workflow is NAMED, not counted as green" ;;
  *) bad "zero-run green probe: detail does not name the unread workflow: $D" ;;
esac
[ -s "$FIX/PUTS.log" ] && bad "zero-run green probe: PUT a gate whose greenness was never read" \
  || ok "zero-run green probe: no PUT even with --apply"

# =============================================================== CASE 11
# THE REGRESSION THE NAIVE FIX WOULD CAUSE, and the reason the probe iterates
# gatewf3 rather than gatewf2.  gatewf2 still holds every NORUN workflow, for
# which an empty runs list is the LEGITIMATE state, not a failed read.  A bare
# `[ -s rids ]` check over gatewf2 would turn every repo owning one run-less
# gate workflow into REFUSED under --require-green.  Note that NO per_page=3
# fixture exists for the NORUN workflow: the shim exits 1 on a missing fixture,
# so if the probe ever asks about it this case goes REFUSED and fails.
reset_fix
R=acme/norun-beside-good
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"},{"name":"codeql.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=3" '{"workflow_runs":[{"id":11},{"id":12}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance","conclusion":"success"}]}'
mkfix "repos/$R/actions/runs/12/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance","conclusion":"success"}]}'
mkfix "repos/$R/actions/workflows/codeql.yml/runs?branch=main&per_page=1" '{"workflow_runs":[]}'
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
mkfix "repos/$R/rulesets/9" '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'

OUT=$(run_applier "$R" --require-green 3)
S=$(state_of "$OUT"); D=$(detail_of "$OUT")
[ "$S" != "REFUSED" ] && ok "NORUN beside a good workflow: not REFUSED (state=$S) — the run-less workflow is not a failed read" \
  || bad "NORUN beside a good workflow: REFUSED ($D) — the rids guard is scoped to gatewf2, not gatewf3"
case "$D" in
  *codeql.yml*) ok "NORUN beside a good workflow: the run-less workflow is reported by name" ;;
  *) bad "NORUN beside a good workflow: detail does not name codeql.yml: $D" ;;
esac

# ---- MUTANT E: delete the empty-runs guard from the greenness probe only. ----
# Keyed on rids so it cannot touch either jobs guard.
MUTE="$WORK/mutant-e.sh"
sed 's|if \[ ! -s "\$WORK/rids" \]; then|if false; then|' "$APPLIER" > "$MUTE"
chmod +x "$MUTE"
if ! cmp -s "$MUTE" "$APPLIER"; then
  reset_fix
  R=acme/zeroruns
  mkfix "repos/$R" '{"default_branch":"main"}'
  mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
  mkfix "repos/$R/contents" '[{"name":"README.md"}]'
  mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
  mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=3" '{"workflow_runs":[]}'
  mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance","conclusion":"success"}]}'
  mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
  mkfix "repos/$R/rulesets/9" '{"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'
  OUT=$(MUTANT="$MUTE" run_applier "$R" --apply --require-green 3)
  if [ "$(state_of "$OUT")" = "REFUSED" ]; then
    bad "MUTANT E SURVIVED: empty-runs guard removed yet still REFUSED — the control is decorative"
  else
    ok "mutant E killed: without the guard it becomes $(state_of "$OUT") and PUTs $(wc -l < "$FIX/PUTS.log") time(s)"
  fi
  if [ -r "$FIX/LAST_PUT.json" ] &&
     [ "$(jq -r '[.rules[]|select(.type=="required_status_checks")|.parameters.required_status_checks[].context]|join(",")' "$FIX/LAST_PUT.json")" = "governance / Governance" ]; then
    ok "mutant E REQUIRED a context across ZERO examined runs — the silent admission being guarded"
  else
    bad "mutant E: expected the unverified context to be required anyway"
  fi
else
  bad "mutant E was not applied — the sed pattern no longer matches the applier"
fi

# =============================================================== CASE 12
# THE ORG-INHERITED TRAP.  repos/{r}/rulesets RETURNS the org's rulesets
# alongside the repo's own.  An inherited one reads back IN FULL at
# repos/{r}/rulesets/{id} -- so every GET succeeds and nothing warns you --
# while the PUT to that same path 404s.  Measured 67 times on this estate, once
# per metadatastician repo reached by the org-level EstateBranching (18225024).
# The cure is at /orgs/{org}/rulesets/{id}, applied ONCE; issuing 67 doomed
# per-repo writes is not a smaller version of it.
# NOTE there is deliberately NO fixture for repos/$R/rulesets/18225024: the shim
# exits 1 on a missing fixture, so if the applier ever READS the inherited
# ruleset this case fails.  That absence is a free assertion.
reset_fix
R=acme/org-inherited
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance"}]}'
mkfix "repos/$R/rulesets" '[{"id":18225024,"name":"EstateBranching","target":"branch","enforcement":"active","source_type":"Organization","source":"metadatastician"}]'

OUT=$(run_applier "$R" --apply)
S=$(state_of "$OUT"); D=$(detail_of "$OUT")
[ "$S" = "ORG-INHERITED" ] && ok "org-inherited: state is ORG-INHERITED" || bad "org-inherited: state=$S (want ORG-INHERITED) — an inherited ruleset was treated as writable"
case "$D" in *18225024*) ok "org-inherited: the inherited ruleset id is NAMED so the org-level cure is actionable" ;; *) bad "org-inherited: id not reported — $D" ;; esac
case "$D" in *"/orgs/"*) ok "org-inherited: the detail says WHERE the cure lives" ;; *) bad "org-inherited: detail does not point at the org endpoint — $D" ;; esac
[ -s "$FIX/PUTS.log" ] && bad "org-inherited: PUT issued despite --apply — this is the 404 being guarded" || ok "org-inherited: no PUT even with --apply"

# =============================================================== CASE 13
# THE REGRESSION THE NAIVE FIX WOULD CAUSE.  Rulesets are ADDITIVE, so a repo
# legitimately carries an inherited ruleset BESIDE its own.  Counting both as
# candidates turns that ordinary repo into AMBIGUOUS and it never gets gated.
# The repo-level one must be selected, and the inherited one REPORTED (it still
# enforces) rather than silently ignored.
# Again: no fixture for rulesets/18225024 -- reading it fails the case.
reset_fix
R=acme/org-plus-own
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance"}]}'
mkfix "repos/$R/rulesets" '[{"id":18225024,"name":"EstateBranching","target":"branch","enforcement":"active","source_type":"Organization"},{"id":9,"target":"branch","enforcement":"active","source_type":"Repository"}]'
mkfix "repos/$R/rulesets/9" '{"id":9,"name":"Base","target":"branch","enforcement":"active","conditions":{"ref_name":{"include":["~DEFAULT_BRANCH"],"exclude":[]}},"bypass_actors":[],"rules":[{"type":"deletion"}]}'

OUT=$(run_applier "$R")
S=$(state_of "$OUT"); D=$(detail_of "$OUT")
[ "$S" = "WOULD-GATE" ] && ok "org beside own: state is WOULD-GATE — the repo-level ruleset was selected" || bad "org beside own: state=$S (want WOULD-GATE) — an additive inherited ruleset made an ordinary repo unreachable"
case "$D" in *"ruleset=9"*) ok "org beside own: the REPO-LEVEL id was chosen, not the org id" ;; *) bad "org beside own: wrong ruleset selected — $D" ;; esac
case "$D" in *"org_inherited=[18225024]"*) ok "org beside own: the inherited ruleset is still REPORTED — it enforces regardless" ;; *) bad "org beside own: inherited ruleset went unreported — $D" ;; esac

# =============================================================== CASE 14
# AN ABSENT DISCRIMINATOR REFUSES.  Writability is exactly what .source_type
# decides; defaulting a missing field to the writable arm is a silent 404.
reset_fix
R=acme/no-source-type
mkfix "repos/$R" '{"default_branch":"main"}'
mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
mkfix "repos/$R/contents" '[{"name":"README.md"}]'
mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance"}]}'
mkfix "repos/$R/rulesets" '[{"id":9,"target":"branch","enforcement":"active"}]'
mkfix "repos/$R/rulesets/9" '{"id":9,"name":"Base","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'

OUT=$(run_applier "$R" --apply)
S=$(state_of "$OUT")
[ "$S" = "UNKNOWN" ] && ok "absent source_type: state is UNKNOWN — refuses rather than assuming repo-level" || bad "absent source_type: state=$S (want UNKNOWN)"
[ -s "$FIX/PUTS.log" ] && bad "absent source_type: PUT issued on an unclassifiable ruleset" || ok "absent source_type: no PUT even with --apply"

# ---- MUTANT F: delete the source_type filter, restoring the original defect. --
# The pre-fix line selected every active branch ruleset regardless of ownership.
MUTF="$WORK/mutant-f.sh"
sed 's|^  command grep -P .\^Repository.*> "\$WORK/ids"$|  cut -f2 "$WORK/active" > "$WORK/ids"|' "$APPLIER" > "$MUTF"
chmod +x "$MUTF"
if ! cmp -s "$MUTF" "$APPLIER" && bash -n "$MUTF" 2>/dev/null; then
  reset_fix
  R=acme/org-inherited
  mkfix "repos/$R" '{"default_branch":"main"}'
  mkfix "repos/$R/contents/.github/workflows" '[{"name":"governance.yml"}]'
  mkfix "repos/$R/contents" '[{"name":"README.md"}]'
  mkfix "repos/$R/actions/workflows/governance.yml/runs?branch=main&per_page=1" '{"workflow_runs":[{"id":11}]}'
  mkfix "repos/$R/actions/runs/11/jobs?per_page=100" '{"jobs":[{"name":"governance / Governance"}]}'
  mkfix "repos/$R/rulesets" '[{"id":18225024,"name":"EstateBranching","target":"branch","enforcement":"active","source_type":"Organization"}]'
  # the mutant WILL read the inherited ruleset, so it needs a fixture the fixed
  # applier must never ask for.
  mkfix "repos/$R/rulesets/18225024" '{"id":18225024,"name":"EstateBranching","target":"branch","enforcement":"active","conditions":{},"bypass_actors":[],"rules":[{"type":"deletion"}]}'
  OUT=$(MUTANT="$MUTF" run_applier "$R" --apply)
  if [ "$(state_of "$OUT")" = "ORG-INHERITED" ]; then
    bad "MUTANT F SURVIVED: source_type filter removed yet still ORG-INHERITED — the control is decorative"
  else
    ok "mutant F killed: without the filter it becomes $(state_of "$OUT") and PUTs $(wc -l < "$FIX/PUTS.log") time(s)"
  fi
  if command grep -qxF "repos/$R/rulesets/18225024" "$FIX/PUTS.log" 2>/dev/null; then
    ok "mutant F PUT to the INHERITED ruleset's repo path — the 404 measured 67 times, reproduced"
  else
    bad "mutant F: expected a PUT to repos/$R/rulesets/18225024"
  fi
else
  bad "mutant F was not applied — the sed pattern no longer matches the applier"
fi

echo
echo "passed=$pass failed=$fail"
[ "$fail" -eq 0 ]
