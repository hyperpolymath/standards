#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Regression suite for scripts/apply-protection-floor.sh
#
# House conventions, shared with scripts/tests/branch-gates-apply-test.sh:
#   * a `gh` shim maps an API path to a fixture by KEY=$(tr '/?&=' '____')
#   * A MISSING FIXTURE IS A FREE ASSERTION that the path is never queried: the shim
#     exits 1, so any code reaching for an unplanned endpoint fails loudly.
#   * every write is appended to $GH_FIX/PUTS.log, so "wrote nothing" is checkable.
#
# The suite ends by KILLING FOUR MUTANTS. A green suite against the real script proves
# only that it agrees with itself; each mutant reintroduces one specific defect and the
# suite must go red for exactly the right reason.
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SUT="$ROOT/scripts/apply-protection-floor.sh"
[ -r "$SUT" ] || { echo "FATAL: script under test missing: $SUT"; exit 2; }

PASS=0; FAIL=0
# Record a passing assertion and print its description.
ok()   { PASS=$((PASS+1)); printf '  ok   %s\n' "$1"; }
# Record a failing assertion and print its expected and actual values.
bad()  { FAIL=$((FAIL+1)); printf '  FAIL %s\n     expected: %s\n     actual:   %s\n' "$1" "$2" "$3"; }
# Compare expected and actual values, then record the assertion result.
check(){ if [ "$2" = "$3" ]; then ok "$1"; else bad "$1" "$2" "$3"; fi; }

WORK="$(mktemp -d -t protfloor-test.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT
BIN="$WORK/bin"; FIX="$WORK/fix"; mkdir -p "$BIN" "$FIX"

cat > "$BIN/gh" <<'SHIM'
#!/usr/bin/env bash
# fixture shim: repos/x/y -> $GH_FIX/repos_x_y
method="GET"; path=""; stdin_body=""
while [ $# -gt 0 ]; do
  case "$1" in
    api)        ;;
    --method)   method="$2"; shift ;;
    --input)    stdin_body="$(cat)"; shift ;;
    -*)         ;;
    *)          [ -z "$path" ] && path="$1" ;;
  esac
  shift
done
KEY="$(printf '%s' "$path" | tr '/?&=' '____')"
if [ "$method" != "GET" ]; then
  printf '%s\t%s\t%s\n' "$method" "$path" "$stdin_body" >> "$GH_FIX/PUTS.log"
  F="$GH_FIX/${method}_${KEY}"
  [ -r "$F" ] || { echo "no fixture for $method $path" >&2; exit 1; }
  cat "$F"; exit 0
fi
F="$GH_FIX/$KEY"
[ -r "$F" ] || { echo "no fixture for $path" >&2; exit 1; }
if [ -r "$F.rc" ]; then cat "$F" >&2; exit "$(cat "$F.rc")"; fi
cat "$F"
SHIM
chmod 755 "$BIN/gh"

# Run a script with the fixture-backed GitHub CLI shim.
run() { GH_FIX="$FIX" PATH="$BIN:$PATH" bash "$1" "${@:2}" 2>/dev/null; }
# Extract one repository's state from tab-separated report output.
state() { printf '%s\n' "$1" | awk -F'\t' -v r="$2" '$1==r{print $2}'; }
# Recreate the fixture directory and initialize an empty write log.
reset_fix() { rm -rf "$FIX"; mkdir -p "$FIX"; : > "$FIX/PUTS.log"; }

# Write repository metadata from a name, default branch, and archived flag.
mkrepo() { # name  default-branch  archived
  printf '{"archived":%s,"default_branch":"%s"}\n' "$3" "$2" > "$FIX/repos_$(printf '%s' "$1" | tr '/' '_')"
}

REPOS="$WORK/repos.txt"
# A SECOND target list for the backoff section. The applier sorts its targets, so the
# three throttled repos are named to sort FIRST and plain-repo to sort after them:
# the assertion is that the sweep never reaches it.
THR="$WORK/throttle-repos.txt"
cat > "$THR" <<'EOF'
hyperpolymath/aaa-throttle-1
hyperpolymath/aaa-throttle-2
hyperpolymath/aaa-throttle-3
hyperpolymath/plain-repo
EOF
cat > "$REPOS" <<'EOF'
hyperpolymath/memory-vault
hyperpolymath/plain-repo
hyperpolymath/converged-repo
hyperpolymath/richer-repo
hyperpolymath/bypassed-twin
hyperpolymath/archived-repo
hyperpolymath/private-repo
hyperpolymath/throttled-repo
hyperpolymath/secondary-throttled-repo
hyperpolymath/nosourcetype-repo
metadatastician/org-covered-repo
metadatastician/org-halffloor-repo
EOF

# Rebuild fixtures, optionally making excluded repositories fully writable.
build_fixtures() { # $1 = "with-vault" to give the vault a complete, writable fixture set
  reset_fix
  # By default memory-vault gets NO fixtures at all. The D50 guard must return before any
  # read, so every missing fixture here asserts that nothing was queried.
  # With "with-vault" the vault is made fully writable, so a mutant that removes the guard
  # produces a REAL POST rather than dying on a missing fixture -- otherwise the mutant's
  # red would measure the shim, not the guard.
  if [ "${1:-}" = "with-vault" ]; then
    mkrepo hyperpolymath/memory-vault main false
    echo '[]' > "$FIX/repos_hyperpolymath_memory-vault_rulesets"
    echo '{"id":9099}' > "$FIX/POST_repos_hyperpolymath_memory-vault_rulesets"
    echo '[{"type":"deletion"},{"type":"non_fast_forward"}]' \
        > "$FIX/repos_hyperpolymath_memory-vault_rules_branches_main"
  fi

  mkrepo hyperpolymath/plain-repo      main  false
  echo '[]' > "$FIX/repos_hyperpolymath_plain-repo_rulesets"
  echo '{"id":9001}' > "$FIX/POST_repos_hyperpolymath_plain-repo_rulesets"
  echo '[{"type":"deletion"},{"type":"non_fast_forward"}]' \
      > "$FIX/repos_hyperpolymath_plain-repo_rules_branches_main"

  mkrepo hyperpolymath/converged-repo  main  false
  echo '[{"id":10,"source_type":"Repository","target":"branch","enforcement":"active"}]' \
      > "$FIX/repos_hyperpolymath_converged-repo_rulesets"
  cat > "$FIX/repos_hyperpolymath_converged-repo_rulesets_10" <<'J'
{"id":10,"rules":[{"type":"deletion"},{"type":"non_fast_forward"}],
 "conditions":{"ref_name":{"include":["~DEFAULT_BRANCH"],"exclude":[]}},"bypass_actors":[]}
J

  mkrepo hyperpolymath/richer-repo     main  false
  echo '[{"id":20,"source_type":"Repository","target":"branch","enforcement":"active"}]' \
      > "$FIX/repos_hyperpolymath_richer-repo_rulesets"
  cat > "$FIX/repos_hyperpolymath_richer-repo_rulesets_20" <<'J'
{"id":20,"rules":[{"type":"deletion"},{"type":"non_fast_forward"},{"type":"required_signatures"}],
 "conditions":{"ref_name":{"include":["~DEFAULT_BRANCH"],"exclude":[]}},"bypass_actors":[]}
J

  # same rule set and same include as the floor, but WITH a bypass actor.
  mkrepo hyperpolymath/bypassed-twin   main  false
  echo '[{"id":30,"source_type":"Repository","target":"branch","enforcement":"active"}]' \
      > "$FIX/repos_hyperpolymath_bypassed-twin_rulesets"
  cat > "$FIX/repos_hyperpolymath_bypassed-twin_rulesets_30" <<'J'
{"id":30,"rules":[{"type":"deletion"},{"type":"non_fast_forward"}],
 "conditions":{"ref_name":{"include":["~DEFAULT_BRANCH"],"exclude":[]}},
 "bypass_actors":[{"actor_id":5,"actor_type":"RepositoryRole","bypass_mode":"always"}]}
J

  mkrepo hyperpolymath/archived-repo   main  true
  # By default NO rulesets fixture: the archived guard must return before any such read.
  # In mutant mode the repo is made fully writable, so a mutant that removes the guard
  # produces a REAL POST instead of dying on a missing fixture.
  if [ "${1:-}" = "with-vault" ]; then
    echo '[]' > "$FIX/repos_hyperpolymath_archived-repo_rulesets"
    echo '{"id":9098}' > "$FIX/POST_repos_hyperpolymath_archived-repo_rulesets"
    echo '[{"type":"deletion"},{"type":"non_fast_forward"}]' \
        > "$FIX/repos_hyperpolymath_archived-repo_rules_branches_main"
  fi

  mkrepo hyperpolymath/private-repo    main  false
  printf 'HTTP 403: Upgrade to GitHub Pro or make this repository public\n' \
      > "$FIX/repos_hyperpolymath_private-repo_rulesets"
  echo 1 > "$FIX/repos_hyperpolymath_private-repo_rulesets.rc"

  # A THROTTLE ALSO ANSWERS 403, with the SAME status as the plan refusal above.
  # These two exist so the discriminator cannot go back to matching *403* alone:
  # that recorded a throttled repo as PLAN-EXCLUDED and under-reported the gap.
  mkrepo hyperpolymath/throttled-repo   main  false
  printf 'HTTP 403: API rate limit exceeded for user ID 12345. (https://api.github.com/repos/hyperpolymath/throttled-repo/rulesets)\n' \
      > "$FIX/repos_hyperpolymath_throttled-repo_rulesets"
  echo 1 > "$FIX/repos_hyperpolymath_throttled-repo_rulesets.rc"

  mkrepo hyperpolymath/secondary-throttled-repo main false
  printf 'HTTP 403: You have exceeded a secondary rate limit. Please wait a few minutes before you try again.\n' \
      > "$FIX/repos_hyperpolymath_secondary-throttled-repo_rulesets"
  echo 1 > "$FIX/repos_hyperpolymath_secondary-throttled-repo_rulesets.rc"

  # The wall shows on the FIRST read of a repo, before any ruleset endpoint is touched.
  mkrepo hyperpolymath/aaa-throttle-1 main false
  mkrepo hyperpolymath/aaa-throttle-2 main false
  mkrepo hyperpolymath/aaa-throttle-3 main false
  printf 'HTTP 403: API rate limit exceeded for user ID 12345.\n' > "$FIX/repos_hyperpolymath_aaa-throttle-1"
  printf 'HTTP 403: API rate limit exceeded for user ID 12345.\n' > "$FIX/repos_hyperpolymath_aaa-throttle-2"
  printf 'HTTP 403: API rate limit exceeded for user ID 12345.\n' > "$FIX/repos_hyperpolymath_aaa-throttle-3"
  echo 1 > "$FIX/repos_hyperpolymath_aaa-throttle-1.rc"
  echo 1 > "$FIX/repos_hyperpolymath_aaa-throttle-2.rc"
  echo 1 > "$FIX/repos_hyperpolymath_aaa-throttle-3.rc"

  mkrepo hyperpolymath/nosourcetype-repo main false
  echo '[{"id":40,"target":"branch","enforcement":"active"}]' \
      > "$FIX/repos_hyperpolymath_nosourcetype-repo_rulesets"

  # An ORG-inherited ruleset that CARRIES the whole floor. It is not writable per repo,
  # so the only correct answer is ORG-INHERITED -- never a per-repo duplicate.
  mkrepo metadatastician/org-covered-repo main false
  echo '[{"id":60,"source_type":"Organization","target":"branch","enforcement":"active"}]' \
      > "$FIX/repos_metadatastician_org-covered-repo_rulesets"
  cat > "$FIX/repos_metadatastician_org-covered-repo_rulesets_60" <<'J'
{"id":60,"rules":[{"type":"deletion"},{"type":"non_fast_forward"},{"type":"required_signatures"}],
 "conditions":{"ref_name":{"include":["~DEFAULT_BRANCH"],"exclude":[]}},"bypass_actors":[]}
J
  # Writable on purpose: a mutant that drops the org union must produce a REAL duplicate
  # POST here, not die on a missing fixture (the false-green trap this suite already hit).
  echo '{"id":9060}' > "$FIX/POST_repos_metadatastician_org-covered-repo_rulesets"
  echo '[{"type":"deletion"},{"type":"non_fast_forward"}]' \
      > "$FIX/repos_metadatastician_org-covered-repo_rules_branches_main"

  # The real EstateBranching shape: an org ruleset carrying HALF the floor (deletion, no
  # non_fast_forward). A half cover is NOT a cover; this repo must still be WOULD-CREATE.
  mkrepo metadatastician/org-halffloor-repo main false
  echo '[{"id":61,"source_type":"Organization","target":"branch","enforcement":"active"}]' \
      > "$FIX/repos_metadatastician_org-halffloor-repo_rulesets"
  cat > "$FIX/repos_metadatastician_org-halffloor-repo_rulesets_61" <<'J'
{"id":61,"rules":[{"type":"deletion"},{"type":"pull_request"}],
 "conditions":{"ref_name":{"include":["~DEFAULT_BRANCH"],"exclude":[]}},"bypass_actors":[{"actor_id":1}]}
J
  echo '{"id":9061}' > "$FIX/POST_repos_metadatastician_org-halffloor-repo_rulesets"
  echo '[{"type":"deletion"},{"type":"non_fast_forward"}]' \
      > "$FIX/repos_metadatastician_org-halffloor-repo_rules_branches_main"
}

echo "== report mode (no --apply) =="
build_fixtures
OUT="$(run "$SUT" --repos "$REPOS")"
check "vault is EXCLUDED-D50"            "EXCLUDED-D50"      "$(state "$OUT" hyperpolymath/memory-vault)"
check "bare repo is WOULD-CREATE"        "WOULD-CREATE"      "$(state "$OUT" hyperpolymath/plain-repo)"
check "exact floor is CONVERGED"         "CONVERGED"         "$(state "$OUT" hyperpolymath/converged-repo)"
check "richer cover is COVERED-BY-RICHER" "COVERED-BY-RICHER" "$(state "$OUT" hyperpolymath/richer-repo)"
check "bypassed twin is NOT converged"   "COVERED-BY-RICHER" "$(state "$OUT" hyperpolymath/bypassed-twin)"
check "archived is ARCHIVED"             "ARCHIVED"          "$(state "$OUT" hyperpolymath/archived-repo)"
check "403 is PLAN-EXCLUDED"             "PLAN-EXCLUDED"     "$(state "$OUT" hyperpolymath/private-repo)"
# The plan arm above is the NEGATIVE CONTROL: it proves the throttle arm below did not
# simply swallow every 403. A throttled read is UNKNOWN -- skipped, never recorded.
check "rate-limit 403 is UNKNOWN"       "UNKNOWN"           "$(state "$OUT" hyperpolymath/throttled-repo)"
check "secondary-limit 403 is UNKNOWN"  "UNKNOWN"           "$(state "$OUT" hyperpolymath/secondary-throttled-repo)"
check "no source_type is REFUSED"        "REFUSED"           "$(state "$OUT" hyperpolymath/nosourcetype-repo)"
check "complete org cover is ORG-INHERITED" "ORG-INHERITED"   "$(state "$OUT" metadatastician/org-covered-repo)"
check "HALF org cover is not a cover"    "WOULD-CREATE"      "$(state "$OUT" metadatastician/org-halffloor-repo)"
check "report mode writes nothing"       "0"                 "$(wc -l < "$FIX/PUTS.log" | tr -d ' ')"

echo "== apply mode =="
build_fixtures
OUT="$(run "$SUT" --repos "$REPOS" --apply)"
check "bare repo is CREATED"             "CREATED"           "$(state "$OUT" hyperpolymath/plain-repo)"
check "half org cover still CREATED"     "CREATED"           "$(state "$OUT" metadatastician/org-halffloor-repo)"
# Exactly two repos lack a floor in force: the bare one and the HALF-org-covered one.
# Every other fixture must be left alone, so the count is an assertion in both directions.
check "exactly two writes"               "2"                 "$(wc -l < "$FIX/PUTS.log" | tr -d ' ')"
check "every write is a POST"            "POST"              "$(cut -f1 "$FIX/PUTS.log" | sort -u)"
check "one write targets plain-repo"     "1"                 "$(command grep -c 'repos/hyperpolymath/plain-repo/rulesets' "$FIX/PUTS.log")"
check "one write targets half-org repo"  "1"                 "$(command grep -c 'repos/metadatastician/org-halffloor-repo/rulesets' "$FIX/PUTS.log")"
POSTED="$(command grep 'plain-repo' "$FIX/PUTS.log" | cut -f3 | jq -S -c .)"
CANONJ="$(jq -S -c . "$ROOT/config/rulesets/branch-floor.json")"
check "POST body equals the canon file"  "$CANONJ"           "$POSTED"
check "posted bypass_actors is empty"    "0"                 "$(printf '%s' "$POSTED" | jq '.bypass_actors | length')"
check "no write touched the vault"       "0"                 "$(command grep -c 'memory-vault' "$FIX/PUTS.log")"
check "no write touched the archived"    "0"                 "$(command grep -c 'archived-repo' "$FIX/PUTS.log")"

echo "== throttle backoff =="
# A throttle is not a per-repo property. Three consecutive throttled reads mean the
# window is spent, so the sweep must ABORT: grinding on filed 267 repos as UNKNOWN twice
# on 2026-09-23, which READS as \"measured and unknowable\" when it means \"never looked\".
build_fixtures
OUTT="$(run "$SUT" --repos "$THR" --apply)"
check "third consecutive throttle aborts"        "ABORTED"  "$(state "$OUTT" -)"
check "the repo beyond the wall is not reported" ""         "$(state "$OUTT" hyperpolymath/plain-repo)"
check "an aborted sweep writes nothing"          "0"        "$(wc -l < "$FIX/PUTS.log" | tr -d ' ')"

echo "== refusals =="
build_fixtures
OUT2="$(GH_FIX="$FIX" PATH="$BIN:$PATH" bash "$SUT" --repos /dev/null 2>&1)"
case "$OUT2" in *"clean sweep over nothing"*) ok "empty repo list is refused";;
  *) bad "empty repo list is refused" "refusal" "$OUT2";; esac

MISSING="$WORK/noclass"; mkdir -p "$MISSING/scripts" "$MISSING/config/rulesets"
cp "$SUT" "$MISSING/scripts/"
cp "$ROOT/config/rulesets/branch-floor.json" "$ROOT/config/rulesets/tag-floor.json" "$MISSING/config/rulesets/"
# deliberately do NOT copy gcrypt-vault-class.txt
OUT3="$(GH_FIX="$FIX" PATH="$BIN:$PATH" bash "$MISSING/scripts/apply-protection-floor.sh" --repos "$REPOS" 2>&1)"
case "$OUT3" in *"class file missing"*) ok "missing vault class file is a refusal";;
  *) bad "missing vault class file is a refusal" "refusal" "$OUT3";; esac

echo "== mutants (each MUST make the suite go red, for the RIGHT reason) =="
# 🪤 A mutant written to a temp dir resolves REPO_ROOT to that dir, cannot find the canon
# file, and dies with FATAL before ANY guard runs -- so all four "reds" would measure a
# broken path rather than the defect. The mutant therefore lives in the real scripts/ dir,
# and every mutant run is asserted to have produced real output first.
MUTROOT="$WORK/mutroot"; mkdir -p "$MUTROOT/scripts" "$MUTROOT/config/rulesets"
cp "$ROOT"/config/rulesets/*.json "$ROOT/config/rulesets/gcrypt-vault-class.txt" "$MUTROOT/config/rulesets/"
MUT="$MUTROOT/scripts/apply-protection-floor.sh"

# Run a named mutation and assert the expected state change or write.
mutant() { # name  sed-expr  assertion-kind(wrote|state)  arg  [repos-file]
  local name="$1" expr="$2" kind="$3" arg="$4" repos="${5:-$REPOS}" o got
  sed "$expr" "$SUT" > "$MUT"
  if ! bash -n "$MUT" 2>/dev/null; then
    bad "mutant '$name'" "parses" "parse error -- red would measure the parser"; return
  fi
  if cmp -s "$MUT" "$SUT"; then
    bad "mutant '$name'" "sed changes the script" "sed matched nothing -- the mutant is the original"; return
  fi
  build_fixtures with-vault
  o="$(GH_FIX="$FIX" PATH="$BIN:$PATH" bash "$MUT" --repos "$repos" --apply 2>/dev/null)"
  # the mutant must still RUN; a FATAL would make every check vacuous
  if [ "$(printf '%s\n' "$o" | wc -l)" -lt 3 ]; then
    bad "mutant '$name'" "runs and reports" "produced no report -- it died early, red is meaningless"; return
  fi
  case "$kind" in
    wrote)  got="$(command grep -c "$arg" "$FIX/PUTS.log")"
            if [ "$got" -gt 0 ]; then ok "mutant '$name' dies (now POSTs to $arg)"
            else bad "mutant '$name' DIES" "a POST to $arg" "no such write -- MUTANT SURVIVED"; fi ;;
    state)  got="$(state "$o" "${arg%%=*}")"
            if [ "$got" = "${arg#*=}" ]; then ok "mutant '$name' dies (${arg%%=*} -> $got)"
            else bad "mutant '$name' DIES" "${arg#*=}" "$got -- MUTANT SURVIVED"; fi ;;
  esac
}

# Baseline: with the vault fully writable, the REAL script must still write nothing to it.
build_fixtures with-vault
OUTV="$(run "$SUT" --repos "$REPOS" --apply)"
check "vault stays EXCLUDED even when writable" "EXCLUDED-D50" "$(state "$OUTV" hyperpolymath/memory-vault)"
check "vault receives no POST when writable"    "0"            "$(command grep -c 'memory-vault' "$FIX/PUTS.log")"
check "archived stays ARCHIVED when writable"  "ARCHIVED"     "$(state "$OUTV" hyperpolymath/archived-repo)"
check "archived receives no POST when writable" "0"           "$(command grep -c 'archived-repo' "$FIX/PUTS.log")"
check "org-covered stays ORG-INHERITED"        "ORG-INHERITED" "$(state "$OUTV" metadatastician/org-covered-repo)"
check "org-covered receives no duplicate POST" "0"             "$(command grep -c 'org-covered-repo' "$FIX/PUTS.log")"

mutant "D50 vault guard removed" \
  's/^  if is_vault "\$repo"; then$/  if false; then/' \
  wrote "memory-vault"

# The pending-fix defect: any 403 mapped to PLAN-EXCLUDED, so a rate-limit refusal was
# filed as "private repo / plan limit". It writes nothing either way, so the tell is the
# STATE, not a POST -- a mutant that silences the throttle arm must flip it back.
mutant "throttle arm removed from the 403 split" \
  's/^    if is_throttled "\$err"; then$/    if false; then/' \
  state "hyperpolymath/throttled-repo=PLAN-EXCLUDED"

mutant "archived guard removed" \
  's/^  if \[ "\$(printf .%s. "\$meta" | jq -r ..archived.)" = "true" \]; then$/  if false; then/' \
  wrote "archived-repo"

mutant "bypass dropped from the shape test" \
  's/ \&\& \[ "\$byp" = "0" \]//' \
  state "hyperpolymath/bypassed-twin=CONVERGED"

# Removing the converged early-return does NOT reach a write: the covered-by-richer check
# catches it next. That second line of defence is the point, so this mutant is asserted on
# the STATE it corrupts, not on a POST that correctly never happens.
# The bug this suite was extended for: org rulesets were COUNTED (org_n) but their rule
# types never entered the cover set, so ORG-INHERITED was unreachable and 67 org-covered
# repos reported WOULD-CREATE. Under --apply that is 67 duplicate rulesets.
mutant "org cover dropped from the union" \
  's/",\$union,\$union_org,"/",$union,"/' \
  wrote "org-covered-repo"

mutant "converged early-return removed" \
  's/^  if \[ "\$exact_n" -eq 1 \]; then$/  if false; then/' \
  state "hyperpolymath/converged-repo=COVERED-BY-RICHER"

# Without the abort the sweep grinds through the whole list against a spent window, so the
# repo beyond the wall is REACHED and written -- the 267-UNKNOWN failure, in miniature.
mutant "throttle backoff removed" \
  's/^    exit 3$/    throttled=0/' \
  wrote "plain-repo" "$THR"

printf '\n%d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
