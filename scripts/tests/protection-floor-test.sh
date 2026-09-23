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
ok()   { PASS=$((PASS+1)); printf '  ok   %s\n' "$1"; }
bad()  { FAIL=$((FAIL+1)); printf '  FAIL %s\n     expected: %s\n     actual:   %s\n' "$1" "$2" "$3"; }
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

run() { GH_FIX="$FIX" PATH="$BIN:$PATH" bash "$1" "${@:2}" 2>/dev/null; }
state() { printf '%s\n' "$1" | awk -F'\t' -v r="$2" '$1==r{print $2}'; }
reset_fix() { rm -rf "$FIX"; mkdir -p "$FIX"; : > "$FIX/PUTS.log"; }

mkrepo() { # name default archived
  printf '{"archived":%s,"default_branch":"%s"}\n' "$3" "$2" > "$FIX/repos_$(printf '%s' "$1" | tr '/' '_')"
}

REPOS="$WORK/repos.txt"
cat > "$REPOS" <<'EOF'
hyperpolymath/memory-vault
hyperpolymath/plain-repo
hyperpolymath/converged-repo
hyperpolymath/richer-repo
hyperpolymath/bypassed-twin
hyperpolymath/archived-repo
hyperpolymath/private-repo
hyperpolymath/nosourcetype-repo
EOF

build_fixtures() {  # $1 = "with-vault" to give the vault a complete, writable fixture set
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

  mkrepo hyperpolymath/nosourcetype-repo main false
  echo '[{"id":40,"target":"branch","enforcement":"active"}]' \
      > "$FIX/repos_hyperpolymath_nosourcetype-repo_rulesets"
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
check "no source_type is REFUSED"        "REFUSED"           "$(state "$OUT" hyperpolymath/nosourcetype-repo)"
check "report mode writes nothing"       "0"                 "$(wc -l < "$FIX/PUTS.log" | tr -d ' ')"

echo "== apply mode =="
build_fixtures
OUT="$(run "$SUT" --repos "$REPOS" --apply)"
check "bare repo is CREATED"             "CREATED"           "$(state "$OUT" hyperpolymath/plain-repo)"
check "exactly one write"                "1"                 "$(wc -l < "$FIX/PUTS.log" | tr -d ' ')"
check "the write is a POST"              "POST"              "$(cut -f1 "$FIX/PUTS.log")"
check "the write targets plain-repo"     "repos/hyperpolymath/plain-repo/rulesets" "$(cut -f2 "$FIX/PUTS.log")"
POSTED="$(cut -f3 "$FIX/PUTS.log" | jq -S -c .)"
CANONJ="$(jq -S -c . "$ROOT/config/rulesets/branch-floor.json")"
check "POST body equals the canon file"  "$CANONJ"           "$POSTED"
check "posted bypass_actors is empty"    "0"                 "$(printf '%s' "$POSTED" | jq '.bypass_actors | length')"
check "no write touched the vault"       "0"                 "$(command grep -c 'memory-vault' "$FIX/PUTS.log")"
check "no write touched the archived"    "0"                 "$(command grep -c 'archived-repo' "$FIX/PUTS.log")"

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
MUT="$ROOT/scripts/.protection-floor-mutant.tmp.sh"
trap 'rm -rf "$WORK"; rm -f "$MUT"' EXIT

mutant() { # name  sed-expr  assertion-kind  arg
  local name="$1" expr="$2" kind="$3" arg="$4" o got
  sed "$expr" "$SUT" > "$MUT"
  if ! bash -n "$MUT" 2>/dev/null; then
    bad "mutant '$name'" "parses" "parse error -- red would measure the parser"; return
  fi
  if cmp -s "$MUT" "$SUT"; then
    bad "mutant '$name'" "sed changes the script" "sed matched nothing -- the mutant is the original"; return
  fi
  build_fixtures with-vault
  o="$(GH_FIX="$FIX" PATH="$BIN:$PATH" bash "$MUT" --repos "$REPOS" --apply 2>/dev/null)"
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

mutant "D50 vault guard removed" \
  's/^  if is_vault "\$repo"; then$/  if false; then/' \
  wrote "memory-vault"

mutant "archived guard removed" \
  's/^  if \[ "\$(printf .%s. "\$meta" | jq -r ..archived.)" = "true" \]; then$/  if false; then/' \
  wrote "archived-repo"

mutant "bypass dropped from the shape test" \
  's/ \&\& \[ "\$byp" = "0" \]//' \
  state "hyperpolymath/bypassed-twin=CONVERGED"

# Removing the converged early-return does NOT reach a write: the covered-by-richer check
# catches it next. That second line of defence is the point, so this mutant is asserted on
# the STATE it corrupts, not on a POST that correctly never happens.
mutant "converged early-return removed" \
  's/^  if \[ "\$exact_n" -eq 1 \]; then$/  if false; then/' \
  state "hyperpolymath/converged-repo=COVERED-BY-RICHER"

printf '\n%d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
