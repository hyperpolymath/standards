#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# regen-dependabot-locks-test.sh — offline known-answer tests for
# scripts/regen-dependabot-locks.sh.
#
# The repair scripts it drives have their own suites, so they are stubbed here
# and each stub's verdict is set per case. What is under test is the bot's OWN
# logic: that it keeps only actions.lock, refuses every unsafe tree with a
# distinct verdict instead of committing it, filters PRs correctly, builds a
# commit that cannot land on a moved branch, and reports a missing credential
# rather than claiming a clean sweep.
#
# Run: bash scripts/tests/regen-dependabot-locks-test.sh

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET="${TARGET:-$SCRIPT_DIR/../regen-dependabot-locks.sh}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

pass=0
fail=0
# Record a passing case named $1.
ok()  { echo "PASS: $1"; pass=$((pass + 1)); }
# Record a failing case named $1.
bad() { echo "FAIL: $1"; fail=$((fail + 1)); }
# expect <label> <want> <got>
expect() { if [ "$2" = "$3" ]; then ok "$1"; else bad "$1 — expected '$2', got '$3'"; fi; }

# --- stubs -----------------------------------------------------------------
TOOLS="$WORK/tools"
mkdir -p "$TOOLS"
for s in relock-sha-keys complete-job-refs prune-stale; do
  printf '#!/usr/bin/env bash\nexit 0\n' > "$TOOLS/$s.sh"
done
printf '#!/usr/bin/env bash\nexit "${CLOSE_RC:-0}"\n' > "$TOOLS/close-lock.sh"
printf '#!/usr/bin/env bash\nexit "${VERIFY_RC:-0}"\n' > "$TOOLS/update-actions-lock.sh"

# The `gh` stub. `actions-lock` (update mode) applies $TOOL_ACTION to the cwd;
# `api graphql` records its stdin payload and answers with a commit oid.
cat > "$WORK/gh" <<'EOF'
#!/usr/bin/env bash
if [ "$1" = actions-lock ]; then
  case "${TOOL_ACTION:-none}" in
    bump)    echo "  - 'a/b@v2'" >> .github/workflows/actions.lock ;;
    rewrite) echo "  - 'a/b@v2'" >> .github/workflows/actions.lock
             sed -i 's/@1111111111111111111111111111111111111111/@v2/' .github/workflows/ci.yml
             echo junk > .github/workflows/new-by-tool.yml ;;
    dollar)  echo "  - '\$/.github/actions/x'" >> .github/workflows/actions.lock ;;
    fail)    exit 1 ;;
    none)    : ;;
  esac
  exit 0
fi
if [ "$1" = api ] && [[ "$2" == repos/*/contents/* ]]; then
  case "${CONTENTS:-200}" in
    200) exit 0 ;;
    404) echo "gh: Not Found (HTTP 404)" >&2; exit 1 ;;
    *)   echo "gh: API rate limit exceeded (HTTP 403)" >&2; exit 1 ;;
  esac
fi
if [ "$1 $2" = "api --paginate" ] && [[ "$3" == repos/*/pulls\?* ]]; then
  [ -n "${PULLS_FAIL:-}" ] && exit 1
  echo '[]'; exit 0
fi
if [ "$1 $2" = "api graphql" ]; then
  cat > "$GH_CAPTURE"
  echo 2222222222222222222222222222222222222222
  exit 0
fi
exit 1
EOF
chmod +x "$WORK/gh"
export GH_BIN="$WORK/gh" REGEN_TOOLS="$TOOLS" GH_CAPTURE="$WORK/payload.json"

# shellcheck source=../regen-dependabot-locks.sh
source "$TARGET"

# A fresh fixture repo: one SHA-pinned workflow and a lock.
fixture() {
  local d="$WORK/repo-$1"
  rm -rf "$d"; mkdir -p "$d/.github/workflows"
  git -C "$d" init --quiet
  printf 'jobs:\n  a:\n    steps:\n      - uses: a/b@1111111111111111111111111111111111111111 # v1\n' \
    > "$d/.github/workflows/ci.yml"
  printf "workflows:\n  '.github/workflows/ci.yml':\n  - 'a/b@v1'\n" > "$d/.github/workflows/actions.lock"
  git -C "$d" add -A
  git -C "$d" -c user.name=t -c user.email=t@t commit --quiet -m init
  printf '%s' "$d"
}

# --- regen_lock verdicts ---------------------------------------------------
d="$(fixture bump)"
expect "a lock-only change is 'changed'" changed "$(TOOL_ACTION=bump regen_lock "$d" 2>/dev/null)"
expect "…and only actions.lock differs" " M .github/workflows/actions.lock" "$(git -C "$d" status --porcelain)"

d="$(fixture rewrite)"
expect "tool workflow rewrites are discarded, lock kept" changed "$(TOOL_ACTION=rewrite regen_lock "$d" 2>/dev/null)"
expect "…the SHA pin in ci.yml survives" 1 "$(grep -c '@1111111111111111111111111111111111111111' "$d/.github/workflows/ci.yml")"
expect "…the tool's new file is removed" no "$([ -e "$d/.github/workflows/new-by-tool.yml" ] && echo yes || echo no)"

d="$(fixture dollar)"
expect "a \$/ ref in the lock is refused" corrupt "$(TOOL_ACTION=dollar regen_lock "$d" 2>/dev/null)"

d="$(fixture unres)"
expect "an unresolvable edge (close-lock exit 3) is refused" unresolvable "$(TOOL_ACTION=bump CLOSE_RC=3 regen_lock "$d" 2>/dev/null)"

d="$(fixture unver)"
expect "a lock the gate's verifier rejects is refused" unverified "$(TOOL_ACTION=bump VERIFY_RC=1 regen_lock "$d" 2>/dev/null)"

d="$(fixture toolfail)"
expect "a failing gh actions-lock is reported" tool-failed "$(TOOL_ACTION=fail regen_lock "$d" 2>/dev/null)"

d="$(fixture current)"
expect "an already-current lock is 'current' (idempotent second run)" current "$(TOOL_ACTION=none regen_lock "$d" 2>/dev/null)"

d="$(fixture nolock)"; git -C "$d" rm --quiet .github/workflows/actions.lock
expect "a repo without actions.lock is skipped" no-lock "$(regen_lock "$d" 2>/dev/null)"

d="$(fixture composite)"; mkdir -p "$d/.github/actions/x"
expect "a repo with local composite actions is refused" composite-unsupported "$(TOOL_ACTION=bump regen_lock "$d" 2>/dev/null)"

# Planted control for the dirty guard: a repair step that writes anything
# besides the lock (after the restore has run) must stop the commit.
d="$(fixture dirty)"
printf '#!/usr/bin/env bash\necho x >> README.md\n' > "$TOOLS/prune-stale.sh"
expect "any change besides actions.lock is refused" dirty "$(TOOL_ACTION=bump regen_lock "$d" 2>/dev/null)"
printf '#!/usr/bin/env bash\nexit 0\n' > "$TOOLS/prune-stale.sh"

# --- select_prs ------------------------------------------------------------
prs='[
 {"number":1,"draft":false,"user":{"login":"dependabot[bot]"},"head":{"ref":"dependabot/x","sha":"aaa","repo":{"full_name":"o/r"}},"base":{"repo":{"full_name":"o/r"}}},
 {"number":2,"draft":false,"user":{"login":"someone"},"head":{"ref":"feat","sha":"bbb","repo":{"full_name":"o/r"}},"base":{"repo":{"full_name":"o/r"}}},
 {"number":3,"draft":true,"user":{"login":"dependabot[bot]"},"head":{"ref":"dependabot/y","sha":"ccc","repo":{"full_name":"o/r"}},"base":{"repo":{"full_name":"o/r"}}},
 {"number":4,"draft":false,"user":{"login":"dependabot[bot]"},"head":{"ref":"dependabot/z","sha":"ddd","repo":{"full_name":"fork/r"}},"base":{"repo":{"full_name":"o/r"}}}
]'
expect "only same-repo, non-draft Dependabot PRs are selected" "1	dependabot/x	aaa" "$(printf '%s' "$prs" | select_prs)"

# --- commit_lock -----------------------------------------------------------
d="$(fixture commit)"
oid="$(commit_lock o/r dependabot/x 3333333333333333333333333333333333333333 "$d")"
expect "commit_lock returns the new oid" 2222222222222222222222222222222222222222 "$oid"
expect "…guarded by expectedHeadOid" 3333333333333333333333333333333333333333 "$(jq -r .variables.in.expectedHeadOid "$GH_CAPTURE")"
expect "…writes exactly one file, the lock" ".github/workflows/actions.lock" "$(jq -r '.variables.in.fileChanges.additions | map(.path) | join(",")' "$GH_CAPTURE")"
expect "…with the lock's exact bytes" "$(base64 -w0 < "$d/.github/workflows/actions.lock")" "$(jq -r '.variables.in.fileChanges.additions[0].contents' "$GH_CAPTURE")"

# --- process_repo: only a 404 is a silent skip ---------------------------
expect "no lock (404) is skipped silently" "" "$(CONTENTS=404 process_repo o/r t)"
expect "a rate-limited lock probe is reported, not skipped" "o/r	api-error" "$(CONTENTS=403 process_repo o/r t 2>/dev/null)"
expect "a failed PR listing is reported" "o/r	api-error" "$(PULLS_FAIL=1 process_repo o/r t 2>/dev/null)"
expect "a repo with no open PRs prints nothing" "" "$(process_repo o/r t)"

# --- missing credential ----------------------------------------------------
out="$(REGEN_TOKENS='' main 2>&1)"; rc=$?
expect "no token: exits 0" 0 "$rc"
expect "…and says nothing was examined" 1 "$(printf '%s' "$out" | grep -c 'Nothing was examined')"

# The exact string the workflow passes when the App is absent.
out="$(REGEN_TOKENS='hyperpolymath= metadatastician=' main 2>&1)"; rc=$?
expect "empty per-owner tokens (workflow shape): exits 0" 0 "$rc"
expect "…and names both owners as NOT examined" 2 "$(printf '%s' "$out" | grep -c 'NOT examined')"
expect "…and claims zero repositories" 1 "$(printf '%s' "$out" | grep -c '^examined 0 repositories$')"

echo
echo "Total: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
