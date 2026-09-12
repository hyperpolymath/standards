#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
# READ ONLY. Estate CI/CD census over GraphQL: what each repository REQUIRES
# and what it actually EMITS — the pair needed to find phantom required checks.
#
# WHY GRAPHQL
# -----------
# The REST equivalent cost ~4 calls per repo for requirements and ~20 more for
# emissions: roughly 4,000 calls for a 424-repo estate. That exhausts the
# 5,000/hour CORE budget in one pass, leaving nothing for the remediation the
# census exists to drive — and it died halfway more than once, silently
# under-reporting.
#
# Over GraphQL the same data costs ONE POINT PER 25 REPOSITORIES for
# requirements and one per repository for emissions, from a 5,000/hour budget
# that is SEPARATE from core and otherwise unused. Measured: full requirements
# census in 38 seconds for 18 points.
#
# IT IS ALSO MORE CORRECT, not merely cheaper:
#
#   * Requirements live in EITHER classic branch protection OR a ruleset. The
#     REST scan read those through different endpoints, so a repo using branch
#     protection was reported as "no ruleset required X" and its fix silently
#     skipped. One query returns both, so they cannot drift apart.
#
#   * `statusCheckRollup` returns CheckRun names AND StatusContext contexts
#     together. Reading only the Actions API would brand every external check
#     (SonarCloud and friends) a phantom.
#
#   * Ruleset TARGET is recorded. Required status checks on a ruleset targeting
#     TAGS are INERT — tags have no pull request to gate. Counting them
#     overstates enforcement; dropping them hides a real misconfiguration.
#
#   * Rulesets with enforcement != ACTIVE are skipped: they gate nothing.
#
# KNOWN LIMIT — READ BEFORE ACTING ON A "PHANTOM"
# A context appearing only on a RARE trigger can be missed by any sample.
# Measured example: `Dependabot` shows up only on Dependabot pull requests, so
# it is absent unless one falls in the sampled window — and would be wrongly
# reported as a phantom. Widen --prs to reduce this, but treat "phantom" as a
# CANDIDATE requiring confirmation, never a verdict. That is the discipline
# docs/CICD-SIGNAL-DISCIPLINE.adoc already sets for fake gates.
#
# Output TSV: repo <TAB> kind <TAB> value
#   kind = req:branch-protection | req:ruleset-BRANCH | req:ruleset-TAG | emit
set -uo pipefail

OUT="${1:?usage: cicd-census.sh OUT.tsv [--commits N] [--prs N]}"; shift || true
COMMITS=8
PRS=5
while [ $# -gt 0 ]; do
  case "$1" in
    --commits) COMMITS="$2"; shift 2 ;;
    --prs)     PRS="$2";     shift 2 ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
done
: > "$OUT"
export OUT COMMITS PRS

REQ_QUERY='
query($login: String!, $cursor: String) {
  repositoryOwner(login: $login) {
    ... on User         { repositories(first: 25, ownerAffiliations: OWNER, orderBy: {field: NAME, direction: ASC}, after: $cursor) { ...P } }
    ... on Organization { repositories(first: 25, ownerAffiliations: OWNER, orderBy: {field: NAME, direction: ASC}, after: $cursor) { ...P } }
  }
}
fragment P on RepositoryConnection {
  pageInfo { hasNextPage endCursor }
  nodes {
    nameWithOwner
    branchProtectionRules(first: 3) { nodes { requiredStatusCheckContexts } }
    rulesets(first: 5) {
      nodes {
        target enforcement
        rules(first: 20) {
          nodes { type parameters { ... on RequiredStatusChecksParameters { requiredStatusChecks { context } } } }
        }
      }
    }
  }
}'

REQ_PARSE='
import json, os, sys
d = json.load(sys.stdin)
if "errors" in d:
    sys.stderr.write("GRAPHQL ERROR: " + json.dumps(d["errors"])[:300] + "\n"); print("STOP"); raise SystemExit
repos = d["data"]["repositoryOwner"]["repositories"]
rows = []
for r in repos["nodes"]:
    n = r["nameWithOwner"]
    for b in r["branchProtectionRules"]["nodes"]:
        for c in (b["requiredStatusCheckContexts"] or []):
            rows.append(n + "\treq:branch-protection\t" + c)
    for rs in r["rulesets"]["nodes"]:
        if rs["enforcement"] != "ACTIVE":
            continue
        for rule in rs["rules"]["nodes"]:
            p = rule.get("parameters") or {}
            for c in (p.get("requiredStatusChecks") or []):
                rows.append(n + "\treq:ruleset-" + rs["target"] + "\t" + c["context"])
with open(os.environ["OUT"], "a") as fh:
    fh.write("".join(x + "\n" for x in rows))
pi = repos["pageInfo"]
print(pi["endCursor"] if pi["hasNextPage"] else "DONE")
'

echo "== requirements ==" >&2
for LOGIN in hyperpolymath metadatastician; do
  CURSOR=""; PAGES=0
  while :; do
    if [ -z "$CURSOR" ]; then
      RESP=$(gh api graphql -F login="$LOGIN" -F cursor=null -f query="$REQ_QUERY" 2>/dev/null)
    else
      RESP=$(gh api graphql -F login="$LOGIN" -F cursor="$CURSOR" -f query="$REQ_QUERY" 2>/dev/null)
    fi
    [ -z "$RESP" ] && break
    NEXT=$(printf '%s' "$RESP" | python3 -c "$REQ_PARSE"); PAGES=$((PAGES+1))
    case "$NEXT" in DONE|STOP|'') break ;; *) CURSOR="$NEXT" ;; esac
    sleep 0.15
  done
  echo "  $LOGIN: $PAGES page(s)" >&2
done

# Emissions only for repos that require something: a repo requiring nothing
# cannot have a phantom, so scanning it is wasted budget.
cut -f1 "$OUT" | sort -u > "$OUT.repos"
echo "== emissions for $(wc -l < "$OUT.repos") repo(s) that require something ==" >&2

EMIT_QUERY='
query($owner: String!, $name: String!, $commits: Int!, $prs: Int!) {
  repository(owner: $owner, name: $name) {
    defaultBranchRef { target { ... on Commit { history(first: $commits) { nodes { ...Roll } } } } }
    pullRequests(first: $prs, states: [OPEN, MERGED], orderBy: {field: UPDATED_AT, direction: DESC}) {
      nodes { commits(last: 1) { nodes { commit { ...Roll } } } }
    }
  }
}
fragment Roll on Commit {
  statusCheckRollup { contexts(first: 100) { nodes { ... on CheckRun { name } ... on StatusContext { context } } } }
}'

EMIT_PARSE='
import json, os, sys
try: d = json.load(sys.stdin)
except Exception: raise SystemExit
if "errors" in d or not (d.get("data") or {}).get("repository"): raise SystemExit
ctx = set()
def take(node):
    roll = (node or {}).get("statusCheckRollup")
    if roll:
        for n in roll["contexts"]["nodes"]:
            v = n.get("name") or n.get("context")
            if v: ctx.add(v)
rep = d["data"]["repository"]
tgt = (rep.get("defaultBranchRef") or {}).get("target") or {}
for c in (tgt.get("history") or {}).get("nodes", []): take(c)
for pr in (rep.get("pullRequests") or {}).get("nodes", []):
    for c in pr["commits"]["nodes"]: take(c["commit"])
with open(os.environ["OUT"], "a") as fh:
    for c in sorted(ctx): fh.write(os.environ["REPO"] + "\temit\t" + c + "\n")
'

while read -r R; do
  [ -z "$R" ] && continue
  OWNER="${R%%/*}"; NAME="${R##*/}"
  REPO="$R" gh api graphql -F owner="$OWNER" -F name="$NAME" \
      -F commits="$COMMITS" -F prs="$PRS" -f query="$EMIT_QUERY" 2>/dev/null \
    | REPO="$R" python3 -c "$EMIT_PARSE"
  sleep 0.1
done < "$OUT.repos"

echo "census rows: $(wc -l < "$OUT")" >&2
gh api graphql -f query='{ rateLimit { used remaining limit } }' \
  -q '.data.rateLimit|"graphql: \(.used) used, \(.remaining)/\(.limit) left"' 2>/dev/null >&2
