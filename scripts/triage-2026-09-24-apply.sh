#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# triage-2026-09-24-apply.sh — apply the 2026-09-24 ultraplan live actions.
#
# WHAT THIS DOES (ULTRAPLAN-2026-09-24.adoc, part 5.2):
#   1. Labels every OPEN issue: the estate classifier's output plus the
#      curated scope/status/priority labels from the ultraplan's part-4
#      dispositions. Additive only; never removes existing labels.
#   2. Closes 10 issues with evidence comments (fixed / superseded /
#      absorbed / subsumed), and preserves #808's mis-triage knowledge on
#      #913 before closing #808.
#   3. Sets the optimised repository description and topics.
#
# IDEMPOTENT: safe to re-run. Skips already-closed issues, only posts what
# is missing.
#
# WHY A SCRIPT: the agent token that wrote the analysis (arena-ai-coding-agent)
# has labels:write but NOT issues:write / repo admin, so it could label but
# not close/comment, and could not PATCH the repo. Run this from a checkout
# of this repo with the owner's `gh` authentication:
#
#   gh auth login            # or: export GH_TOKEN=<owner PAT with repo scope>
#   bash scripts/triage-2026-09-24-apply.sh
#
# Dry run:  DRY_RUN=1 bash scripts/triage-2026-09-24-apply.sh
set -uo pipefail
cd "$(git rev-parse --show-toplevel)"
REPO=hyperpolymath/standards
DRY=${DRY_RUN:-0}
run() { if [ "$DRY" = "1" ]; then echo "[dry-run] $*"; else eval "$@"; fi; }
pause() { [ "$DRY" = "1" ] || sleep 0.15; }

echo "== 0. preflight"
gh auth status >/dev/null 2>&1 || { echo "gh is not authenticated"; exit 1; }
PERM_OK=$(gh api repos/$REPO/issues/1/comments --jq 'length' 2>/dev/null || echo err)
if [ "$PERM_OK" = "err" ]; then echo "token cannot even read issues"; exit 1; fi

# Defined labels (GitHub per-repo ceiling is 1000).
mapfile -t DEFINED < <(gh label list -R "$REPO" --limit 1000 --json name --jq '.[].name')
defined() { local w=$1; for d in "${DEFINED[@]}"; do [ "$d" = "$w" ] && return 0; done; return 1; }

# ---------------------------------------------------------------------------
# 1. LABELS: disposition per issue (ULTRAPLAN part 4). Columns: num|disposition
#    scope is derived: KEEP->scope:repo, ESTATE->scope:estate, else per map.
# ---------------------------------------------------------------------------
read -r -d '' DISPOSITIONS <<'EOF' || true
89|ESTATE
90|ESTATE
91|ESTATE
92|ESTATE
93|ESTATE
100|PARK
124|ESTATE
156|ESTATE
239|PARK
252|ESTATE
272|ESTATE
276|ESTATE
278|ESTATE
279|ESTATE
281|ESTATE
288|ESTATE
306|ESTATE
307|ESTATE
309|ESTATE
323|ESTATE
324|ESTATE
331|ESTATE
342|ESTATE
343|ESTATE
346|KEEP
348|ESTATE
401|KEEP
403|ESTATE
404|DECIDE
408|ESTATE
409|KEEP
410|ESTATE
411|ESTATE
438|VERIFY
443|PARK
446|KEEP
460|ESTATE
462|ESTATE
463|KEEP
479|VERIFY
493|ESTATE
495|KEEP
497|DECIDE
633|ESTATE
634|ESTATE
635|ESTATE
636|ESTATE
645|ESTATE
646|KEEP
653|ESTATE
657|ESTATE
659|ESTATE
662|ESTATE
663|ESTATE
664|ESTATE
668|ESTATE
669|ESTATE
670|ESTATE
674|ESTATE
675|ESTATE
677|ESTATE
678|ESTATE
681|VERIFY
687|ESTATE
689|ESTATE
692|DECIDE
698|ESTATE
702|ESTATE
703|ESTATE
704|KEEP
705|ESTATE
706|ESTATE
707|ESTATE
727|ESTATE
732|KEEP
750|ESTATE
751|ESTATE
759|ESTATE
787|DECIDE
791|KEEP
792|ESTATE
795|ESTATE
803|ESTATE
837|KEEP
887|VERIFY
888|KEEP
889|ESTATE
890|VERIFY
894|KEEP
895|KEEP
896|KEEP
897|KEEP
903|KEEP
904|ESTATE
905|KEEP
906|KEEP
907|KEEP
908|KEEP
909|KEEP
910|KEEP
911|KEEP
913|KEEP
914|KEEP
915|KEEP
916|KEEP
917|ESTATE
918|ESTATE
919|ESTATE
921|KEEP
922|KEEP
923|DECIDE
924|ESTATE
925|KEEP
926|KEEP
928|KEEP
929|DECIDE
930|KEEP
932|KEEP
933|KEEP
934|KEEP
935|VERIFY
936|KEEP
937|KEEP
938|KEEP
939|KEEP
940|KEEP
941|DECIDE
942|KEEP
943|KEEP
944|DECIDE
945|KEEP
949|ESTATE
950|ESTATE
953|KEEP
955|KEEP
957|KEEP
958|KEEP
960|KEEP
963|VERIFY
964|KEEP
967|KEEP
968|ESTATE
969|ESTATE
972|KEEP
975|KEEP
976|KEEP
980|KEEP
981|KEEP
987|ESTATE
991|DECIDE
992|KEEP
993|KEEP
994|KEEP
998|KEEP
999|KEEP
1000|KEEP
1001|KEEP
1002|ESTATE
1003|ESTATE
1004|ESTATE
1005|ESTATE
1006|ESTATE
1007|ESTATE
1008|DECIDE
1010|ESTATE
1013|VERIFY
1014|KEEP
1015|KEEP
1018|KEEP
1019|ESTATE
1021|KEEP
1022|KEEP
1023|DECIDE
1024|ESTATE
1025|ESTATE
1028|DECIDE
1031|ESTATE
1032|ESTATE
1033|KEEP
1035|ESTATE
1036|KEEP
1037|ESTATE
1040|KEEP
EOF

labelled=0
while IFS='|' read -r num disp; do
  state=$(gh issue view "$num" -R "$REPO" --json state --jq .state 2>/dev/null) || continue
  [ "$state" = "OPEN" ] || continue
  title=$(gh issue view "$num" -R "$REPO" --json title --jq .title 2>/dev/null)

  mapfile -t CLASS < <(jq -r --arg title "$title" --argjson have '[]' \
      -f .github/scripts/classify-issue.jq .github/label-classifier.json 2>/dev/null)

  cur=()
  case "$disp" in
    KEEP)   cur+=(scope:repo) ;;
    ESTATE) cur+=(scope:estate) ;;
    DECIDE) cur+=(decision status:needs-ruling)
            case "$num" in
              404|497|692|787|941|1008|1023|1028) cur+=(scope:estate) ;;
              *) cur+=(scope:repo) ;;
            esac ;;
    VERIFY) case "$num" in
              438|681|887|890|1013) cur+=(scope:estate) ;;
              *) cur+=(scope:repo) ;;
            esac ;;
    PARK)   cur+=(status:blocked scope:estate) ;;
  esac
  case "$num" in
    903|955|975|913|1037) cur+=(priority:p1) ;;
    803) cur+=(meta:recurring) ;;
  esac

  want=()
  for l in "${CLASS[@]:-}" "${cur[@]:-}"; do
    [ -z "$l" ] && continue
    dup=0; for w in "${want[@]:-}"; do [ "$w" = "$l" ] && dup=1 && break; done
    [ "$dup" -eq 1 ] && continue
    defined "$l" && want+=("$l")
  done
  [ ${#want[@]} -eq 0 ] && continue
  json=$(printf '%s\n' "${want[@]}" | jq -R . | jq -s .)
  printf '{"labels":%s}' "$json" > /tmp/arena-label-body.json
  if run "gh api -X POST repos/$REPO/issues/$num/labels --input /tmp/arena-label-body.json >/dev/null 2>&1"; then
    labelled=$((labelled+1))
  else
    echo "  label POST failed on #$num - continuing"
  fi
  pause
done <<< "$DISPOSITIONS"
echo "1. labels applied on $labelled issue(s)"

# ---------------------------------------------------------------------------
# 2. CLOSES (ULTRAPLAN 5.2). Evidence comment per issue; subsumption of #808
#    is preceded by a preservation comment on #913 (no orphaned evidence).
# ---------------------------------------------------------------------------
close_if_open() { # $1=number $2=reason-tag
  local n=$1
  local state=$(gh issue view "$n" -R "$REPO" --json state --jq .state 2>/dev/null) || return
  [ "$state" = "OPEN" ] || { echo "  #$n already $state - skipping"; return; }
  if [ "$DRY" = "1" ]; then echo "[dry-run] close #$n"; else
    gh issue close "$n" -R "$REPO" --comment "$(cat "/tmp/arena-close-$n.md")" >/dev/null \
      && echo "  closed #$n" || echo "  close FAILED #$n"
  fi
  pause
}

cat > /tmp/arena-close-956.md <<'EOF'
Closed as fixed by #1034 (2026-09-23). Verified via the rulesets API on 2026-09-24: the active ruleset `main gate: append-only + required checks + signatures + scanning` enforces 21 required status contexts, required signatures, and code-scanning thresholds on the default branch. Every gate this repo ships is now actually required here.

(ULTRAPLAN-2026-09-24.adoc, part 5.2)
EOF

cat > /tmp/arena-close-637.md <<'EOF'
Closed as absorbed: #787 (Owner decision sheet D1-D72) is "one answerable place for #637 + #715 + #709 + #658", so this register is superseded by it. Rulings continue on #787.

(ULTRAPLAN-2026-09-24.adoc, part 5.2)
EOF
cp /tmp/arena-close-637.md /tmp/arena-close-709.md
cp /tmp/arena-close-637.md /tmp/arena-close-715.md

cat > /tmp/arena-close-784.md <<'EOF'
Closed as answered: the census this issue was waiting for is #968 - 39 repos with step-level actions.lock desync (plus #969 for the job-level population). The hypothesis is no longer open.

(ULTRAPLAN-2026-09-24.adoc, part 5.2)
EOF

cat > /tmp/arena-close-808.md <<'EOF'
Closed as subsumed by #913: the 2026-09-22 census measured this same defect population at 76 `uses: ../../` refs, with acceptance criteria. The mis-triage knowledge from this issue (failure + 0 jobs + run name == path, indistinguishable from callee-lockfile poisoning) is preserved in a comment on #913 before this close. The fix continues under #913.

(ULTRAPLAN-2026-09-24.adoc, part 5.2)
EOF

cat > /tmp/arena-close-708.md <<'EOF'
Closed as fixed: the current `lockfile-drift-detect.yml` implements rc-honesty (1 = drift, 2 = usage/env error), per-repo slugs (no more anonymised `_w`), and counts tab-delimited data rows only (banner lines no longer counted as drift) - its comments cite this issue as the resolved reference. "Has only ever run once" is stale: the sweep runs weekly.

(ULTRAPLAN-2026-09-24.adoc, part 5.2)
EOF

cat > /tmp/arena-close-658.md <<'EOF'
Closed as superseded by the 2026-09-22 ruling (Deno banned outright; Bun is tier 1). The "migrate Deno to Bun" premise is replaced by Deno retirement: #919 sizes it (95 `deno task` definitions across 23 files in the 11 ledgered repos) and #926 covers the k9-coordination harness. Preserved data from this issue: 30 deno.json locations assessed, of which 18 were blocked on npm packages that do not exist.

(ULTRAPLAN-2026-09-24.adoc, part 5.2)
EOF

cat > /tmp/arena-close-920.md <<'EOF'
Closed as fixed: `rhodium-standard-repositories/.github/workflows/language-policy.yml` line 116 now reads "the JS runtime is Bun per the 2026-09-22 ruling, which banned Deno as well" (committed with the 2026-09-24 intake/canon changes). Remaining estate copies of the old comment sit in the template-sync population tracked under #659.

(ULTRAPLAN-2026-09-24.adoc, part 5.2)
EOF

cat > /tmp/arena-close-927.md <<'EOF'
Closed as fixed: `:source-repo:` now points at https://github.com/hyperpolymath/standards (committed with the 2026-09-24 intake/canon changes).

(ULTRAPLAN-2026-09-24.adoc, part 5.2)
EOF

cat > /tmp/arena-913-preserve.md <<'EOF'
Preserved from #808 (subsumed by this issue, closed 2026-09-24): why the malformed `uses: ../../` refs were invisible. A workflow that fails to parse produces the triple `conclusion=failure`, **0 jobs**, and a run `name` equal to its file *path* - the same triple produced by callee-lockfile poisoning. These were repeatedly mis-triaged as lockfile faults; they never parsed. Also: do not use `gh actions-lock` rewrite mode to fix these refs - it previously invented `uses: $/.github/actions/...` refs that fail the same way.
EOF
if [ "$(gh issue view 913 -R "$REPO" --json state --jq .state 2>/dev/null)" = "OPEN" ]; then
  run "gh issue comment 913 -R $REPO --body-file /tmp/arena-913-preserve.md >/dev/null" \
    || echo "  (913 preservation comment skipped/failed - continuing)"
fi

close_if_open 956
close_if_open 637
close_if_open 709
close_if_open 715
close_if_open 784
close_if_open 808
close_if_open 708
close_if_open 658
close_if_open 920
close_if_open 927
echo "2. closes done"

# ---------------------------------------------------------------------------
# 3. DESCRIPTION + TOPICS (ULTRAPLAN part 7)
# ---------------------------------------------------------------------------
DESC='Canonical standards, specifications and governance for the Hyperpolymath estate: policy-as-code, machine-readable specs (A2ML/DEED), and the reusable CI/CD + security canon for a 500+ repository software estate.'
run "gh api -X PATCH repos/$REPO -f description=\"$DESC\" -f topics[]=standards -f topics[]=specification -f topics[]=\"policy-as-code\" -f topics[]=governance -f topics[]=compliance -f topics[]=\"machine-readable\" -f topics[]=documentation -f topics[]=\"open-standards\" -f topics[]=deed -f topics[]=k9 -f topics[]=\"epistemic-computing\" -f topics[]=hyperpolymath >/dev/null"
echo "3. description + topics set"

echo "done. (DRY_RUN=$DRY)"
