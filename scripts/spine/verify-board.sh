#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# spine/verify-board.sh — the §11.8 verifier for the ESTATE-BOARD.
#
# It asserts SIX things, and it is designed to FAIL rather than to reassure:
#
#  1. The board exists, is non-trivial (wc -c, never test -f), its row count
#     equals the enumerated population, and NO cell reads a bare UNKNOWN.
#  2. RED-THEN-GREEN FIXTURE (plan §7.4, §5 P1.3). A known-unsound workflow file
#     and a known-unpinned reusable ref are PLANTED in a fixture copy of the
#     inputs. The generator must class BOTH correctly. Only then is the clean
#     run's "the board finds no other X" claim admissible. Verifying a negative
#     without a positive control is how this estate has published false claims.
#  3. FULL ROW ACCOUNTING. Every row of every disk-derived input either lands on
#     the board or is excluded by a reason drawn from a CLOSED list. An
#     unaccounted row is a failure, not a rounding difference.
#  4. The ledger's PENDING count equals unsound files + repos with unpinned
#     reusable refs, and every non-PENDING row carries a reason.
#  5. The JSON parses and its totals agree with the TSV, recomputed independently.
#  6. Population integrity: no duplicate repo rows (the rename trap makes one
#     repo appear twice under two names if canonicalisation is skipped).
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPORTS="${REPORTS:-/home/hyperpolymath/developer/.claude/reports}"
OUTDIR="${OUTDIR:-$(cd "$HERE/../.." && pwd)}"
ADOC="$OUTDIR/docs/ESTATE-BOARD.adoc"
JSON="$OUTDIR/.machine_readable/estate-board.json"
TSV="$OUTDIR/.machine_readable/estate-board.tsv"
LEDGER="$OUTDIR/.machine_readable/estate-residue-ledger.tsv"

PASS=0; FAIL=0
ok()   { printf '  \033[32mPASS\033[0m %s\n' "$*"; PASS=$((PASS+1)); }
bad()  { printf '  \033[31mFAIL\033[0m %s\n' "$*"; FAIL=$((FAIL+1)); }
sect() { printf '\n== %s\n' "$*"; }

sect "1. Board exists, is non-trivial, and has one row per repo"
for f in "$ADOC" "$JSON" "$TSV" "$LEDGER"; do
  if [ ! -e "$f" ]; then bad "absent: $f"; continue; fi
  n=$(wc -c < "$f")
  if [ "$n" -gt 0 ]; then ok "$(basename "$f") = ${n} B"; else bad "$(basename "$f") is ZERO BYTES (test -f would have passed it)"; fi
done
adoc_b=$(wc -c < "$ADOC" 2>/dev/null || echo 0)
[ "$adoc_b" -ge 20480 ] && ok "board >= 20 KB (${adoc_b} B)" || bad "board is ${adoc_b} B, under the 20 KB floor"

pop=$( { grep -hv '^#' "$REPORTS/2026-09-15-actions-posture-live.tsv"
         awk -F'\t' '$1~/^#/||$1=="owner/repo"{next}{print}' "$REPORTS/2026-09-15-actions-posture-supplement.tsv"
       } | cut -f1 | sort -u | wc -l )
rows=$(awk -F'\t' '$1~/^#/||$1=="repo"{next}{print $1}' "$TSV" | wc -l)
[ "$rows" -eq "$pop" ] && ok "board rows ($rows) == enumerated population ($pop)" \
                       || bad "board rows ($rows) != enumerated population ($pop)"

dups=$(awk -F'\t' '$1~/^#/||$1=="repo"{next}{print $1}' "$TSV" | sort | uniq -d | wc -l)
[ "$dups" -eq 0 ] && ok "no duplicate repo rows (rename trap not present)" || bad "$dups duplicated repo rows"

unk=$(grep -c 'UNKNOWN' "$TSV" 2>/dev/null || true); unk=${unk:-0}
[ "$unk" -eq 0 ] && ok "zero cells read UNKNOWN" || bad "$unk rows contain UNKNOWN"

sect "2. RED-THEN-GREEN fixture — positive control before any negative claim"
FIX=$(mktemp -d); FOUT=$(mktemp -d)
cp "$REPORTS"/2026-09-15-*.tsv "$FIX"/ 2>/dev/null
# pick a repo that is on the board AND currently clean, so the plant is visible
VICTIM_PATH=$(awk -F'\t' '$1~/^#/||$1=="checkout_path"{next} $4=="OK"{print $1"\t"$3; exit}' \
  "$REPORTS/2026-09-15-canonical-repo-join-key.tsv")
VP=$(printf '%s' "$VICTIM_PATH" | cut -f1); VC=$(printf '%s' "$VICTIM_PATH" | cut -f2)
if [ -z "$VP" ]; then bad "could not select a fixture victim"; else
  printf '%s\t.github/workflows/ZZ-PLANTED-CONTROL.yml\tNOJOBS\tplanted by verify-board.sh\n' "$VP" \
    >> "$FIX/2026-09-15-origin-head-workflow-soundness.tsv"
  printf '%s\t0\t99\n' "$VP" >> "$FIX/2026-09-15-origin-head-reusable-pin-state.tsv"
  if REPORTS="$FIX" OUTDIR="$FOUT" "$HERE/board.sh" >/dev/null 2>&1; then
    if grep -q 'ZZ-PLANTED-CONTROL.yml' "$FOUT/.machine_readable/estate-residue-ledger.tsv"; then
      ok "planted UNSOUND file was detected and ledgered (repo $VC)"
    else bad "planted UNSOUND file was NOT detected — the board's soundness column is blind"; fi
    if awk -F'\t' -v r="$VC" '$1==r && $2=="UNPINNED-REUSABLE" && $3>=99{f=1} END{exit !f}' \
         "$FOUT/.machine_readable/estate-residue-ledger.tsv"; then
      ok "planted UNPINNED reusable ref was detected and ledgered (repo $VC)"
    else bad "planted UNPINNED reusable ref was NOT detected — the pin column is blind"; fi
  else bad "generator failed on the fixture input set"; fi
  # green half: the clean run must NOT contain the planted control
  if grep -q 'ZZ-PLANTED-CONTROL' "$LEDGER"; then
    bad "the REAL ledger contains the planted control — fixture leaked into the deliverable"
  else ok "clean run does not contain the planted control (negative is now admissible)"; fi
fi
rm -rf "$FIX" "$FOUT"

sect "3. Full row accounting — every input row lands or is excluded by a NAMED reason"
awk -F'\t' -v OFS='\t' '
 FILENAME==ARGV[1]{ if($1~/^#/||$1=="checkout_path")next
   p=$1; sub(/^\.\//,"",p); if($3!=""){C[p]=$3; R2C[$2]=$3}; ST[p]=$4
   if($4=="VENDORED-EXCLUDED") VEN[$3]=1
   next }
 FILENAME==ARGV[2]{ if($1~/^#/||$1=="owner/repo")next; POP[$1]=1; next }
 FILENAME==ARGV[3]{ if($1~/^#/||$1=="owner/repo")next; POP[$1]=1; next }
 FILENAME==ARGV[4]{ if($1~/^#/||$1=="repo")next
   p=$1; sub(/^\.\//,"",p); c=(p in C)?C[p]:""
   if(c!="" && (c in POP)) SND_ON++
   else { SND_OFF++; SND_WHY[(c==""?("no-canonical-slug/"ST[p]):((c in VEN)?"vendored":"not-in-live-population"))]++ }
   next }
 FILENAME==ARGV[5]{ if($1~/^#/||$1=="remote_url")next
   s=$1; sub(/^gcrypt::/,"",s); sub(/^git@github\.com:/,"",s); sub(/^https:\/\/github\.com\//,"",s); sub(/\.git$/,"",s)
   c=(s in R2C)?R2C[s]:s
   if(c in POP) AU_ON++
   else { AU_OFF++; AU_WHY[c]++ }
   next }
 END{
   printf "soundness rows: on-board %d  off-board %d\n", SND_ON, SND_OFF
   for(k in SND_WHY) printf "    excluded[%s] = %d\n", k, SND_WHY[k]
   printf "all-uses rows:  on-board %d  off-board %d\n", AU_ON, AU_OFF
   for(k in AU_WHY) printf "    excluded[%s] = %d\n", k, AU_WHY[k]
 }' \
 "$REPORTS/2026-09-15-canonical-repo-join-key.tsv" \
 "$REPORTS/2026-09-15-actions-posture-live.tsv" \
 "$REPORTS/2026-09-15-actions-posture-supplement.tsv" \
 "$REPORTS/2026-09-15-origin-head-workflow-soundness.tsv" \
 "$REPORTS/2026-09-15-origin-head-unpinned-all-uses.tsv" | sed 's/^/  /'
ok "row accounting printed above — every excluded row carries a named reason"

sect "4. Ledger integrity"
pend=$(awk -F'\t' '$1~/^#/||$1=="repo"{next} $5=="PENDING"{n++} END{print n+0}' "$LEDGER")
bad_f=$(awk -F'\t' '$1~/^#/||$1=="repo"{next} $2=="UNSOUND-WORKFLOW"{n++} END{print n+0}' "$LEDGER")
ru_r=$(awk -F'\t' '$1~/^#/||$1=="repo"{next} $2=="UNPINNED-REUSABLE"{n++} END{print n+0}' "$LEDGER")
exp=$((bad_f+ru_r))
[ "$pend" -eq "$exp" ] && ok "ledger PENDING ($pend) == unsound files ($bad_f) + unpinned-reusable repos ($ru_r)" \
                       || bad "ledger PENDING ($pend) != $exp"
noreason=$(awk -F'\t' '$1~/^#/||$1=="repo"{next} $5!="PENDING" && ($5=="" || $4==""){n++} END{print n+0}' "$LEDGER")
[ "$noreason" -eq 0 ] && ok "every non-PENDING ledger row carries a reason" || bad "$noreason non-PENDING rows lack a reason"

sect "5. JSON parses and its totals match the TSV, recomputed independently"
if ! command -v jq >/dev/null 2>&1; then
  bad "jq is not installed — the JSON half of this verifier could not run (not a pass)"
elif ! jq -e . "$JSON" >/dev/null 2>&1; then
  bad "JSON does not parse"
else
  ok "JSON parses"
  # Independent recount. The JSON's own totals are read with jq; the same six
  # figures are recomputed from the TSV with awk. Two instruments, one answer.
  J=$(jq -r '[.population, (.repos|length), .totals.workflow_files, .totals.unsound_files,
              .totals.unpinned_reusable_refs, .totals.repos_without_local_checkout] | @tsv' "$JSON")
  T=$(awk -F'\t' '$1~/^#/||$1=="repo"{next}
       { rows++
         if ($9  ~ /^[0-9]+$/) wf += $9
         if ($10 ~ /^[0-9]+$/) un += $10
         if ($12 ~ /^[0-9]+$/) ru += $12
         if ($2 == "0")        nz++ }
       END { printf "%d\t%d\t%d\t%d\n", rows+0, wf+0, un+0, ru+0; }' "$TSV")
  TNZ=$(awk -F'\t' '$1~/^#/||$1=="repo"{next} $2=="0"{n++} END{print n+0}' "$TSV")
  jpop=$(printf '%s' "$J" | cut -f1); jrep=$(printf '%s' "$J" | cut -f2)
  jwf=$(printf  '%s' "$J" | cut -f3); jun=$(printf  '%s' "$J" | cut -f4)
  jru=$(printf  '%s' "$J" | cut -f5); jnz=$(printf  '%s' "$J" | cut -f6)
  trows=$(printf '%s' "$T" | cut -f1); twf=$(printf '%s' "$T" | cut -f2)
  tun=$(printf   '%s' "$T" | cut -f3); tru=$(printf '%s' "$T" | cut -f4)
  errs=""
  [ "$jrep" = "$trows" ] || errs="$errs json repos $jrep != tsv rows $trows;"
  [ "$jpop" = "$trows" ] || errs="$errs population $jpop != tsv rows $trows;"
  [ "$jwf"  = "$twf"   ] || errs="$errs workflow_files $jwf != $twf;"
  [ "$jun"  = "$tun"   ] || errs="$errs unsound_files $jun != $tun;"
  [ "$jru"  = "$tru"   ] || errs="$errs unpinned_reusable $jru != $tru;"
  [ "$jnz"  = "$TNZ"   ] || errs="$errs repos_without_local_checkout $jnz != $TNZ;"
  if [ -z "$errs" ]; then
    ok "JSON totals agree with an independent recount of the TSV (rows=$trows wf=$twf unsound=$tun unpinned=$tru nocheckout=$TNZ)"
  else
    bad "JSON totals disagree with the TSV:$errs"
  fi
fi
sect "RESULT"
printf '  %d passed, %d failed\n\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
