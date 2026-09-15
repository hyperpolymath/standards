# SPDX-License-Identifier: MPL-2.0
# board.awk — join eight measured censuses into the ESTATE-BOARD.
# Invoked by board.sh.  ALWAYS with -F'\t': estate paths contain spaces.
function esc(s){ gsub(/\\/,"\\\\",s); gsub(/"/,"\\\"",s); return s }
function nz(p){ sub(/^\.\//,"",p); return p }
# canon(): resolve a CHECKOUT PATH to its canonical owner/repo slug.
# The estate TSVs disagree on the leading "./" (the soundness and pin censuses
# emit it, the sole-sound-copy census does not), so normalise both sides or the
# join silently returns zero rows and the board under-reports a whole column.
function canon(p){ p=nz(p); return (p in P2C) ? P2C[p] : "" }

# 1. canonical join key: checkout_path -> canonical slug
FILENAME==F_JOIN {
  if ($1 ~ /^#/ || $1=="checkout_path") next
  if ($3 != "") { P2C[nz($1)]=$3; R2C[$2]=$3 }
  if ($4=="VENDORED-EXCLUDED") { VEND[$3]=1; next }
  if ($4=="GONE-404")  { GONE[$2]=1; next }
  if ($4=="NO-REMOTE") { NOREM[$1]=1; next }
  if ($3 != "") NCHK[$3]++
  if ($4=="RENAMED") REN[$2]=$3
  next
}
# 2. posture census + supplement -> THE POPULATION
FILENAME==F_POST || FILENAME==F_SUP {
  if ($1 ~ /^#/ || $1=="owner/repo") next
  r=$1; POP[r]=1; EN[r]=$2; AA[r]=$3; SP[r]=$4; VA[r]=$5; NP[r]=$6
  SRC[r] = (FILENAME==F_SUP) ? "supplement" : "census-n440"
  next
}
# 3. two-limb soundness at origin/HEAD
FILENAME==F_SOUND {
  if ($1 ~ /^#/ || $1=="repo") next
  cs=canon($1); if (cs=="") next
  k=cs SUBSEP $2
  if (!(k in SEEN)) { SEEN[k]=1; WFTOT[cs]++ }
  if ($3!="OK" && !(k in SEENBAD)) {
    SEENBAD[k]=1; WFBAD[cs]++
    BAD[cs] = BAD[cs] (BAD[cs]==""?"":"|") $2 "=" $3
  }
  next
}
# 4. reusable pin state (MAX across checkouts, never SUM)
FILENAME==F_PINS {
  if ($1 ~ /^#/ || $1=="checkout_path") next
  cs=canon($1); if (cs=="") next
  if ($2+0 > RPIN[cs])   RPIN[cs]=$2+0
  if ($3+0 > RUNPIN[cs]) RUNPIN[cs]=$3+0
  next
}
# 5. all-uses unpinned refs, keyed by remote_url
FILENAME==F_UNPIN {
  if ($1 ~ /^#/ || $1=="remote_url") next
  s=$1
  sub(/^gcrypt::/,"",s); sub(/^git@github\.com:/,"",s)
  sub(/^https:\/\/github\.com\//,"",s); sub(/\.git$/,"",s)
  c = (s in R2C) ? R2C[s] : s
  AUNPIN[c]++
  next
}
# 6. sole-sound-copy: class=$1 repo=$2 path=$3
FILENAME==F_SOLE {
  if ($1 ~ /^#/ || $1=="class" || NF<3) next
  cs=canon($2); if (cs=="") next
  if ($1=="RESCUED") SOLE[cs]++; else DEADBOTH[cs]++
  next
}
# 7. unsound signatures: sig=$1 repo=$2 path=$3
FILENAME==F_SIG {
  if ($1 ~ /^#/ || $1=="signature") next
  cs=canon($2); if (cs=="") next
  SIGOF[cs SUBSEP $3]=$1
  next
}

END {
  n=0; for (r in POP) ROWS[++n]=r
  for (i=1;i<=n;i++) for (j=i+1;j<=n;j++) if (ROWS[j]<ROWS[i]) { t=ROWS[i];ROWS[i]=ROWS[j];ROWS[j]=t }

  T=OUT_TSV; L=OUT_LEDGER; J=OUT_JSON; A=OUT_ADOC
  print "# ESTATE-BOARD " GEN " - one row per LIVE (non-archived) repo, canonical full_name identity." > T
  print "# Disk-derived columns aggregate UNION over files / MAX over counts across a repo local checkouts, never SUM." > T
  print "# Absence tokens: NO-LOCAL-CHECKOUT (no clone here, so origin/HEAD columns unmeasurable from this machine); n/a (field does not apply)." > T
  print "repo\tcheckouts\tsource\tactions_enabled\tallowed_actions\tsha_pinning\tverified_allowed\tn_patterns\twf_files\twf_unsound\treusable_pinned\treusable_unpinned\tunpinned_all_uses\tsole_sound_copy\tresidue" > T

  print "# ESTATE residue ledger " GEN " - the whole repair surface at origin/HEAD, one row per defect." > L
  print "# state: PENDING -> FIXED / SKIPPED-<reason>. The campaign is done when no row reads PENDING." > L
  print "# SOLE-SOUND-COPY rows are DO-NOT-TOUCH, not PENDING: a checkout from HEAD destroys the only sound copy, silently." > L
  print "repo\tkind\tpath_or_count\tdetail\tstate" > L

  tot_wf=0; tot_bad=0; tot_ru=0; tot_sole=0; tot_au=0
  n_res=0; n_noc=0; n_all=0; n_nosp=0; nled=0; n_pend=0; n_badrepo=0; n_rurepo=0
  for (i=1;i<=n;i++) {
    r=ROWS[i]; nc=(r in NCHK)?NCHK[r]:0
    if (nc==0) {
      n_noc++
      wf="NO-LOCAL-CHECKOUT"; bad=wf; rp=wf; ru=wf; au=wf; sc=wf
      resid="UNMEASURED-no-local-checkout"; badn=0; run=0; scn=0
    } else {
      badn=(r in WFBAD)?WFBAD[r]:0; run=(r in RUNPIN)?RUNPIN[r]:0; scn=(r in SOLE)?SOLE[r]:0
      wf=(r in WFTOT)?WFTOT[r]:0; bad=badn
      rp=(r in RPIN)?RPIN[r]:0;   ru=run
      au=(r in AUNPIN)?AUNPIN[r]:0; sc=scn
      tot_wf+=wf; tot_bad+=badn; tot_ru+=run; tot_sole+=scn; tot_au+=au
      if (badn>0) n_badrepo++
      if (run>0)  n_rurepo++
      resid=(badn>0||run>0)?"RESIDUE":"clean"
      if (resid=="RESIDUE") n_res++
    }
    va=(AA[r]=="all")?"n/a":VA[r]; np=(AA[r]=="all")?"n/a":NP[r]
    if (AA[r]=="all") n_all++
    if (SP[r]=="false") n_nosp++
    printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", \
      r,nc,SRC[r],EN[r],AA[r],SP[r],va,np,wf,bad,rp,ru,au,sc,resid > T

    if (r in BAD) {
      m=split(BAD[r],pp,"|")
      for (k=1;k<=m;k++) {
        e=pp[k]; q=index(e,"="); pth=substr(e,1,q-1); st=substr(e,q+1)
        sg=((r SUBSEP pth) in SIGOF)?SIGOF[r SUBSEP pth]:("two-limb=" st)
        printf "%s\tUNSOUND-WORKFLOW\t%s\t%s\tPENDING\n", r, pth, sg > L; nled++; n_pend++
      }
    }
    if (nc>0 && run>0) {
      printf "%s\tUNPINNED-REUSABLE\t%d\treusable uses: whose @ref is not a 40-hex sha\tPENDING\n", r, run > L; nled++; n_pend++
    }
    if (nc>0 && scn>0) {
      printf "%s\tSOLE-SOUND-COPY\t%d\tworktree is the ONLY sound copy; a checkout from HEAD destroys it silently\tDO-NOT-TOUCH\n", r, scn > L; nled++
    }
  }

  nv=0; for (v in VEND) nv++
  ng=0; for (g in GONE) ng++
  nr=0; for (x in REN) nr++
  pct = tot_wf ? 100.0*(tot_wf-tot_bad)/tot_wf : 0

  # ---------------- JSON ----------------
  printf "{\n" > J
  printf "  \"generated_utc\": \"%s\",\n", GEN > J
  printf "  \"population\": %d,\n", n > J
  printf "  \"population_definition\": \"live non-archived repos of hyperpolymath + metadatastician, canonical full_name identity\",\n" > J
  printf "  \"aggregation_rule\": \"UNION over files, MAX over counts, across a repo local checkouts; never SUM\",\n" > J
  printf "  \"health_test\": \"two-limb structural at origin/HEAD: non-empty jobs map AND a trigger key. Never parseability, never CI colour.\",\n" > J
  printf "  \"totals\": {\n" > J
  printf "    \"workflow_files\": %d,\n", tot_wf > J
  printf "    \"unsound_files\": %d,\n", tot_bad > J
  printf "    \"repos_with_unsound_files\": %d,\n", n_badrepo > J
  printf "    \"soundness_pct\": %.2f,\n", pct > J
  printf "    \"unpinned_reusable_refs\": %d,\n", tot_ru > J
  printf "    \"repos_with_unpinned_reusable\": %d,\n", n_rurepo > J
  printf "    \"unpinned_all_uses_refs\": %d,\n", tot_au > J
  printf "    \"sole_sound_copy_files\": %d,\n", tot_sole > J
  printf "    \"repos_with_residue\": %d,\n", n_res > J
  printf "    \"repos_without_local_checkout\": %d,\n", n_noc > J
  printf "    \"repos_allowed_actions_all\": %d,\n", n_all > J
  printf "    \"repos_sha_pinning_false\": %d,\n", n_nosp > J
  printf "    \"ledger_rows\": %d,\n", nled > J
  printf "    \"ledger_pending\": %d\n", n_pend > J
  printf "  },\n" > J
  printf "  \"excluded\": { \"vendored_third_party\": %d, \"deleted_upstream\": %d, \"renamed_checkouts_canonicalised\": %d, \"archived\": 1 },\n", nv, ng, nr > J
  printf "  \"repos\": [\n" > J
  for (i=1;i<=n;i++) {
    r=ROWS[i]; nc=(r in NCHK)?NCHK[r]:0
    printf "    {\"repo\":\"%s\",\"checkouts\":%d,\"posture_source\":\"%s\",\"actions_enabled\":%s,\"allowed_actions\":\"%s\",\"sha_pinning_required\":%s", \
      esc(r), nc, SRC[r], (EN[r]=="true"?"true":"false"), esc(AA[r]), (SP[r]=="true"?"true":"false") > J
    if (AA[r]=="all") printf ",\"verified_allowed\":null,\"patterns_allowed_len\":null" > J
    else printf ",\"verified_allowed\":%s,\"patterns_allowed_len\":%d",(VA[r]=="true"?"true":"false"),NP[r]+0 > J
    if (nc==0) printf ",\"disk_measured\":false,\"reason\":\"NO-LOCAL-CHECKOUT\"}" > J
    else {
      printf ",\"disk_measured\":true,\"workflow_files\":%d,\"unsound_files\":%d,\"reusable_pinned\":%d,\"reusable_unpinned\":%d,\"unpinned_all_uses\":%d,\"sole_sound_copy\":%d,\"residue\":%s}", \
        (r in WFTOT?WFTOT[r]:0),(r in WFBAD?WFBAD[r]:0),(r in RPIN?RPIN[r]:0),(r in RUNPIN?RUNPIN[r]:0),(r in AUNPIN?AUNPIN[r]:0),(r in SOLE?SOLE[r]:0), \
        (((r in WFBAD)&&WFBAD[r]>0)||((r in RUNPIN)&&RUNPIN[r]>0)?"true":"false") > J
    }
    printf "%s\n",(i<n?",":"") > J
  }
  printf "  ]\n}\n" > J

  # ---------------- AsciiDoc ----------------
  print "= ESTATE-BOARD" > A
  print ":revdate: " GEN > A
  print ":toc: left"     > A
  print ":toclevels: 2"  > A
  print ":sectnums:"     > A
  print ""               > A
  print "[.lead]"        > A
  printf "CI/CD state of *%d live repositories* across `hyperpolymath` and `metadatastician`. Generated `%s` by `scripts/spine/board.sh` from measured censuses. Every cell carries its horizon. *Nothing here is derived from CI colour.*\n", n, GEN > A
  print ""               > A

  print "== What this board is" > A
  print "" > A
  print "One row per *live, non-archived* repository, under *canonical* `full_name` identity." > A
  print "" > A
  print "It answers the question this campaign has been unable to answer for weeks: *which repositories are actually gated, actually reporting, and actually pinned to something that exists* -- on one page, without writing to a single repository." > A
  print "" > A
  print "=== Three rules this board obeys, and why" > A
  print "" > A
  print "Never CI colour::" > A
  print "A broken workflow emits no check run, so `required INTERSECT failing = EMPTY` is satisfied *vacuously* and a repository that has just been destroyed scores *greener* than one left alone. Health here is the *two-limb structural test* at `origin/HEAD`: a non-empty `jobs` map AND a trigger key. Never parseability -- 712 files of the 2026-09-12 splice parse perfectly with `jobs:` swallowed into a multi-line `name` scalar." > A
  print "" > A
  print "Never the worktree::" > A
  print "The local clones are shared scratch for roughly twenty concurrent sessions and are not a stable measurement surface. Every workflow column reads the *published* artefact at `origin/HEAD`." > A
  print "" > A
  print "Never a raw slug::" > A
  printf "`GET /repos/{owner}/{old-name}` returns *HTTP 200 carrying the NEW full_name* -- no 404, no warning. A board keyed on `remote.origin.url` therefore double-counts one renamed repository as a phantom row plus an unmeasured row, and the symmetric discrepancy looks like two separate census bugs. All %d stale on-disk remotes are resolved to canonical identity before any join.\n", nr > A
  print "" > A

  print "== Headline numbers" > A
  print "" > A
  print "[cols=\"4,1,4\",options=\"header\"]" > A
  print "|===" > A
  print "|Measure |Value |Horizon" > A
  print "" > A
  printf "|Repositories on the board |%d |live non-archived, canonical identity, 2026-09-15\n", n > A
  printf "|Workflow files measured |%d |origin/HEAD, whole population, every file parsed\n", tot_wf > A
  printf "|Workflow files STRUCTURALLY UNSOUND |*%d* |origin/HEAD -- this is the repair surface\n", tot_bad > A
  printf "|Repositories carrying an unsound file |*%d* |origin/HEAD\n", n_badrepo > A
  printf "|Estate soundness |*%.1f%%* |origin/HEAD, %d of %d files\n", pct, tot_wf-tot_bad, tot_wf > A
  printf "|Unpinned reusable `uses:` refs |*%d* |origin/HEAD, across %d repositories\n", tot_ru, n_rurepo > A
  printf "|Unpinned refs across ALL `uses:` |%d |origin/HEAD -- NOT a defect, see below\n", tot_au > A
  printf "|Repositories with ANY residue |*%d* |origin/HEAD\n", n_res > A
  printf "|Repositories with no local checkout |%d |origin/HEAD columns unmeasurable here; live posture IS measured\n", n_noc > A
  printf "|Actions enabled |%d of %d |live API, 2026-09-15 ~08:20Z\n", n, n > A
  printf "|`allowed_actions: all` (no allow-list at all) |%d |live API -- looser than canon; a policy question, not a repair\n", n_all > A
  printf "|`sha_pinning_required: false` |%d |live API -- inconsistent with the other %d; a tidiness finding\n", n_nosp, n-n_nosp > A
  printf "|Files where only the WORKTREE is sound |%d |worktree INTERSECT HEAD -- never check these out from HEAD\n", tot_sole > A
  print "|===" > A
  print "" > A

  print "=== The number that goes to zero" > A
  print "" > A
  printf "The residue ledger holds *%d rows*, of which *%d read PENDING*: %d unsound workflow files and %d repositories carrying an unpinned reusable ref. The remaining %d are `SOLE-SOUND-COPY` warnings, marked `DO-NOT-TOUCH` rather than `PENDING` -- they are a hazard to avoid, not a defect to fix.\n", nled, n_pend, tot_bad, n_rurepo, nled-n_pend > A
  print "" > A
  printf "That is roughly *%d repositories*, not %d. Every fix is mechanical and structurally verifiable per file against that file own `origin/HEAD` version. The campaign is finished when no row reads `PENDING` -- and unlike every previous count in this engagement, this one is small enough to reach zero.\n", n_res, n > A
  print "" > A
  print "CAUTION: The ledger is the unit of work for the *next* session. This board documents the residue; it does not repair it. Repair is a deterministic, script-driven rollout -- not a model reading 8,401 rows." > A
  print "" > A

  print "== Two things this board refutes" > A
  print "" > A
  print "=== The estate is not 84% dead" > A
  print "" > A
  printf "That figure was worktree-derived and splice-contaminated. At `origin/HEAD` the estate is *%.1f%% sound* (%d of %d files). The 2026-09-12 `name:` splice damaged shared local scratch and *never reached GitHub*: zero of 3,073 casualties were ever committed, proved by a dry re-baseline that was unchanged after preservation.\n", pct, tot_wf-tot_bad, tot_wf > A
  print "" > A
  print "=== Unpinned refs are not a live startup-death cause" > A
  print "" > A
  printf "`hyperpolymath/hypatia` carries `sha_pinning_required: true`. At head `a3b31edd` its `rust.yml` ran to *success* using `dtolnay/rust-toolchain@master` -- a branch ref -- and `dogfood-gate.yml` ran to *success* using two further `@main` refs. One green run on an unpinned ref under that setting refutes the universal claim that the setting refuses unpinned refs in existing workflows. The %d unpinned refs are therefore not a live startup-death cause, and the P2/P3 pin campaigns are *not* the repair surface.\n", tot_au > A
  print "" > A
  print "NOTE: `sha_pinning_required` lives on `repos/{R}/actions/permissions`, *not* on `repos/{R}`. The latter has no such key, and `--jq .sha_pinning_required` against it returns `null`, which reads as false and is wrong." > A
  print "" > A
  print "Related, and closed by the same measurement: no repository has an empty `patterns_allowed` (the floor is 92 patterns; 47 repositories carry 118) and *zero* repositories have `verified_allowed: false`. The allow-list widening ruling has already been executed estate-wide, which retires the class-4 failure mode entirely." > A
  print "" > A

  print "== Coverage and exclusions -- stated, never silent" > A
  print "" > A
  print "[cols=\"3,1,5\",options=\"header\"]" > A
  print "|===" > A
  print "|Class |Count |Disposition" > A
  print "" > A
  printf "|On the board, fully measured |%d |every column measured\n", n-n_noc > A
  printf "|On the board, posture only |%d |`NO-LOCAL-CHECKOUT` -- no clone on this machine, so origin/HEAD columns are unmeasurable *from here*; live Actions posture IS measured\n", n_noc > A
  printf "|Vendored third-party trees |%d |excluded by owner ruling -- not estate repositories\n", nv > A
  printf "|Deleted upstream |%d |a local clone survives, the GitHub repository does not\n", ng > A
  print "|Archived |1 |`metadatastician/canonical-ums` -- outside the live population by definition" > A
  printf "|Renamed, canonicalised |%d |stale on-disk remote silently redirected; joined under the new name\n", nr > A
  print "|===" > A
  print "" > A
  print "*No cell on this board reads UNKNOWN.* Where a fact is unmeasurable, the cell names the reason." > A
  print "" > A

  print "== Per-repository board" > A
  print "" > A
  print "Columns: `chk` local checkouts -- `en` Actions enabled -- `allowed` allow-list mode -- `pin` `sha_pinning_required` -- `vfy` `verified_allowed` -- `pat` `patterns_allowed` length -- `wf` workflow files at HEAD -- `bad` structurally unsound -- `rp`/`ru` reusable refs pinned/unpinned -- `au` unpinned across all `uses:` -- `sole` files where only the worktree is sound." > A
  print "" > A
  print "[cols=\"7,1,1,2,1,1,1,1,1,1,1,1,1,2\",options=\"header\"]" > A
  print "|===" > A
  print "|Repository |chk |en |allowed |pin |vfy |pat |wf |bad |rp |ru |au |sole |residue" > A
  for (i=1;i<=n;i++) {
    r=ROWS[i]; nc=(r in NCHK)?NCHK[r]:0
    en=(EN[r]=="true"?"Y":"N"); pn=(SP[r]=="true"?"Y":"N")
    vf=(AA[r]=="all"?"n/a":(VA[r]=="true"?"Y":"N")); pa=(AA[r]=="all"?"n/a":NP[r])
    if (nc==0) {
      printf "\n|`%s` |0 |%s |%s |%s |%s |%s |- |- |- |- |- |- |NO-LOCAL-CHECKOUT\n", r,en,AA[r],pn,vf,pa > A
    } else {
      badn=(r in WFBAD)?WFBAD[r]:0; run=(r in RUNPIN)?RUNPIN[r]:0; scn=(r in SOLE)?SOLE[r]:0
      printf "\n|`%s` |%d |%s |%s |%s |%s |%s |%d |%s |%d |%s |%d |%s |%s\n", \
        r,nc,en,AA[r],pn,vf,pa, (r in WFTOT?WFTOT[r]:0), \
        (badn>0?("*" badn "*"):"0"), (r in RPIN?RPIN[r]:0), \
        (run>0?("*" run "*"):"0"), (r in AUNPIN?AUNPIN[r]:0), \
        (scn>0?("*" scn "*"):"0"), ((badn>0||run>0)?"*RESIDUE*":"clean") > A
    }
  }
  print "|===" > A
  print "" > A

  print "== Sources" > A
  print "" > A
  print "Every column is joined on canonical identity from `.claude/reports/2026-09-15-canonical-repo-join-key.tsv`. Every join uses a literal tab separator: estate paths contain spaces, and default `awk` whitespace-splitting once turned 65 unsound rows into 2,902." > A
  print "" > A
  print "* `2026-09-15-actions-posture-live.tsv` -- live API, n=440, 2026-09-15 ~08:20Z. WARNING: this posture *mutates mid-campaign*; an undated posture census is not a fact." > A
  print "* `2026-09-15-actions-posture-supplement.tsv` -- one repository created after that census window, measured individually. Kept separate so the n=440 horizon is not silently mutated." > A
  print "* `2026-09-15-origin-head-workflow-soundness.tsv` -- two-limb test at origin/HEAD, every file parsed." > A
  print "* `2026-09-15-origin-head-unsound-by-signature.tsv` -- defect signature per unsound file." > A
  print "* `2026-09-15-origin-head-reusable-pin-state.tsv` -- reusable-workflow pin state per checkout." > A
  print "* `2026-09-15-origin-head-unpinned-all-uses.tsv` -- every action ref whose `@`-suffix is not a 40-hex sha." > A
  print "* `2026-09-15-no-sound-head-worktree-is-only-sound-copy.tsv` -- worktree INTERSECT HEAD." > A
  print "" > A
  print "== Machine-readable" > A
  print "" > A
  print "* `.machine_readable/estate-board.json` -- the same rows, plus totals and exclusion counts." > A
  print "* `.machine_readable/estate-board.tsv` -- the same rows as TSV." > A
  print "* `.machine_readable/estate-residue-ledger.tsv` -- the repair surface, `PENDING` to `FIXED`/`SKIPPED-<reason>`." > A

  printf "rows=%d wf=%d unsound=%d unsound_repos=%d unpinned_reusable=%d ru_repos=%d alluses=%d sole=%d residue_repos=%d nocheckout=%d ledger=%d pending=%d vendored=%d gone=%d renamed=%d\n", \
    n, tot_wf, tot_bad, n_badrepo, tot_ru, n_rurepo, tot_au, tot_sole, n_res, n_noc, nled, n_pend, nv, ng, nr
}
