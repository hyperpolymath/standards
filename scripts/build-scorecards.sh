#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# build-scorecards.sh — regenerate COMPLIANCE-DASHBOARD.md from the per-spec
# scorecards under .machine_readable/scorecards/.
#
# This mirrors scripts/build-registry.sh exactly in spirit:
#   * The SCORECARDS are the hand-authored source of truth (one per LOCAL spec
#     in REGISTRY.a2ml, keyed by spec_id, validated by scorecard.schema.json).
#   * COMPLIANCE-DASHBOARD.md is DERIVED and MUST NOT be hand-edited.
#   * Deterministic + idempotent (no timestamps in generated output — the
#     assessed_date lives in each source scorecard). Run twice → identical.
#   * Honest. A `pass` requires evidence; an `aspirational` requirement is NEVER
#     counted as pass; a requirement with system="none" is counted as
#     un-automated, which lowers the spec's systems-coverage %. The dashboard
#     measures enforcement-vs-theatre, not aspiration.
#
# The generator also enforces the registry↔scorecard correspondence:
#   * every LOCAL registered spec SHOULD have a scorecard (missing → reported;
#     fails only in --strict),
#   * every scorecard MUST key to a registered spec (orphan → hard error).
#
# Usage:
#   bash scripts/build-scorecards.sh            # write COMPLIANCE-DASHBOARD.md
#   bash scripts/build-scorecards.sh --check    # verify in sync; non-zero on drift
#   bash scripts/build-scorecards.sh --strict   # also fail if any spec lacks a scorecard
#   bash scripts/build-scorecards.sh --verify   # RUN every pass-row's `check`;
#                                               #   a claimed pass whose check fails is a hard error
#   (flags may combine, e.g. --check --strict --verify)
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

MODE="write"; STRICT=0; VERIFY=0
for arg in "$@"; do
  case "$arg" in
    --check)  MODE="check" ;;
    --strict) STRICT=1 ;;
    --verify) VERIFY=1 ;;
    *) echo "error: unknown option: $arg" >&2; exit 2 ;;
  esac
done

REGISTRY=".machine_readable/REGISTRY.a2ml"
SCDIR=".machine_readable/scorecards"
DASHBOARD="COMPLIANCE-DASHBOARD.md"
SCHEMA="$SCDIR/scorecard.schema.json"

[ -f "$REGISTRY" ] || { echo "error: $REGISTRY not found (run: just registry)" >&2; exit 2; }

# ---------------------------------------------------------------------------
# Local spec inventory from REGISTRY.a2ml: emit "id<TAB>name" per LOCAL [[spec]]
# (external pointers — kind="external" — are excluded: their SSOT lives in
# another repo and cannot be self-assessed here).
# ---------------------------------------------------------------------------
local_specs() {
  awk '
    /^\[\[spec\]\]/ { in_spec=1; id=""; name=""; ext=0; next }
    in_spec && /^id = "/       { id=$0; sub(/^id = "/,"",id); sub(/".*$/,"",id) }
    in_spec && /^name = "/      { name=$0; sub(/^name = "/,"",name); sub(/".*$/,"",name) }
    in_spec && /^kind = "external"/ { ext=1 }
    in_spec && /^route = "/     { if (id!="" && ext==0) printf "%s\t%s\n", id, name; in_spec=0 }
  ' "$REGISTRY"
}

# ---------------------------------------------------------------------------
# Parse one scorecard file. Emits TSV lines: tier<TAB>status<TAB>has_system<TAB>has_check
#   tier ∈ must|should|could ; status ∈ pass|fail|aspirational|manual-only
#   has_system ∈ 1 (system present and != "none") | 0
#   has_check  ∈ 1 (an executable `check` is present) | 0
# Also validates: pass requires non-empty evidence; status is from the enum.
# Exits non-zero (message on stderr) on a malformed scorecard.
# ---------------------------------------------------------------------------
parse_scorecard() {
  awk -v file="$1" '
    function fail(msg) { printf "SCORECARD ERROR [%s]: %s\n", file, msg > "/dev/stderr"; errors++ }
    function flush() {
      if (tier=="") return
      if (id=="")     fail("a [[" tier "]] block has no id")
      if (status=="") { fail((id==""?"(unnamed)":id) ": missing status"); return }
      if (status!="pass" && status!="fail" && status!="aspirational" && status!="manual-only")
        fail(id ": invalid status \"" status "\"")
      if (status=="pass" && evidence=="")
        fail(id ": status=pass requires evidence")
      printf "%s\t%s\t%s\t%s\n", tier, status, (sys!="" && sys!="none") ? 1 : 0, (chk!="") ? 1 : 0
    }
    /^\[\[(must|should|could)\]\]/ {
      flush()
      tier=$0; sub(/^\[\[/,"",tier); sub(/\]\].*$/,"",tier)
      id=""; status=""; sys=""; evidence=""; chk=""; next
    }
    /^\[scorecard\]/ { flush(); tier=""; next }
    tier!="" && /^id = "/       { id=$0;       sub(/^id = "/,"",id);             sub(/".*$/,"",id) }
    tier!="" && /^status = "/   { status=$0;   sub(/^status = "/,"",status);     sub(/".*$/,"",status) }
    tier!="" && /^system = "/   { sys=$0;      sub(/^system = "/,"",sys);        sub(/".*$/,"",sys) }
    tier!="" && /^evidence = "/ { evidence=$0; sub(/^evidence = "/,"",evidence); sub(/".*$/,"",evidence) }
    tier!="" && /^check = "/    { chk=$0;      sub(/^check = "/,"",chk);         sub(/".*$/,"",chk) }
    END { flush(); if (errors>0) exit 1 }
  ' "$1"
}

# ---------------------------------------------------------------------------
# Extract executable checks from one scorecard, one record per row that HAS a
# `check`. Record format (US-delimited, \x1f — never appears in shell text):
#   id US status US check
# The check VALUE may contain TOML-escaped quotes (\") and backslashes (\\):
# strip only the TRAILING quote (not the first embedded one) and unescape,
# otherwise any check containing a quoted argument is silently truncated into
# a different (broken) command.
# ---------------------------------------------------------------------------
extract_checks() {
  awk '
    function unescape(s) {
      gsub(/\\\\/, "\x01", s)   # protect literal backslashes first
      gsub(/\\"/, "\"", s)      # \"  -> "
      gsub(/\x01/, "\\", s)     # restore backslashes
      return s
    }
    function flush() {
      if (tier=="" || chk=="") return
      printf "%s\x1f%s\x1f%s\n", id, status, unescape(chk)
    }
    /^\[\[(must|should|could)\]\]/ {
      flush()
      tier=$0; sub(/^\[\[/,"",tier); sub(/\]\].*$/,"",tier)
      id=""; status=""; chk=""; next
    }
    /^\[scorecard\]/ { flush(); tier=""; next }
    tier!="" && /^id = "/     { id=$0;     sub(/^id = "/,"",id);         sub(/".*$/,"",id) }
    tier!="" && /^status = "/ { status=$0; sub(/^status = "/,"",status); sub(/".*$/,"",status) }
    tier!="" && /^check = "/  { chk=$0;    sub(/^check = "/,"",chk);     sub(/"[[:space:]]*$/,"",chk) }
    END { flush() }
  ' "$1"
}

# ---------------------------------------------------------------------------
# --verify: RUN every pass-row check. DYADT applied to the scorecards —
# a `pass` is only as good as a check that exits 0 right now.
#   * pass + check exits 0   -> grounded pass (good)
#   * pass + check exits !=0 -> HARD ERROR (the claimed pass is not real)
#   * pass + no check        -> self-asserted (reported; visible debt)
#   * fail/other + check exits 0 -> stale-fail (advisory: re-grade candidate)
# ---------------------------------------------------------------------------
run_verify() {
  local rc=0 grounded=0 selfasserted=0 broken=0 stale=0 f base id status chk crc
  echo "== scorecard --verify: running pass-row checks =="
  shopt -s nullglob
  for f in "$SCDIR"/*.scorecard.a2ml; do
    base="$(basename "$f" .scorecard.a2ml)"
    # count self-asserted passes (pass rows minus pass rows with checks)
    local p_total p_chk
    p_total="$(parse_scorecard "$f" | awk -F'\t' '$2=="pass"' | wc -l)"
    p_chk="$(parse_scorecard "$f"   | awk -F'\t' '$2=="pass" && $4==1' | wc -l)"
    selfasserted=$((selfasserted + p_total - p_chk))
    while IFS=$'\x1f' read -r id status chk; do
      [ -z "$chk" ] && continue
      # Checks run read-only from the repo root; they must not mutate.
      ( cd "$(git rev-parse --show-toplevel)" && bash -c "$chk" ) >/dev/null 2>&1; crc=$?
      if [ "$status" = "pass" ]; then
        if [ "$crc" -eq 0 ]; then
          grounded=$((grounded + 1))
        else
          echo "  ❌ $base/$id: claimed PASS but check exited $crc — the pass is not real"
          echo "     check: $chk"
          broken=$((broken + 1)); rc=1
        fi
      else
        if [ "$crc" -eq 0 ]; then
          echo "  ℹ️  $base/$id: status=$status but its check now passes — re-grade candidate (stale-fail)"
          stale=$((stale + 1))
        fi
      fi
    done < <(extract_checks "$f")
  done
  shopt -u nullglob
  echo "  ── verify: $grounded grounded pass · $broken broken pass · $selfasserted self-asserted pass · $stale stale-fail"
  if [ "$broken" -gt 0 ]; then
    echo "❌ --verify FAILED: $broken claimed pass(es) whose check does not hold" >&2
  fi
  return $rc
}

# Read a [scorecard] header field.
sc_field() { # file key
  grep -E "^$2 = \"" "$1" 2>/dev/null | head -1 | sed -E "s/^$2 = \"//; s/\".*$//"
}

# ---------------------------------------------------------------------------
# Emit the dashboard to stdout.
# ---------------------------------------------------------------------------
emit_dashboard() {
  local total_specs=0 scored_specs=0
  local g_must=0 g_must_pass=0 g_must_fail=0 g_reqs=0 g_reqs_sys=0

  cat <<'HEADER'
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- COMPLIANCE-DASHBOARD.md — DERIVED from .machine_readable/scorecards/*.scorecard.a2ml -->
<!-- GENERATED FILE — DO NOT EDIT BY HAND. Run: just scorecards (scripts/build-scorecards.sh) -->

# Standards Compliance Dashboard (derived)

> Generated from `.machine_readable/scorecards/<spec-id>.scorecard.a2ml` by
> `scripts/build-scorecards.sh`. One scorecard per LOCAL spec in
> `.machine_readable/REGISTRY.a2ml`. Do not edit by hand — edit the scorecards.
>
> **How to read this.** Each spec is audited as MUST / SHOULD / COULD
> requirements. **MUST-status** is the compliance verdict: ✅ met (every MUST
> passes or is manual-only) or ❌ gap (some MUST fails). **Systems coverage**
> is the share of requirements with a real mechanical check (`system` ≠ `none`)
> — the honest measure of *enforcement vs. assertion*. **Aspirational**
> requirements (intentionally-unreachable reach targets) are never counted as
> passing.

HEADER

  # Per-spec table
  printf '## Per-spec scorecards\n\n'
  printf '| Spec | MUST status | MUST (pass/total) | SHOULD (pass/total) | COULD (pass/total) | Systems coverage | Grounded passes | Assessed |\n'
  printf '|---|---|---|---|---|---|---|---|\n'

  local g_pass=0 g_pass_chk=0
  local missing=()
  while IFS=$'\t' read -r id name; do
    [ -z "$id" ] && continue
    total_specs=$((total_specs + 1))
    local file="$SCDIR/$id.scorecard.a2ml"
    if [ ! -f "$file" ]; then
      missing+=("$id")
      printf '| `%s` | ⚠️ no scorecard | – | – | – | – | – | – |\n' "$id"
      continue
    fi
    scored_specs=$((scored_specs + 1))

    # tallies
    local m_t=0 m_p=0 m_f=0 s_t=0 s_p=0 c_t=0 c_p=0 reqs=0 reqs_sys=0 p_all=0 p_chk=0
    local parsed; parsed="$(parse_scorecard "$file")"
    while IFS=$'\t' read -r tier status has_sys has_chk; do
      [ -z "$tier" ] && continue
      reqs=$((reqs + 1)); [ "$has_sys" = "1" ] && reqs_sys=$((reqs_sys + 1))
      if [ "$status" = pass ]; then
        p_all=$((p_all + 1)); [ "$has_chk" = "1" ] && p_chk=$((p_chk + 1))
      fi
      case "$tier" in
        must)   m_t=$((m_t+1)); [ "$status" = pass ] && m_p=$((m_p+1)); [ "$status" = fail ] && m_f=$((m_f+1)) ;;
        should) s_t=$((s_t+1)); [ "$status" = pass ] && s_p=$((s_p+1)) ;;
        could)  c_t=$((c_t+1)); [ "$status" = pass ] && c_p=$((c_p+1)) ;;
      esac
    done <<< "$parsed"

    local verdict; if [ "$m_f" -gt 0 ]; then verdict="❌ gap"; else verdict="✅ met"; fi
    local cov="n/a"
    [ "$reqs" -gt 0 ] && cov="$(awk "BEGIN{printf \"%d%%\", ($reqs_sys/$reqs)*100}")"
    local grounded="–"
    [ "$p_all" -gt 0 ] && grounded="${p_chk}/${p_all}"
    local assessed; assessed="$(sc_field "$file" assessed_date)"

    printf '| `%s` | %s | %d/%d | %d/%d | %d/%d | %s | %s | %s |\n' \
      "$id" "$verdict" "$m_p" "$m_t" "$s_p" "$s_t" "$c_p" "$c_t" "$cov" "$grounded" "${assessed:-–}"

    g_must=$((g_must + m_t)); g_must_pass=$((g_must_pass + m_p)); g_must_fail=$((g_must_fail + m_f))
    g_reqs=$((g_reqs + reqs)); g_reqs_sys=$((g_reqs_sys + reqs_sys))
    g_pass=$((g_pass + p_all)); g_pass_chk=$((g_pass_chk + p_chk))
  done < <(local_specs)

  # Rollup
  local est_cov="n/a"
  [ "$g_reqs" -gt 0 ] && est_cov="$(awk "BEGIN{printf \"%d%%\", ($g_reqs_sys/$g_reqs)*100}")"
  local est_grounded="n/a"
  [ "$g_pass" -gt 0 ] && est_grounded="$(awk "BEGIN{printf \"%d%%\", ($g_pass_chk/$g_pass)*100}")"
  printf '\n## Estate rollup\n\n'
  printf -- '- **Specs registered (local):** %d\n' "$total_specs"
  printf -- '- **Specs with a scorecard:** %d / %d\n' "$scored_specs" "$total_specs"
  printf -- '- **MUST requirements:** %d passing / %d total (%d failing)\n' "$g_must_pass" "$g_must" "$g_must_fail"
  printf -- '- **Estate systems coverage:** %s of %d graded requirements have a mechanical check\n' "$est_cov" "$g_reqs"
  printf -- '- **Grounded passes:** %d / %d (%s) pass rows carry an executable `check` run by `--verify`\n' "$g_pass_chk" "$g_pass" "$est_grounded"
  if [ "${#missing[@]}" -gt 0 ]; then
    printf -- '- **Specs still needing a scorecard (%d):** %s\n' "${#missing[@]}" "$(printf '`%s` ' "${missing[@]}")"
  fi

  cat <<'FOOTER'

## How this dashboard stays honest

```
scorecards/*.scorecard.a2ml ──► scripts/build-scorecards.sh ──► COMPLIANCE-DASHBOARD.md
        (hand-authored)                      │
   validated vs scorecard.schema.json        ▼
                                    just scorecards-check (CI)
```

- A `pass` requires cited `evidence`; the generator rejects a pass without it.
- `aspirational` requirements never count as passing (no intuition-plucked
  Grade-A gate can inflate a score — standards#446).
- `system = "none"` is legal but visible, and lowers systems coverage.
- A pass MAY carry an executable `check`; `--verify` RUNS every such check and
  **fails loudly if a claimed pass does not hold right now** (DYADT applied to
  the scorecards themselves). Passes without a check are reported as
  self-asserted — visible debt, tracked by the Grounded column.
- Regenerate after editing any scorecard: `just scorecards`.
FOOTER
}

# ---------------------------------------------------------------------------
# Orphan check: every scorecard must key to a LOCAL registered spec.
# ---------------------------------------------------------------------------
check_orphans() {
  local ids; ids="$(local_specs | cut -f1)"
  local rc=0 f base id
  shopt -s nullglob
  for f in "$SCDIR"/*.scorecard.a2ml; do
    base="$(basename "$f" .scorecard.a2ml)"
    id="$(sc_field "$f" spec_id)"
    if [ "$base" != "$id" ]; then
      echo "ORPHAN: $f — filename '$base' != spec_id '$id'" >&2; rc=1; fi
    if ! grep -qx "$id" <<< "$ids"; then
      echo "ORPHAN: $f — spec_id '$id' is not a LOCAL spec in $REGISTRY" >&2; rc=1; fi
  done
  shopt -u nullglob
  return $rc
}

# ---------------------------------------------------------------------------
# Optional JSON-schema validation of every scorecard, if a validator is present.
# (Best-effort: the format is TOML-ish, so we validate the parsed logical form
# via parse_scorecard, which enforces the schema's load-bearing rules. A real
# JSON-schema check runs estate-side; here we ensure parseability + rules.)
# ---------------------------------------------------------------------------
validate_all() {
  local rc=0 f
  shopt -s nullglob
  for f in "$SCDIR"/*.scorecard.a2ml; do
    parse_scorecard "$f" >/dev/null || rc=1
  done
  shopt -u nullglob
  return $rc
}

# --- run ---------------------------------------------------------------------
validate_all || { echo "error: one or more scorecards are malformed (see above)" >&2; exit 1; }
check_orphans || { echo "error: orphan scorecard(s) (see above)" >&2; exit 1; }

# Missing-scorecard gate (strict only).
if [ "$STRICT" = "1" ]; then
  miss="$(comm -23 <(local_specs | cut -f1 | sort) <(ls "$SCDIR"/*.scorecard.a2ml 2>/dev/null | xargs -n1 basename 2>/dev/null | sed 's/\.scorecard\.a2ml$//' | sort) || true)"
  if [ -n "$miss" ]; then
    echo "STRICT: the following LOCAL specs have no scorecard:" >&2
    echo "$miss" | sed 's/^/  - /' >&2
    exit 1
  fi
fi

# Executable grounding gate (--verify): run every pass-row check.
if [ "$VERIFY" = "1" ]; then
  run_verify || exit 1
fi

if [ "$MODE" = "check" ]; then
  tmp="$(mktemp)"
  emit_dashboard > "$tmp"
  if ! diff -q "$tmp" "$DASHBOARD" >/dev/null 2>&1; then
    echo "DRIFT: $DASHBOARD is stale — run 'just scorecards'"; rm -f "$tmp"; exit 1
  fi
  rm -f "$tmp"
  echo "OK: $DASHBOARD is in sync with the scorecards."
  exit 0
fi

emit_dashboard > "$DASHBOARD"
echo "Wrote $DASHBOARD."
