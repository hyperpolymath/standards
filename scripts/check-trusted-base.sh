#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# check-trusted-base.sh
#
# Implements the enforcement leg of the estate Trusted-Base Reduction Policy
# (docs/TRUSTED-BASE-REDUCTION-POLICY.adoc — standards#203).
#
# For every soundness-relevant escape hatch in a proof-bearing repo, verifies
# the hatch is one of:
#   (a) discharged — entry in `docs/proof-debt.md` §(a)
#   (b) budgeted   — entry in §(b) AND preceded by a `TRUSTED:` leading comment
#   (c) necessary  — entry in §(c) AND preceded by an `AXIOM:` leading comment
#   (d) debt       — entry in §(d) with deadline + owner (still counts as
#                    documented; CI passes but the entry is enumerated in the
#                    CI summary)
#
# Markers searched:
#   Coq      *.v       — `^Axiom`, `^Admitted`, `admit\.`
#   Lean     *.lean    — `\bsorry\b`, `^axiom\b`
#   Agda     *.agda    — `^postulate`, `^\s*postulate`
#   Idris2   *.idr*    — `believe_me`, `really_believe_me`, `assert_total`,
#                        top-level `^partial$`, `^%default partial`
#   F\*      *.fst     — `assume val`, `admit_p`
#   Rust/Hs  *.rs|*.hs — `unsafePerformIO`, `unsafeCoerce` (soundness-relevant)
#
# Exit codes:
#   0 — repo has no proof-bearing files, OR every marker is documented.
#   1 — at least one marker is undocumented (neither annotated inline nor
#       enumerated in docs/proof-debt.md).
#   2 — invocation/environment error.
#
# Wired into governance-reusable.yml as the `trusted-base` job.

set -u

repo_root="${1:-.}"
cd "$repo_root" || { echo "ERROR: cannot cd to $repo_root" >&2; exit 2; }

# ─────────────────────────────────────────────────────────────────────────────
# Find all proof-bearing files (excluding vendored / build artefacts).
# ─────────────────────────────────────────────────────────────────────────────
PRUNE_PATHS=(-path '*/.git' -prune -o
             -path '*/.lake' -prune -o
             -path '*/_build' -prune -o
             -path '*/build' -prune -o
             -path '*/target' -prune -o
             -path '*/node_modules' -prune -o
             -path '*/dist' -prune -o
             -path '*/.venv' -prune -o
             -path '*/vendored' -prune -o)

proof_files=$(find . "${PRUNE_PATHS[@]}" -type f \( \
  -name '*.v'    -o -name '*.lean'  -o -name '*.agda' -o \
  -name '*.idr'  -o -name '*.idr2'  -o -name '*.fst'  -o \
  -name '*.dfy'  -o -name '*.tla'   -o -name '*.ads'  -o -name '*.adb' \) -print)

unsafe_files=$(find . "${PRUNE_PATHS[@]}" -type f \( \
  -name '*.rs' -o -name '*.hs' \) -print)

if [ -z "$proof_files" ]; then
  echo "[OK] No proof-bearing files in this repo; check-trusted-base.sh is a no-op."
  exit 0
fi

# ─────────────────────────────────────────────────────────────────────────────
# Gather every marker — one row per (file, line, kind, identifier).
# ─────────────────────────────────────────────────────────────────────────────
markers_tsv=$(mktemp)
trap 'rm -f "$markers_tsv"' EXIT

is_comment_line() {
  # Heuristic: line whose first non-whitespace chars are a comment marker.
  # Returns 0 (true) when the line is a comment.
  local trimmed
  trimmed="$(echo "$1" | sed -E 's/^[[:space:]]+//')"
  case "$trimmed" in
    --*|"||| "*|"|||"*|//*|/\**|"* "*|"(*"*|"#"*) return 0 ;;
  esac
  return 1
}

emit_marker() {
  # 1=file, 2=line, 3=kind, 4=identifier-or-context
  # Skip if the line itself is a comment line (false positives — talking *about*
  # the construct rather than using it).
  if is_comment_line "$4"; then
    return 0
  fi
  printf '%s\t%s\t%s\t%s\n' "$1" "$2" "$3" "$4" >> "$markers_tsv"
}

# Coq Axiom / Admitted
echo "$proof_files" | grep -E '\.v$' | while read -r f; do
  [ -z "$f" ] && continue
  grep -nE '^[[:space:]]*(Axiom|Admitted|admit\.)' "$f" 2>/dev/null | while IFS=: read -r ln rest; do
    emit_marker "$f" "$ln" "coq-axiom-or-admit" "$(echo "$rest" | head -c 80)"
  done
done

# Lean sorry / axiom
echo "$proof_files" | grep -E '\.lean$' | while read -r f; do
  [ -z "$f" ] && continue
  grep -nE '\bsorry\b|^[[:space:]]*axiom[[:space:]]' "$f" 2>/dev/null | while IFS=: read -r ln rest; do
    emit_marker "$f" "$ln" "lean-sorry-or-axiom" "$(echo "$rest" | head -c 80)"
  done
done

# Agda postulate (top-level only)
echo "$proof_files" | grep -E '\.agda$' | while read -r f; do
  [ -z "$f" ] && continue
  grep -nE '^[[:space:]]*postulate' "$f" 2>/dev/null | while IFS=: read -r ln rest; do
    emit_marker "$f" "$ln" "agda-postulate" "$(echo "$rest" | head -c 80)"
  done
done

# Idris2 believe_me / assert_total / really_believe_me
echo "$proof_files" | grep -E '\.(idr|idr2)$' | while read -r f; do
  [ -z "$f" ] && continue
  grep -nE 'believe_me|really_believe_me|assert_total' "$f" 2>/dev/null | while IFS=: read -r ln rest; do
    # Skip comment-only mentions (the entire line is a comment)
    case "$rest" in
      *"||| "*|*"-- "*"believe_me"*) [ -n "$(echo "$rest" | grep -E '^[[:space:]]*\|\|\||^[[:space:]]*--')" ] && continue ;;
    esac
    emit_marker "$f" "$ln" "idris-believe-or-assert" "$(echo "$rest" | head -c 80)"
  done
done

# Idris2 top-level partial
echo "$proof_files" | grep -E '\.(idr|idr2)$' | while read -r f; do
  [ -z "$f" ] && continue
  grep -nE '^partial$|^%default[[:space:]]+partial' "$f" 2>/dev/null | while IFS=: read -r ln rest; do
    emit_marker "$f" "$ln" "idris-partial" "$(echo "$rest" | head -c 80)"
  done
done

# F* assume val / admit_p
echo "$proof_files" | grep -E '\.fst$' | while read -r f; do
  [ -z "$f" ] && continue
  grep -nE 'assume[[:space:]]+val|admit_p' "$f" 2>/dev/null | while IFS=: read -r ln rest; do
    emit_marker "$f" "$ln" "fstar-assume-or-admit" "$(echo "$rest" | head -c 80)"
  done
done

# Rust/Haskell soundness-relevant escapes (only inside src/)
if [ -n "$unsafe_files" ]; then
  echo "$unsafe_files" | head -2000 | while read -r f; do
    [ -z "$f" ] && continue
    grep -nE 'unsafePerformIO|unsafeCoerce' "$f" 2>/dev/null | while IFS=: read -r ln rest; do
      # Skip if this is a comment line
      case "$rest" in
        *"//"*"unsafePerformIO"*|*"--"*"unsafePerformIO"*)
          [ -n "$(echo "$rest" | grep -E '^[[:space:]]*//|^[[:space:]]*--')" ] && continue ;;
      esac
      emit_marker "$f" "$ln" "rust-or-hs-unsafe" "$(echo "$rest" | head -c 80)"
    done
  done
fi

marker_count=$(wc -l < "$markers_tsv")

if [ "$marker_count" -eq 0 ]; then
  echo "[OK] No soundness-relevant escape hatches detected."
  exit 0
fi

echo "[INFO] Found $marker_count soundness-relevant escape hatch(es)."

# ─────────────────────────────────────────────────────────────────────────────
# Verify each marker is documented:
#   - inline `TRUSTED:` or `AXIOM:` comment within 5 lines preceding, OR
#   - enumerated in docs/proof-debt.md (substring match on file:line OR on the
#     identifier captured)
# ─────────────────────────────────────────────────────────────────────────────
debt_docs=()
for cand in docs/proof-debt.md docs/proof-debt.adoc PROOF-NEEDS.md docs/PROOF-NEEDS.md; do
  if [ -f "$cand" ]; then
    debt_docs+=("$cand")
  fi
done

if [ ${#debt_docs[@]} -eq 0 ]; then
  echo "[ERROR] No docs/proof-debt.md (or equivalent) found, but $marker_count escape hatches exist."
  echo "[ERROR] Seed one per the schema at hyperpolymath/standards/docs/TRUSTED-BASE-REDUCTION-POLICY.adoc."
  exit 1
fi

echo "[OK] proof-debt document(s) found: ${debt_docs[*]}"

# For each marker, check documentation
undocumented=0
while IFS=$'\t' read -r f ln kind ctx; do
  # Strip leading ./
  f_clean="${f#./}"

  # 1. Inline TRUSTED:/AXIOM: comment on any of the 5 lines preceding the marker
  local_start=$(( ln - 5 ))
  [ "$local_start" -lt 1 ] && local_start=1
  if sed -n "${local_start},${ln}p" "$f" 2>/dev/null | grep -qE 'TRUSTED:|AXIOM:'; then
    continue
  fi

  # 2. Mention in ANY of the proof-debt documents — match on `<file>:<line>`
  #    substring, or on the file path alone (in which case we accept any
  #    reference).
  documented_in=""
  for doc in "${debt_docs[@]}"; do
    if grep -qF "$f_clean:$ln" "$doc" 2>/dev/null; then
      documented_in="$doc"
      break
    fi
    if grep -qF "$f_clean" "$doc" 2>/dev/null; then
      documented_in="$doc"
      break
    fi
  done
  if [ -n "$documented_in" ]; then
    continue
  fi

  # Undocumented.
  echo "[ERROR] Undocumented escape hatch at $f_clean:$ln ($kind):"
  echo "        $ctx"
  echo "        Annotate with a 'TRUSTED:' or 'AXIOM:' leading comment, or"
  echo "        enumerate in any of: ${debt_docs[*]}"
  undocumented=$((undocumented + 1))
done < "$markers_tsv"

if [ "$undocumented" -gt 0 ]; then
  echo ""
  echo "[ERROR] $undocumented/$marker_count escape hatch(es) are undocumented."
  echo "[ERROR] Each must be annotated inline OR enumerated in one of: ${debt_docs[*]}"
  echo "[ERROR] See https://github.com/hyperpolymath/standards/blob/main/docs/TRUSTED-BASE-REDUCTION-POLICY.adoc"
  exit 1
fi

echo "[OK] All $marker_count escape hatch(es) are documented (inline annotation or entry in: ${debt_docs[*]})."
exit 0
