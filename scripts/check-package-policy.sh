#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# check-package-policy.sh — gate on the Guix-primary / Nix-fallback policy.
#
# Replaces the echo-only "Enforce Guix primary / Nix fallback" step in
# governance-reusable.yml, whose every branch echoed and which terminated with
# an unconditional `✅ Package policy check passed` (standards#505). It could
# not detect a violation, and it claimed a pass over any input.
#
# POLICY — canonical source is `rhodium-standard-repositories/spec/
# LANGUAGE-POLICY.adoc` §Package Management, NOT CLAUDE.md:
#
#   RULED 2026-05-18 (estate-wide): Guix primary + sealed-container escape;
#   NO Nix mirror. One packager per repo. A `flake.nix` that only mirrors a
#   Guix manifest is drift to remove, not a fallback. A second packager is
#   permitted only where it is the *sole* source of a *specific named*
#   dependency, and that dependency is documented as the reason.
#   **Supersedes the prior "Nix fallback everywhere" rule.**
#
# Tiers: Guix (guix.scm/manifest.scm) is PRIMARY; a sealed container
# (Containerfile, Podman/Svalinn-sealed) is the ESCAPE HATCH for the
# not-in-Guix / non-free tail. Nix is NOT a tier.
#
# This script previously cited CLAUDE.md and printed
# `✅ Nix package management detected (fallback)`. CLAUDE.md's packaging
# section is STALE — it still describes Nix as a fallback, in 472 copies
# estate-wide — and CLAUDE.md itself defers to LANGUAGE-POLICY.adoc as
# canonical, so the .adoc wins. Blessing a flake as compliant is what let the
# 2026-07-21 remediation sweep ship `flake.nix` to 59 repos that should have
# received Guix or a container.
#
# It also had NO sealed-container detection at all, so the policy's own escape
# hatch could not satisfy the policy — a repo doing exactly the right thing for
# the not-in-Guix tail was reported as having no packaging.
#
# PREDICATE — deliberately tightened. The previous step accepted *any* `*.scm`
# anywhere in the tree as proof of "Guix package management detected", which a
# stray Guile source file satisfies without any packaging existing. This script
# requires a genuine packaging artefact (guix.scm / manifest.scm / channels.scm
# / .guix-channel) and prunes vendored trees so a dependency's file cannot
# satisfy the policy on the repo's behalf. Measured 2026-07-21 over the 412 real
# repo-root callers, tightening moved only 13 repos (57 -> 70 failing), so the
# honest predicate is nearly free.
#
# GRACE WINDOW: 70/412 callers (17%) currently satisfy neither. They warn until
# the cutoff, then fail. The cutoff is a real date that flips itself with no
# further edit to this file.
#
# NOTE ON LOCKFILES: the replaced step also grepped `git diff HEAD~1` for
# package-lock.json / yarn.lock / Gemfile.lock / Pipfile.lock / poetry.lock.
# That branch was inert — governance checks out at `ref: github.sha` without
# `fetch-depth: 0`, so `HEAD~1` does not resolve and the command was swallowed
# by `2>/dev/null` / `|| true`. It is dropped here rather than kept as a stub:
# the blocking rule for lockfiles is hypatia `cicd_rules/nodejs_detected`.
# Broadening that rule beyond package-lock.json is tracked as follow-up.
#
# Usage: check-package-policy.sh [repo-root]
#
# Environment (test seams — the shipped policy is the default in each case):
#   ENFORCE_PACKAGE_POLICY_FROM  YYYY-MM-DD; enforcement begins ON this date.
#   ENFORCE_NIX_RETIREMENT_FROM  YYYY-MM-DD; date Nix-only stops warning and
#                                starts failing. Owner-set: 2026-06-01.
#   PKG_TODAY                    YYYY-MM-DD; overrides "now" so the pre-cutoff
#                                and post-cutoff branches are both testable.
#
# Exit: 0 = pass (or in-grace warning), 1 = policy failure or bad configuration.

set -euo pipefail

ROOT="${1:-.}"

ENFORCE_PACKAGE_POLICY_FROM="${ENFORCE_PACKAGE_POLICY_FROM:-2026-08-21}"
TODAY="${PKG_TODAY:-$(date -u +%Y-%m-%d)}"

# An unparseable cutoff would make the comparison below pick the grace branch
# forever, restoring the fake gate this script replaces. Refuse to run.
valid_date() {
  case "$1" in
    [0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]) return 0 ;;
    *) return 1 ;;
  esac
}
require_date() {
  local name="$1" value="$2"
  if ! valid_date "$value"; then
    echo "::error::check-package-policy: $name='$value' is not YYYY-MM-DD."
    echo "Refusing to run: an unparseable cutoff would silently disarm this gate."
    exit 1
  fi
}
require_date ENFORCE_PACKAGE_POLICY_FROM "$ENFORCE_PACKAGE_POLICY_FROM"
require_date PKG_TODAY "$TODAY"

if [ ! -d "$ROOT" ]; then
  echo "::error::check-package-policy: '$ROOT' is not a directory."
  exit 1
fi

# Vendored trees cannot satisfy the policy on the repo's behalf.
PRUNE=( -name .git -o -name node_modules -o -name deps -o -name .lake -o -name vendor )

# NB: `find … | head -1` under `set -o pipefail` is a SIGPIPE race — head exits
# after the first line, find keeps writing, takes SIGPIPE, and pipefail
# propagates the non-zero status, aborting the script under `set -e`. It only
# manifests on trees large enough that find is still running when head exits, so
# small fixtures never catch it (it red two real estate repos intermittently
# while every unit fixture passed). `|| true` neutralises the pipeline status;
# the guix/nix decision is made from the captured value, not the exit code.
find_first() {
  local out
  out="$(find "$ROOT" \( "${PRUNE[@]}" \) -prune -o \( "$@" \) -print 2>/dev/null | head -1 || true)"
  printf '%s' "$out"
}

GUIX="$(find_first -name guix.scm -o -name manifest.scm -o -name channels.scm -o -name .guix-channel)"
NIX="$(find_first -name flake.nix -o -name default.nix -o -name shell.nix)"
# Sealed container — the policy's named escape hatch, previously undetectable.
# `Containerfile*` and `Dockerfile*` both count: the estate standardises on
# Podman/Containerfile, but a repo already carrying a Dockerfile is served by
# the same escape hatch and should not be told it has no packaging.
CONTAINER="$(find_first -name 'Containerfile*' -o -name 'Dockerfile*')"

if [ -n "$GUIX" ]; then
  echo "✅ Guix package management detected (primary): ${GUIX#"$ROOT"/}"
  exit 0
fi

# A Containerfile only counts if it BUILDS something. The estate scaffold ships
# a template whose every install/build line is a commented `# TODO:` example —
# measured 2026-07-27: 17 of 60 estate Containerfiles are that stub. Accepting
# them on presence alone reproduces exactly the fault this script was written to
# remove (standards#505 accepted any *.scm as "Guix detected"). A stub provides
# no environment, so it is not packaging.
#
# The predicate is deliberately cheap and syntactic: at least one ACTIVE
# RUN / ENTRYPOINT / CMD instruction. It cannot prove the image is useful, but
# it does separate "someone filled this in" from "this is the untouched
# template", which is the distinction that matters at gate time.
if [ -n "$CONTAINER" ]; then
  if grep -qE '^[[:space:]]*(RUN|ENTRYPOINT|CMD)[[:space:]]' "$CONTAINER"; then
    echo "✅ Sealed-container packaging detected (escape hatch): ${CONTAINER#"$ROOT"/}"
    echo "::notice::Guix is the estate primary; a sealed container is the" \
         "accepted escape hatch for the not-in-Guix / non-free tail."
    exit 0
  fi
  echo "::warning::${CONTAINER#"$ROOT"/} is the UNFILLED scaffold template —" \
       "every install/build step is a commented '# TODO:' example, so it" \
       "provides no environment and does not satisfy the policy."
  CONTAINER=""
fi

# Nix-only. Under the 2026-05-18 ruling this is NOT compliance — Nix is not a
# tier — but it is also not the same as having no packaging at all, and the
# repos in this state are overwhelmingly there because a *sweep put them there*
# rather than through any author's choice. So it warns until the retirement
# date, then fails. It never prints a ✅.
#
# ⚠ SEQUENCING — read before changing ENFORCE_NIX_RETIREMENT_FROM.
# Nix retirement must TRAIL per-repo Guix functionality. Campaign #102 closed
# COMPLETED having hand-diffed 277 candidates and removed exactly ONE flake;
# ~270 repos carry a `guix.scm` that is a non-functional scaffold stub, so for
# them "delete the flake" means "have no working packaging". Measured over the
# local estate checkout: 22 repos are Nix-only and would fail the moment this
# date passes. Setting a date in the past makes that immediate, with no grace.
if [ -n "$NIX" ]; then
  ENFORCE_NIX_RETIREMENT_FROM="${ENFORCE_NIX_RETIREMENT_FROM:-2026-06-01}"
  require_date ENFORCE_NIX_RETIREMENT_FROM "$ENFORCE_NIX_RETIREMENT_FROM"

  if [[ "$TODAY" < "$ENFORCE_NIX_RETIREMENT_FROM" ]]; then
    echo "::warning::Nix-only packaging (${NIX#"$ROOT"/}). Nix is NOT an estate" \
         "tier — Guix is primary, sealed container is the escape hatch. This" \
         "becomes a BLOCKING failure on $ENFORCE_NIX_RETIREMENT_FROM (today is $TODAY)."
    echo "NOT YET ENFORCED: Nix-only packaging inside the retirement grace window."
    exit 0
  fi

  echo "::error::Nix-only packaging is not compliant: ${NIX#"$ROOT"/}"
  echo
  echo "Estate policy (LANGUAGE-POLICY.adoc, RULED 2026-05-18) is Guix primary"
  echo "+ sealed-container escape; NO Nix mirror. Replace the flake with:"
  echo "  guix.scm | manifest.scm | channels.scm | .guix-channel   (primary)"
  echo "  Containerfile                                            (escape hatch)"
  echo
  echo "HARDENED 2026-07-28 (owner ruling): Nix is REMOVED from the estate, not"
  echo "tolerated. Retire the flake opportunistically whenever you touch a repo."
  echo
  echo "But removal is not the whole job: a repo whose guix.scm is a scaffold stub"
  echo "has no working packaging once the flake is gone. Make the Guix side real"
  echo "(or fill the Containerfile, which is Podman-verifiable where Guix is not"
  echo "installable) IN THE SAME CHANGE as retiring the mirror. Do not leave the"
  echo "repo unpackaged, and do not allowlist the flake instead"
  echo "(spec/scaffold-stub-debt.adoc, step 3)."
  exit 1
fi

# Violation: neither packaging system is present.
if [[ "$TODAY" < "$ENFORCE_PACKAGE_POLICY_FROM" ]]; then
  echo "::warning::No packaging found (no Guix, no sealed container) — this" \
       "becomes a BLOCKING failure on $ENFORCE_PACKAGE_POLICY_FROM (today is $TODAY)."
  # Never claim a pass while the policy is unmet.
  echo "NOT YET ENFORCED: package policy unmet but inside the grace window."
  exit 0
fi

echo "::error::Package policy violation: no packaging found."
echo
echo "Estate policy (LANGUAGE-POLICY.adoc, RULED 2026-05-18) is Guix primary"
echo "+ sealed-container escape; NO Nix mirror. Add one of:"
echo "  guix.scm | manifest.scm | channels.scm | .guix-channel   (primary)"
echo "  Containerfile                                            (escape hatch)"
echo
echo "Files inside .git/ node_modules/ deps/ .lake/ vendor/ do not count."
exit 1
