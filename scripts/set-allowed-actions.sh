#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Apply and verify the canonical GitHub Actions policy (standards#486).
#
# Estate default: allowed_actions=all + sha_pinning_required=true. Set
# ACTIONS_POSTURE=selected for designated high-sensitivity repositories; that
# mode also installs the canonical selected-actions payload.
#
# Usage: set-allowed-actions.sh <owner/repo> [allowed-actions.json] [enterprise]
set -uo pipefail

REPO="${1:?usage: set-allowed-actions.sh <owner/repo> [allowed-actions.json] [enterprise]}"
CANON="${2:-rhodium-standard-repositories/actions-allowlist/allowed-actions.json}"
ENTERPRISE="${3:-${ENTERPRISE:-}}"
POSTURE="${ACTIONS_POSTURE:-all}"
GH_BIN="${GH_BIN:-gh}"
OWNER="${REPO%%/*}"

[ "$POSTURE" = all ] || [ "$POSTURE" = selected ] || {
  echo "ERROR: ACTIONS_POSTURE must be 'all' or 'selected'" >&2
  exit 2
}
[ -f "$CANON" ] || { echo "ERROR: canonical allowlist not found: $CANON" >&2; exit 2; }
command -v "$GH_BIN" >/dev/null 2>&1 || { echo "ERROR: GitHub CLI not found: $GH_BIN" >&2; exit 2; }
PATTERN_COUNT="$(jq -r '.patterns_allowed | length' "$CANON")"

# Return 0=applied+verified, 42=governed at a higher scope, 1=other failure.
apply_policy() {
  local label="$1" prefix="$2" current enabled_key enabled_value out rc verify
  local -a fields

  current="$($GH_BIN api "$prefix/actions/permissions" 2>&1)" || {
    echo "ERROR: cannot read $label Actions permissions: $current" >&2
    return 1
  }

  case "$prefix" in
    repos/*)
      enabled_key=enabled
      enabled_value="$(jq -r '.enabled // true' <<<"$current")"
      fields=(-F "enabled=$enabled_value")
      ;;
    *)
      enabled_key=enabled_repositories
      enabled_value="$(jq -r '.enabled_repositories // "all"' <<<"$current")"
      fields=(-f "enabled_repositories=$enabled_value")
      ;;
  esac

  out="$($GH_BIN api -X PUT "$prefix/actions/permissions" \
    "${fields[@]}" -f "allowed_actions=$POSTURE" -F sha_pinning_required=true 2>&1)"
  rc=$?
  if [ "$rc" -ne 0 ]; then
    if grep -qiE 'organization or enterprise level|enterprise level' <<<"$out"; then
      echo "   $label policy is governed higher up (409) — escalating"
      return 42
    fi
    echo "ERROR: $label permissions update failed: $out" >&2
    return 1
  fi

  if [ "$POSTURE" = selected ]; then
    out="$($GH_BIN api -X PUT "$prefix/actions/permissions/selected-actions" --input "$CANON" 2>&1)" || {
      if grep -qiE 'organization or enterprise level|enterprise level' <<<"$out"; then
        echo "   $label selected policy is governed higher up (409) — escalating"
        return 42
      fi
      echo "ERROR: $label selected-actions update failed: $out" >&2
      return 1
    }
  fi

  # The read-after-write is mandatory: an endpoint that silently drops or
  # resets sha_pinning_required must never be reported as successfully fixed.
  verify="$($GH_BIN api "$prefix/actions/permissions")" || {
    echo "ERROR: $label policy update could not be verified" >&2
    return 1
  }
  if [ "$(jq -r '.sha_pinning_required // false' <<<"$verify")" != true ]; then
    echo "ERROR: $label update left sha_pinning_required=false; refusing success" >&2
    return 1
  fi
  if [ "$(jq -r '.allowed_actions // "missing"' <<<"$verify")" != "$POSTURE" ]; then
    echo "ERROR: $label update did not retain allowed_actions=$POSTURE" >&2
    return 1
  fi
  if [ "$(jq -r --arg key "$enabled_key" '.[$key]' <<<"$verify")" != "$enabled_value" ]; then
    echo "ERROR: $label update changed $enabled_key unexpectedly" >&2
    return 1
  fi

  if [ "$POSTURE" = selected ]; then
    selected="$($GH_BIN api "$prefix/actions/permissions/selected-actions")" || return 1
    if [ "$(jq -r '(.patterns_allowed // []) | length' <<<"$selected")" -lt "$PATTERN_COUNT" ]; then
      echo "ERROR: selected-actions verification returned fewer than $PATTERN_COUNT patterns" >&2
      return 1
    fi
  fi

  echo "OK: $label policy verified (allowed=$POSTURE sha_pinning_required=true)"
}

echo "==> repository level: $REPO"
apply_policy repository "repos/$REPO"; rc=$?
[ "$rc" -eq 0 ] && exit 0
[ "$rc" -ne 42 ] && exit 1

echo "==> organization level: $OWNER"
apply_policy organization "orgs/$OWNER"; rc=$?
[ "$rc" -eq 0 ] && exit 0
[ "$rc" -ne 42 ] && exit 1

if [ -n "$ENTERPRISE" ]; then
  echo "==> enterprise level: $ENTERPRISE"
  apply_policy enterprise "enterprises/$ENTERPRISE"; rc=$?
  [ "$rc" -eq 0 ] && exit 0
  exit 1
fi

echo "ERROR: policy is enterprise-governed; provide the enterprise slug" >&2
exit 3
