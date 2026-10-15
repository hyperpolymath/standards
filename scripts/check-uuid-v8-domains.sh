#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# check-uuid-v8-domains.sh — the ADR-008 profile C domain register.
#
# Validates docs/decisions/ADR-008-profile-c-domains.json and, given DOMAIN
# arguments, checks that each one is registered. A repository that mints
# profile C ids runs it in CI with the domains it uses, against a pinned
# standards checkout, so an unregistered domain fails there.
#
# Usage: check-uuid-v8-domains.sh [--registry FILE] [DOMAIN...]
#
# Exit contract:
#   0 = the register is valid, and every DOMAIN given is registered
#   1 = the register is invalid, or a DOMAIN is not registered (each is named)
#   2 = the check could not run (jq missing, register missing or unparseable)
#
# What "valid" means:
#   * One line of printable ASCII and exactly one final LF. Within that shape
#     (objects, arrays and strings only, ASCII keys) `jq -cS .` is the JCS
#     (RFC 8785) form, so the bytes must equal it (JSON-POLICY, D306).
#   * Top level is exactly {domains, schema}, schema "estate-uuid-v8-domains/1".
#   * Every entry is exactly {domain, name, purpose, repos}:
#       domain   non-empty printable ASCII (0x21..0x7e) without ":" (ADR-008);
#       name     what is hashed after "domain:", in words;
#       purpose  what the ids identify;
#       repos    the owner/repo list that mints ids in this domain, sorted,
#                unique. A domain shared by several repositories is one
#                registration (for example a cross-tool content address).
#   * Domains are strictly increasing, so no domain is registered twice.
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
REGISTRY="$ROOT/docs/decisions/ADR-008-profile-c-domains.json"
if [ "${1:-}" = "--registry" ]; then
  [ $# -ge 2 ] || { echo "check-uuid-v8-domains: --registry needs a file" >&2; exit 2; }
  REGISTRY="$2"
  shift 2
fi

command -v jq >/dev/null 2>&1 || { echo "check-uuid-v8-domains: jq is not on PATH" >&2; exit 2; }
[ -f "$REGISTRY" ] || { echo "check-uuid-v8-domains: no register at $REGISTRY" >&2; exit 2; }
jq -e . "$REGISTRY" >/dev/null 2>&1 || {
  echo "check-uuid-v8-domains: $REGISTRY does not parse as JSON" >&2; exit 2;
}

errors=()

# Shape of the bytes: one line, printable ASCII, one final LF, JCS order.
if [ "$(tail -c 1 "$REGISTRY" | od -An -c | tr -d ' ')" != '\n' ] || [ "$(wc -l < "$REGISTRY")" -ne 1 ]; then
  errors+=("the register must be exactly one line ending in one LF")
fi
if LC_ALL=C grep -q '[^ -~]' "$REGISTRY"; then
  errors+=("the register must be printable ASCII")
fi
if ! cmp -s <(jq -cS . "$REGISTRY") "$REGISTRY"; then
  errors+=("the register is not in JCS form (jq -cS . | cmp)")
fi

# Structure and content.
while IFS= read -r e; do
  [ -n "$e" ] && errors+=("$e")
done < <(jq -r '
  def domain_ok: type == "string" and test("^[!-9;-~]+$");
  def repo_ok: type == "string" and test("^[A-Za-z0-9-]+/[A-Za-z0-9._-]+$");
  def text_ok: type == "string" and length > 0;
  if type != "object" then "the register is not an object"
  elif keys != ["domains", "schema"] then "top-level keys must be exactly domains, schema"
  elif .schema != "estate-uuid-v8-domains/1" then "schema must be \"estate-uuid-v8-domains/1\""
  elif (.domains | type) != "array" or (.domains | length) == 0 then "domains must be a non-empty array"
  else
    (.domains | to_entries[] | .key as $i | .value as $e |
      if ($e | type) != "object" then "domains[\($i)] is not an object"
      elif ($e | keys) != ["domain", "name", "purpose", "repos"] then
        "domains[\($i)]: keys must be exactly domain, name, purpose, repos"
      else
        (if ($e.domain | domain_ok) then empty else
          "domains[\($i)]: invalid domain \($e.domain | tojson) (non-empty printable ASCII, no \":\")" end),
        (if ($e.name | text_ok) then empty else "domains[\($i)]: name must be a non-empty string" end),
        (if ($e.purpose | text_ok) then empty else "domains[\($i)]: purpose must be a non-empty string" end),
        (if ($e.repos | type) != "array" or ($e.repos | length) == 0 or ([$e.repos[] | repo_ok] | all | not) then
          "domains[\($i)]: repos must be a non-empty array of owner/repo"
        elif $e.repos != ($e.repos | unique) then "domains[\($i)]: repos must be sorted and unique"
        else empty end)
      end),
    ([.domains[] | objects | .domain] as $d |
      if $d != ($d | unique) then "domains must be strictly increasing by domain (sorted, none repeated)" else empty end)
  end' "$REGISTRY")

# Each DOMAIN given must be registered.
for d in "$@"; do
  jq -e --arg d "$d" 'any(.domains[]?; .domain == $d)' "$REGISTRY" >/dev/null 2>&1 \
    || errors+=("domain \"$d\" is not registered in $(basename "$REGISTRY")")
done

if [ ${#errors[@]} -gt 0 ]; then
  for e in "${errors[@]}"; do
    echo "::error file=$REGISTRY::$e"
  done
  exit 1
fi
echo "profile C domain register: $(jq '.domains | length' "$REGISTRY") domain(s) valid${*:+; required domain(s) registered: $*}"
