#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# A CI policy must not require files that the structural-drift gate forbids.
set -euo pipefail
status=0
while IFS= read -r -d '' file; do
  [[ -f "$file" ]] || continue
  # Recognise direct shell tests and check_file calls at a command start,
  # including inline YAML run steps. Quoted echo examples are not execution.
  # Compound expressions and dynamically constructed paths need shell analysis.
  if awk '
    /^[[:space:]]*#/ { next }
    /^[[:space:]]*(-[[:space:]]+)?run:[[:space:]]*/ { sub(/^[[:space:]]*(-[[:space:]]+)?run:[[:space:]]*/, "") }
    /^[[:space:]]*((if|elif|while|until)[[:space:]]+)?(![[:space:]]+)?((test|\[\[?)[[:space:]]+(![[:space:]]+)?-[fe][[:space:]]+|check_file[[:space:]]+)["\047]?\.machine_readable\/(6a2\/)?(STATE|META|ECOSYSTEM|AGENTIC|NEUROSYM|PLAYBOOK|ANCHOR)\.a2ml(["\047]|[[:space:];]|$)/ { found=1; print FNR ":" $0 }
    END { exit !found }
  ' "$file"; then
    printf '::error file=%s::Policy requires a retired descriptile path; use .machine_readable/descriptiles/ and reconcile existing files\n' "$file"
    status=1
  fi
done < <(git ls-files -z -- '.github/workflows/*.yml' '.github/workflows/*.yaml' 'scripts/*.sh' '.githooks/*.sh' Justfile justfile)
exit "$status"
