#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# A CI policy must not require files that the structural-drift gate forbids.
set -euo pipefail
status=0
while IFS= read -r -d '' file; do
  [[ -f "$file" ]] || continue
  # Restrict this check to executable file-existence tests. Historical prose
  # and commented examples are not policy enforcement.
  if awk '
    /^[[:space:]]*#/ { next }
    /(-f[[:space:]]|-e[[:space:]]|check_file[[:space:]])/ && /\.machine_readable\/(6a2\/)?(STATE|META|ECOSYSTEM|AGENTIC|NEUROSYM|PLAYBOOK|ANCHOR)\.a2ml/ { found=1; print FNR ":" $0 }
    END { exit !found }
  ' "$file"; then
    printf '::error file=%s::Policy requires a retired descriptile path; use .machine_readable/descriptiles/ and reconcile existing files\n' "$file"
    status=1
  fi
done < <(git ls-files -z -- '.github/workflows/*.yml' '.github/workflows/*.yaml' 'scripts/*.sh' '.githooks/*.sh' Justfile justfile)
exit "$status"
