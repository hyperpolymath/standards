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
    {
      source=$0
      # Mask quoted prose while retaining literal path arguments. Keep command
      # substitutions visible: an echo can still execute a file test in $().
      code=""; quote=""; quoted=""
      for (i=1; i<=length(source); i++) {
        ch=substr(source,i,1)
        if (quote != "") {
          if (ch == quote) {
            if (quoted ~ /^\.machine_readable\/(6a2\/)?(STATE|META|ECOSYSTEM|AGENTIC|NEUROSYM|PLAYBOOK|ANCHOR)\.a2ml$/ ||
                (quote == "\"" && quoted ~ /\$\(|`/)) code=code quoted
            else code=code " "
            quote=""; quoted=""
          } else if (ch == "\\" && quote == "\"") {
            quoted=quoted ch substr(source,++i,1)
          } else quoted=quoted ch
        } else if (ch == "\"" || ch == sprintf("%c",39)) quote=ch
        else code=code ch
      }
      # A multiline shell quote cannot be classified from this physical line.
      if (quote != "") code=code quoted
      if (code ~ /(-f[[:space:]]|-e[[:space:]]|check_file[[:space:]])/ &&
          code ~ /\.machine_readable\/(6a2\/)?(STATE|META|ECOSYSTEM|AGENTIC|NEUROSYM|PLAYBOOK|ANCHOR)\.a2ml/) {
        found=1; print FNR ":" source
      }
    }
    END { exit !found }
  ' "$file"; then
    printf '::error file=%s::Policy requires a retired descriptile path; use .machine_readable/descriptiles/ and reconcile existing files\n' "$file"
    status=1
  fi
done < <(git ls-files -z -- '.github/workflows/*.yml' '.github/workflows/*.yaml' 'scripts/*.sh' '.githooks/*.sh' Justfile justfile)
exit "$status"
