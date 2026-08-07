#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Tests for the workflow linter's SPDX check, which reads a file's LEADING
# COMMENT BLOCK rather than line 1.
#
# ⚠ TEST 2 IS THE REASON THIS EXISTS. `gh actions-lock` inserts
# `# This workflow is managed by gh actions-lock.` at line 1 whenever it mints a
# lockfile, displacing whatever was there. A line-1 test therefore fights the
# estate's own tool and re-fails on every lockfile refresh — it reported 40
# workflows across the estate as missing a header they all had, and the attempt
# to "fix" that by prepending a default mis-licensed three files.
set -uo pipefail
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT
pass=0; fail=0

# The check, verbatim from governance-reusable.yml.
has_spdx() {
  awk '/^---[[:space:]]*$/ { next } /^#/ { print; next } { exit }' "$1" \
    | grep -q "^# SPDX-License-Identifier:"
}

ck() { # name expected(0=pass,1=fail) file
  if has_spdx "$3"; then got=0; else got=1; fi
  if [ "$got" = "$2" ]; then printf '  ok    %s\n' "$1"; pass=$((pass+1))
  else printf '  FAIL  %s (expected %s, got %s)\n' "$1" "$2" "$got"; fail=$((fail+1)); fi
}

printf '# SPDX-License-Identifier: MPL-2.0\nname: x\n' > "$T/1.yml"
ck "identifier on line 1 passes" 0 "$T/1.yml"

printf '# This workflow is managed by gh actions-lock.\n# SPDX-License-Identifier: MPL-2.0\nname: x\n' > "$T/2.yml"
ck "identifier BELOW the managed-by comment passes (the regression)" 0 "$T/2.yml"

printf '#\n# some preamble\n#\n# SPDX-License-Identifier: AGPL-3.0-or-later\n#\nname: x\n' > "$T/3.yml"
ck "identifier deeper in the header block passes" 0 "$T/3.yml"

printf -- '---\n# SPDX-License-Identifier: MPL-2.0\nname: x\n' > "$T/4.yml"
ck "a YAML document marker before the block is tolerated" 0 "$T/4.yml"

printf 'name: x\njobs: {}\n' > "$T/5.yml"
ck "no identifier anywhere fails" 1 "$T/5.yml"

printf 'name: x\n# SPDX-License-Identifier: MPL-2.0\njobs: {}\n' > "$T/6.yml"
ck "an identifier AFTER the header block fails (it is not a file header)" 1 "$T/6.yml"

printf '# SPDX-FileCopyrightText: 2026 someone\nname: x\n' > "$T/7.yml"
ck "a copyright line alone is not a licence declaration" 1 "$T/7.yml"

printf '' > "$T/8.yml"
ck "an empty file fails" 1 "$T/8.yml"

printf '# SPDX-License-Identifier: PMPL-1.0-or-later\nname: x\n' > "$T/9.yml"
ck "a non-default licence is accepted as readily as the common one" 0 "$T/9.yml"

printf '\n  %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
