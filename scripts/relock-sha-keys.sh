#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# relock-sha-keys.sh — rewrite gh-actions-lock's tag-form keys back to the
# inline SHA refs used in workflow files.
#
# `gh actions-lock` prettifies `uses: owner/repo@<sha> # vX` to `owner/repo@vX`
# and keys the lockfile by that tag. This estate keeps inline SHA pins in
# workflow files — Mustfile actions-sha-pinned, the governance linter, the
# Hypatia rule and GitHub's own sha_pinning_required all enforce them — so
# after generating the lockfile the original workflows are restored and the
# lockfile entries are re-keyed to the SHA form. Key SHA == recorded commit
# digest, which the lockfile format already supports (its validator requires
# key-SHA == digest).
#
# Ported from Python: estate language policy bans Python with no exceptions,
# and this file was one of three keeping standards' own governance gate red.
# Shell, not Bun or Deno — this does text rewriting that awk does natively, and
# a script with no runtime dependency cannot be invalidated by the next runtime
# migration. This estate migrates runtimes.
#
# ⚠ ONLY entries whose digest matches a SHA actually written inline in a
# workflow file are re-keyed. Transitive dependencies inside external composite
# actions keep whatever ref form their composite wrote — re-keying those would
# claim a pin the estate does not control.
set -euo pipefail

WF_DIR="${1:-.github/workflows}"
LOCK="$WF_DIR/actions.lock"

[ -f "$LOCK" ] || { echo "relock-sha-keys: no lockfile at $LOCK" >&2; exit 1; }

# 1. Collect inline SHA refs from the workflows: "owner/repo<TAB>sha" per line.
#    Lockfile entries key the repo root, while an inline ref may carry a
#    subpath (github/codeql-action/init@sha), so index by owner/repo only.
INLINE="$(mktemp)"
trap 'rm -f "$INLINE" "$MAP" "$TMPLOCK"' EXIT
find "$WF_DIR" -maxdepth 1 -type f \( -name '*.yml' -o -name '*.yaml' \) -print0 \
  | xargs -0 -r awk '
      match($0, /uses:[ \t]*[A-Za-z0-9_.-]+\/[A-Za-z0-9_.\/-]+@[0-9a-f]{40}/) {
        s = substr($0, RSTART, RLENGTH)
        sub(/^uses:[ \t]*/, "", s)
        n = split(s, parts, "@")
        path = parts[1]; sha = parts[2]
        split(path, seg, "/")
        print tolower(seg[1] "/" seg[2]) "\t" sha
      }' | sort -u > "$INLINE"

# 2. Walk the lockfile, building tag-key -> sha-key replacements.
MAP="$(mktemp)"
awk -v inline="$INLINE" '
  BEGIN {
    while ((getline line < inline) > 0) { split(line, f, "\t"); seen[f[1] SUBSEP f[2]] = 1 }
  }
  # A dependency entry:    '\''action@ref'\'':
  match($0, /^    '\''[^'\''@]+@[^'\'']+'\'':$/) {
    key = $0
    sub(/^    '\''/, "", key); sub(/'\'':$/, "", key)
    n = index(key, "@")
    action = substr(key, 1, n - 1); ref = substr(key, n + 1)
    # already SHA-keyed: nothing to do
    if (ref ~ /^[0-9a-f]{40}$/) { cur = ""; next }
    cur = key; curaction = action; next
  }
  # its body carries    commit: '\''sha1-<sha>'\''
  cur != "" && match($0, /commit:[ \t]*'\''sha1-[0-9a-f]{40}'\''/) {
    c = substr($0, RSTART, RLENGTH)
    sub(/.*sha1-/, "", c); sub(/'\''.*/, "", c)
    if ((tolower(curaction) SUBSEP c) in seen) print cur "\t" curaction "@" c
    cur = ""
  }
' "$LOCK" > "$MAP"

# 3. Apply. ⚠ LITERAL replacement, not regex. The keys contain `.` and `/`
#    (actions/checkout@v7.0.1), and awk's gsub treats its first argument as a
#    REGEX — `.` would match any character, so `@v7.0.1` could match `@v7x0y1`
#    in a neighbouring entry. index()-based splicing has no such hazard.
TMPLOCK="$(mktemp)"
awk -v mapfile="$MAP" '
  BEGIN {
    n = 0
    while ((getline line < mapfile) > 0) {
      split(line, f, "\t")
      if (f[1] == "") continue
      n++; olds[n] = "\047" f[1] "\047"; news[n] = "\047" f[2] "\047"
    }
  }
  {
    for (i = 1; i <= n; i++) {
      while ((p = index($0, olds[i])) > 0) {
        $0 = substr($0, 1, p - 1) news[i] substr($0, p + length(olds[i]))
      }
    }
    print
  }
  END { print n > "/dev/stderr" }
' "$LOCK" > "$TMPLOCK" 2>"$MAP.count"
count="$(cat "$MAP.count" 2>/dev/null || echo 0)"
rm -f "$MAP.count"
mv "$TMPLOCK" "$LOCK"
TMPLOCK=""

echo "re-keyed ${count} entries:"
while IFS=$'\t' read -r old new; do
  [ -z "${old:-}" ] && continue
  printf '  %s -> @%s\n' "$old" "$(printf '%s' "${new##*@}" | cut -c1-12)"
done < <(sort "$MAP")
