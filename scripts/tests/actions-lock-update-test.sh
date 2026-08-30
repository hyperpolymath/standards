#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
UPDATE="$SCRIPT_DIR/../update-actions-lock.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

mkdir -p "$WORK/.github/workflows" "$WORK/bin"
cat > "$WORK/.github/workflows/ci.yml" <<'EOF'
# SPDX-License-Identifier: MPL-2.0
# This workflow is managed by gh actions-lock.
name: CI
on: push
permissions: {}
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
EOF

cat > "$WORK/bin/fake-gh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
if [ "${2:-}" = "--verify-local" ]; then
  grep -q "actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1" \
    .github/workflows/actions.lock
  sed -i 's#uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1#uses: actions/checkout@v7.0.1#' \
    .github/workflows/ci.yml
  case "${FAKE_VERIFY_FINDING:-}" in
    reusable-exact)
      printf '%s\n' '{"valid":false,"findings":[{"workflow":".github/workflows/reusable.yml","category":"stale","dependency":"hyperpolymath/standards@abc123"}]}'
      exit 1
      ;;
    reusable-wrong-ref)
      printf '%s\n' '{"valid":false,"findings":[{"workflow":".github/workflows/reusable.yml","category":"stale","dependency":"hyperpolymath/standards@wrong456"}]}'
      exit 1
      ;;
    reusable-non-stale)
      printf '%s\n' '{"valid":false,"findings":[{"workflow":".github/workflows/reusable.yml","category":"missing","dependency":"hyperpolymath/standards@abc123"}]}'
      exit 1
      ;;
    malformed-success)
      printf '%s\n' 'not valid JSON'
      exit 0
      ;;
    valid-with-warning)
      printf '%s\n' '{"valid":true,"findings":[{"workflow":".github/workflows/ci.yml","category":"sha-as-ref","severity":"warning","dependency":"actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1"}]}'
      exit 1
      ;;
    *)
      printf '%s\n' '{"valid":true,"findings":[]}'
      exit
      ;;
  esac
fi
sed -i '1i# This workflow is managed by gh actions-lock.' .github/workflows/ci.yml
sed -i 's#actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1#actions/checkout@v7.0.1#' \
  .github/workflows/ci.yml
cat > .github/workflows/actions.lock <<'LOCK'
version: 'v0.0.2'
workflows:
    '.github/workflows/ci.yml':
        - 'actions/checkout@v7.0.1'
dependencies:
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-3d3c42e5aac5ba805825da76410c181273ba90b1'
        owner_id: 1
        repo_id: 2
LOCK
EOF
chmod +x "$WORK/bin/fake-gh"

cd "$WORK"
GH_BIN="$WORK/bin/fake-gh" bash "$UPDATE" .github/workflows >/dev/null

grep -q 'uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1' \
  .github/workflows/ci.yml
[ "$(grep -c '^# This workflow is managed by gh actions-lock.$' .github/workflows/ci.yml)" -eq 1 ]
grep -q "'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1':" \
  .github/workflows/actions.lock

cp .github/workflows/ci.yml "$WORK/ci.before"
cp .github/workflows/actions.lock "$WORK/lock.before"
GH_BIN="$WORK/bin/fake-gh" bash "$UPDATE" .github/workflows >/dev/null
cmp -s "$WORK/ci.before" .github/workflows/ci.yml
cmp -s "$WORK/lock.before" .github/workflows/actions.lock

echo "PASS: Actions lock refresh preserves inline SHA source and is idempotent"

# Verification is mutating in released gh-actions-lock versions too. The safe
# verification mode must restore workflow bytes while leaving the lock intact.
cp .github/workflows/ci.yml "$WORK/ci.before-verify"
cp .github/workflows/actions.lock "$WORK/lock.before-verify"
GH_BIN="$WORK/bin/fake-gh" bash "$UPDATE" --verify-local .github/workflows >/dev/null
cmp -s "$WORK/ci.before-verify" .github/workflows/ci.yml
cmp -s "$WORK/lock.before-verify" .github/workflows/actions.lock
echo "PASS: Actions lock verification restores tool-authored workflow edits"

FAKE_VERIFY_FINDING=valid-with-warning GH_BIN="$WORK/bin/fake-gh" \
  bash "$UPDATE" --verify-local .github/workflows >/dev/null
echo "PASS: valid lock with advisory warning is accepted"

cat > .github/workflows/reusable.yml <<'EOF'
# SPDX-License-Identifier: MPL-2.0
name: Reusable caller
on: push
permissions: {}
jobs:
  governance:
    uses: hyperpolymath/standards/.github/workflows/governance-reusable.yml@abc123
EOF

FAKE_VERIFY_FINDING=reusable-exact GH_BIN="$WORK/bin/fake-gh" \
  bash "$UPDATE" --verify-local .github/workflows >/dev/null
echo "PASS: exact reusable-workflow dependency is accepted"

if FAKE_VERIFY_FINDING=reusable-wrong-ref GH_BIN="$WORK/bin/fake-gh" \
   bash "$UPDATE" --verify-local .github/workflows >/dev/null 2>&1; then
  echo "FAIL: wrong reusable-workflow ref was accepted" >&2
  exit 1
fi
echo "PASS: wrong reusable-workflow ref remains blocking"

if FAKE_VERIFY_FINDING=reusable-non-stale GH_BIN="$WORK/bin/fake-gh" \
   bash "$UPDATE" --verify-local .github/workflows >/dev/null 2>&1; then
  echo "FAIL: non-stale reusable-workflow finding was accepted" >&2
  exit 1
fi
echo "PASS: non-stale reusable-workflow finding remains blocking"

if FAKE_VERIFY_FINDING=malformed-success GH_BIN="$WORK/bin/fake-gh" \
   bash "$UPDATE" --verify-local .github/workflows >/dev/null 2>&1; then
  echo "FAIL: malformed successful verifier output was accepted" >&2
  exit 1
fi
echo "PASS: malformed verifier output fails closed"

# A failed refresh must restore both authored workflows and the previous
# lockfile; this is the production failure mode that left the original checkout
# half-rewritten when DNS resolution failed.
cp .github/workflows/ci.yml "$WORK/ci.before-failure"
cp .github/workflows/actions.lock "$WORK/lock.before-failure"
cat > "$WORK/bin/failing-gh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
sed -i '1i# This workflow is managed by gh actions-lock.' .github/workflows/ci.yml
sed -i 's#actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1#actions/checkout@v7.0.1#' \
  .github/workflows/ci.yml
printf 'partial lock\n' > .github/workflows/actions.lock
exit 1
EOF
chmod +x "$WORK/bin/failing-gh"

if GH_BIN="$WORK/bin/failing-gh" bash "$UPDATE" .github/workflows >/dev/null 2>&1; then
  echo "FAIL: simulated lock refresh unexpectedly succeeded" >&2
  exit 1
fi
cmp -s "$WORK/ci.before-failure" .github/workflows/ci.yml
cmp -s "$WORK/lock.before-failure" .github/workflows/actions.lock
echo "PASS: failed Actions lock refresh rolls back partial generated edits"
