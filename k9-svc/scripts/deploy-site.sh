#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Build the k9-svc.net site with ddraig-ssg and deploy it to the Cloudflare
# Pages project "k9-svc". Requires CLOUDFLARE_API_TOKEN (Pages:Edit) and
# CLOUDFLARE_ACCOUNT_ID in the environment (e.g. `source ~/.secrets/cloudflare.env`).
# wrangler is run via Deno's npm-compat layer (no npm/node_modules/package.json
# on disk) per estate policy — "JS deps: Deno".
set -euo pipefail

SITE_DIR="$(cd "$(dirname "$0")/.." && pwd)/site"
DDRAIG_REPO="${DDRAIG_REPO:-$(cd "$(dirname "$0")/../../.." && pwd)/ddraig-ssg}"
DDRAIG_BIN="$DDRAIG_REPO/build/exec/ddraig"
OUT_DIR="${OUT_DIR:-$(mktemp -d)}"
BASE_URL="https://k9-svc.net"
PROJECT_NAME="k9-svc"

if [ ! -x "$DDRAIG_BIN" ]; then
  echo "Building ddraig-ssg..."
  ( cd "$DDRAIG_REPO" && idris2 Ddraig.idr -o ddraig )
fi

echo "Building $SITE_DIR -> $OUT_DIR (base $BASE_URL)"
"$DDRAIG_BIN" build "$SITE_DIR" "$OUT_DIR" "$BASE_URL"

: "${CLOUDFLARE_API_TOKEN:?Set CLOUDFLARE_API_TOKEN (Pages:Edit) before deploying}"
: "${CLOUDFLARE_ACCOUNT_ID:?Set CLOUDFLARE_ACCOUNT_ID before deploying}"

echo "Deploying $OUT_DIR to Cloudflare Pages project '$PROJECT_NAME'"
deno run -A npm:wrangler@4 pages deploy "$OUT_DIR" \
  --project-name="$PROJECT_NAME" \
  --branch=main

echo "Done. Verify: curl -I $BASE_URL/ and $BASE_URL/.well-known/security.txt"
