#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Deploy all repos to Cloudflare Pages
#
# Required environment:
#   CLOUDFLARE_API_TOKEN   Cloudflare API token with Pages:Edit
#   CLOUDFLARE_ACCOUNT_ID  Cloudflare account ID
#
# Source these from a private location (e.g. ~/.config/hyperpolymath/cloudflare.env)
# before invoking. Do NOT hardcode them in this file.

set -euo pipefail

: "${CLOUDFLARE_API_TOKEN:?CLOUDFLARE_API_TOKEN must be set (do not hardcode)}"
: "${CLOUDFLARE_ACCOUNT_ID:?CLOUDFLARE_ACCOUNT_ID must be set (do not hardcode)}"
export CLOUDFLARE_API_TOKEN CLOUDFLARE_ACCOUNT_ID

REPOS_DIR="$HOME/Documents/hyperpolymath-repos"

declare -A PROJECTS=(
  [affinescript]="affinescript"
  [anvomidav]="anvomidav"
  [betlang]="betlang"
  [eclexia]="eclexia"
  [ephapax]="ephapax"
  [error-lang]="error-lang"
  [my-lang]="my-lang"
  [oblibeny]="oblibeny"
  [reposystem]="reposystem"
  [verisimdb]="verisimdb"
)

echo "🚀 Deploying repos to Cloudflare Pages"
echo "═══════════════════════════════════════════════════════════════════"

for project in "${!PROJECTS[@]}"; do
  repo="${PROJECTS[$project]}"
  repo_path="$REPOS_DIR/$repo"

  echo ""
  echo "📦 $project"

  if [ ! -d "$repo_path" ]; then
    echo "   ❌ Repo not found: $repo_path"
    continue
  fi

  echo "   📤 Deploying from $repo..."

  cd "$repo_path" || continue

  deno run --allow-read --allow-write --allow-env --allow-net --allow-run npm:wrangler pages deploy . --project-name="$project" --branch=main 2>&1 | grep -E "✨|Deployment complete|View your site"

  if [ ${PIPESTATUS[0]} -eq 0 ]; then
    echo "   ✅ Deployed successfully"
  else
    echo "   ⚠️  Deployment completed (check output)"
  fi
done

echo ""
echo "═══════════════════════════════════════════════════════════════════"
echo "✅ Deployment batch complete!"
echo ""
echo "📋 Next: Check deployment status at https://dash.cloudflare.com/pages"
echo "═══════════════════════════════════════════════════════════════════"
