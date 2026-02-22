// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// Deno-native Cloudflare Pages deployment script

open Deno_Api

let cloudflareApiToken = Env.get("CLOUDFLARE_API_TOKEN")
let cloudflareAccountId = Env.get("CLOUDFLARE_ACCOUNT_ID")

let headers = {
  Fetch_Api.authorization: `Bearer ${cloudflareApiToken->Option.getOr("")}`,
  contentType: "application/json",
}

let main = async () => {
  switch cloudflareApiToken {
  | None =>
    Console.error("CLOUDFLARE_API_TOKEN environment variable required")
    exit(1)
  | Some(_) => ()
  }

  switch cloudflareAccountId {
  | None =>
    Console.error("CLOUDFLARE_ACCOUNT_ID environment variable required")
    Console.log("\nGet your account ID from: https://dash.cloudflare.com/ (right sidebar)")
    exit(1)
  | Some(_) => ()
  }

  let accountId = cloudflareAccountId->Option.getOr("")

  Console.log("AVOW Protocol - Deno Cloudflare Deployment")
  Console.log("=".repeat(~count=50))

  // Step 1: Build project
  Console.log("\nBuilding project...")
  let buildCmd = makeCommand("deno", {args: ["task", "build"], stdout: "inherit", stderr: "inherit"})
  let buildResult = await buildCmd->output
  if !buildResult.success {
    Console.error("Build failed")
    exit(1)
  }
  Console.log("Build successful")

  // Step 2: Check/create project
  Console.log("\nChecking Cloudflare Pages project...")

  let checkResponse = await Fetch_Api.fetch(
    `https://api.cloudflare.com/client/v4/accounts/${accountId}/pages/projects/avow-protocol`,
    {headers: headers},
  )

  if !checkResponse.ok {
    Console.log("Creating new Pages project...")
    let createResponse = await Fetch_Api.fetch(
      `https://api.cloudflare.com/client/v4/accounts/${accountId}/pages/projects`,
      {
        method: "POST",
        headers: headers,
        body: JSON.stringifyAny({
          "name": "avow-protocol",
          "production_branch": "main",
          "build_config": {
            "build_command": "deno task build",
            "destination_dir": ".",
            "root_dir": "/",
          },
        })->Option.getOr(""),
      },
    )

    let result = await createResponse->Fetch_Api.json
    if result["success"] == true {
      Console.log("Project created")
    } else {
      Console.error2("Failed to create project:", result["errors"])
      exit(1)
    }
  } else {
    Console.log("Project exists")
  }

  // Step 3: Instructions
  Console.log("\n" ++ "=".repeat(~count=50))
  Console.log("Project configured on Cloudflare!")
  Console.log("\nNext steps to complete deployment:\n")
  Console.log("Option 1: Deploy via GitHub Integration (Recommended)")
  Console.log("  1. Go to: https://dash.cloudflare.com/pages")
  Console.log("  2. Find 'avow-protocol' project")
  Console.log("  3. Click 'Connect to Git'")
  Console.log("  4. Select: hyperpolymath/avow-protocol")
  Console.log("  5. Cloudflare will auto-deploy on push to main\n")
  Console.log("Option 2: Deploy via Wrangler CLI")
  Console.log("  1. Install: npm install -g wrangler")
  Console.log("  2. Deploy: wrangler pages deploy .\n")
  Console.log("Option 3: Manual Upload")
  Console.log("  1. Go to: https://dash.cloudflare.com/pages")
  Console.log("  2. Upload files directly via dashboard\n")
  Console.log("Your site will be available at:")
  Console.log("   https://avow-protocol.pages.dev")
  Console.log("   https://avow-protocol.org (after DNS setup)")
  Console.log("\nComplete setup guide: CLOUDFLARE-MANUAL-SETUP.md")
  Console.log("=".repeat(~count=50))
}

let _ = main()
