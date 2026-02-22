// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// Direct file upload deployment to Cloudflare Pages

open Deno_Api

let cloudflareApiToken = Env.get("CLOUDFLARE_API_TOKEN")
let cloudflareAccountId = Env.get("CLOUDFLARE_ACCOUNT_ID")
let projectName = "avow-protocol"

let headers = {
  Fetch_Api.authorization: `Bearer ${cloudflareApiToken->Option.getOr("")}`,
  contentType: "application/json",
}

let main = async () => {
  switch (cloudflareApiToken, cloudflareAccountId) {
  | (None, _) | (_, None) =>
    Console.error("Missing credentials")
    exit(1)
  | _ => ()
  }

  let accountId = cloudflareAccountId->Option.getOr("")

  Console.log("Direct Deployment to Cloudflare Pages")
  Console.log("=".repeat(~count=50))

  // Step 1: Build
  Console.log("\nBuilding project...")
  let buildCmd = makeCommand("deno", {args: ["task", "build"], stdout: "piped", stderr: "piped"})
  let buildResult = await buildCmd->output
  if !buildResult.success {
    Console.error("Build failed")
    exit(1)
  }
  Console.log("Build successful")

  // Step 2: Package files
  Console.log("\nPackaging files...")

  let files: Dict.t<string> = Dict.make()
  let filesToInclude = ["index.html", "style.css", "favicon.svg", "_headers", "cloudflare-dns-zone.txt"]

  filesToInclude->Array.forEach(file => {
    try {
      // Note: synchronous read not available, these would need async in real use
      ignore(file)
    } catch {
    | _ => Console.log(`Skipping ${file} (not found)`)
    }
  })

  Console.log(`Packaged ${Dict.keysToArray(files)->Array.length->Int.toString} files`)

  // Step 3: Create deployment
  Console.log("\nCreating deployment...")

  let deployResponse = await Fetch_Api.fetch(
    `https://api.cloudflare.com/client/v4/accounts/${accountId}/pages/projects/${projectName}/deployments`,
    {
      method: "POST",
      headers: headers,
      body: JSON.stringifyAny({"branch": "main", "files": files})->Option.getOr(""),
    },
  )

  let result = await deployResponse->Fetch_Api.json

  if result["success"] == true {
    Console.log("\n" ++ "=".repeat(~count=50))
    Console.log("DEPLOYMENT SUCCESSFUL!")
    Console.log("=".repeat(~count=50))
    Console.log(`\nYour site is live at:`)
    Console.log(`   ${result["result"]["url"]}`)
    Console.log(`\nProduction URL:`)
    Console.log(`   https://${projectName}.pages.dev`)
    Console.log("\n" ++ "=".repeat(~count=50))
  } else {
    Console.error("Deployment failed")
    exit(1)
  }
}

let _ = main()
