// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// Create Cloudflare Pages projects via API

open Deno_Api

let cloudflareApiToken = Env.get("CLOUDFLARE_API_TOKEN")
let cloudflareAccountId = Env.get("CLOUDFLARE_ACCOUNT_ID")

type project = {name: string, domain: string}

let projects: array<project> = [
  {name: "affinescript", domain: "affinescript.dev"},
  {name: "anvomidav", domain: "anvomidav.org"},
  {name: "betlang", domain: "betlang.org"},
  {name: "eclexia", domain: "eclexia.org"},
  {name: "ephapax", domain: "ephapax.org"},
  {name: "error-lang", domain: "error-lang.org"},
  {name: "my-lang", domain: "my-lang.net"},
  {name: "oblibeny", domain: "oblibeny.net"},
  {name: "reposystem", domain: "reposystem.dev"},
  {name: "verisimdb", domain: "verisimdb.org"},
]

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

  Console.log("Creating Cloudflare Pages projects")
  Console.log("=".repeat(~count=70))

  for i in 0 to Array.length(projects) - 1 {
    let project = projects[i]->Option.getOr({name: "", domain: ""})
    Console.log(`\n${project.name}`)

    // Create project
    let createResponse = await Fetch_Api.fetch(
      `https://api.cloudflare.com/client/v4/accounts/${accountId}/pages/projects`,
      {
        method: "POST",
        headers: headers,
        body: JSON.stringifyAny({
          "name": project.name,
          "production_branch": "main",
        })->Option.getOr(""),
      },
    )

    let createResult = await createResponse->Fetch_Api.json
    if createResult["success"] == true {
      Console.log(`   Project created`)
      Console.log(`   URL: https://${project.name}.pages.dev`)
    } else {
      Console.log(`   Project already exists or failed`)
    }

    // Add custom domain
    Console.log(`   Adding domain: ${project.domain}`)
    let domainResponse = await Fetch_Api.fetch(
      `https://api.cloudflare.com/client/v4/accounts/${accountId}/pages/projects/${project.name}/domains`,
      {
        method: "POST",
        headers: headers,
        body: JSON.stringifyAny({"name": project.domain})->Option.getOr(""),
      },
    )

    let domainResult = await domainResponse->Fetch_Api.json
    if domainResult["success"] == true {
      Console.log(`   Domain added`)
    } else {
      Console.log(`   Domain already added or failed`)
    }
  }

  Console.log("\n" ++ "=".repeat(~count=70))
  Console.log("Projects created!")
  Console.log("\nNow run:")
  Console.log("   ./deploy-repos.sh")
  Console.log("=".repeat(~count=70))
}

let _ = main()
