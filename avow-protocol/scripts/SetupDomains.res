// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// Configure custom domains for Cloudflare Pages projects

open Deno_Api

let cloudflareApiToken = Env.get("CLOUDFLARE_API_TOKEN")
let cloudflareAccountId = Env.get("CLOUDFLARE_ACCOUNT_ID")

type project = {name: string, domains: array<string>}

let projects: array<project> = [
  {name: "avow-protocol", domains: ["avow-protocol.org", "www.avow-protocol.org"]},
  {name: "a2ml", domains: ["a2ml.org", "www.a2ml.org"]},
  {name: "k9-svc", domains: ["k9-svc.org", "www.k9-svc.org"]},
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

  Console.log("Setting up custom domains for Cloudflare Pages")
  Console.log("=".repeat(~count=60))

  for i in 0 to Array.length(projects) - 1 {
    let project = projects[i]->Option.getOr({name: "", domains: []})
    Console.log(`\nProject: ${project.name}`)

    for j in 0 to Array.length(project.domains) - 1 {
      let domain = project.domains[j]->Option.getOr("")
      Console.log(`\n  Adding domain: ${domain}`)

      let response = await Fetch_Api.fetch(
        `https://api.cloudflare.com/client/v4/accounts/${accountId}/pages/projects/${project.name}/domains`,
        {
          method: "POST",
          headers: headers,
          body: JSON.stringifyAny({"name": domain})->Option.getOr(""),
        },
      )

      let result = await response->Fetch_Api.json
      if result["success"] == true {
        Console.log(`  Domain added: ${domain}`)
        Console.log(`  Status: ${result["result"]["status"]}`)
      } else {
        Console.log(`  Domain already added or failed: ${domain}`)
      }
    }
  }

  Console.log("\n" ++ "=".repeat(~count=60))
  Console.log("Domain setup complete!")
  Console.log("\nNext steps:")
  Console.log("1. DNS records will be auto-configured by Cloudflare")
  Console.log("2. Wait 1-5 minutes for activation")
  Console.log("3. Verify at: https://dash.cloudflare.com/pages")
  Console.log("\nYour sites will be available at:")
  projects->Array.forEach(project => {
    switch project.domains[0] {
    | Some(domain) => Console.log(`   https://${domain}`)
    | None => ()
    }
  })
  Console.log("=".repeat(~count=60))
}

let _ = main()
