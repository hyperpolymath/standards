// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// Deploy all hyperpolymath projects to Cloudflare Pages

open Deno_Api

let cloudflareApiToken = Env.get("CLOUDFLARE_API_TOKEN")
let cloudflareAccountId = Env.get("CLOUDFLARE_ACCOUNT_ID")

type project = {name: string, domain: string, path: string}
type deployResult = {name: string, domain: string, status: string, url: option<string>}

let projects: array<project> = [
  {name: "affinescript", domain: "affinescript.dev", path: "affinescript"},
  {name: "anvomidav", domain: "anvomidav.org", path: "anvomidav"},
  {name: "betlang", domain: "betlang.org", path: "betlang"},
  {name: "eclexia", domain: "eclexia.org", path: "eclexia"},
  {name: "ephapax", domain: "ephapax.org", path: "ephapax"},
  {name: "error-lang", domain: "error-lang.org", path: "error-lang"},
  {name: "my-lang", domain: "my-lang.net", path: "my-lang"},
  {name: "oblibeny", domain: "oblibeny.net", path: "oblibeny"},
  {name: "reposystem", domain: "reposystem.dev", path: "reposystem"},
  {name: "verisimdb", domain: "verisimdb.org", path: "verisimdb"},
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
  let home = Env.get("HOME")->Option.getOr("")

  Console.log("Deploying all projects to Cloudflare Pages")
  Console.log("=".repeat(~count=70))

  let results: array<deployResult> = []

  for i in 0 to Array.length(projects) - 1 {
    let project = projects[i]->Option.getOr({name: "", domain: "", path: ""})
    Console.log(`\nProject: ${project.name}`)
    Console.log(`   Domain: ${project.domain}`)

    let repoPath = `${home}/Documents/hyperpolymath-repos/${project.path}`

    // Check if repo exists
    try {
      let _ = await stat(repoPath)
    } catch {
    | _ =>
      Console.log(`   Repo not found at ${repoPath}`)
      let _ = results->Array.push({
        name: project.name,
        domain: project.domain,
        status: "repo_not_found",
        url: None,
      })
    }

    // Deploy using wrangler
    Console.log(`   Deploying...`)

    let deployCmd = makeCommand(
      "deno",
      {
        args: [
          "run",
          "-A",
          "npm:wrangler",
          "pages",
          "deploy",
          ".",
          "--project-name=" ++ project.name,
          "--branch=main",
        ],
        stdout: "piped",
        stderr: "piped",
        cwd: repoPath,
      },
    )

    try {
      let deployOutput = await deployCmd->output

      if deployOutput.code == 0 {
        let deployUrl = `https://${project.name}.pages.dev`
        Console.log(`   Deployed: ${deployUrl}`)

        // Add custom domain
        Console.log(`   Adding custom domain: ${project.domain}`)
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
          Console.log(`   Custom domain added`)
        } else {
          Console.log(`   Domain add failed or already exists`)
        }

        let _ = results->Array.push({
          name: project.name,
          domain: project.domain,
          status: "success",
          url: Some(deployUrl),
        })
      } else {
        Console.log(`   Deployment failed`)
        let _ = results->Array.push({
          name: project.name,
          domain: project.domain,
          status: "deploy_failed",
          url: None,
        })
      }
    } catch {
    | Exn.Error(e) =>
      Console.log(`   Error: ${Exn.message(e)->Option.getOr("unknown")}`)
      let _ = results->Array.push({
        name: project.name,
        domain: project.domain,
        status: "error",
        url: None,
      })
    }
  }

  // Summary
  Console.log("\n" ++ "=".repeat(~count=70))
  Console.log("Deployment Summary\n")

  let successful = results->Array.filter(r => r.status == "success")
  let failed = results->Array.filter(r => r.status != "success")

  Console.log(`Successfully deployed: ${successful->Array.length->Int.toString}`)
  successful->Array.forEach(r => {
    Console.log(`   - ${r.name}: ${r.url->Option.getOr("unknown")}`)
    Console.log(`     Custom domain: https://${r.domain}`)
  })

  if Array.length(failed) > 0 {
    Console.log(`\nFailed/Issues: ${failed->Array.length->Int.toString}`)
    failed->Array.forEach(r => {
      Console.log(`   - ${r.name}: ${r.status}`)
    })
  }

  Console.log("\nNext Steps:")
  Console.log("1. Set up DNS zones for domains not in Cloudflare")
  Console.log("2. Add CNAME records pointing to <project>.pages.dev")
  Console.log("3. Wait 1-5 minutes for DNS propagation")
  Console.log("4. Verify domains are accessible")
  Console.log("=".repeat(~count=70))
}

let _ = main()
