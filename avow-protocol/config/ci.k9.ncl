# SPDX-License-Identifier: AGPL-3.0-or-later
# K9 Contractile: Yard Level (Validated Config)
# CI/CD configuration with Nickel contracts

let Port = std.contract.from_predicate (fun port =>
  std.is_number port && port >= 1 && port <= 65535
) in

let NonEmptyString = std.contract.from_predicate (fun s =>
  std.is_string s && std.string.length s > 0
) in

{
  leash = 'Yard,

  ci = {
    provider = "GitHub Actions" | NonEmptyString,

    triggers = {
      push = {
        branches | Array NonEmptyString = ["main", "develop"],
      },
      pull_request = {
        branches | Array NonEmptyString = ["main"],
      },
    },

    build = {
      command | NonEmptyString = "deno task build",
      output_dir | NonEmptyString = "/",
      node_version = null,  # Not using Node.js
      deno_version | NonEmptyString = "2.0+",
    },

    test = {
      command | NonEmptyString = "deno task test",
      coverage = false,  # TODO: Add coverage
    },

    security = {
      hypatia_scan = true,
      codeql = true,
      scorecard = true,
      secret_scanning = true,
      dependency_review = true,
    },

    deployment = {
      platform | NonEmptyString = "Cloudflare Pages",
      production_branch | NonEmptyString = "main",
      preview_branches | Array NonEmptyString = ["develop", "staging"],

      environment = {
        NODE_ENV = "production",
        DENO_ENV = "production",
      },
    },
  },

  local_dev = {
    server_command | NonEmptyString = "deno run -A jsr:@std/http/file-server .",
    port | Port = 8000,
    watch_command | NonEmptyString = "deno task watch",
    hot_reload = false,
  },

  quality = {
    linting = {
      enabled = true,
      command | NonEmptyString = "deno lint",
    },

    formatting = {
      enabled = true,
      command | NonEmptyString = "deno fmt",
    },

    editorconfig = {
      enabled = true,
      check_command | NonEmptyString = "editorconfig-checker",
    },
  },

  validation = {
    proven_validation = true,
    dom_safety_checks = true,
    url_validation = "ProvenSafeUrl",
    html_validation = "balanced-tags",
  },
}
