K9!
# SPDX-License-Identifier: MPL-2.0
# K9 Contractile: Yard Level (Validated Config)
# CI/CD configuration with Nickel contracts
#
# The leash lives at pedigree.security.leash (K9-S014). A top-level `leash`
# was the old shape: nothing that applies the K9 contract reads it, so the
# file declared a security level no host enforced.
#
# 'Yard and not 'Kennel, because this file is *evaluated*: `Port` and
# `NonEmptyString` below are Nickel contracts applied to the values that flow
# through them. K9-CONTRACT-SPEC §7 defines 'Yard as "Nickel contract
# evaluation. No filesystem, no network, no subprocess. Pure." and 'Kennel as
# "Nothing. Read, parse, display. No evaluation of any kind." Declaring
# 'Kennel here would tell a conforming host not to evaluate the contracts this
# file exists to carry.

let Port = std.contract.from_predicate (fun port =>
  std.is_number port && port >= 1 && port <= 65535
) in

let NonEmptyString = std.contract.from_predicate (fun s =>
  std.is_string s && std.string.length s > 0
) in

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "ci-configuration",
    security = {
      leash = 'Yard,
      trust_level = "validated-config",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
      capabilities = [],
    },
    metadata = {
      name = "axel-ci-config",
      version = "0.1.0",
      description = "CI/CD configuration for axel-protocol, typed by Nickel contracts.",
    },
    side_effects = [],
  },

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
      idris2_required = true,
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
    formal_verification = true,
    idris2_proofs = true,
    protocol_validation = true,
    execution_attestation = true,
  },
}
