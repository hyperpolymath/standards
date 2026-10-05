K9!
# SPDX-License-Identifier: MPL-2.0
# K9 Contractile: Kennel Level (Pure Data)
# Project metadata for axel-protocol
#
# The leash lives at pedigree.security.leash (K9-S014). A top-level `leash`
# was the old shape: nothing that applies the K9 contract reads it, so the
# file declared a security level no host enforced.
#
# 'Kennel is the honest level for this body: it is a record of literals with
# no contract annotation anywhere, so it is read, parsed and displayed and
# never evaluated (§7). The leash a host reads is now the one a host enforces.

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "project-metadata",
    security = {
      leash = 'Kennel,
      trust_level = "data-only",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
      capabilities = [],
    },
    metadata = {
      name = "axel-project-metadata",
      version = "0.1.0",
      description = "Project metadata for axel-protocol; pure data, never evaluated.",
    },
    side_effects = [],
  },

  project = {
    name = "axel-protocol",
    version = "0.1.0",
    description = "Attested eXecution Language - Formally verified protocol execution",
    homepage = "https://axel-protocol.org",
    repository = "https://github.com/hyperpolymath/axel-protocol",

    license = "PMPL-1.0-or-later",
    author = {
      name = "Jonathan D.A. Jewell",
      email = "j.d.a.jewell@open.ac.uk",
    },

    keywords = [
      "axel",
      "formal-verification",
      "protocol-execution",
      "attested-execution",
      "rescript",
      "proven",
      "idris2",
    ],
  },

  tech_stack = {
    languages = ["ReScript", "JavaScript", "Idris2", "HTML", "CSS"],
    runtime = "Deno",
    libraries = [
      "rescript-tea",
      "rescript-dom-mounter",
      "proven",
      "cadre-tea-router",
    ],

    build_tools = ["ReScript", "Deno", "Idris2"],
    package_manager = "Deno",
  },

  deployment = {
    platform = "Cloudflare Pages",
    domain = "axel-protocol.org",
    build_command = "deno task build",
    output_directory = "/",
  },

  features = {
    proven_validation = true,
    formal_verification = true,
    attested_execution = true,
    protocol_validation = true,
    tea_architecture = true,
    dom_mounting = "rescript-dom-mounter",
  },

  status = {
    phase = "Early Development",
    completion_percentage = 30,
    production_ready = false,
  },
}
