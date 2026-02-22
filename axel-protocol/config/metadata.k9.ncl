# SPDX-License-Identifier: AGPL-3.0-or-later
# K9 Contractile: Kennel Level (Pure Data)
# Project metadata for axel-protocol

{
  leash = 'Kennel,

  project = {
    name = "axel-protocol",
    version = "0.1.0",
    description = "Attested eXecution Language - Formally verified protocol execution",
    homepage = "https://axel-protocol.org",
    repository = "https://github.com/hyperpolymath/axel-protocol",

    license = "AGPL-3.0-or-later",
    author = {
      name = "Jonathan D.A. Jewell",
      email = "jonathan.jewell@open.ac.uk",
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
