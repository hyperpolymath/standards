# SPDX-License-Identifier: AGPL-3.0-or-later
# K9 Contractile: Kennel Level (Pure Data)
# Project metadata for stamp-protocol

{
  leash = 'Kennel,

  project = {
    name = "stamp-protocol",
    version = "1.0.0",
    description = "Interactive demonstration of STAMP Protocol consent architecture",
    homepage = "https://stamp-protocol.org",
    repository = "https://github.com/hyperpolymath/stamp-protocol",

    license = "AGPL-3.0-or-later",
    author = {
      name = "Jonathan D.A. Jewell",
      email = "jonathan.jewell@open.ac.uk",
    },

    keywords = [
      "stamp-protocol",
      "consent",
      "formal-verification",
      "rescript",
      "proven",
      "idris2",
    ],
  },

  tech_stack = {
    languages = ["ReScript", "JavaScript", "HTML", "CSS"],
    runtime = "Deno",
    libraries = [
      "rescript-tea",
      "rescript-dom-mounter",
      "proven",
      "cadre-tea-router",
    ],

    build_tools = ["ReScript", "Deno"],
    package_manager = "Deno",
  },

  deployment = {
    platform = "Cloudflare Pages",
    domain = "stamp-protocol.org",
    build_command = "deno task build",
    output_directory = "/",
  },

  features = {
    proven_validation = true,
    formal_verification = true,
    security_hardening = true,
    tea_architecture = true,
    dom_mounting = "rescript-dom-mounter",
  },

  status = {
    phase = "MVP Development",
    completion_percentage = 65,
    production_ready = false,
  },
}
