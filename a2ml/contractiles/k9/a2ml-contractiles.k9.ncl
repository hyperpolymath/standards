# SPDX-License-Identifier: AGPL-3.0-or-later
# a2ml-contractiles.k9.ncl - Yard-level K9 component for A2ML contractiles
# Validates A2ML contractiles files with the A2ML CLI.

# NOTE: Update the import path to your local k9-svc checkout if needed.
let pedigree = import "../../../k9-svc/pedigree.ncl" in

pedigree.K9Pedigree & {
  metadata = {
    name = "a2ml-contractiles",
    version = "0.0.1",
    description = "Self-validating A2ML contractiles (Yard level)",
  },

  target = {
    os = 'Linux,
    is_edge = false,
    requires_podman = false,
  },

  security = {
    trust_level = 'Yard,
    allow_network = false,
    allow_filesystem_write = false,
    allow_subprocess = false,
  },

  validation = {
    checksum = "sha256:placeholder",
    pedigree_version = "1.0.0",
    hunt_authorized = false,
  },

  recipes = {
    install = "echo 'No install required'",
    validate = "a2ml validate contractiles/must/Mustfile.a2ml contractiles/trust/Trustfile.a2ml contractiles/dust/Dustfile.a2ml contractiles/lust/Intentfile.a2ml",
    deploy = "echo 'No deploy step'",
    migrate = "echo 'No migration needed'",
  },
}
