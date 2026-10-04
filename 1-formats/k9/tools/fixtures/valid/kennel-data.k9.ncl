K9!
# SPDX-License-Identifier: MPL-2.0
# kennel-data.k9.ncl — positive control: a data-only component.
# Rule coverage: the smallest conforming component. No flags, so §8.4 derives
# no capability requirement and the empty grant is correct rather than thin.
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
      name = "kennel-data",
      version = "1.0.0",
      description = "Data-only K9 component: parsed and displayed, never evaluated.",
    },
    side_effects = [],
  },

  config = {
    project = "k9-conformance",
    languages = ["Nickel", "Bash"],
  },
}
