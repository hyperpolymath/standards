K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §8.4: a security flag with nothing in the grant to pay for
# it. Before #1058 this was accepted — the flags and the grant were two
# unrelated opinions in the same block, so "default-deny" denied nothing.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "ungranted-flag",
    security = {
      leash = 'Yard,
      allow_network = true,
      allow_filesystem_write = false,
      allow_subprocess = false,
      capabilities = [],
    },
    metadata = { name = "ungranted-flag" },
    side_effects = [],
  },
}
