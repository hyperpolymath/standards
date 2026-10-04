K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §5.4: a component from a different schema major. It is not
# "older" or "newer", it is unreadable: a v1 reader that guesses at a v2 field
# layout produces a confident wrong answer about a security posture.
{
  pedigree = {
    schema_version = "2.0.0",
    component_type = "future-component",
    security = {
      leash = 'Kennel,
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = { name = "future-component" },
    side_effects = [],
  },
}
