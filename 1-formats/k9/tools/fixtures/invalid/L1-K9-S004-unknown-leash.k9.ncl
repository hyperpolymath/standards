K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §7.1: a leash tag outside the closed set. The taxonomy is
# closed on purpose — a fourth level whose meaning each host invents is exactly
# how a "restricted" component ends up with full access on one implementation.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "unknown-leash",
    security = {
      leash = 'Paddock,
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = { name = "unknown-leash" },
    side_effects = [],
  },
}
