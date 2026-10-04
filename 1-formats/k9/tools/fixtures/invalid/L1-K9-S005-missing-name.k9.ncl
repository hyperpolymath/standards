K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §6.4: no metadata.name. A receipt records what ran; an
# anonymous component produces a receipt that attributes nothing.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "anonymous",
    security = {
      leash = 'Kennel,
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = {
      version = "1.0.0",
    },
    side_effects = [],
  },
}
