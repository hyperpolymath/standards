K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §6.2: an unfilled component_type placeholder. This is the
# exact state of .machine_readable/svc/k9/template-hunt.k9.ncl today: the field
# exists, so a presence check passes, and the value says nothing.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "TODO: describe component type (e.g., 'deployment')",
    security = {
      leash = 'Kennel,
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = { name = "placeholder-type" },
    side_effects = [],
  },
}
