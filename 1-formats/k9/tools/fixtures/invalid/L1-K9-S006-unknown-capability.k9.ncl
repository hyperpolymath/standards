K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §8.1: a capability name that is neither core nor a
# well-formed "x-<vendor>.<path>" extension. Reserving the namespace is what
# stops a vendor name from being silently promoted into a core meaning later.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "bad-capability",
    security = {
      leash = 'Yard,
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
      capabilities = [
        "fs.delete",
        "x-acme",
      ],
    },
    metadata = { name = "bad-capability" },
    side_effects = [],
  },
}
