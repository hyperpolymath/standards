k9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §3.1: the magic number is exactly 0x4B 0x39 0x21.
# `k9!` is a different three octets and must not be tolerated: a magic number
# with a tolerance is not a magic number, and a kernel that sniffs on it would
# disagree with a validator that forgives it.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "bad-magic",
    security = {
      leash = 'Kennel,
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = { name = "bad-magic" },
    side_effects = [],
  },
}
