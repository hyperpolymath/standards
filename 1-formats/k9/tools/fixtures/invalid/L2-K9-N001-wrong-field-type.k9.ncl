K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §12.2: a defect only L2 can see.
#
# Every lexical check passes on this file. `allow_network` is spelled correctly,
# sits at the right path, and carries a value — the value is just a String
# where the contract requires a Bool. L1 compares it against the literal
# "true", finds no match, and moves on; it has no notion of a type.
#
# This fixture is the argument for the layering: a validator that stopped at L1
# would report this component as conforming, and a host would then have to
# decide for itself what "yes" means.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "wrong-field-type",
    security = {
      leash = 'Kennel,
      allow_network = "yes",
      allow_filesystem_write = false,
      allow_subprocess = false,
      capabilities = [],
    },
    metadata = {
      name = "wrong-field-type",
      version = "1.0.0",
    },
    side_effects = [],
  },
}
