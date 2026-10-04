K9!
# Negative control §3.2: no SPDX-License-Identifier in the first five lines.
# The licence identifier is envelope metadata, so this is an L0 defect, not a
# style note: an unlicensed component cannot be redistributed.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "no-licence",
    security = {
      leash = 'Kennel,
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = { name = "no-licence" },
    side_effects = [],
  },
}
