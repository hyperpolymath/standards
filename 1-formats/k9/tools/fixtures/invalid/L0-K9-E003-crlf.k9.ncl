K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §3.3: CRLF line endings. K9 is LF-only; a CR inside
# the magic line makes it four octets, so a reader that compares bytes and a
# reader that trims whitespace would disagree about what this file is.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "crlf",
    security = {
      leash = 'Kennel,
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
    },
    metadata = { name = "crlf" },
    side_effects = [],
  },
}
