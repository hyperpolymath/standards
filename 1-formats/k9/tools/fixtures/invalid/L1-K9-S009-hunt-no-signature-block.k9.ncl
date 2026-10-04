K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §10.1: 'Hunt that requires a signature and carries none.
# Note the limit of this check: it is PRESENCE. Passing it would prove only
# that a signature block exists, never that the signature verifies. K9-C001 is
# the check that could establish that, and it needs a verifier this validator
# does not embed.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "hunt-unsigned",
    security = {
      leash = 'Hunt,
      allow_network = true,
      allow_filesystem_write = true,
      allow_subprocess = true,
      signature_required = true,
      capabilities = ["net.fetch", "fs.write", "process.spawn"],
    },
    metadata = { name = "hunt-unsigned" },
    side_effects = ["spawns a child process"],
  },
}
