K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §6.5: 'Hunt with an empty side_effects list.
# A component claiming network, filesystem and subprocess access while
# describing none of it gives the dry-run reviewer nothing to review, which
# makes the `dry_run` precondition (§9) impossible to discharge honestly.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "hunt-undocumented",
    security = {
      leash = 'Hunt,
      allow_network = true,
      allow_filesystem_write = true,
      allow_subprocess = true,
      signature_required = true,
      capabilities = ["net.fetch", "fs.write", "process.spawn"],
    },
    metadata = { name = "hunt-undocumented" },
    side_effects = [],
    signature = {
      algorithm = "Ed25519",
      key_id = "fixture",
    },
  },
}
