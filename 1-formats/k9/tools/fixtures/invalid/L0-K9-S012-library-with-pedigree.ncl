# SPDX-License-Identifier: MPL-2.0
# Negative control §11.2: an execution licence in a file with no envelope.
# A pedigree is a leash declaration, and the leash is detected from the
# envelope. Without `K9!` on line 1 nothing downstream can tell this file is a
# component at all, so the licence it grants is unenforceable — which is worse
# than no licence, because a reviewer sees one.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "envelopeless-component",
    security = {
      leash = 'Hunt,
      allow_network = true,
      allow_filesystem_write = true,
      allow_subprocess = true,
      signature_required = true,
      capabilities = ["net.fetch", "fs.write", "process.spawn"],
    },
    metadata = { name = "envelopeless-component" },
    side_effects = ["writes to the host filesystem"],
  },
}
