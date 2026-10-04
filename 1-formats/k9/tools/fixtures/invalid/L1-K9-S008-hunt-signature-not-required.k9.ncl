K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §10.1: 'Hunt with signature_required = false.
# A Hunt component that does not require a signature has declared full system
# access and then declined the one control SPEC.adoc makes non-negotiable. The
# signature block below is present, so presence is not the problem — the
# pedigree says it is optional, and "optional" is how a gate gets skipped.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "hunt-optional-signature",
    security = {
      leash = 'Hunt,
      allow_network = true,
      allow_filesystem_write = true,
      allow_subprocess = true,
      signature_required = false,
      capabilities = ["net.fetch", "fs.write", "process.spawn"],
    },
    metadata = { name = "hunt-optional-signature" },
    side_effects = ["spawns a child process"],
    signature = {
      algorithm = "Ed25519",
      key_id = "fixture",
    },
  },
}
