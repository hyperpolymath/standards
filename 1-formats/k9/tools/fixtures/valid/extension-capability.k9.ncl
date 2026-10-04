K9!
# SPDX-License-Identifier: MPL-2.0
# extension-capability.k9.ncl — positive control: the reserved namespace.
# Rule coverage: §8.1 — a non-core capability MUST be "x-<vendor>.<path>" so
# it can never collide with a future core name.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "accelerator-job",
    security = {
      leash = 'Yard,
      trust_level = "contract-eval-only",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
      capabilities = [
        "x-acme.gpu.alloc",
        "x-acme.gpu.release",
      ],
    },
    metadata = {
      name = "extension-capability",
      version = "0.3.0",
      description = "Grants vendor-extension capabilities only.",
    },
    side_effects = [],
  },
}
