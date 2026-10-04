K9!
# SPDX-License-Identifier: MPL-2.0
# yard-typed-config.k9.ncl — positive control: contract evaluation, no I/O.
# Rule coverage: 'Yard permits Nickel evaluation, so a `validation` block is
# allowed; it is not an execution surface, so K9-S011 does not fire.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "typed-configuration",
    security = {
      leash = 'Yard,
      trust_level = "contract-eval-only",
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
      capabilities = [],
    },
    metadata = {
      name = "yard-typed-config",
      version = "1.2.0",
      description = "Typed configuration validated by Nickel contracts.",
    },
    warnings = [
      "This component is evaluated; it does not execute.",
    ],
    side_effects = [],
  },

  config = {
    port | Number = 8080,
    host | String = "localhost",
  },

  validation = {
    port_in_range = config.port > 0 && config.port < 65536,
  },
}
