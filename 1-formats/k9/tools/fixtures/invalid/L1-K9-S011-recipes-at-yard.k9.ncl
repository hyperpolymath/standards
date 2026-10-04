K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §6.7: a `recipes` block at 'Yard.
# 'Yard permits Nickel evaluation and nothing else. A recipes block is a list
# of shell commands, so declaring one at 'Yard asks for the leash to be
# believed rather than checked — the exact inversion the leash exists to stop.
{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "sneaky-recipes",
    security = {
      leash = 'Yard,
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
      capabilities = [],
    },
    metadata = { name = "sneaky-recipes" },
    side_effects = [],
  },

  recipes = {
    default = { recipe = "run" },
    run = {
      commands = ["echo 'this should never be reachable at Yard'"],
    },
  },
}
