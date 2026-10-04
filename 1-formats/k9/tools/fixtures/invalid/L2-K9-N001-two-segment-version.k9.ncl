K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §5.2: a two-segment schema_version.
#
# "1.0" is the major this reader speaks, and every character in it is a digit
# or a dot, so L1's bounded lexical screen accepts it — correctly, given what
# L1 claims to establish. §5.2 requires a numeric dot-TRIPLE, and it is the
# contract's `is_semver_of` that counts the segments.
#
# Read this fixture together with L2-K9-N001-wrong-field-type: between them
# they show that L1 is a screen and L2 is the decision.
{
  pedigree = {
    schema_version = "1.0",
    component_type = "short-version",
    security = {
      leash = 'Kennel,
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
      capabilities = [],
    },
    metadata = { name = "short-version" },
    side_effects = [],
  },
}
