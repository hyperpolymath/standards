K9!
# SPDX-License-Identifier: MPL-2.0
# Negative control §11.4: an import that does not resolve.
# The pedigree is built by merging the imported file, so a dangling import
# means the pedigree a reviewer read is not the pedigree a host would evaluate.
# This is the live state of all six .machine_readable/contractiles/*/*.k9.ncl
# files, which import "../k9/template-hunt.k9.ncl" — a path that does not exist.
let base = import "does-not-exist.ncl" in

{
  pedigree = {
    schema_version = "1.0.0",
    component_type = "dangling-import",
    security = {
      leash = 'Kennel,
      allow_network = false,
      allow_filesystem_write = false,
      allow_subprocess = false,
      capabilities = [],
    },
    metadata = { name = "dangling-import" },
    side_effects = [],
  },
}
