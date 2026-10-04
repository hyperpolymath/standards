# SPDX-License-Identifier: MPL-2.0
# Negative control §11.3: a leash claim outside pedigree.security.
# This is the shape of 2-protocols/axel/config/*.k9.ncl. A top-level `leash`
# is not a pedigree field, so no host applying the contract would ever read
# it — the file declares a security level nothing enforces.
{
  leash = 'Kennel,

  project = {
    name = "stray-leash",
    version = "0.1.0",
  },
}
