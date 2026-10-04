# SPDX-License-Identifier: MPL-2.0
# library-base.ncl — positive control: the library dialect (§11).
#
# No `K9!` envelope, no pedigree, no leash claim. This file is imported; it is
# never executed as a component. That is the whole contract a library has to
# satisfy, and it is a negative one: an execution licence in a file with no
# envelope would be unenforceable, because nothing downstream could see it.
{
  shared_metadata = {
    maintainer = "K9 conformance suite",
    licence = "MPL-2.0",
  },

  default_warnings = [
    "Imported from library-base.ncl; this file carries no execution licence.",
  ],
}
