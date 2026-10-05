# SPDX-License-Identifier: MPL-2.0
# Negative control §4.4: a `.k9.ncl` suffix with neither a Nickel body nor the
# coordination dialect's YAML body. This is the shape of the estate's twelve
# session-management stubs, which claimed a K9 suffix and belonged to no K9
# dialect (renamed to PROTOCOL.yaml by migration item M2, 2026-10-05; kept as
# the regression control for this rule). Reserving the suffix is what stops a reviewer (or `file`)
# from believing a format promise the body does not keep.
protocol:
  family: "handover"
  name: "unclaimed"
  status: "stub"
