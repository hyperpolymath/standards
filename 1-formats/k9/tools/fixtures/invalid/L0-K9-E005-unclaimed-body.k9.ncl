# SPDX-License-Identifier: MPL-2.0
# Negative control §4.4: a `.k9.ncl` suffix with neither a Nickel body nor the
# coordination dialect's YAML body. This is the shape of the estate's twelve
# session-management PROTOCOL.k9 stubs, which claim a K9 suffix and belong to
# no K9 dialect. Reserving the suffix is what stops a reviewer (or `file`)
# from believing a format promise the body does not keep.
protocol:
  family: "handover"
  name: "unclaimed"
  status: "stub"
