# SPDX-License-Identifier: MPL-2.0
# K9-SVC template: trust level 2 — Nickel contract evaluation permitted.
{
  trust_level = 2,
  contract = fun v => v | {
    service | String,
    replicas | Number,
  },
  data = { service = "edge", replicas = 3 },
}
