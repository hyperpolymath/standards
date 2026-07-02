# SPDX-License-Identifier: MPL-2.0
# K9-SVC template: trust level 3 — full execution, requires a cryptographic
# signature. This file is illustrative: `signature` below is a PLACEHOLDER.
# Unsigned or placeholder-signed level-3 components are refused by the
# toolchain — generate a real signature with your k9-svc tooling before use.
{
  trust_level = 3,
  contract = fun v => v | {
    action | String,
  },
  data = { action = "deploy" },
  signature = "REPLACE_WITH_REAL_SIGNATURE",
}
