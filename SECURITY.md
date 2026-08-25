<!--
SPDX-License-Identifier: CC-BY-SA-4.0
SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->

# Security Policy

**The security policy for this repository lives in [`SECURITY.adoc`](SECURITY.adoc).**

Please read that file. It is the authoritative document and covers the GitHub
Security Advisories workflow, the response timeline, and the coordinated
disclosure policy. See also [`SECURITY-ADVISORIES.adoc`](SECURITY-ADVISORIES.adoc).

## Why this file exists

This estate writes prose in AsciiDoc, so every community-health document here is
`.adoc` — `README`, `CONTRIBUTING`, `CODE_OF_CONDUCT`, `GOVERNANCE` and
`SECURITY` alike. That is a deliberate convention, not an omission.

Several tools nonetheless look for the literal filename `SECURITY.md` and
report the policy as missing when they do not find it:

- `hypatia/scorecard/SecurityPolicy` and `hypatia/cicd_rules/missing_requirement`
  raise code-scanning alerts against this repository
- `.github/workflows/scorecard-enforcer.yml`'s `check-critical` job runs
  `if [ ! -f "SECURITY.md" ]` and **fails on every push** without it

This pointer satisfies those literal filename checks without duplicating the
policy text, so there is exactly one source of truth. Do not copy the policy
into this file — it will drift.

The alternative fix would be to teach the Hypatia rule and the enforcer script
to accept `SECURITY.adoc`. That is the more consistent long-term answer and is
worth doing, but it changes two pieces of shared infrastructure rather than
adding one pointer file.
