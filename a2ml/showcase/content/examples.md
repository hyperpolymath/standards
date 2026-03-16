---
title: Examples
date: 2026-03-16
order: 3
---

# Examples

Real-world A2ML manifests demonstrating the format in practice. Each example shows a different use case, from minimal declarations to multi-agent orchestration.

## 1. Minimal Manifest

The simplest possible A2ML file. Declares an agent with a single self-attestation.

```
;; SPDX-License-Identifier: PMPL-1.0-or-later

# Greeter Bot

@abstract:
A simple bot that greets new contributors in pull requests.
@end

@attestation:
agent-id: greeter-bot
attested-by: greeter-bot
trust-level: self-declared
capabilities:
  - pr-comment
scope: repository
@end
```

This is enough for tooling to identify the agent, understand what it does, and record that its capabilities are self-declared (i.e., not yet independently verified).

---

## 2. CI/CD Agent Manifest

A GitHub Actions bot that runs in CI pipelines, with a verified attestation from the security team.

```
;; SPDX-License-Identifier: PMPL-1.0-or-later

# rhodibot — Repository Automation Agent

@abstract:
rhodibot automates repository maintenance tasks including
label management, issue triage, and PR hygiene checks.
It operates within GitHub Actions workflows.
@end

@provenance:
created-by: Jonathan D.A. Jewell
created: 2026-01-20
last-modified: 2026-03-14
source: https://github.com/hyperpolymath/gitbot-fleet
version: 2.1.0
@end

## Capabilities

- Applies labels based on file paths and PR content
- Triages issues by parsing title and body against known patterns
- Enforces branch naming conventions
- Validates commit message format
- Checks PR size and flags oversized changes

@attestation:
agent-id: rhodibot
attested-by: rhodibot
trust-level: self-declared
timestamp: 2026-01-20T09:00:00Z
capabilities:
  - label-management
  - issue-triage
  - branch-validation
  - commit-validation
  - pr-size-check
scope: organization
@end

@attestation:
agent-id: rhodibot
attested-by: security-team
trust-level: verified
timestamp: 2026-02-15T16:45:00Z
verifies: rhodibot/self-declared/2026-01-20
signature: sha256:4e2a91f7c8d3b0...
note: Verified that rhodibot only reads repository metadata
      and does not modify code or secrets.
@end

@policy:
require: attestation.trust-level >= verified
enforce: github-actions
action: allow-execution
@end

@refs:
[1] gitbot-fleet Documentation, https://github.com/hyperpolymath/gitbot-fleet
[2] Rhodium Standard Repositories, https://github.com/hyperpolymath/rhodium-standard-repositories
@end
```

---

## 3. Security Scanner Manifest

Hypatia, a neurosymbolic security scanner, declaring its scanning capabilities with an audited attestation.

```
;; SPDX-License-Identifier: PMPL-1.0-or-later

# Hypatia — Neurosymbolic Security Scanner

@abstract:
Hypatia performs multi-layered security analysis combining
rule-based scanning with neurosymbolic reasoning. It detects
secrets, vulnerable dependencies, misconfigured workflows,
and policy violations.
@end

@provenance:
created-by: Jonathan D.A. Jewell
created: 2025-11-01
last-modified: 2026-03-16
source: https://github.com/hyperpolymath/hypatia
version: 3.4.0
@end

## Scan Modules

- **Secret detection** — API keys, tokens, credentials in source
- **Dependency audit** — CVE matching against known vulnerabilities
- **Workflow analysis** — GitHub Actions misconfigurations
- **Policy enforcement** — RSR compliance, license headers, file locations
- **Neurosymbolic reasoning** — pattern inference beyond static rules

@attestation:
agent-id: hypatia-scanner-v3
attested-by: hypatia-scanner-v3
trust-level: self-declared
timestamp: 2025-11-01T12:00:00Z
capabilities:
  - secret-detection
  - dependency-audit
  - workflow-analysis
  - policy-enforcement
  - neurosymbolic-reasoning
scope: global
@end

@attestation:
agent-id: hypatia-scanner-v3
attested-by: security-team
trust-level: verified
timestamp: 2026-01-10T14:00:00Z
verifies: hypatia-scanner-v3/self-declared/2025-11-01
signature: sha256:8b3d7f2e1a...
@end

@attestation:
agent-id: hypatia-scanner-v3
attested-by: independent-security-review
trust-level: audited
timestamp: 2026-02-28T10:30:00Z
verifies: hypatia-scanner-v3/verified/2026-01-10
audit-report: https://audits.hyperpolymath.dev/hypatia-v3
signature: sha256:c91e5a3b0d...
@end

@refs:
[1] Hypatia Documentation, https://github.com/hyperpolymath/hypatia
[2] OWASP Top 10 for LLM Applications
[3] SLSA Build Provenance, https://slsa.dev
@end
```

---

## 4. Multi-Agent Orchestration

Multiple agents referencing each other's attestations to establish a trust network for a deployment pipeline.

```
;; SPDX-License-Identifier: PMPL-1.0-or-later

# Deployment Pipeline — Agent Trust Network

@abstract:
This manifest defines the trust relationships between agents
involved in the production deployment pipeline. Each agent
attests to the capabilities of the agents it depends on.
@end

## Pipeline Agents

The deployment pipeline involves four agents, each responsible
for a stage of the process:

1. **codebot** — code review and static analysis
2. **hypatia** — security scanning
3. **sustainabot** — supply chain and dependency health
4. **finishbot** — final approval and deployment trigger

## Trust Chain

@attestation:
agent-id: codebot-v2
attested-by: codebot-v2
trust-level: self-declared
timestamp: 2026-03-01T09:00:00Z
capabilities:
  - code-review
  - style-enforcement
  - complexity-analysis
scope: pipeline
@end

@attestation:
agent-id: sustainabot
attested-by: sustainabot
trust-level: self-declared
timestamp: 2026-03-01T09:00:00Z
capabilities:
  - dependency-health
  - license-compliance
  - supply-chain-audit
scope: pipeline
@end

;; Finishbot trusts codebot and sustainabot, and requires
;; hypatia to have been audited before it will approve.

@attestation:
agent-id: finishbot
attested-by: finishbot
trust-level: self-declared
timestamp: 2026-03-01T09:00:00Z
capabilities:
  - deployment-approval
  - rollback-trigger
requires:
  - codebot-v2/verified
  - hypatia-scanner-v3/audited
  - sustainabot/verified
scope: pipeline
@end

@policy:
require: all-agents.trust-level >= verified
require: hypatia-scanner-v3.trust-level == audited
enforce: deployment-gate
action: block-deploy
message: All pipeline agents must be verified.
         Hypatia must be independently audited.
@end

@refs:
[1] gitbot-fleet, https://github.com/hyperpolymath/gitbot-fleet
[2] Hypatia Scanner, https://github.com/hyperpolymath/hypatia
[3] SLSA Framework, https://slsa.dev
@end
```

This example shows how `finishbot` will refuse to approve a deployment unless `codebot` and `sustainabot` are at least `verified`, and `hypatia` has been independently `audited`. The policy block makes this machine-enforceable.
