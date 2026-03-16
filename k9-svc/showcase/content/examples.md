---
title: Examples
date: 2026-03-16
order: 3
---

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) -->

# Examples

Concrete K9 files at each security level, with explanations of what is
happening and why.

---

## 1. Kennel: Project Metadata

<span class="badge badge-kennel">Kennel</span> Pure data — safe from any source.

```
K9!
# project.k9 — Project metadata for a Rust CLI tool
# Security Level: Kennel (pure data, no execution)

---
metadata:
  name: panic-attacker
  version: 3.2.1
  description: Pre-commit validation tool for RSR repositories
  author: Jonathan D.A. Jewell
  license: PMPL-1.0-or-later

repository:
  forge: github
  owner: hyperpolymath
  name: panic-attacker
  default_branch: main

dependencies:
  runtime:
    - name: nickel
      version: ">=1.5.0"
      purpose: Contract evaluation
    - name: pandoc
      version: ">=3.1"
      purpose: K9 document processing
  build:
    - name: rust
      version: ">=1.82.0"

tags:
  - cli
  - validation
  - pre-commit
  - rsr
```

This is the simplest K9 form. It starts with the `K9!` magic bytes, contains
only structured data, and declares its pedigree in the `metadata` section.
Any tool can parse this safely — there is nothing to evaluate, nothing to
execute. The file is self-describing: you know what it is, who wrote it, and
what license governs it without consulting any external source.

---

## 2. Kennel: Container Deployment Manifest

<span class="badge badge-kennel">Kennel</span> Deployment data for a container orchestrator.

```
K9!
# deploy-manifest.k9 — Stapeln deployment for the K9 service
# Security Level: Kennel (data only)

---
metadata:
  name: k9-svc-deploy
  version: 1.0.0
  description: Deployment manifest for k9-svc container
  license: PMPL-1.0-or-later

deployment:
  image: cgr.dev/hyperpolymath/k9-svc:1.0.0
  replicas: 3
  port: 8443
  protocol: https

  resources:
    cpu_limit: "500m"
    memory_limit: "256Mi"
    cpu_request: "100m"
    memory_request: "128Mi"

  health_check:
    path: /healthz
    interval_seconds: 30
    timeout_seconds: 5

  environment:
    K9_LOG_LEVEL: info
    K9_TRUST_STORE: /etc/k9/trust
    K9_SIGNATURE_VERIFY: "true"

  volumes:
    - name: trust-store
      mount_path: /etc/k9/trust
      read_only: true
```

Even deployment manifests can be Kennel-level. The container orchestrator reads
this data and acts on it, but the manifest itself does not execute anything. The
`K9!` magic and pedigree let tooling identify this as a K9 component rather
than generic YAML.

---

## 3. Yard: CI/CD Configuration with Contracts

<span class="badge badge-yard">Yard</span> Nickel evaluation with contract validation.

```
# ci-config.k9.ncl — CI/CD pipeline configuration
# Security Level: Yard (contracts and types, no execution)

let pedigree = import "pedigree.ncl" in
let ci_contracts = import "ci-contracts.ncl" in

pedigree.K9Pedigree & {
  metadata = {
    name = "ci-pipeline",
    version = "2.1.0",
    description = "CI/CD pipeline for pandoc-k9",
  },

  security = {
    trust_level = 'Yard,
    allow_network = false,
    allow_filesystem_write = false,
    allow_subprocess = false,
  },

  # Nickel contracts enforce structural rules at type-check time
  pipeline | ci_contracts.Pipeline = {
    stages = [
      {
        name = "lint",
        image = "cgr.dev/hyperpolymath/ci-base:latest",
        commands = ["just lint", "just check-format"],
        timeout_minutes | ci_contracts.Timeout = 10,
      },
      {
        name = "test",
        image = "cgr.dev/hyperpolymath/ci-base:latest",
        commands = ["just test"],
        timeout_minutes | ci_contracts.Timeout = 30,
        needs = ["lint"],
      },
      {
        name = "build",
        image = "cgr.dev/hyperpolymath/ci-base:latest",
        commands = ["just build"],
        timeout_minutes | ci_contracts.Timeout = 20,
        needs = ["test"],
      },
    ],

    # Contract: at least one stage must exist
    # Contract: timeout_minutes must be between 1 and 120
    # Contract: 'needs' references must name existing stages
    # These are enforced by ci_contracts.Pipeline — not by comments
  },

  validation = {
    checksum = "a1b2c3d4e5f6...",
    pedigree_version = "1.0.0",
    hunt_authorized = false,
  },
}
```

This is where K9 diverges from static data formats. The `ci_contracts.Pipeline`
contract is not a schema validated externally — it is applied during Nickel
evaluation and rejects invalid configurations at type-check time. If you set
`timeout_minutes = 200`, the contract fails before the file is ever used by
a CI runner. The `allow_subprocess = false` declaration is enforced by the K9
runtime: even though this is Nickel code, it cannot spawn processes.

---

## 4. Hunt: Setup Script with Signed Recipes

<span class="badge badge-hunt">Hunt</span> Executable recipes — cryptographic signature required.

```
# setup.k9.ncl — Project setup with executable recipes
# Security Level: Hunt (execution, signature REQUIRED)
#
# K9-Signature: ed25519:JDAJewell:2026-03-16:a8f9c2...

let pedigree = import "pedigree.ncl" in

pedigree.K9Pedigree & {
  metadata = {
    name = "project-setup",
    version = "1.0.0",
    description = "Initial setup for pandoc-k9 development environment",
  },

  target = {
    os = 'Linux,
    is_edge = false,
    requires_podman = true,
  },

  security = {
    trust_level = 'Hunt,
    allow_network = true,
    allow_filesystem_write = true,
    allow_subprocess = true,
  },

  validation = {
    checksum = "b7e4f8a1c3d9...",
    pedigree_version = "1.0.0",
    hunt_authorized = true,
  },

  # Recipe blocks — these actually execute
  recipes = {
    install = m%"
      #!/bin/sh
      set -euo pipefail
      echo "Installing pandoc-k9 dependencies..."
      cargo install nickel-lang-cli
      cargo install pandoc-k9-tools
      echo "Done."
    "%,

    validate = m%"
      #!/bin/sh
      set -euo pipefail
      nickel typecheck src/contracts/*.ncl
      just test
    "%,

    deploy = m%"
      #!/bin/sh
      set -euo pipefail
      podman build -t k9-svc:latest -f Containerfile .
      podman push k9-svc:latest cgr.dev/hyperpolymath/k9-svc:latest
    "%,

    migrate = m%"
      #!/bin/sh
      echo "No migration needed for v1.0.0"
    "%,
  },
}
```

This is Hunt level. The `recipes` block contains shell scripts that will
actually run on the host. Notice the `K9-Signature` header — without a valid
Ed25519 signature, every conforming K9 tool will refuse to execute these
recipes. The pedigree tells you who authored it, the contracts prove it is
structurally valid, and the signature proves it has not been tampered with.

The `target.requires_podman = true` declaration means this component expects
a Podman runtime. The `security` block explicitly declares what I/O
capabilities the recipes require. A K9 runtime can enforce these declarations
as a sandbox policy.

---

## Key Observations

1. **Kennel files are always safe.** You can accept them from untrusted
   sources, pipe them through CI, and display them to users without risk.

2. **Yard files are safe but powerful.** They compute values and enforce
   contracts, but they cannot reach outside the Nickel evaluator's sandbox.

3. **Hunt files are powerful but accountable.** Every execution is traceable
   to a signed author, a specific version, and a validated contract.

4. **The pedigree is always present.** At every level, you know the component's
   name, version, and capabilities before you process it.
