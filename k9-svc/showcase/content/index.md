---
title: K9
date: 2026-03-16
order: 1
---

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) -->

<div class="hero">

# K9: Self-Validating Components

<p class="tagline">Configuration that knows what it is and proves what it does.</p>

<div class="hero-levels">
  <div class="hero-level hero-level-kennel">
    <span class="level-name">Kennel</span>
    <span class="level-desc">Pure data, safe anywhere</span>
  </div>
  <div class="hero-level hero-level-yard">
    <span class="level-name">Yard</span>
    <span class="level-desc">Contracts and types</span>
  </div>
  <div class="hero-level hero-level-hunt">
    <span class="level-name">Hunt</span>
    <span class="level-desc">Execution, signed</span>
  </div>
</div>

</div>

## What is K9?

K9 is a configuration format where every component carries a **pedigree** — a
machine-readable declaration of its name, version, and security level. Unlike
YAML, TOML, or JSON, K9 files are not passive blobs of data waiting for some
external tool to interpret them. They declare what they are and what they are
allowed to do.

At the simplest level, a `.k9` file is structured data (like YAML). At the
highest level, a `.k9.ncl` file is a Nickel program with formal contracts,
type checking, and even executable recipes — but only when cryptographically
signed. The security level is not an annotation bolted on after the fact; it
is intrinsic to the file format.

## Three Security Levels

K9 organises trust into three graduated levels:

- <span class="badge badge-kennel">Kennel</span> **Pure data.** No evaluation,
  no execution. `.k9` files are safe to process from any source, any pipeline,
  any CI runner. Think `package.json` or `Cargo.toml` — just data with a
  pedigree.

- <span class="badge badge-yard">Yard</span> **Nickel evaluation.** `.k9.ncl`
  files at this level use Nickel contracts and types to compute values and
  validate structure. They can express constraints like "this port must be
  between 1024 and 65535" or "if `target.os` is Linux then `requires_podman`
  must be true." No shell access, no filesystem writes — just typed computation.

- <span class="badge badge-hunt">Hunt</span> **Full execution.** `.k9.ncl`
  files at this level can contain recipe blocks that run shell commands. A
  cryptographic signature is **required**. Unsigned Hunt-level files are
  rejected. This is the level for setup scripts, deployment automation, and
  migration tasks — all with an auditable trust chain.

## Every Component Has a Pedigree

Every K9 file starts with the magic bytes `K9!` and carries a pedigree
containing at minimum:

- **name** — the component's identity
- **version** — semantic version
- **security_level** — Kennel, Yard, or Hunt
- **description** — what the component does
- **license** — SPDX identifier

This pedigree is not metadata hidden in a comment. It is part of the format,
validated by tooling, and queryable by machines.

## Where K9 is Used

K9 is designed for anywhere configuration meets trust:

- **CI/CD pipelines** — declare build steps with contract-validated parameters
- **Container deployment** — manifests that prove their own correctness
- **Project metadata** — richer than TOML, safer than arbitrary scripting
- **Deployment automation** — Hunt-level recipes with cryptographic signing
- **Package manifests** — self-validating dependency declarations

## Tooling Ecosystem

K9 is not a specification without implementation. The tooling exists today:

- **Pandoc** — full reader, writer, filter, and template support via
  [pandoc-k9](https://github.com/hyperpolymath/pandoc-k9)
- **VS Code** — syntax highlighting for `.k9` and `.k9.ncl`
- **Tree-sitter** — grammar for editor integration
- **GitHub Linguist** — language detection (PR pending)
- **Nickel** — native contract system for Yard and Hunt levels
- **Contractile CLI** — K9 validation via the `k9 eval` command

> K9 closes the gap between "configuration as data" and "configuration as code"
> by making the boundary explicit, graduated, and auditable.
