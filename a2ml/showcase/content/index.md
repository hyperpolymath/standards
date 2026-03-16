---
title: A2ML
date: 2026-03-16
order: 1
---

<div class="hero">
<h1>A2ML</h1>
<p class="tagline">Attested Markup Language</p>
<p>Every AI agent needs an identity. A2ML gives them one.</p>
<p>A universal manifest format that lets AI agents declare their capabilities,
prove their provenance through attestation chains, and establish trust
through verifiable metadata.</p>
<a class="cta" href="getting-started.html">Get Started</a>
</div>

## What is A2ML?

A2ML (Attested Markup Language) is a structured document format designed for the age of AI agents. It solves a fundamental problem: **how do you know what an AI agent is, what it can do, and whether you should trust it?**

A2ML files (`.a2ml`) act as identity documents for software agents. They combine human-readable markup with machine-verifiable attestation blocks, creating a chain of trust that can be audited by both people and automated systems.

<div class="feature-grid">
<div class="feature-card">
<h3>Attestation Chains</h3>
<p>Every claim is backed by a verifiable attestation. Agents sign their capabilities, and auditors countersign. Trust is earned, not assumed.</p>
</div>
<div class="feature-card">
<h3>Provenance Tracking</h3>
<p>Know exactly where a manifest came from, who authored it, and what has changed. Full lineage from creation to deployment.</p>
</div>
<div class="feature-card">
<h3>Agent Identity</h3>
<p>Unique agent identifiers, capability declarations, and trust levels. Every agent in your system has a clear, auditable identity.</p>
</div>
<div class="feature-card">
<h3>CI/CD Native</h3>
<p>Designed to live in repositories alongside code. Validate manifests in pipelines, enforce policies in pull requests, audit in production.</p>
</div>
</div>

## Why A2ML?

As AI agents proliferate across CI/CD pipelines, security scanners, code reviewers, and orchestration systems, the question of **agent accountability** becomes critical. Who deployed this bot? What permissions does it have? Who attested to its behaviour?

A2ML answers these questions with a format that is:

- **Human-readable** — uses familiar markup syntax with headings, lists, and paragraphs
- **Machine-parseable** — directive blocks (`@attestation:` ... `@end`) carry structured data
- **Auditable** — attestation chains create a verifiable trust graph
- **Composable** — agents can reference each other's manifests to build multi-agent trust networks

## Tooling Ecosystem

A2ML is not a paper specification. It ships with real, working tools:

- **[Pandoc](https://pandoc.org)** reader, writer, filter, and template via [pandoc-a2ml](https://github.com/hyperpolymath/pandoc-a2ml) — convert A2ML to HTML, PDF, Markdown, and 40+ other formats
- **[VS Code](https://code.visualstudio.com)** syntax highlighting via [vscode-a2ml](https://github.com/hyperpolymath/vscode-a2ml)
- **[Tree-sitter](https://tree-sitter.github.io)** grammar via [tree-sitter-a2ml](https://github.com/hyperpolymath/tree-sitter-a2ml) — works in Neovim, Helix, Zed, and GitHub
- **GitHub Linguist** — language detection (submission pending)

## Part of a Larger Ecosystem

A2ML integrates with the broader hyperpolymath standards:

- **[K9 Validators](https://github.com/hyperpolymath/pandoc-k9)** — contractile enforcement for repository policies
- **[Hypatia](https://github.com/hyperpolymath/hypatia)** — neurosymbolic CI/CD security scanning that consumes A2ML manifests
- **[PanLL](https://github.com/hyperpolymath/panll)** — panel framework with A2ML-based panel identity
- **[Rhodium Standard Repositories](https://github.com/hyperpolymath/rhodium-standard-repositories)** — repository quality standard that requires A2ML manifests
