---
title: Get Started
date: 2026-03-16
order: 5
---

# Get Started

A step-by-step guide to creating your first A2ML manifest, validating it, and integrating it into your project.

## Prerequisites

- [Pandoc](https://pandoc.org/installing.html) 3.0 or later
- Git (for cloning the tooling)
- A text editor (VS Code recommended for syntax highlighting)

---

<div class="step" data-step="1">

## Install pandoc-a2ml

Clone the pandoc-a2ml repository and note the path to the Lua scripts. No compilation needed — the reader, writer, and filter are pure Lua.

```bash
git clone https://github.com/hyperpolymath/pandoc-a2ml.git
cd pandoc-a2ml
```

The key files are:
- `a2ml-reader.lua` — reads `.a2ml` files into Pandoc's AST
- `a2ml-writer.lua` — writes Pandoc AST as `.a2ml` output
- `a2ml-filter.lua` — Lua filter for attestation processing

You can either add these to your Pandoc data directory (`~/.local/share/pandoc/`) or reference them by path.

</div>

<div class="step" data-step="2">

## Install the VS Code Extension

For syntax highlighting while editing `.a2ml` files:

1. Open VS Code
2. Go to Extensions (Ctrl+Shift+X)
3. Search for **"A2ML"**
4. Click Install

Alternatively, clone and install manually:

```bash
git clone https://github.com/hyperpolymath/vscode-a2ml.git
cd vscode-a2ml
code --install-extension .
```

You should now see syntax highlighting for `.a2ml` files, including directive blocks, headings, and comments.

</div>

<div class="step" data-step="3">

## Create Your First Manifest

Create a file called `0-AI-MANIFEST.a2ml` in your repository root. This is the entry point that AI agents and tooling will read first.

```
;; SPDX-License-Identifier: PMPL-1.0-or-later

# AI Manifest — my-project

@abstract:
This repository contains my-project, a tool for doing useful things.
This manifest declares the AI agents that operate on this repository
and the policies that govern their behaviour.
@end

@provenance:
created-by: Your Name
created: 2026-03-16
source: https://github.com/your-org/my-project
version: 1.0.0
@end

## Repository Structure

- Source code in `src/`
- Machine-readable metadata in `.machine_readable/`
- CI/CD workflows in `.github/workflows/`

## Agents

No AI agents currently operate on this repository.

@refs:
[1] A2ML Specification, https://a2ml.hyperpolymath.dev/specification.html
@end
```

</div>

<div class="step" data-step="4">

## Validate with Pandoc

Check that your manifest parses correctly by converting it to HTML:

```bash
pandoc -f path/to/a2ml-reader.lua 0-AI-MANIFEST.a2ml -o manifest.html
```

If the file is well-formed, Pandoc will produce clean HTML output. Open `manifest.html` in a browser to verify the structure.

For deeper validation using the A2ML filter (checks attestation structure):

```bash
pandoc -f path/to/a2ml-reader.lua \
       --lua-filter path/to/a2ml-filter.lua \
       0-AI-MANIFEST.a2ml \
       -o validated.html
```

</div>

<div class="step" data-step="5">

## Add Machine-Readable Metadata

For full Rhodium Standard compliance, create the `.machine_readable/` directory and add A2ML metadata files:

```bash
mkdir -p .machine_readable/anchors .machine_readable/policies
```

Create `.machine_readable/STATE.a2ml`:

```
;; SPDX-License-Identifier: PMPL-1.0-or-later

# Project State

@abstract:
Current state of my-project development.
@end

## Status

- Phase: initial setup
- Completion: 10%
- Next milestone: core functionality

## Blockers

None currently.
```

Create `.machine_readable/META.a2ml`:

```
;; SPDX-License-Identifier: PMPL-1.0-or-later

# Project Metadata

@abstract:
Architecture decisions and governance for my-project.
@end

## Architecture Decisions

- Language: chosen based on team expertise
- License: PMPL-1.0-or-later for original code
```

These files give AI agents and automated tooling a structured understanding of your project's state, architecture, and ecosystem position.

</div>

<div class="step" data-step="6">

## Add an Agent Attestation

When you introduce an AI agent to your workflow (a CI bot, a code reviewer, a security scanner), declare it in the manifest:

```
;; Add this to 0-AI-MANIFEST.a2ml or a dedicated agent file

@attestation:
agent-id: my-ci-bot
attested-by: my-ci-bot
trust-level: self-declared
timestamp: 2026-03-16T12:00:00Z
capabilities:
  - lint-checking
  - test-execution
scope: repository
@end
```

As the agent is reviewed and verified, add higher-trust attestations:

```
@attestation:
agent-id: my-ci-bot
attested-by: security-team
trust-level: verified
timestamp: 2026-03-20T15:00:00Z
verifies: my-ci-bot/self-declared/2026-03-16
signature: sha256:your-signature-here
@end
```

</div>

---

## Next Steps

- Read the full [Specification](specification.html) for all directives and fields
- Browse [Examples](examples.html) for real-world patterns
- Explore [Integrations](integrations.html) for editor and CI tooling
- Check out the source repositories:
  - [pandoc-a2ml](https://github.com/hyperpolymath/pandoc-a2ml)
  - [vscode-a2ml](https://github.com/hyperpolymath/vscode-a2ml)
  - [tree-sitter-a2ml](https://github.com/hyperpolymath/tree-sitter-a2ml)
  - [pandoc-k9](https://github.com/hyperpolymath/pandoc-k9)
