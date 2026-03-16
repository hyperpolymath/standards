---
title: Integrations
date: 2026-03-16
order: 4
---

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) -->

# Integrations

K9 is supported across editors, build tools, language ecosystems, and CI/CD
systems. These are not aspirational — they are implemented and available.

---

<div class="integration-grid">

<div class="integration-item">

#### Pandoc

Full reader, writer, Lua filter, and HTML template for K9 documents. Convert
`.k9` and `.k9.ncl` files to HTML, Markdown, JSON, or any Pandoc output
format. The reader parses pedigree, contracts, and recipes into Pandoc's AST.
The writer emits valid K9 from any Pandoc source.

[pandoc-k9 on GitHub](https://github.com/hyperpolymath/pandoc-k9)

</div>

<div class="integration-item">

#### VS Code

Syntax highlighting for `.k9` (Kennel) and `.k9.ncl` (Yard/Hunt) files.
Security-level-aware colouring: Kennel keywords in green, Yard in amber, Hunt
in red. Bracket matching, comment toggling, and folding for K9 structures.

[vscode-k9 on GitHub](https://github.com/hyperpolymath/vscode-k9)

</div>

<div class="integration-item">

#### Tree-sitter

Tree-sitter grammar for K9 syntax, enabling editor support in Neovim,
Helix, Emacs, and any tree-sitter-compatible editor. Provides structural
parsing for syntax highlighting, code folding, and incremental parsing.

[tree-sitter-k9 on GitHub](https://github.com/hyperpolymath/tree-sitter-k9)

</div>

<div class="integration-item">

#### GitHub Linguist

Language detection for K9 files in GitHub repositories. When merged,
GitHub will recognise `.k9` and `.k9.ncl` files, display them with
K9 syntax highlighting, and include them in repository language statistics.

[Linguist PR (pending)](https://github.com/github-linguist/linguist/pulls)

</div>

<div class="integration-item">

#### Nickel

K9's contract system is built on [Nickel](https://nickel-lang.org/), the
configuration language with formal contracts. Yard and Hunt-level K9 files
are valid Nickel programs that import a shared pedigree contract. Nickel's
type system, merge semantics, and contract checking are K9's foundation.

[nickel-lang.org](https://nickel-lang.org/)

</div>

<div class="integration-item">

#### Contractile CLI

The Contractile CLI includes a `k9` subcommand for evaluating K9 components.
Run `k9 eval` to type-check contracts, verify signatures, and validate
pedigrees. Integrates with the broader Contractile toolchain (`must`, `trust`,
`dust`, `intend`).

[contractiles on GitHub](https://github.com/hyperpolymath/contractiles)

</div>

<div class="integration-item">

#### LuaRocks

Lua library for parsing and emitting K9 files. Used internally by the Pandoc
reader and writer but also available as a standalone library for Lua projects.
Handles all three security levels and validates pedigree structure.

[lua-k9 on LuaRocks](https://luarocks.org/modules/hyperpolymath/lua-k9)

</div>

<div class="integration-item">

#### Hackage (coming soon)

Haskell library for K9 parsing, validation, and emission. Will provide
`Data.K9.Pedigree`, `Data.K9.Parse`, and `Data.K9.Contract` modules. Planned
for release alongside the Pandoc native reader.

</div>

</div>

---

## Integration Architecture

K9 tooling follows a layered model:

### Layer 1: Parsing

Every tool that reads K9 must handle the `K9!` magic number and parse
the pedigree. For Kennel-level files, this is all that is needed — the
rest is standard YAML-like structured data.

### Layer 2: Evaluation

Tools that support Yard-level files must integrate the Nickel evaluator.
This means importing contracts, running type checks, and resolving merge
operations. The Nickel evaluator runs in a sandbox with no I/O access.

### Layer 3: Execution

Tools that support Hunt-level files must verify cryptographic signatures
before executing recipe blocks. The signature verification happens before
any code runs — an invalid or missing signature means the recipes are
never executed.

### Layer 4: Transformation

Pandoc sits at this layer. It can read K9 at any level, transform it
through its AST, and emit it in any output format. The K9 Lua filter
annotates the AST with security-level metadata, contract types, and
pedigree information for rich HTML output.

---

## Repository Links

| Integration | Repository | Status |
|---|---|---|
| Pandoc reader/writer/filter/template | [pandoc-k9](https://github.com/hyperpolymath/pandoc-k9) | Released |
| VS Code extension | [vscode-k9](https://github.com/hyperpolymath/vscode-k9) | Released |
| Tree-sitter grammar | [tree-sitter-k9](https://github.com/hyperpolymath/tree-sitter-k9) | Released |
| GitHub Linguist | [linguist PR](https://github.com/github-linguist/linguist) | Pending |
| Nickel contracts | [pandoc-k9/src/contracts](https://github.com/hyperpolymath/pandoc-k9/tree/main/src/contracts) | Released |
| Contractile CLI | [contractiles](https://github.com/hyperpolymath/contractiles) | Released |
| Lua library | [lua-k9](https://luarocks.org/modules/hyperpolymath/lua-k9) | Released |
| Haskell library | — | Planned |
