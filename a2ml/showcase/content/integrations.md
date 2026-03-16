---
title: Integrations
date: 2026-03-16
order: 4
---

# Integrations

A2ML has working integrations across editors, build tools, and language ecosystems. This is not a roadmap — these tools exist today.

---

<div class="integration-item">

## Pandoc <span class="badge badge-live">Live</span>

Full Pandoc integration via [pandoc-a2ml](https://github.com/hyperpolymath/pandoc-a2ml): custom reader, writer, Lua filter, and HTML template.

Convert A2ML to any of Pandoc's 40+ output formats (HTML, PDF, DOCX, LaTeX, Markdown, reStructuredText, EPUB, and more), or convert *from* any Pandoc input format into A2ML.

```bash
# Read A2ML, output HTML
pandoc -f a2ml-reader.lua manifest.a2ml -o manifest.html

# Read A2ML, output PDF
pandoc -f a2ml-reader.lua manifest.a2ml -o manifest.pdf

# Convert Markdown to A2ML
pandoc README.md -t a2ml-writer.lua -o README.a2ml

# Apply the A2ML filter for attestation validation
pandoc -f a2ml-reader.lua --lua-filter a2ml-filter.lua manifest.a2ml -o report.html
```

**Install:** Clone the repo and add the Lua scripts to your Pandoc data directory, or reference them by path.

**Repository:** [github.com/hyperpolymath/pandoc-a2ml](https://github.com/hyperpolymath/pandoc-a2ml)

</div>

<div class="integration-item">

## VS Code <span class="badge badge-live">Live</span>

Syntax highlighting, bracket matching, and snippet support for `.a2ml` files in Visual Studio Code.

Features:
- Full TextMate grammar for A2ML syntax
- Highlighting for directives (`@attestation:` ... `@end`), headings, inline formatting, and comments
- Snippets for common patterns (attestation block, policy block, provenance block)
- File icon for `.a2ml` files
- Language configuration for bracket/comment auto-pairing

**Install:** Search "A2ML" in the VS Code marketplace, or install from VSIX.

**Repository:** [github.com/hyperpolymath/vscode-a2ml](https://github.com/hyperpolymath/vscode-a2ml)

</div>

<div class="integration-item">

## Tree-sitter <span class="badge badge-live">Live</span>

A [tree-sitter](https://tree-sitter.github.io) grammar for A2ML, enabling syntax highlighting and structural queries in any editor that supports tree-sitter.

Works with:
- **Neovim** (via nvim-treesitter)
- **Helix** (built-in tree-sitter support)
- **Zed** (built-in tree-sitter support)
- **GitHub** (syntax highlighting in code views, via tree-sitter)
- **Emacs** (via tree-sitter-langs)

The grammar parses the full A2ML syntax including nested directive blocks, attestation fields, and inline formatting.

**Repository:** [github.com/hyperpolymath/tree-sitter-a2ml](https://github.com/hyperpolymath/tree-sitter-a2ml)

</div>

<div class="integration-item">

## GitHub Linguist <span class="badge badge-pending">Pending</span>

A pull request to add A2ML to [GitHub Linguist](https://github.com/github-linguist/linguist) is in preparation. Once merged, GitHub will:

- Detect `.a2ml` files automatically
- Show A2ML in repository language statistics
- Apply syntax highlighting using the tree-sitter grammar
- Recognise `application/vnd.a2ml` as a registered media type

</div>

<div class="integration-item">

## K9 Validators <span class="badge badge-live">Live</span>

[pandoc-k9](https://github.com/hyperpolymath/pandoc-k9) provides contractile validation for A2ML files. K9 validators enforce structural and policy constraints:

- **must** — required fields and sections
- **trust** — attestation chain validity
- **dust** — deprecated pattern detection
- **intend** — intent declaration verification

K9 runs in CI/CD pipelines and can block merges when A2ML manifests fail validation.

**Repository:** [github.com/hyperpolymath/pandoc-k9](https://github.com/hyperpolymath/pandoc-k9)

</div>

<div class="integration-item">

## LuaRocks <span class="badge badge-live">Live</span>

The A2ML Pandoc components are available as a Lua library via [LuaRocks](https://luarocks.org), making it straightforward to integrate A2ML parsing into any Lua-based toolchain.

```bash
luarocks install pandoc-a2ml
```

</div>

<div class="integration-item">

## Hypatia CI/CD Scanner <span class="badge badge-live">Live</span>

[Hypatia](https://github.com/hyperpolymath/hypatia) consumes A2ML manifests as part of its neurosymbolic security scanning. It:

- Reads `0-AI-MANIFEST.a2ml` to understand repository structure
- Validates attestation chains in `.machine_readable/*.a2ml` files
- Enforces that all agents operating on a repository have valid manifests
- Reports trust-level gaps in CI pipeline output

</div>

<div class="integration-item">

## Hackage (Haskell) <span class="badge badge-soon">Coming Soon</span>

A native Haskell library for parsing and generating A2ML is in development. It will provide:

- Pure Haskell parser (no Pandoc dependency)
- Type-safe AST for A2ML documents
- Attestation chain validation
- Integration with the Pandoc Haskell library

</div>
