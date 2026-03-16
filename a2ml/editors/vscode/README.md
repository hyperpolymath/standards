# A2ML for Visual Studio Code

Syntax highlighting for [A2ML](https://github.com/hyperpolymath/standards/tree/main/a2ml) (Attested Markup Language) — a structured document format for AI agent manifests and project metadata.

## Features

- Syntax highlighting for `.a2ml` files
- Directive recognition (`@abstract`, `@opaque`, `@fig`, `@ref`, `@include`, etc.)
- Section header highlighting (`[metadata]`, `[dependencies]`, etc.)
- Inline formatting (bold, italic, links, code spans)
- Fenced code block support with language annotations
- SPDX license header recognition

## File Associations

| Extension | Language |
|-----------|----------|
| `.a2ml`   | A2ML     |

## LSP Support

For diagnostics, completions, and hover documentation, install the
[A2ML LSP server](https://github.com/hyperpolymath/standards/tree/main/a2ml/lsp)
and configure the `a2ml.server.path` setting.

## Related

- [A2ML Specification](https://github.com/hyperpolymath/standards/tree/main/a2ml/SPEC-v1.0.adoc)
- [Pandoc Reader/Writer](https://github.com/hyperpolymath/standards/tree/main/a2ml/pandoc)

## License

PMPL-1.0-or-later (Palimpsest License)
