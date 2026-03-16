pandoc-a2ml
===========

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

A collection of Pandoc custom reader, writer, filter, and HTML
template for [A2ML][] (Attested Markup Language) documents.

A2ML is a typed, attested markup format designed for documents
that need structural guarantees: cross-references that resolve,
directives with typed attributes, and byte-exact opaque
payloads. Think of it as Markdown with attestation.

Files
-----

- `a2ml.lua` -- Custom reader (full-featured, 360 lines)
- `a2ml-reader.lua` -- Standalone reader (lightweight, 189 lines)
- `a2ml-writer.lua` -- Custom writer producing A2ML output
- `a2ml-filter.lua` -- Lua filter with 6 post-processing passes
- `a2ml.html` -- HTML5 template with A2ML-specific CSS

Usage
-----

### Reading A2ML

Convert A2ML documents to any Pandoc output format:

    pandoc -f a2ml.lua input.a2ml -o output.html
    pandoc -f a2ml.lua input.a2ml -t markdown
    pandoc -f a2ml.lua input.a2ml -o output.pdf

### Writing A2ML

Convert any Pandoc-supported format to A2ML:

    pandoc input.md -t a2ml-writer.lua -o output.a2ml

### Full pipeline

Use the reader, filter, and template together:

    pandoc -f a2ml.lua input.a2ml \
      --lua-filter=a2ml-filter.lua \
      --template=a2ml.html \
      -o output.html

### Round-trip

    pandoc -f a2ml.lua input.a2ml \
      -t a2ml-writer.lua -o roundtrip.a2ml

A2ML syntax
-----------

A2ML uses a familiar surface syntax:

    # Heading

    @abstract:
    This is an abstract directive block.
    @end

    Paragraphs are separated by blank lines.

    - Bullet lists work as expected
    - **Bold** and *italic* inline formatting

    @ref(heading) creates an internal cross-reference.

    @opaque(lang="json"):
    {"preserved": "byte-exact"}
    @end

    ```lua
    -- Fenced code blocks with language tags
    ```

Supported elements:

| A2ML Syntax                    | Pandoc Element |
|--------------------------------|----------------|
| `# Heading`                    | Header         |
| `@directive: ... @end`         | Div            |
| `@opaque(lang="x"): ... @end`  | CodeBlock      |
| `**bold**`                     | Strong         |
| `*italic*`                     | Emph           |
| `[label](url)`                 | Link           |
| `@ref(id)`                     | Link (internal)|
| `- list item`                  | BulletList     |
| `` ```lang ... ``` ``          | CodeBlock      |

Filter capabilities
-------------------

The `a2ml-filter.lua` provides these post-processing passes:

1. **Cross-reference resolver** -- validates `@ref(id)` links
   against actual heading anchors; unresolved refs get a
   warning and red styling.
2. **Include directive** -- `@include(file.a2ml)` pulls in
   external file content via `pandoc.read()`.
3. **TOC generator** -- auto-generates a table of contents
   from document headings.
4. **Diagram rendering** -- `mermaid` and `graphviz` code
   blocks are rendered to inline SVG via `pandoc.pipe()`.
5. **SPDX validator** -- checks for `SPDX-License-Identifier`
   in metadata or early document blocks.
6. **Metadata enrichment** -- populates version, date, and
   author from git when not present in the document.

Disable individual capabilities via metadata:

    ---
    a2ml-includes: false
    a2ml-diagrams: false
    a2ml-validate: false
    ---

HTML template
-------------

The `a2ml.html` template provides:

- SPDX badge in the document header
- Directive styling with distinct colours per type
  (`@abstract` blue, `@opaque` amber, `@fig` green,
  `@note` orange, `@warning` red)
- Responsive layout with mobile breakpoints
- Print-friendly stylesheet
- Full Pandoc syntax highlighting coverage

Requirements
------------

- Pandoc 3.0+ with Lua support
- Optional: `mmdc` (Mermaid CLI) for diagram rendering
- Optional: `dot` (Graphviz) for diagram rendering

Installation
------------

Copy the Lua files to your pandoc data directory:

    mkdir -p ~/.local/share/pandoc/
    cp a2ml.lua a2ml-reader.lua a2ml-writer.lua a2ml-filter.lua ~/.local/share/pandoc/
    cp a2ml.html ~/.local/share/pandoc/templates/

Or use them directly from this directory with explicit paths.

License
-------

MIT — see [LICENSE](LICENSE) for details.

[A2ML]: https://github.com/hyperpolymath/standards/tree/main/a2ml
