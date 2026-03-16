k9
==

A collection of Pandoc custom reader, writer, filter, and HTML
template for [K9][] (Self-Validating Components) documents.

K9 is a configuration format for self-validating software
components. Each K9 file carries a **pedigree** (name, version,
security level) and validates itself against a Nickel contract
schema. K9 has three security levels:

- **Kennel** -- pure data, no execution (YAML-style `.k9`)
- **Yard** -- Nickel evaluation with contracts (`.k9.ncl`)
- **Hunt** -- full execution with shell commands, signature
  required (`.k9.ncl`)

Files
-----

- `k9.lua` -- Custom reader with YAML + Nickel support
  (597 lines)
- `k9-reader.lua` -- Lightweight standalone reader (92 lines)
- `k9-writer.lua` -- Custom writer producing K9 Nickel output
  (256 lines)
- `k9-filter.lua` -- Lua filter with 4 post-processing passes
  (407 lines)
- `k9.html` -- HTML5 template with security-level colour
  scheme (591 lines)

Usage
-----

### Reading K9

Convert K9 documents to any Pandoc output format:

    pandoc -f k9.lua hello.k9 -o output.html
    pandoc -f k9.lua config.k9.ncl -o output.html
    pandoc -f k9.lua deploy.k9.ncl -t markdown

### Writing K9

Convert any Pandoc-supported format to K9 Nickel:

    pandoc input.md -t k9-writer.lua -o component.k9.ncl

### Full pipeline

Use the reader, filter, and template together:

    pandoc -f k9.lua input.k9.ncl \
      --lua-filter=k9-filter.lua \
      --template=k9.html \
      -o output.html

Auto-detection
--------------

The reader automatically detects:

- **Syntax**: YAML (`.k9`) vs Nickel (`.k9.ncl`) from
  content patterns
- **Security level**: Kennel / Yard / Hunt from
  `trust_level` or content analysis
- **Magic number**: `K9!` presence at file start
- **SPDX license**: extracted from comment headers

K9 syntax
---------

### Kennel level (YAML-style)

    K9!
    # SPDX-License-Identifier: MIT
    ---
    metadata:
      name: hello-k9
      version: 1.0.0
      description: A friendly greeting

    content:
      greeting: "Hello from K9!"

### Yard level (Nickel)

    let pedigree = import "pedigree.ncl" in

    pedigree.K9Pedigree & {
      metadata = {
        name = "my-component",
        version = "1.0.0",
      },
      security = {
        trust_level = 'Yard,
        allow_network = false,
      },
    }

Filter capabilities
-------------------

The `k9-filter.lua` provides these post-processing passes:

1. **Security badge colouring** -- Kennel = green, Yard =
   amber, Hunt = red. Badges render as inline HTML with
   colour-coded backgrounds.
2. **Contract highlighting** -- Nickel type contracts get
   monospace styling with type-specific colours (green for
   String, blue for Number, orange for Bool, etc.)
3. **Recipe validation** -- recipe command blocks are scanned
   against 80+ known tools; unknown commands produce warnings.
4. **Pedigree completeness** -- checks that name, version,
   and description are present; warns for missing fields.

Disable capabilities via metadata:

    ---
    k9-validate: false
    k9-diagrams: false
    ---

HTML template
-------------

The `k9.html` template provides:

- Security-level header bar with gradient background
- Pedigree summary card (structured table)
- Contract annotations in monospace
- Recipe blocks with "Recipe" label badge
- Copy-to-clipboard on all code blocks
- Responsive layout with mobile grid
- Print-friendly stylesheet

Requirements
------------

- Pandoc 3.0+ with Lua support

[K9]: https://github.com/hyperpolymath/standards/tree/main/k9-svc
