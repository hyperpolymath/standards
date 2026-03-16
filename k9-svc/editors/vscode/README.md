# K9-SVC for Visual Studio Code

Syntax highlighting for [K9](https://github.com/hyperpolymath/standards/tree/main/k9-svc) (Self-Validating Components) — a security-tiered service configuration language built on Nickel.

## Features

- Syntax highlighting for `.k9` and `.k9.ncl` files
- Trust level keyword recognition (Kennel, Yard, Hunt)
- Pedigree field highlighting (`name`, `version`, `description`, `trust_level`)
- Nickel contract annotation colouring (`| Type`)
- Recipe block identification
- SPDX license header recognition
- K9 magic number (`K9!`) detection

## File Associations

| Extension  | Language |
|------------|----------|
| `.k9`      | K9       |
| `.k9.ncl`  | K9       |

## LSP Support

For diagnostics, completions, and hover documentation, install the
[K9 LSP server](https://github.com/hyperpolymath/standards/tree/main/k9-svc/lsp)
and configure the `k9.lsp.path` setting.

## Related

- [K9 Specification](https://github.com/hyperpolymath/standards/tree/main/k9-svc/SPEC.adoc)
- [Pandoc Reader/Writer](https://github.com/hyperpolymath/standards/tree/main/k9-svc/pandoc)
- [Pedigree Contract](https://github.com/hyperpolymath/standards/tree/main/k9-svc/pedigree.ncl)

## License

PMPL-1.0-or-later (Palimpsest License)
