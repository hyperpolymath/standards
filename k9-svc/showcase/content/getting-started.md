---
title: Get Started
date: 2026-03-16
order: 5
---

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) -->

# Get Started

From zero to a validated K9 component in six steps.

---

## Prerequisites

You will need:

- **Pandoc** 3.1 or later — [pandoc.org/installing.html](https://pandoc.org/installing.html)
- **Nickel** 1.5 or later — [nickel-lang.org](https://nickel-lang.org/) (for Yard/Hunt levels)
- A text editor (VS Code recommended for syntax highlighting)

---

<ol class="steps">

<li>

### Install pandoc-k9

Clone the pandoc-k9 repository and make the Lua files available to Pandoc:

```
git clone https://github.com/hyperpolymath/pandoc-k9.git
cd pandoc-k9

# Copy the Lua reader, writer, and filter to your Pandoc data directory
cp k9-reader.lua k9-writer.lua k9-filter.lua k9.lua \
   ~/.local/share/pandoc/
cp k9.html ~/.local/share/pandoc/templates/
```

Verify the installation:

```
pandoc --list-input-formats | grep k9
```

</li>

<li>

### Install the VS Code extension (optional)

For syntax highlighting in VS Code:

```
# From the VS Code marketplace
code --install-extension hyperpolymath.vscode-k9
```

This gives you security-level-aware highlighting: Kennel keywords in green,
Yard in amber, Hunt in red. It also provides bracket matching and folding
for K9 structures.

</li>

<li>

### Create your first Kennel-level file

Create a file called `hello.k9`:

```
K9!
# hello.k9 — My first K9 component
# Security Level: Kennel (pure data, no execution)

---
metadata:
  name: hello-k9
  version: 1.0.0
  description: My first self-validating component
  author: Your Name
  license: PMPL-1.0-or-later

content:
  greeting: "Hello from K9!"
  message: |
    This is a Kennel-level component.
    It contains only data — no code, no execution.
    Safe to parse from any source.

tags:
  - example
  - kennel
  - hello-world
```

This is the simplest possible K9 file. It starts with `K9!`, carries a
pedigree in the `metadata` section, and contains structured data. Nothing
more.

</li>

<li>

### Convert it with Pandoc

Use the K9 reader to convert your file to HTML:

```
pandoc -f k9.lua hello.k9 \
       --lua-filter=k9-filter.lua \
       --template=k9.html \
       -o hello.html
```

Open `hello.html` in a browser. You will see a formatted K9 document with
a security-level header bar (green for Kennel), a pedigree summary card,
and styled content sections.

Convert to other formats just as easily:

```
# To Markdown
pandoc -f k9.lua hello.k9 -t markdown -o hello.md

# To JSON (Pandoc AST)
pandoc -f k9.lua hello.k9 -t json -o hello.json
```

</li>

<li>

### Graduate to Yard level with Nickel contracts

Create a file called `config.k9.ncl`:

```
# config.k9.ncl — Yard-level configuration with contracts
# Security Level: Yard (contracts and types, no execution)

let pedigree = import "pedigree.ncl" in

pedigree.K9Pedigree & {
  metadata = {
    name = "my-service-config",
    version = "1.0.0",
    description = "Service configuration with validated contracts",
  },

  security = {
    trust_level = 'Yard,
    allow_network = false,
    allow_filesystem_write = false,
    allow_subprocess = false,
  },

  # The | operator applies a Nickel contract
  config = {
    port | std.number.Nat
         | std.contract.from_predicate (fun x => x >= 1024 && x <= 65535)
         = 8443,

    log_level | [| 'debug, 'info, 'warn, 'error |]
              = 'info,

    max_connections | std.number.Nat = 100,
  },

  validation = {
    checksum = "...",
    pedigree_version = "1.0.0",
    hunt_authorized = false,
  },
}
```

Type-check it with Nickel:

```
nickel typecheck config.k9.ncl
```

If you change `port` to `80` (below 1024) or `log_level` to `'verbose`
(not in the enum), Nickel will reject the configuration at type-check time
— before it ever reaches a running service.

</li>

<li>

### Validate with the Contractile CLI

If you have the Contractile CLI installed:

```
# Install contractile
cargo install contractile

# Create the k9 symlink
ln -sf $(which contractile) ~/.local/bin/k9

# Evaluate a K9 component
k9 eval hello.k9           # Kennel: validates pedigree
k9 eval config.k9.ncl      # Yard: validates pedigree + contracts
```

The `k9 eval` command checks pedigree structure, runs Nickel type checking
(for Yard/Hunt), and verifies signatures (for Hunt). It reports the security
level, any contract violations, and whether the component is valid.

</li>

</ol>

---

## What Next?

- **Read the [Security Levels](security-levels.html) page** to understand the
  trust model in depth.
- **Browse the [Examples](examples.html)** for real-world K9 files at each level.
- **Check the [Integrations](integrations.html)** to see how K9 fits into your
  existing toolchain.
- **Explore the [pandoc-k9 repository](https://github.com/hyperpolymath/pandoc-k9)**
  for the full source, tests, and documentation.

---

## Common Patterns

### Using K9 in CI/CD

Add K9 validation as a CI step:

```yaml
# .github/workflows/k9-validate.yml
name: K9 Validation
on: [push, pull_request]
jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v5
      - name: Install Nickel
        run: cargo install nickel-lang-cli
      - name: Validate K9 components
        run: |
          for f in $(find . -name '*.k9' -o -name '*.k9.ncl'); do
            echo "Validating $f..."
            k9 eval "$f"
          done
```

### Using K9 for Container Manifests

Pair K9 with the Stapeln container ecosystem:

```
K9!
# my-service.k9 — Container deployment manifest

---
metadata:
  name: my-service
  version: 2.0.0
  description: Production deployment for my-service
  license: PMPL-1.0-or-later

container:
  image: cgr.dev/my-org/my-service:2.0.0
  replicas: 3
  health_check:
    path: /healthz
    interval_seconds: 15
```

### Using K9 for Project Configuration

Replace your `config.yaml` or `settings.json` with a self-describing K9 file:

```
K9!
# app-config.k9 — Application configuration

---
metadata:
  name: app-config
  version: 1.0.0
  description: Runtime configuration for the application
  license: PMPL-1.0-or-later

database:
  host: localhost
  port: 5432
  name: app_production
  pool_size: 10

cache:
  backend: redis
  ttl_seconds: 300
```

The advantage over plain YAML: every tool that encounters this file knows
exactly what it is, who authored it, and what version it represents — without
parsing comments or guessing from file paths.
