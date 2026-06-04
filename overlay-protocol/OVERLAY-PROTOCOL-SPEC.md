<!--
SPDX-License-Identifier: CC-BY-4.0
Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
-->

# Overlay Protocol Specification

**Version**: 1.0.0
**Author**: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
**License**: PMPL-1.0-or-later
**Status**: Draft

---

## 1. Introduction

The Overlay Protocol defines a pattern for non-invasive extensions to existing
software projects. An overlay declares a relationship to a base project and
maintains strict non-modification invariants, allowing the base to be updated,
rebased, or replaced independently.

The protocol recognises two peer types that share core invariants but differ in
activation mechanism and intent:

| Peer Type | Intent | Activation |
|-----------|--------|------------|
| **o-extension** | Add new capabilities alongside the base | Flag-based (environment variable, CLI flag) |
| **aggregate-library** | Curate/re-export a subset of the base | Dependency-based (import/require) |

Both are instances of the same protocol. A project MAY implement one or both
peer types. Multiple overlays MAY target the same base project.

### 1.1 Terminology

- **Base project**: The upstream project that an overlay relates to, unmodified
- **Overlay**: A project that extends or curates the base without modifying it
- **o-extension**: An overlay that adds new functionality (theories, tactics,
  modules, plugins) alongside the base
- **aggregate-library** (aLib): An overlay that selects, curates, and
  re-exports a subset of the base's API with additional semantics or tests
- **Activation**: The mechanism by which an overlay becomes effective
- **Deactivation**: Returning to vanilla base behaviour
- **Protocol peer**: Another project using the Overlay Protocol against the
  same or a related base

---

## 2. Core Invariants

All overlays, regardless of peer type, MUST satisfy these invariants:

### 2.1 Non-Modification

The overlay MUST NOT modify any file, directory, or build artefact belonging
to the base project. The base project MUST be able to function identically
with or without the overlay present.

```
INVARIANT: base_state(with_overlay) = base_state(without_overlay)
```

### 2.2 Additive Only

The overlay MUST be purely additive. It MAY:
- Add new files alongside the base
- Extend load paths, search paths, or module registries
- Provide new types, functions, theories, tactics, or bindings
- Curate and re-export subsets of the base API

It MUST NOT:
- Patch, overwrite, or shadow base files
- Modify base configuration files
- Inject code into the base's build process
- Require changes to the base to function

### 2.3 Switchable

The overlay MUST be switchable: it can be activated and deactivated without
side effects. Deactivation MUST restore exactly the vanilla base behaviour.

```
INVARIANT: deactivate(activate(base)) = base
```

### 2.4 Idempotent Activation

Activating an already-active overlay MUST be a no-op. Deactivating an
already-inactive overlay MUST be a no-op.

```
INVARIANT: activate(activate(base)) = activate(base)
INVARIANT: deactivate(deactivate(base)) = deactivate(base)
```

### 2.5 Declared Relationship

The overlay MUST declare its relationship to the base project in its
`ECOSYSTEM.scm` file using the `overlay-protocol` section (see Section 5).

---

## 3. Peer Type: o-extension

An o-extension adds new capabilities alongside a base project without
modifying it. The canonical example is adding custom theories and tactics
to a theorem prover.

### 3.1 Activation Mechanism

o-extensions use flag-based activation: environment variables, CLI flags,
or configuration toggles that extend the base's behaviour at runtime.

```bash
# Example: activate an o-extension via shell script
source overlay/activate.sh

# Example: activate via environment variable
export HOL_OEXT_ACTIVE=1
export HOL_OEXT_DIR=/path/to/overlay

# Example: deactivate
unset HOL_OEXT_ACTIVE
unset HOL_OEXT_DIR
```

### 3.2 Directory Structure

```
base-project/          # Upstream, untouched
overlay-name/          # The o-extension (peer-level sibling)
├── activate.sh        # Activation script (REQUIRED)
├── 0-AI-MANIFEST.a2ml # AI manifest
├── .machine_readable/
│   ├── ECOSYSTEM.scm  # Declares overlay-protocol section
│   ├── META.scm       # ADRs explaining o-extension choice
│   └── STATE.scm      # Progress tracking
├── theories/          # New theories (example for theorem provers)
├── tactics/           # New tactics
└── overlays/          # Load path extensions, config overlays
```

### 3.3 Activation Script Requirements

The `activate.sh` script MUST:
1. Set environment variables declaring the overlay is active
2. Extend relevant paths (load paths, module paths, etc.)
3. Be idempotent (safe to source multiple times)
4. Print no output on success (silent activation)

The corresponding deactivation MUST:
1. Unset all environment variables set by activation
2. Restore all modified paths to their original values

### 3.4 When to Use

Use an o-extension when:
- You need to add new functionality to an upstream project
- The upstream project supports path-based or plugin-based extension
- You want to track upstream without merge conflicts
- The overlay should be toggleable per-run or per-session

---

## 4. Peer Type: aggregate-library

An aggregate-library (aLib) curates and re-exports a subset of one or more
base projects' APIs, optionally adding conformance tests, semantic
documentation, or cross-implementation validation.

### 4.1 Activation Mechanism

Aggregate-libraries use dependency-based activation: they are imported or
required by consumer projects. They do not modify the base; they wrap it.

```
# Consumer adds the aLib as a dependency
# The aLib re-exports a curated subset of the base API
import { map, filter, fold } from "aggregate-library"
```

### 4.2 Directory Structure

```
aggregate-library/
├── specs/             # Formal specifications per operation
│   ├── arithmetic/
│   ├── collection/
│   └── ...
├── src/               # Reference implementation
├── scripts/           # Validation and conformance tooling
├── test/              # Conformance tests
├── .machine_readable/
│   ├── ECOSYSTEM.scm  # Declares overlay-protocol section
│   ├── META.scm
│   └── STATE.scm
└── 0-AI-MANIFEST.a2ml
```

### 4.3 Specification Requirements

Each curated operation MUST have:
1. An interface signature
2. Documented behavioural semantics (properties, edge cases)
3. Executable test cases

### 4.4 When to Use

Use an aggregate-library when:
- You need a curated, tested subset of a larger API
- You want to validate implementations across languages/ecosystems
- The base project's API is larger than what consumers typically need
- You want to add formal specifications to an informal API

---

## 5. ECOSYSTEM.scm Declaration

Every overlay MUST include an `overlay-protocol` section in its
`ECOSYSTEM.scm` file. This is the machine-readable declaration of the
overlay relationship.

### 5.1 Required Fields

```scheme
(overlay-protocol
  ((base . "<relative-path-to-base>")
   (upstream . "<upstream-URL>")
   (peer-type . "<o-extension|aggregate-library>")
   (activation . "<activation-command-or-method>")
   (deactivation . "<deactivation-command-or-method>")
   (switchable . #t)
   (modifies-base . #f)
   (description . "<human-readable description>")))
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `base` | String | Yes | Relative path from overlay to base project |
| `upstream` | URL | Yes | Canonical upstream URL of the base project |
| `peer-type` | Enum | Yes | `o-extension` or `aggregate-library` |
| `activation` | String | Yes | Command or method to activate the overlay |
| `deactivation` | String | Yes | Command or method to deactivate |
| `switchable` | Boolean | Yes | MUST be `#t` (true) |
| `modifies-base` | Boolean | Yes | MUST be `#f` (false) |
| `description` | String | Yes | Human-readable explanation of the overlay |

### 5.2 Protocol Peers

Overlays SHOULD declare awareness of other overlays targeting the same or
related base projects in the `related-projects` section:

```scheme
(overlay-protocol-peers
  ((peer-name
     ((relationship . "protocol-peer")
      (peer-type . "<o-extension|aggregate-library>")
      (description . "<what this peer does>")
      (status . "<active|planned|deprecated>")))))
```

---

## 6. Composition

### 6.1 Multiple Overlays on One Base

Multiple overlays MAY target the same base project. Each overlay operates
independently. Overlays MUST NOT depend on each other unless explicitly
declared in their `ECOSYSTEM.scm`.

When multiple o-extensions are active simultaneously:
- Load path extensions are concatenated (order-independent where possible)
- Namespace collisions MUST be avoided by the overlay authors
- Each overlay's `activate.sh` MUST be independently sourceable

### 6.2 Mixed Peer Types

A base project MAY have both o-extensions and aggregate-libraries targeting
it simultaneously. These are complementary:
- The o-extension adds new capabilities
- The aggregate-library curates existing capabilities

They do not conflict because they operate at different layers.

### 6.3 Overlay of an Overlay

An overlay MAY target another overlay as its base, creating a chain:

```
upstream → o-extension-A → o-extension-B
```

Each link in the chain MUST satisfy all core invariants independently.
`o-extension-B` MUST NOT modify `o-extension-A` or `upstream`.

---

## 7. Conformance

### 7.1 Conformance Checklist

An overlay is conformant with the Overlay Protocol if:

- [ ] `ECOSYSTEM.scm` contains `overlay-protocol` section with all required fields
- [ ] `modifies-base` is `#f`
- [ ] `switchable` is `#t`
- [ ] No files in the base project directory are created, modified, or deleted
- [ ] Activation is idempotent
- [ ] Deactivation restores vanilla base behaviour
- [ ] For o-extensions: `activate.sh` exists and is executable
- [ ] For aggregate-libraries: specs exist with test cases
- [ ] `0-AI-MANIFEST.a2ml` declares the non-modification invariant

### 7.2 Automated Validation

Conformance MAY be validated by CI/CD tooling (e.g., echidnabot, hypatia)
by checking:

1. No files exist in the base project path that are tracked by the overlay's VCS
2. The `overlay-protocol` section parses correctly
3. `modifies-base` is `#f` and `switchable` is `#t`
4. For o-extensions: `activate.sh` exits 0 and sets expected variables

---

## 8. Reference Implementations

### 8.1 o-extension: HOL-o-extension

**Location**: `echidna/HOL-o-extension/`
**Base**: HOL4 theorem prover
**Purpose**: Custom theories and tactics for ECHIDNA neurosymbolic proof search

This is the first implementation of the Overlay Protocol. It extends HOL4's
load path with custom theories and tactics via `activate.sh`, without
modifying any HOL4 source files.

### 8.2 aggregate-library: aggregate-library (aLib)

**Location**: `developer-ecosystem/aggregate-library/`
**Base**: Multiple ecosystem standard libraries
**Purpose**: Methods lab demonstrating minimal overlap specification

This demonstrates the aggregate-library peer type, providing formal
specifications and conformance tests for a curated set of operations
across programming ecosystems.

---

## 9. Future Extensions

The protocol is designed to accommodate additional peer types as they emerge.
Potential future peer types include:

- **adapter**: Translates between incompatible interfaces without modification
- **shim**: Provides backward compatibility for deprecated APIs
- **lens**: Exposes a focused view of a complex base API

New peer types MUST satisfy all core invariants from Section 2 and declare
their `peer-type` in `ECOSYSTEM.scm`.

---

## Appendix A: Full ECOSYSTEM.scm Example (o-extension)

```scheme
;; SPDX-License-Identifier: AGPL-3.0-or-later
(ecosystem
  ((metadata
     ((version . "1.0.0")
      (name . "my-o-extension")
      (type . "o-extension")
      (purpose . "Extends base-project with additional modules")))

   (overlay-protocol
     ((base . "../base-project")
      (upstream . "https://github.com/org/base-project")
      (peer-type . "o-extension")
      (activation . "source activate.sh")
      (deactivation . "unset OEXT_ACTIVE; unset OEXT_DIR")
      (switchable . #t)
      (modifies-base . #f)
      (description . "Optional extension adding modules to base-project")))

   (position-in-ecosystem
     "Peer-level sibling to base-project, extending its functionality
      without forking or patching.")

   (related-projects
     ((base-project
        ((relationship . "base")
         (path . "../base-project")
         (upstream . "https://github.com/org/base-project")
         (interaction . "Load path extension, never modification")))))))
```

## Appendix B: Full ECOSYSTEM.scm Example (aggregate-library)

```scheme
;; SPDX-License-Identifier: AGPL-3.0-or-later
(ecosystem
  ((metadata
     ((version . "1.0.0")
      (name . "my-aggregate-library")
      (type . "aggregate-library")
      (purpose . "Curated subset of base-project APIs with conformance tests")))

   (overlay-protocol
     ((base . "../base-project")
      (upstream . "https://github.com/org/base-project")
      (peer-type . "aggregate-library")
      (activation . "import my-aggregate-library")
      (deactivation . "remove dependency")
      (switchable . #t)
      (modifies-base . #f)
      (description . "Spec-driven curated subset with conformance testing")))

   (position-in-ecosystem
     "Methods lab curating and validating a subset of base-project APIs.")))
```
