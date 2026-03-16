---
title: Security Levels
date: 2026-03-16
order: 2
---

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) -->

# Security Levels

K9 organises trust into three graduated levels. Each level defines precisely
what a component is allowed to do. The security level is not an annotation —
it is a structural property of the file, enforced by the format and validated
by every tool in the chain.

---

<div class="level-card level-card-kennel">

### Kennel — Pure Data

**File extension:** `.k9`
**Indicator:** <span class="badge badge-kennel">Kennel</span>

Kennel-level components are inert. They contain structured data and nothing
else. No expressions are evaluated, no functions are called, no external
resources are accessed. A Kennel-level file can be safely processed by any
tool, in any environment, from any source.

**What Kennel can do:**

- Declare key-value pairs, nested records, and arrays
- Carry a pedigree (name, version, description, license)
- Be parsed, displayed, queried, and transformed
- Be included in pipelines without sandboxing

**What Kennel cannot do:**

- Evaluate expressions or call functions
- Access the filesystem, network, or environment variables
- Import other files or modules
- Execute any code whatsoever

**Analogues:** `package.json`, `Cargo.toml`, YAML data files, JSON configs.

A Kennel file is what you reach for when you need structured data with
identity. It is the safest possible configuration: machine-readable, human-
writable, and provably inert.

</div>

<div class="level-card level-card-yard">

### Yard — Contracts and Types

**File extension:** `.k9.ncl`
**Indicator:** <span class="badge badge-yard">Yard</span>

Yard-level components are Nickel programs. They can use the full Nickel
contract system: types, merge operations, conditional logic, and computed
values. This is where K9 becomes more than data — it becomes configuration
that validates itself.

**What Yard can do:**

- Everything Kennel can do
- Evaluate Nickel expressions and merge records
- Apply type contracts (`Std.number.Nat`, `Std.string.NonEmpty`, custom)
- Use conditional logic (`if target.os == 'Linux then ...`)
- Import other `.ncl` files and apply contract composition
- Compute derived values from inputs

**What Yard cannot do:**

- Execute shell commands or spawn processes
- Write to the filesystem
- Access the network
- Run recipe blocks
- Anything requiring a cryptographic signature

**Analogues:** Dhall, Jsonnet, CUE — but with Nickel's formal contract system
rather than informal schema checking.

Yard is the level for CI/CD configurations, deployment manifests, and any
scenario where you need computed values with provable constraints. A Yard-level
component can guarantee that "every port in this list is unique and within the
valid range" or "if high-availability is enabled then replica count must be at
least 3."

</div>

<div class="level-card level-card-hunt">

### Hunt — Execution with Trust

**File extension:** `.k9.ncl`
**Indicator:** <span class="badge badge-hunt">Hunt</span>

Hunt-level components can do everything Yard can do, plus execute shell
commands via recipe blocks. This is the most powerful level and the most
restricted: a Hunt-level file **must** carry a valid cryptographic signature.
Unsigned Hunt files are rejected by all conforming tools.

**What Hunt can do:**

- Everything Yard can do
- Define recipe blocks (`install`, `validate`, `deploy`, `migrate`)
- Execute shell commands within those recipes
- Access the filesystem and network (within recipe scope)
- Chain recipes in defined order

**What Hunt requires:**

- A valid cryptographic signature over the file contents
- An `hunt_authorized = true` flag in the validation section
- A checksum matching the file's content hash

**Analogues:** Makefiles, Justfiles, CI/CD scripts — but with a pedigree,
contracts, and a mandatory trust chain.

Hunt is the level for setup scripts, migration tasks, deployment automation,
and anything that needs to run commands. The signature requirement ensures
that Hunt-level files cannot be injected or tampered with. You know who
authored it, you know it has not changed, and the contracts prove it is
structurally valid.

</div>

---

## Comparison

| Capability | <span class="badge badge-kennel">Kennel</span> | <span class="badge badge-yard">Yard</span> | <span class="badge badge-hunt">Hunt</span> |
|---|---|---|---|
| Structured data | Yes | Yes | Yes |
| Pedigree | Yes | Yes | Yes |
| Nickel expressions | No | Yes | Yes |
| Type contracts | No | Yes | Yes |
| Conditional logic | No | Yes | Yes |
| File imports | No | Yes | Yes |
| Shell execution | No | No | Yes |
| Filesystem access | No | No | Yes |
| Network access | No | No | Yes |
| Signature required | No | No | **Yes** |

## Trust Escalation

Moving between levels is a deliberate act with clear semantics:

**Kennel to Yard:** You are adding computation. The file moves from `.k9` to
`.k9.ncl` and gains access to the Nickel evaluator. This is appropriate when
static data is insufficient — when you need derived values, conditional logic,
or contract validation. No signature is required, but the evaluator runs in a
sandbox with no I/O access.

**Yard to Hunt:** You are adding execution. The file remains `.k9.ncl` but
the `security.trust_level` changes from `'Yard` to `'Hunt`, and the
`validation.hunt_authorized` flag must be set to `true`. A cryptographic
signature over the full file content is now mandatory. This is the most
significant escalation: you are granting the configuration the ability to run
commands on the host system.

**There is no path from Kennel directly to Hunt.** A component must pass
through Yard to reach Hunt. This ensures that every executable configuration
has been through the contract validation stage first.

### Why Not Just One Level?

Because most configuration does not need execution, and treating all
configuration as equally dangerous leads to either:

1. **Over-permissive systems** where every config file can run arbitrary code
   (the Makefile problem), or
2. **Over-restrictive systems** where configuration is limited to static data
   and all logic lives elsewhere (the JSON problem).

K9's three levels let you match the trust model to the actual requirements.
Data stays as data. Computation stays sandboxed. Execution stays signed.
