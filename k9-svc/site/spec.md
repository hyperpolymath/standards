<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
---
title: K9-SVC specification
site: K9-SVC
date: 2026-06-22
---

# Specification

K9-SVC is at **v1.0.0-alpha**. The normative contractile specification is maintained in the [standards](https://github.com/hyperpolymath/standards) repository.

## Identity

| Property | Value |
|----------|-------|
| Extensions | `.k9`, `.k9.ncl` |
| Media type | `application/vnd.k9+nickel` |
| Encoding | UTF-8 |
| Built on | Nickel, Just |
| Licence | MPL-2.0 (code), CC-BY-SA-4.0 (docs) |

## Trust levels

The core of the specification is the three-level trust model. A component must declare its level; the host environment grants capability up to — and no further than — that level.

1. **Data** — pure data, no execution, safe in all environments.
2. **Validation** — Nickel contract evaluation permitted.
3. **Execution** — full execution, requires a cryptographic signature.

## Conformance

Conformance fixtures (positive and negative) are coordinated in the [k9-ecosystem](https://github.com/hyperpolymath/k9-ecosystem) hub. Implementations validate against these vectors to claim support at a given trust level.
