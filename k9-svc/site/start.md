<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
---
title: Get started with K9-SVC
site: K9-SVC
description: Learn how to write K9-SVC components with trust levels, contracts, and signed execution
brand: K9-SVC
date: 2026-06-22
---

# Get started

A K9-SVC component is a Nickel value that declares its own trust level and contract. The toolchain reads the level and grants no more capability than the component asks for.

## 1. Write a component

```nickel
# config.k9.ncl
{
  trust_level = 1,            # pure data — safe anywhere
  data = {
    service = "edge",
    replicas = 3,
  },
}
```

## 2. Add a contract (level 2)

```nickel
{
  trust_level = 2,            # Nickel contract evaluation permitted
  contract = fun v => v | {
    service | String,
    replicas | Number,
  },
  data = { service = "edge", replicas = 3 },
}
```

## 3. Sign for execution (level 3)

Level 3 permits full execution and **requires a cryptographic signature**. Unsigned level-3 components are refused — escalation is never implicit.

## Files & types

- Extensions: `.k9`, `.k9.ncl`
- Media type: `application/vnd.k9+nickel`
- Encoding: UTF-8

## Tooling

Implementations and CI live in the [k9-ecosystem](https://github.com/hyperpolymath/k9-ecosystem) hub; the normative spec is in [standards](https://github.com/hyperpolymath/standards).
