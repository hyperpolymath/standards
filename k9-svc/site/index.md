<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> -->
---
title: K9-SVC
site: K9-SVC
description: K9-SVC is a Nickel-based format for self-validating configuration and contractiles, with a three-level trust model from inert data to signed execution.
date: 2026-06-29
---

# K9-SVC — Self-Validating Components

<p class="lede">A Nickel-based format for configuration and contractiles that carries its own guarantees. Components validate themselves, with a trust model that scales from inert data to cryptographically signed execution.</p>

<div class="badges">
<span class="badge">v1.0.0-alpha</span>
<span class="badge">Nickel contractiles</span>
<span class="badge">.k9 / .k9.ncl</span>
<span class="badge">application/vnd.k9+nickel</span>
<span class="badge">MPL-2.0 / CC-BY-SA-4.0</span>
</div>

<div class="btn-row">
<a class="btn btn-primary" href="/start.html">Get started</a>
<a class="btn btn-ghost" href="/spec.html">Read the spec</a>
<a class="btn btn-ghost" href="https://github.com/hyperpolymath/k9-ecosystem">GitHub</a>
</div>

## The three-level trust model

K9-SVC is safe by construction: a component declares how much trust it requires, and the environment grants no more.

| Level | Name | What's permitted |
|-------|------|------------------|
| **1** | Data | Pure data, no execution — safe in all environments. |
| **2** | Validation | Nickel contract evaluation permitted. |
| **3** | Execution | Full execution — requires a cryptographic signature. |

## What it does

<div class="cards">
<div class="card">
<h3>Self-validating</h3>
<p>A component states its own contract. Validation is intrinsic, not bolted on.</p>
</div>
<div class="card">
<h3>Contractiles in Nickel</h3>
<p>Contracts are written in Nickel — composable, evaluable, and precise.</p>
</div>
<div class="card">
<h3>Signed execution</h3>
<p>Anything beyond validation requires a cryptographic signature. No silent escalation.</p>
</div>
<div class="card">
<h3>First-class media type</h3>
<p><code>application/vnd.k9+nickel</code> — registered, addressable, toolable.</p>
</div>
</div>

## A taste

```nickel
# greeting.k9.ncl — a level-2 self-validating component
{
  trust_level = 2,
  contract = fun value => value | { name | String, repeat | Number },
  data = { name = "world", repeat = 3 },
}
```

## Downloads & roadmap

Grab a [starter template](/downloads.html) for whichever trust level fits, or see the [roadmap](/roadmap.html) for what's next — including an interactive teaching area and [k9iser](https://github.com/hyperpolymath/k9-ecosystem/tree/main/members/tooling/k9iser).

## Part of the standards estate

K9-SVC is coordinated through the [k9-ecosystem](https://github.com/hyperpolymath/k9-ecosystem) hub — implementations (`k9-rs`, `k9_ex`, `k9_gleam`, `k9-deno`, `k9-haskell`), tooling and CI. The normative specification lives in [standards](https://github.com/hyperpolymath/standards).
