<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> -->
---
title: A2ML
site: A2ML
description: A2ML is a lightweight, Djot-like markup that compiles into a typed, attested core, with progressive strictness from lax to fully attested.
date: 2026-06-29
---

# A2ML — Attested Markup Language

<p class="lede">A lightweight, Djot-like markup that compiles into a typed, attested core. Authoring stays simple; structural guarantees switch on when you want them.</p>

<div class="badges">
<span class="badge">Spec v1.1.0</span>
<span class="badge">Typed core</span>
<span class="badge">Progressive strictness</span>
<span class="badge">Byte-for-byte payloads</span>
<span class="badge">MPL-2.0 / CC-BY-SA-4.0</span>
</div>

<div class="btn-row">
<a class="btn btn-primary" href="/start.html">Get started</a>
<a class="btn btn-ghost" href="/spec.html">Read the spec</a>
<a class="btn btn-ghost" href="https://github.com/hyperpolymath/a2ml-ecosystem">GitHub</a>
</div>

## What it does

<div class="cards">
<div class="card">
<h3>Readable surface</h3>
<p>Write a clean, Djot-like format. No ceremony when you don't need it.</p>
</div>
<div class="card">
<h3>Typed, attested core</h3>
<p>Required sections, resolved references and unique IDs — verified, not hoped for.</p>
</div>
<div class="card">
<h3>Faithful payloads</h3>
<p>Opaque content is preserved byte-for-byte for reliable embedding.</p>
</div>
<div class="card">
<h3>Portable rendering</h3>
<p>One source, many targets — HTML, Markdown and PDF pipelines.</p>
</div>
</div>

## Progressive strictness

A2ML lets you dial guarantees up as a document matures:

| Mode | Guarantee |
|------|-----------|
| **lax** | Parse and render; no structural enforcement. |
| **checked** | Required sections exist, references resolve, IDs are unique. |
| **attested** | Checked, plus a verifiable attestation over the typed core. |

## A taste

```a2ml
# A2ML Overview

@abstract:
A2ML is a typed, attested markup format. It verifies structure and references.
@end

## Claims
- Required sections must exist.
- References must resolve.

@refs:
[1] Attested Markup Language Spec (draft)
@end
```

## Downloads & roadmap

Grab a [starter template](/downloads.html) in whichever strictness mode fits, or see the [roadmap](/roadmap.html) for what's next — including an interactive teaching area and [a2mliser](https://github.com/hyperpolymath/a2mliser).

## Part of the standards estate

A2ML is a satellite of the [Hyperpolymath standards hub](https://github.com/hyperpolymath/standards). The normative specification, conformance vectors and profiles live there; this site is the front door.
