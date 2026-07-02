<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> -->
---
title: Get started with A2ML
site: A2ML
description: Write, validate and render A2ML in three steps — from a plain-text document to a portable, typed core.
date: 2026-06-29
---

# Get started

A2ML is authored as plain text and validated in progressive modes. Start lax, tighten to checked, attest when ready.

## 1. Write a document

```a2ml
# Release Notes

@abstract:
What changed in this release, in one paragraph.
@end

## Changes
- Added profiles for domain validation.
- Resolved all cross-references.

@refs:
[1] A2ML Spec v1.1.0
@end
```

## 2. Validate

Run the validator over your document. In **checked** mode it confirms that required sections exist, references resolve, and IDs are unique. In **attested** mode it additionally produces a verifiable attestation over the typed core.

## 3. Render

The typed core is renderer-portable: emit HTML, Markdown or feed a PDF pipeline from a single source, with opaque payloads preserved byte-for-byte.

## Tooling

Implementations, editor support and CI actions are coordinated in the [a2ml-ecosystem](https://github.com/hyperpolymath/a2ml-ecosystem) hub. The normative spec and conformance vectors live in [standards](https://github.com/hyperpolymath/standards).
