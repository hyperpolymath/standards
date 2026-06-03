# LLM Warm-up — standards (User / Consumer)

> Thin stub. Canonical machine entry: **`0-AI-MANIFEST.a2ml`**.
> Canonical human entry: **`README.adoc`**. Spec index: **`.machine_readable/REGISTRY.a2ml`**.

## You are *applying* these standards to another repo

This monorepo is a set of specs to consume, not an app to run. Typical path:

- **Stand up a compliant repo:** `A2ML-REPO-TEMPLATE.adoc` + the *Usage* section
  of `README.adoc`; copy the 7 A2ML files from `a2ml-templates/`.
- **Pick the spec you need:** `.machine_readable/REGISTRY.a2ml` routes you by
  `id` → `home` → `canonical_doc` (A2ML family, K9, protocols, readiness grades,
  RSR, pre-flight gates).
- **Adopt the enforcement:** copy the relevant `.github/workflows/` + `hooks/`.
- **Language rules you must follow:** `.claude/CLAUDE.md`.

## Quick context

- Licence: see `LICENSE`.
- Part of the Hyperpolymath ecosystem.
- "If you want X, go here" routing lives at the top of `README.adoc`.
