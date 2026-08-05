<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

# REQUIRES INITIALISATION

**This repository is not finished being set up.** 54 substitution token(s) across 177 file(s) still have no value.

## Why this is not already done

This repo was created from `hyperpolymath/rsr-template-repo`. The mint
(`just repo-init`) fills every token that has a single mechanical answer —
owner, repo, author, dates, licence, branch — and it has done so here.

The tokens below are the ones it *deliberately cannot* answer. They need a
decision or a fact that exists only in your head: what this project is for,
what command builds it, which port the service listens on, whether a PGP key
is held at all. The template's own token vocabulary says as much — you cannot
sensibly answer "required invariants" in a thirty-second bootstrap.

They were left **visibly unfilled on purpose**. The alternatives were both
worse: inventing plausible values would put confident falsehoods into a
security policy and an architecture document, and silently deleting the
sections would hide the fact that a decision is owed. A visible gap is
honest; a fabricated answer is not.

## Do not delete this file until every item below is resolved

This file is the only marker that the work is outstanding. Deleting it early
does not finish the setup, it just conceals it — and the next person or agent
to arrive will reasonably assume the repo is complete.

- **If you are a person:** delete this file yourself once the last item is done.
- **If you are an agent:** resolve what you legitimately can, leave the rest,
  and delete this file only when no token below remains anywhere in the tree.
  Do not delete it to make a gate go green.

Re-running the estate top-up tool will remove this file automatically once
nothing is outstanding, so the safest way to finish is to fix the tokens and
let the check confirm it.

## What is needed, and where it goes

### `{{AGENT_ID}}`

Appears in:

- `.machine_readable/scorecards/a2ml-templates.scorecard.a2ml`

### `{{ALT}}`

Appears in:

- `consent-aware-http/Justfile`

### `{{AUTHOR_EMAIL_ALT}}`

Appears in:

- `a2ml/bindings/deno/.github/.mailmap`
- `a2ml/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/haskell/.github/.mailmap`
- `a2ml/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/rust/.github/.mailmap`
- `a2ml/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/editors/vscode/.github/.mailmap`
- `a2ml/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/pandoc/.github/.mailmap`
- `a2ml/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/deno/.github/.mailmap`
- `k9-svc/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/haskell/.github/.mailmap`
- `k9-svc/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/rust/.github/.mailmap`
- `k9-svc/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/editors/vscode/.github/.mailmap`
- `k9-svc/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/pandoc/.github/.mailmap`
- `k9-svc/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`

### `{{AUTHOR_ORG}}`

Author's organisation. NOTE: no filled instance of this exists anywhere in the estate — consider deleting the field instead.

Appears in:

- `.machine_readable/svc/k9/examples/project-metadata.k9.ncl`
- `a2ml/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`

### `{{BACKUP}}`

Appears in:

- `lol/Justfile`

### `{{CODE_STYLE}}`

Appears in:

- `.machine_readable/scorecards/a2ml-templates.scorecard.a2ml`

### `{{CONDUCT_TEAM}}`

Name of the conduct body. If there is no committee, rewrite the sentence rather than substituting a plural noun into 'a {{CONDUCT_TEAM}} member'.

Appears in:

- `0-ai-gatekeeper-protocol/CODE_OF_CONDUCT.md`
- `CODE_OF_CONDUCT.md`
- `a2ml/CODE_OF_CONDUCT.md`
- `a2ml/bindings/deno/.github/CODE_OF_CONDUCT.md`
- `a2ml/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/haskell/.github/CODE_OF_CONDUCT.md`
- `a2ml/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/rust/.github/CODE_OF_CONDUCT.md`
- `a2ml/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/editors/vscode/.github/CODE_OF_CONDUCT.md`
- `a2ml/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/pandoc/.github/CODE_OF_CONDUCT.md`
- `a2ml/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/CODE_OF_CONDUCT.md`
- `k9-svc/bindings/deno/.github/CODE_OF_CONDUCT.md`
- `k9-svc/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/haskell/.github/CODE_OF_CONDUCT.md`
- `k9-svc/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/rust/.github/CODE_OF_CONDUCT.md`
- `k9-svc/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/editors/vscode/.github/CODE_OF_CONDUCT.md`
- `k9-svc/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/pandoc/.github/CODE_OF_CONDUCT.md`
- `k9-svc/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `outreach/CODE_OF_CONDUCT.md`
- `rhodium-standard-repositories/satellites/ECOSYSTEM.scm/CODE_OF_CONDUCT.md`
- `rhodium-standard-repositories/satellites/META.scm/CODE_OF_CONDUCT.md`
- `rhodium-standard-repositories/satellites/cccp/CODE_OF_CONDUCT.md`
- `rhodium-standard-repositories/satellites/mustfile/CODE_OF_CONDUCT.md`
- `rhodium-standard-repositories/satellites/rsr-deployer/CODE_OF_CONDUCT.md`

### `{{CORPUS}}`

Appears in:

- `lol/Justfile`

### `{{COST_CEILING_POLICY}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{DESC}}`

Appears in:

- `consent-aware-http/Justfile`

### `{{DOMAIN}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{DS_RECORD}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{FILE}}`

Appears in:

- `consent-aware-http/Justfile`
- `lol/Justfile`

### `{{FORMAT}}`

Appears in:

- `lol/Justfile`

### `{{ID}}`

Appears in:

- `docs/proofs/spec-templates/SPEC-TEMPLATE.md`

### `{{KEY_TAG}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{LANG}}`

Appears in:

- `lol/Justfile`
- `rhodium-standard-repositories/Justfile`

### `{{LAST_PROOF_CHECK_DATE}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{LICENSE}}`

SPDX identifier for this repo's licence.

Appears in:

- `a2ml/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/deno/container/Containerfile`
- `a2ml/bindings/deno/container/manifest.toml`
- `a2ml/bindings/deno/docs/developer/ABI-FFI-README.adoc`
- `a2ml/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/haskell/container/Containerfile`
- `a2ml/bindings/haskell/container/manifest.toml`
- `a2ml/bindings/haskell/docs/developer/ABI-FFI-README.adoc`
- `a2ml/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/rust/container/Containerfile`
- `a2ml/bindings/rust/container/manifest.toml`
- `a2ml/bindings/rust/docs/developer/ABI-FFI-README.adoc`
- `a2ml/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/editors/vscode/container/Containerfile`
- `a2ml/editors/vscode/container/manifest.toml`
- `a2ml/editors/vscode/docs/developer/ABI-FFI-README.adoc`
- `a2ml/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/pandoc/container/Containerfile`
- `a2ml/pandoc/container/manifest.toml`
- `a2ml/pandoc/docs/developer/ABI-FFI-README.adoc`
- `k9-svc/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/deno/container/Containerfile`
- `k9-svc/bindings/deno/container/manifest.toml`
- `k9-svc/bindings/deno/docs/developer/ABI-FFI-README.adoc`
- `k9-svc/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/haskell/container/Containerfile`
- `k9-svc/bindings/haskell/container/manifest.toml`
- `k9-svc/bindings/haskell/docs/developer/ABI-FFI-README.adoc`
- `k9-svc/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/rust/container/Containerfile`
- `k9-svc/bindings/rust/container/manifest.toml`
- `k9-svc/bindings/rust/docs/developer/ABI-FFI-README.adoc`
- `k9-svc/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/editors/vscode/container/Containerfile`
- `k9-svc/editors/vscode/container/manifest.toml`
- `k9-svc/editors/vscode/docs/developer/ABI-FFI-README.adoc`
- `k9-svc/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/pandoc/container/Containerfile`
- `k9-svc/pandoc/container/manifest.toml`
- `k9-svc/pandoc/docs/developer/ABI-FFI-README.adoc`

### `{{MTA_STS_ID}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{N}}`

Appears in:

- `docs/proofs/spec-templates/SPEC-TEMPLATE.md`
- `session-management-standards/CONCURRENT-WRITE-COLLISION-PROTOCOL.adoc`

### `{{NAME}}`

Appears in:

- `consent-aware-http/Justfile`
- `rhodium-standard-repositories/Justfile`

### `{{PATTERN}}`

Appears in:

- `lol/Justfile`

### `{{PGP_FINGERPRINT}}`

Full fingerprint of the security-contact PGP key. NOTE: no key is published anywhere in this estate — if none is held, delete the PGP block rather than inventing one.

Appears in:

- `a2ml/actions/validate/.github/SECURITY.md`
- `a2ml/actions/validate/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/deno/.github/SECURITY.md`
- `a2ml/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/haskell/.github/SECURITY.md`
- `a2ml/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/rust/.github/SECURITY.md`
- `a2ml/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/editors/vscode/.github/SECURITY.md`
- `a2ml/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/pandoc/.github/SECURITY.md`
- `a2ml/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/actions/validate/.github/SECURITY.md`
- `k9-svc/actions/validate/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/deno/.github/SECURITY.md`
- `k9-svc/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/haskell/.github/SECURITY.md`
- `k9-svc/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/rust/.github/SECURITY.md`
- `k9-svc/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/editors/vscode/.github/SECURITY.md`
- `k9-svc/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/pandoc/.github/SECURITY.md`
- `k9-svc/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`

### `{{PGP_KEY_URL}}`

Public URL the PGP key can be fetched from. Same caveat as PGP_FINGERPRINT.

Appears in:

- `0-ai-gatekeeper-protocol/SECURITY.md`
- `a2ml/SECURITY.md`
- `a2ml/actions/validate/.github/SECURITY.md`
- `a2ml/actions/validate/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/actions/validate/.well-known/security.txt`
- `a2ml/bindings/deno/.github/SECURITY.md`
- `a2ml/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/deno/.well-known/security.txt`
- `a2ml/bindings/haskell/.github/SECURITY.md`
- `a2ml/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/haskell/.well-known/security.txt`
- `a2ml/bindings/rust/.github/SECURITY.md`
- `a2ml/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/rust/.well-known/security.txt`
- `a2ml/editors/vscode/.github/SECURITY.md`
- `a2ml/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/editors/vscode/.well-known/security.txt`
- `a2ml/pandoc/.github/SECURITY.md`
- `a2ml/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/pandoc/.well-known/security.txt`
- `k9-svc/actions/validate/.github/SECURITY.md`
- `k9-svc/actions/validate/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/actions/validate/.well-known/security.txt`
- `k9-svc/bindings/deno/.github/SECURITY.md`
- `k9-svc/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/deno/.well-known/security.txt`
- `k9-svc/bindings/haskell/.github/SECURITY.md`
- `k9-svc/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/haskell/.well-known/security.txt`
- `k9-svc/bindings/rust/.github/SECURITY.md`
- `k9-svc/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/rust/.well-known/security.txt`
- `k9-svc/editors/vscode/.github/SECURITY.md`
- `k9-svc/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/editors/vscode/.well-known/security.txt`
- `k9-svc/pandoc/.github/SECURITY.md`
- `k9-svc/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/pandoc/.well-known/security.txt`
- `rhodium-standard-repositories/satellites/ECOSYSTEM.scm/SECURITY.md`
- `rhodium-standard-repositories/satellites/META.scm/SECURITY.md`
- `rhodium-standard-repositories/satellites/cccp/SECURITY.md`
- `rhodium-standard-repositories/satellites/mustfile/SECURITY.md`
- `rhodium-standard-repositories/satellites/rsr-deployer/SECURITY.md`

### `{{PORT}}`

Port the container service listens on.

Appears in:

- `a2ml/bindings/deno/container/Containerfile`
- `a2ml/bindings/deno/container/compose.toml`
- `a2ml/bindings/deno/container/deploy.k9.ncl`
- `a2ml/bindings/deno/container/entrypoint.sh`
- `a2ml/bindings/deno/container/manifest.toml`
- `a2ml/bindings/deno/container/vordr.toml`
- `a2ml/bindings/haskell/container/Containerfile`
- `a2ml/bindings/haskell/container/compose.toml`
- `a2ml/bindings/haskell/container/deploy.k9.ncl`
- `a2ml/bindings/haskell/container/entrypoint.sh`
- `a2ml/bindings/haskell/container/manifest.toml`
- `a2ml/bindings/haskell/container/vordr.toml`
- `a2ml/bindings/rust/container/Containerfile`
- `a2ml/bindings/rust/container/compose.toml`
- `a2ml/bindings/rust/container/deploy.k9.ncl`
- `a2ml/bindings/rust/container/entrypoint.sh`
- `a2ml/bindings/rust/container/manifest.toml`
- `a2ml/bindings/rust/container/vordr.toml`
- `a2ml/editors/vscode/container/Containerfile`
- `a2ml/editors/vscode/container/compose.toml`
- `a2ml/editors/vscode/container/deploy.k9.ncl`
- `a2ml/editors/vscode/container/entrypoint.sh`
- `a2ml/editors/vscode/container/manifest.toml`
- `a2ml/editors/vscode/container/vordr.toml`
- `a2ml/pandoc/container/Containerfile`
- `a2ml/pandoc/container/compose.toml`
- `a2ml/pandoc/container/deploy.k9.ncl`
- `a2ml/pandoc/container/entrypoint.sh`
- `a2ml/pandoc/container/manifest.toml`
- `a2ml/pandoc/container/vordr.toml`
- `k9-svc/bindings/deno/container/Containerfile`
- `k9-svc/bindings/deno/container/compose.toml`
- `k9-svc/bindings/deno/container/deploy.k9.ncl`
- `k9-svc/bindings/deno/container/entrypoint.sh`
- `k9-svc/bindings/deno/container/manifest.toml`
- `k9-svc/bindings/deno/container/vordr.toml`
- `k9-svc/bindings/haskell/container/Containerfile`
- `k9-svc/bindings/haskell/container/compose.toml`
- `k9-svc/bindings/haskell/container/deploy.k9.ncl`
- `k9-svc/bindings/haskell/container/entrypoint.sh`
- `k9-svc/bindings/haskell/container/manifest.toml`
- `k9-svc/bindings/haskell/container/vordr.toml`
- `k9-svc/bindings/rust/container/Containerfile`
- `k9-svc/bindings/rust/container/compose.toml`
- `k9-svc/bindings/rust/container/deploy.k9.ncl`
- `k9-svc/bindings/rust/container/entrypoint.sh`
- `k9-svc/bindings/rust/container/manifest.toml`
- `k9-svc/bindings/rust/container/vordr.toml`
- `k9-svc/editors/vscode/container/Containerfile`
- `k9-svc/editors/vscode/container/compose.toml`
- `k9-svc/editors/vscode/container/deploy.k9.ncl`
- `k9-svc/editors/vscode/container/entrypoint.sh`
- `k9-svc/editors/vscode/container/manifest.toml`
- `k9-svc/editors/vscode/container/vordr.toml`
- `k9-svc/pandoc/container/Containerfile`
- `k9-svc/pandoc/container/compose.toml`
- `k9-svc/pandoc/container/deploy.k9.ncl`
- `k9-svc/pandoc/container/entrypoint.sh`
- `k9-svc/pandoc/container/manifest.toml`
- `k9-svc/pandoc/container/vordr.toml`

### `{{PREFIX}}`

Appears in:

- `rhodium-standard-repositories/examples/rhodium-minimal/Justfile`

### `{{PRIMARY_LANGUAGE}}`

Appears in:

- `.machine_readable/scorecards/a2ml-templates.scorecard.a2ml`

### `{{PROJECT_DESCRIPTION}}`

One-line description, matching the forge description.

Appears in:

- `a2ml/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/deno/container/Containerfile`
- `a2ml/bindings/deno/container/manifest.toml`
- `a2ml/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/haskell/container/Containerfile`
- `a2ml/bindings/haskell/container/manifest.toml`
- `a2ml/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/rust/container/Containerfile`
- `a2ml/bindings/rust/container/manifest.toml`
- `a2ml/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/editors/vscode/container/Containerfile`
- `a2ml/editors/vscode/container/manifest.toml`
- `a2ml/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/pandoc/container/Containerfile`
- `a2ml/pandoc/container/manifest.toml`
- `k9-svc/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/deno/container/Containerfile`
- `k9-svc/bindings/deno/container/manifest.toml`
- `k9-svc/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/haskell/container/Containerfile`
- `k9-svc/bindings/haskell/container/manifest.toml`
- `k9-svc/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/rust/container/Containerfile`
- `k9-svc/bindings/rust/container/manifest.toml`
- `k9-svc/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/editors/vscode/container/Containerfile`
- `k9-svc/editors/vscode/container/manifest.toml`
- `k9-svc/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/pandoc/container/Containerfile`
- `k9-svc/pandoc/container/manifest.toml`

### `{{PROJECT_DOMAIN}}`

Taxonomy value for the subject domain.

Appears in:

- `a2ml/actions/validate/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/bindings/deno/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/bindings/haskell/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/bindings/rust/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/editors/vscode/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/pandoc/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/actions/validate/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/bindings/deno/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/bindings/haskell/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/bindings/rust/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/editors/vscode/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/pandoc/.machine_readable/anchors/ANCHOR.a2ml`

### `{{PROJECT_KIND}}`

Taxonomy value (library, service, tool, lab…).

Appears in:

- `a2ml/actions/validate/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/bindings/deno/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/bindings/haskell/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/bindings/rust/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/editors/vscode/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/pandoc/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/actions/validate/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/bindings/deno/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/bindings/haskell/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/bindings/rust/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/editors/vscode/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/pandoc/.machine_readable/anchors/ANCHOR.a2ml`

### `{{PROJECT_PURPOSE}}`

One line: what this exists to do.

Appears in:

- `a2ml/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/deno/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/haskell/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/rust/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/editors/vscode/.machine_readable/anchors/ANCHOR.a2ml`
- `a2ml/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/pandoc/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/deno/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/haskell/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/rust/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/editors/vscode/.machine_readable/anchors/ANCHOR.a2ml`
- `k9-svc/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/pandoc/.machine_readable/anchors/ANCHOR.a2ml`

### `{{PROJECT_UNIQUE_STRENGTH}}`

What this does that its alternatives do not.

Appears in:

- `.machine_readable/agent_instructions/methodology.a2ml`

### `{{PROOF}}`

Appears in:

- `lol/Justfile`

### `{{PROVEN_HASH}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{RECIPE}}`

Appears in:

- `lol/Justfile`

### `{{REGISTRY}}`

Container registry to publish to.

Appears in:

- `a2ml/bindings/deno/container/compose.toml`
- `a2ml/bindings/deno/container/ct-build.sh`
- `a2ml/bindings/deno/container/deploy.k9.ncl`
- `a2ml/bindings/haskell/container/compose.toml`
- `a2ml/bindings/haskell/container/ct-build.sh`
- `a2ml/bindings/haskell/container/deploy.k9.ncl`
- `a2ml/bindings/rust/container/compose.toml`
- `a2ml/bindings/rust/container/ct-build.sh`
- `a2ml/bindings/rust/container/deploy.k9.ncl`
- `a2ml/editors/vscode/container/compose.toml`
- `a2ml/editors/vscode/container/ct-build.sh`
- `a2ml/editors/vscode/container/deploy.k9.ncl`
- `a2ml/pandoc/container/compose.toml`
- `a2ml/pandoc/container/ct-build.sh`
- `a2ml/pandoc/container/deploy.k9.ncl`
- `k9-svc/bindings/deno/container/compose.toml`
- `k9-svc/bindings/deno/container/ct-build.sh`
- `k9-svc/bindings/deno/container/deploy.k9.ncl`
- `k9-svc/bindings/haskell/container/compose.toml`
- `k9-svc/bindings/haskell/container/ct-build.sh`
- `k9-svc/bindings/haskell/container/deploy.k9.ncl`
- `k9-svc/bindings/rust/container/compose.toml`
- `k9-svc/bindings/rust/container/ct-build.sh`
- `k9-svc/bindings/rust/container/deploy.k9.ncl`
- `k9-svc/editors/vscode/container/compose.toml`
- `k9-svc/editors/vscode/container/ct-build.sh`
- `k9-svc/editors/vscode/container/deploy.k9.ncl`
- `k9-svc/pandoc/container/compose.toml`
- `k9-svc/pandoc/container/ct-build.sh`
- `k9-svc/pandoc/container/deploy.k9.ncl`
- `lol/Justfile`

### `{{REPO_NAME}}`

Appears in:

- `docs/proofs/spec-templates/SPEC-TEMPLATE.md`

### `{{REPO_SOPS_ADOPTION_STATUS}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{RESPONSE_TIME}}`

Initial-response SLA for a security or conduct report. Promise only what a solo maintainer can actually meet.

Appears in:

- `0-ai-gatekeeper-protocol/CODE_OF_CONDUCT.md`
- `CODE_OF_CONDUCT.md`
- `a2ml/CODE_OF_CONDUCT.md`
- `a2ml/bindings/deno/.github/CODE_OF_CONDUCT.md`
- `a2ml/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/haskell/.github/CODE_OF_CONDUCT.md`
- `a2ml/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/rust/.github/CODE_OF_CONDUCT.md`
- `a2ml/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/editors/vscode/.github/CODE_OF_CONDUCT.md`
- `a2ml/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/pandoc/.github/CODE_OF_CONDUCT.md`
- `a2ml/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/CODE_OF_CONDUCT.md`
- `k9-svc/bindings/deno/.github/CODE_OF_CONDUCT.md`
- `k9-svc/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/haskell/.github/CODE_OF_CONDUCT.md`
- `k9-svc/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/rust/.github/CODE_OF_CONDUCT.md`
- `k9-svc/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/editors/vscode/.github/CODE_OF_CONDUCT.md`
- `k9-svc/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/pandoc/.github/CODE_OF_CONDUCT.md`
- `k9-svc/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `outreach/CODE_OF_CONDUCT.md`
- `rhodium-standard-repositories/satellites/ECOSYSTEM.scm/CODE_OF_CONDUCT.md`
- `rhodium-standard-repositories/satellites/META.scm/CODE_OF_CONDUCT.md`
- `rhodium-standard-repositories/satellites/cccp/CODE_OF_CONDUCT.md`
- `rhodium-standard-repositories/satellites/mustfile/CODE_OF_CONDUCT.md`
- `rhodium-standard-repositories/satellites/rsr-deployer/CODE_OF_CONDUCT.md`

### `{{RGTV_ENABLED_FOR_REPO}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{SECURITY_CONTACT}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{SECURITY_EMAIL}}`

Address for private vulnerability reports. Two competing values exist in the estate (`6759885+hyperpolymath@users.noreply.github.com` and `security@hyperpolymath.org`) — pick one deliberately.

Appears in:

- `a2ml/bindings/deno/.github/SECURITY.md`
- `a2ml/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/deno/.well-known/security.txt`
- `a2ml/bindings/haskell/.github/SECURITY.md`
- `a2ml/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/haskell/.well-known/security.txt`
- `a2ml/bindings/rust/.github/SECURITY.md`
- `a2ml/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/rust/.well-known/security.txt`
- `a2ml/editors/vscode/.github/SECURITY.md`
- `a2ml/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/editors/vscode/.well-known/security.txt`
- `a2ml/pandoc/.github/SECURITY.md`
- `a2ml/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/pandoc/.well-known/security.txt`
- `k9-svc/bindings/deno/.github/SECURITY.md`
- `k9-svc/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/deno/.well-known/security.txt`
- `k9-svc/bindings/haskell/.github/SECURITY.md`
- `k9-svc/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/haskell/.well-known/security.txt`
- `k9-svc/bindings/rust/.github/SECURITY.md`
- `k9-svc/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/rust/.well-known/security.txt`
- `k9-svc/editors/vscode/.github/SECURITY.md`
- `k9-svc/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/editors/vscode/.well-known/security.txt`
- `k9-svc/pandoc/.github/SECURITY.md`
- `k9-svc/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/pandoc/.well-known/security.txt`

### `{{SECURITY_TXT_EXPIRES}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{SERVICE_NAME}}`

Container service name.

Appears in:

- `_shared/container/.gatekeeper.yaml`
- `a2ml/bindings/deno/container/Containerfile`
- `a2ml/bindings/deno/container/compose.toml`
- `a2ml/bindings/deno/container/ct-build.sh`
- `a2ml/bindings/deno/container/deploy.k9.ncl`
- `a2ml/bindings/deno/container/entrypoint.sh`
- `a2ml/bindings/deno/container/manifest.toml`
- `a2ml/bindings/deno/container/vordr.toml`
- `a2ml/bindings/haskell/container/Containerfile`
- `a2ml/bindings/haskell/container/compose.toml`
- `a2ml/bindings/haskell/container/ct-build.sh`
- `a2ml/bindings/haskell/container/deploy.k9.ncl`
- `a2ml/bindings/haskell/container/entrypoint.sh`
- `a2ml/bindings/haskell/container/manifest.toml`
- `a2ml/bindings/haskell/container/vordr.toml`
- `a2ml/bindings/rust/container/Containerfile`
- `a2ml/bindings/rust/container/compose.toml`
- `a2ml/bindings/rust/container/ct-build.sh`
- `a2ml/bindings/rust/container/deploy.k9.ncl`
- `a2ml/bindings/rust/container/entrypoint.sh`
- `a2ml/bindings/rust/container/manifest.toml`
- `a2ml/bindings/rust/container/vordr.toml`
- `a2ml/editors/vscode/container/Containerfile`
- `a2ml/editors/vscode/container/compose.toml`
- `a2ml/editors/vscode/container/ct-build.sh`
- `a2ml/editors/vscode/container/deploy.k9.ncl`
- `a2ml/editors/vscode/container/entrypoint.sh`
- `a2ml/editors/vscode/container/manifest.toml`
- `a2ml/editors/vscode/container/vordr.toml`
- `a2ml/pandoc/container/Containerfile`
- `a2ml/pandoc/container/compose.toml`
- `a2ml/pandoc/container/ct-build.sh`
- `a2ml/pandoc/container/deploy.k9.ncl`
- `a2ml/pandoc/container/entrypoint.sh`
- `a2ml/pandoc/container/manifest.toml`
- `a2ml/pandoc/container/vordr.toml`
- `k9-svc/bindings/deno/container/Containerfile`
- `k9-svc/bindings/deno/container/compose.toml`
- `k9-svc/bindings/deno/container/ct-build.sh`
- `k9-svc/bindings/deno/container/deploy.k9.ncl`
- `k9-svc/bindings/deno/container/entrypoint.sh`
- `k9-svc/bindings/deno/container/manifest.toml`
- `k9-svc/bindings/deno/container/vordr.toml`
- `k9-svc/bindings/haskell/container/Containerfile`
- `k9-svc/bindings/haskell/container/compose.toml`
- `k9-svc/bindings/haskell/container/ct-build.sh`
- `k9-svc/bindings/haskell/container/deploy.k9.ncl`
- `k9-svc/bindings/haskell/container/entrypoint.sh`
- `k9-svc/bindings/haskell/container/manifest.toml`
- `k9-svc/bindings/haskell/container/vordr.toml`
- `k9-svc/bindings/rust/container/Containerfile`
- `k9-svc/bindings/rust/container/compose.toml`
- `k9-svc/bindings/rust/container/ct-build.sh`
- `k9-svc/bindings/rust/container/deploy.k9.ncl`
- `k9-svc/bindings/rust/container/entrypoint.sh`
- `k9-svc/bindings/rust/container/manifest.toml`
- `k9-svc/bindings/rust/container/vordr.toml`
- `k9-svc/editors/vscode/container/Containerfile`
- `k9-svc/editors/vscode/container/compose.toml`
- `k9-svc/editors/vscode/container/ct-build.sh`
- `k9-svc/editors/vscode/container/deploy.k9.ncl`
- `k9-svc/editors/vscode/container/entrypoint.sh`
- `k9-svc/editors/vscode/container/manifest.toml`
- `k9-svc/editors/vscode/container/vordr.toml`
- `k9-svc/pandoc/container/Containerfile`
- `k9-svc/pandoc/container/compose.toml`
- `k9-svc/pandoc/container/ct-build.sh`
- `k9-svc/pandoc/container/deploy.k9.ncl`
- `k9-svc/pandoc/container/entrypoint.sh`
- `k9-svc/pandoc/container/manifest.toml`
- `k9-svc/pandoc/container/vordr.toml`

### `{{SOURCE}}`

Appears in:

- `lol/Justfile`

### `{{TERM}}`

Appears in:

- `lol/Justfile`

### `{{THIS_REPO}}`

Appears in:

- `.machine_readable/contractiles/trust/Trustfile.a2ml`

### `{{TIER}}`

Appears in:

- `rhodium-standard-repositories/Justfile`

### `{{TIMESTAMP}}`

Appears in:

- `.machine_readable/scorecards/a2ml-templates.scorecard.a2ml`

### `{{VERSION}}`

Version/tag for the container image.

Appears in:

- `a2ml/actions/validate/validate-a2ml.sh`
- `a2ml/bindings/deno/container/deploy.k9.ncl`
- `a2ml/bindings/deno/container/manifest.toml`
- `a2ml/bindings/deno/container/vordr.toml`
- `a2ml/bindings/haskell/container/deploy.k9.ncl`
- `a2ml/bindings/haskell/container/manifest.toml`
- `a2ml/bindings/haskell/container/vordr.toml`
- `a2ml/bindings/rust/container/deploy.k9.ncl`
- `a2ml/bindings/rust/container/manifest.toml`
- `a2ml/bindings/rust/container/vordr.toml`
- `a2ml/editors/vscode/container/deploy.k9.ncl`
- `a2ml/editors/vscode/container/manifest.toml`
- `a2ml/editors/vscode/container/vordr.toml`
- `a2ml/pandoc/container/deploy.k9.ncl`
- `a2ml/pandoc/container/manifest.toml`
- `a2ml/pandoc/container/vordr.toml`
- `consent-aware-http/Justfile`
- `k9-svc/bindings/deno/container/deploy.k9.ncl`
- `k9-svc/bindings/deno/container/manifest.toml`
- `k9-svc/bindings/deno/container/vordr.toml`
- `k9-svc/bindings/haskell/container/deploy.k9.ncl`
- `k9-svc/bindings/haskell/container/manifest.toml`
- `k9-svc/bindings/haskell/container/vordr.toml`
- `k9-svc/bindings/rust/container/deploy.k9.ncl`
- `k9-svc/bindings/rust/container/manifest.toml`
- `k9-svc/bindings/rust/container/vordr.toml`
- `k9-svc/editors/vscode/container/deploy.k9.ncl`
- `k9-svc/editors/vscode/container/manifest.toml`
- `k9-svc/editors/vscode/container/vordr.toml`
- `k9-svc/pandoc/container/deploy.k9.ncl`
- `k9-svc/pandoc/container/manifest.toml`
- `k9-svc/pandoc/container/vordr.toml`
- `lol/Justfile`
- `rhodium-standard-repositories/Justfile`

### `{{WEBSITE}}`

Project homepage URL, or delete the field if there is none.

Appears in:

- `0-ai-gatekeeper-protocol/SECURITY.md`
- `a2ml/SECURITY.md`
- `a2ml/bindings/deno/.github/SECURITY.md`
- `a2ml/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/deno/.well-known/security.txt`
- `a2ml/bindings/haskell/.github/SECURITY.md`
- `a2ml/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/haskell/.well-known/security.txt`
- `a2ml/bindings/rust/.github/SECURITY.md`
- `a2ml/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/bindings/rust/.well-known/security.txt`
- `a2ml/editors/vscode/.github/SECURITY.md`
- `a2ml/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/editors/vscode/.well-known/security.txt`
- `a2ml/pandoc/.github/SECURITY.md`
- `a2ml/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `a2ml/pandoc/.well-known/security.txt`
- `k9-svc/bindings/deno/.github/SECURITY.md`
- `k9-svc/bindings/deno/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/deno/.well-known/security.txt`
- `k9-svc/bindings/haskell/.github/SECURITY.md`
- `k9-svc/bindings/haskell/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/haskell/.well-known/security.txt`
- `k9-svc/bindings/rust/.github/SECURITY.md`
- `k9-svc/bindings/rust/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/bindings/rust/.well-known/security.txt`
- `k9-svc/editors/vscode/.github/SECURITY.md`
- `k9-svc/editors/vscode/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/editors/vscode/.well-known/security.txt`
- `k9-svc/pandoc/.github/SECURITY.md`
- `k9-svc/pandoc/.machine_readable/ai/PLACEHOLDERS.adoc`
- `k9-svc/pandoc/.well-known/security.txt`
- `rhodium-standard-repositories/satellites/ECOSYSTEM.scm/SECURITY.md`
- `rhodium-standard-repositories/satellites/META.scm/SECURITY.md`
- `rhodium-standard-repositories/satellites/cccp/SECURITY.md`
- `rhodium-standard-repositories/satellites/mustfile/SECURITY.md`
- `rhodium-standard-repositories/satellites/rsr-deployer/SECURITY.md`

### `{{WORKERS}}`

Appears in:

- `lol/Justfile`

### `{{X}}`

Appears in:

- `docs/proofs/spec-templates/SPEC-TEMPLATE.md`

---

Generated by the estate top-up pass. Rationale and the governing rulings are
in `hyperpolymath/standards`; the token vocabulary is
`.machine_readable/ai/PLACEHOLDERS.adoc` in `rsr-template-repo`.
