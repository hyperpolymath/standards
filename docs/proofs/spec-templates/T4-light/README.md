# T4 Light Tier — Template ABI Only
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->

T4 repos only need the 5 ABI proofs from rsr-template-repo:
- ABI-1: Non-null pointer safety
- ABI-2: Memory layout correctness
- ABI-3: Platform type sizes
- ABI-4: FFI return types
- ABI-5: C ABI compliance

See `~/Desktop/proof-specs/T1-critical/rsr-template-repo.md` for full spec.

## Repos in T4

Copy the starter proof files from `rsr-template-repo/verification/proofs/` and verify they compile.

| Repo | Notes |
|------|-------|
| awesome-idris2 | Curated list |
| awesome-mcp-servers | Curated list |
| awesome-nickel | Curated list |
| blog-drafts | Content |
| hyperpolymath | GitHub profile |
| HyperpolymathRegistry | Julia registry |
| homebrew-tap | Homebrew formulae |
| im-docs | Documentation |
| manifesto | Policy document |
| palimpsest-license | License text |
| palimpsest-plasma | License tooling |
| a2ml-showcase | Demo |
| k9-showcase | Demo |
| squisher-corpus | Test corpus |
| nexia-list | Curated list |
| no-nonsense-nntps | Docs |
| boinc-boinc | External integration |

## Handoff

For each T4 repo:
1. Copy `verification/proofs/idris2/ABI/*.idr` from rsr-template-repo
2. Update SPDX headers with repo name
3. Run `just proof-check-all`
4. Commit: `proof: apply baseline ABI proofs (T4 template)`
