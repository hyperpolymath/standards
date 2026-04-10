# Proof Spec: PolyglotFormalisms.jl
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/PolyglotFormalisms.jl`
**Tier**: T3 — Standard
**Archetype**: [julia-pkg](../archetypes/julia-pkg.md)

## Theorems

All theorems from archetype `julia-pkg` apply:
- See `~/Desktop/proof-specs/T3-standard/archetypes/julia-pkg.md`

Plus ABI-1 through ABI-5 from rsr-template-repo.

## Repo-specific

- No extra theorems beyond archetype.

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/PolyglotFormalisms.jl
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete PolyglotFormalisms.jl proofs`
