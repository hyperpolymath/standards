# Proof Spec: k9-showcase
<!-- SPDX-License-Identifier: CC-BY-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/k9-showcase`
**Tier**: T3 — Standard
**Archetype**: [a2ml-k9](../archetypes/a2ml-k9.md)

## Theorems

All theorems from archetype `a2ml-k9` apply:
- See `~/Desktop/proof-specs/T3-standard/archetypes/a2ml-k9.md`

Plus ABI-1 through ABI-5 from rsr-template-repo.

## Repo-specific

- No extra theorems beyond archetype.

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/k9-showcase
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete k9-showcase proofs`
