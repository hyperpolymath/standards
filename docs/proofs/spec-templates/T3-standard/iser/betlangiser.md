# Proof Spec: betlangiser
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/betlangiser`
**Tier**: T3 — Standard
**Archetype**: [iser](../archetypes/iser.md)

## Theorems

All theorems from archetype `iser` apply:
- See `~/Desktop/proof-specs/T3-standard/archetypes/iser.md`

Plus ABI-1 through ABI-5 from rsr-template-repo.

## Repo-specific

- No extra theorems beyond archetype.

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/betlangiser
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete betlangiser proofs`
