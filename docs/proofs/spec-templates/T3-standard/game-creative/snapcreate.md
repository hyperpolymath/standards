# Proof Spec: snapcreate
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/snapcreate`
**Tier**: T3 — Standard
**Archetype**: [game-creative](../archetypes/game-creative.md)

## Theorems

All theorems from archetype `game-creative` apply:
- See `~/Desktop/proof-specs/T3-standard/archetypes/game-creative.md`

Plus ABI-1 through ABI-5 from rsr-template-repo.

## Repo-specific

- No extra theorems beyond archetype.

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/snapcreate
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete snapcreate proofs`
