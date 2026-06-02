# Proof Spec: a2ml_ex
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/a2ml_ex`
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
cd /var/mnt/eclipse/repos/a2ml_ex
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete a2ml_ex proofs`
