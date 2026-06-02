# Proof Spec: academic-workflow-suite
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/academic-workflow-suite`
**Tier**: T3 — Standard
**Archetype**: [webapp](../archetypes/webapp.md)

## Theorems

All theorems from archetype `webapp` apply:
- See `~/Desktop/proof-specs/T3-standard/archetypes/webapp.md`

Plus ABI-1 through ABI-5 from rsr-template-repo.

## Repo-specific

- No extra theorems beyond archetype.

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/academic-workflow-suite
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete academic-workflow-suite proofs`
