# Proof Spec: resource-record-fluctuator
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/resource-record-fluctuator`
**Tier**: T3 — Standard
**Archetype**: [config-infra](../archetypes/config-infra.md)

## Theorems

All theorems from archetype `config-infra` apply:
- See `~/Desktop/proof-specs/T3-standard/archetypes/config-infra.md`

Plus ABI-1 through ABI-5 from rsr-template-repo.

## Repo-specific

- No extra theorems beyond archetype.

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/resource-record-fluctuator
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete resource-record-fluctuator proofs`
