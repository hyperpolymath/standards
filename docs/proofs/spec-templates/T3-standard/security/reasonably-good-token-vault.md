# Proof Spec: reasonably-good-token-vault
<!-- SPDX-License-Identifier: CC-BY-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/reasonably-good-token-vault`
**Tier**: T3 — Standard
**Archetype**: [security](../archetypes/security.md)

## Theorems

All theorems from archetype `security` apply:
- See `~/Desktop/proof-specs/T3-standard/archetypes/security.md`

Plus ABI-1 through ABI-5 from rsr-template-repo.

## Repo-specific

- No extra theorems beyond archetype.

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/reasonably-good-token-vault
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete reasonably-good-token-vault proofs`
