# Proof Spec: polyglot-i18n
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/polyglot-i18n`
**Tier**: T3 — Standard
**Archetype**: [integration-sdk](../archetypes/integration-sdk.md)

## Theorems

All theorems from archetype `integration-sdk` apply:
- See `~/Desktop/proof-specs/T3-standard/archetypes/integration-sdk.md`

Plus ABI-1 through ABI-5 from rsr-template-repo.

## Repo-specific

- No extra theorems beyond archetype.

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/polyglot-i18n
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete polyglot-i18n proofs`
