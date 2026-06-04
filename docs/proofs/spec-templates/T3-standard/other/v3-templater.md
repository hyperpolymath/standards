# Proof Spec: v3-templater
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/v3-templater`
**Tier**: T3 — Standard
**Archetype**: [cli-tool](../archetypes/cli-tool.md)

## Theorems

All theorems from archetype `cli-tool` apply:
- See `~/Desktop/proof-specs/T3-standard/archetypes/cli-tool.md`

Plus ABI-1 through ABI-5 from rsr-template-repo.

## Repo-specific

- Custom repo — ABI proofs required. Verify if archetype fits; may need bespoke theorems.

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/v3-templater
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete v3-templater proofs`
