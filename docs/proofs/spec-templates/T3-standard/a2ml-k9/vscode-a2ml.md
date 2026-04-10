# Proof Spec: vscode-a2ml
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/vscode-a2ml`
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
cd /var/mnt/eclipse/repos/vscode-a2ml
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete vscode-a2ml proofs`
