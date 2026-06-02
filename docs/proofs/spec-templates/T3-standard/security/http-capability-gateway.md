# Proof Spec: http-capability-gateway
<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->

**Repo Path**: `/var/mnt/eclipse/repos/http-capability-gateway`
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
cd /var/mnt/eclipse/repos/http-capability-gateway
just proof-check-all
just proof-scan-dangerous
```

## Handoff Checklist

- [ ] Archetype proofs complete
- [ ] ABI-1..ABI-5 complete
- [ ] Repo-specific items (if any) complete
- [ ] `PROOF-STATUS.md` updated
- [ ] Commit: `proof: complete http-capability-gateway proofs`
