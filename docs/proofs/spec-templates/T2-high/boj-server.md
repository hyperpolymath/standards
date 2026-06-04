# Proof Spec: boj-server
<!-- SPDX-License-Identifier: CC-BY-4.0 -->

**Repo Path**: `/var/mnt/eclipse/repos/boj-server`
**Tier**: T2 — High
**Total Theorems**: 5
**Primary Prover(s)**: Idris2 (3), Coq/V (2)
**Existing Proof Coverage**: 110 Idris2 files (cartridge ABI specs), Coq/V adapter proofs

## Status Tracker

| # | Theorem | Prover | Status | Verified |
|---|---------|--------|--------|----------|
| BJ1 | Cartridge dispatch type safety | I2 | [x] Done 2026-04-11 | `src/abi/Boj/CartridgeDispatch.idr` |
| BJ2 | Auth/credential handling (full isolation model) | I2 | [ ] Partial | `Safe{APIKey,HTTP}.idr` partial |
| BJ3 | API contract compliance (95 cartridges) | I2 | [ ] Pending | — |

## Context

Unified MCP server, 95+ cartridges (GitHub, GitLab, Cloudflare, Vercel, Gmail, browsers). Runtime cartridge auto-discovery, MCP tool registration.

### Key files
- `mcp-bridge/lib/cartridge-loader.ts`
- `mcp-bridge/lib/tool-mapper.ts`
- `cartridges/*/manifest.json` (95 manifests)
- `lib/cartridge-manager.ex`
- `panll/lib/autowire.ts`

## Theorems to Prove

### BJ1: Manifest schema validation

**Target**: `verification/proofs/idris2/ManifestSchema.idr`
**Priority**: P0

**Statement**: All 95 manifests conform to schema. Validation rejects malformed manifests.

**Obligations**:
- [ ] Define manifest schema
- [ ] Prove validator completeness

---

### BJ2: No duplicate tool names

**Target**: `verification/proofs/idris2/UniqueToolNames.idr`
**Priority**: P0

**Statement**: Tool name uniqueness across all 95 cartridges enforced at load time.

**Obligations**:
- [ ] Model tool registry
- [ ] Prove name uniqueness

---

### BJ3: Credential isolation

**Target**: `verification/proofs/idris2/CredIsolation.idr`
**Priority**: P0

**Statement**: Each cartridge's credentials isolated. No shared vault. Compromise of one ≠ compromise of others.

**Obligations**:
- [ ] Model credential store per cartridge
- [ ] Prove access control

---

### BJ4: Panel autowire determinism

**Target**: `verification/proofs/idris2/AutowireDet.idr`
**Priority**: P1

**Statement**: Autowiring produces same layout for same panel set.

**Obligations**:
- [ ] Model constraint solver
- [ ] Prove determinism

---

### BJ5: MCP stdio channel integrity

**Target**: `cartridges/database-mcp/adapter/stdio_integrity.v` (NEW)
**Priority**: P1

**Statement**: MCP protocol messages correctly escape newlines. No protocol violations.

**Obligations**:
- [ ] Model stdio protocol
- [ ] Prove escape completeness

---

## Verification Commands

```bash
cd /var/mnt/eclipse/repos/boj-server
just proof-check-all
```

## Handoff Checklist

- [ ] All 5 theorems proven
- [ ] Commit: `proof: complete boj-server proofs (5/5)`
