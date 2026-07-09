# Port Registry — hyperpolymath ecosystem
# Last audited: 2026-07-09 — reconciled against the groove registry (authority split noted, groove surfaces mirrored, contradictions flagged in UNRESOLVED below)
#
# RULE: Every project MUST use a unique port. No two projects share a port.
# Check this file BEFORE assigning ports in new projects.

## Authority split

For groove-speaking services, `groove/registry/groove-registry.json`
(hyperpolymath/groove, ADR 0006, 2026-07-02) is the single source of truth for
the GROOVE-DISCOVERY surface. This file governs app/bind ports and mirrors
groove assignments; each contradiction is flagged below (see UNRESOLVED), not
silently fixed.

## Assigned Ports (CANONICAL — use these)

| Port | Project | Service | Protocol |
|------|---------|---------|----------|
| 4010 | stapeln | Phoenix backend | HTTP |
| 4020 | burble | Phoenix voice server — legacy app port, moving to 6473; see hyperpolymath/burble branch claude/cleave-spline-groove-arch-4zq111 | HTTP |
| 4030 | idaptik | Sync server | HTTP |
| 4040 | gossamer | Dev server | HTTP |
| 4050 | ambientops | Composer | HTTP |
| 4060 | reposystem | GUI backend | HTTP |
| 6465 | groove-ref | Groove reference provider | HTTP |
| 6470 | gossamer | Groove consumer/reference | HTTP |
| 6473 | burble | Groove surface | HTTP |
| 6475 | verisimdb | Groove surface (per groove registry; agrees with the "8080 often intercepted — consider changing" remark below) | HTTP |
| 6480 | vext | Verifiable comms protocol | HTTP |
| 7600 | panic-attacker | Static analysis server | HTTP |
| 7700 | boj-server | Main MCP API | HTTP |
| 7701 | boj-server | Browser bridge | HTTP |
| 7800 | boj-server | Cartridge runner | HTTP |
| 8000 | panll | Groove + dev server | HTTP |
| 8080 | verisimdb | Default API (standalone) | HTTP |
| 8081 | echidna | Neural/prover API | HTTP |
| 8090 | idaptik | Game server (dev) | HTTP |
| 8093 | verisimdb | Stapeln instance | HTTP |
| 8094 | verisimdb | Kategoria instance | HTTP |
| 8095 | verisimdb | Project-M instance | HTTP |
| 8096 | verisimdb | Work instance (RAM disk) | HTTP |
| 8097 | verisimdb | Standards innervation instance (live per standards/.verisimdb/config.toml) | HTTP |
| 9090 | hypatia | Web + Groove endpoints | HTTP |
| 9100 | gitbot-fleet | Bot orchestration (was 7500; dropped per groove ADR 0006) | HTTP |
| 50051 | verisimdb | gRPC | gRPC |

## UNRESOLVED — owner decision needed (flagged, not fixed)

Contradictions between this file and the groove registry
(`groove/registry/groove-registry.json` in hyperpolymath/groove). Per estate
doctrine these are flagged only — neither side has been rewritten here.

1. **echidna** — 8081 (this file) vs 9000 (groove registry + panll code).
2. **boj-server** — 7700/7800 (this file) vs conflow 7700 / rpa-elysium 7800
   (groove registry) — double-booking.
3. **burble AI-bridge** — WS default 6475 collides with verisimdb's groove
   registry assignment 6475. Recommendation: bridge moves to its documented
   alt 7474/7475.

## Commonly Blocked Ports (AVOID)

These are frequently blocked by corporate firewalls, ISPs, or proxies:

| Port | Why blocked | Our status |
|------|-----------|-----------|
| 25 | SMTP — spam prevention | Not used |
| 80 | HTTP — requires root | Not used (dev) |
| 443 | HTTPS — requires root | Not used (dev) |
| 1080 | SOCKS proxy — abuse | Not used |
| 3128 | Squid proxy | Not used |
| 8080 | Common proxy port — **often intercepted** | **VeriSimDB default — consider changing** (groove surface already moved to 6475 per groove registry, which agrees with this remark) |
| 8443 | Alt HTTPS — sometimes blocked | IDApTIK uses — review |
| 9090 | Prometheus default — may conflict | Hypatia uses — acceptable |

## Previously Conflicting (FIXED 2026-03-24)

These projects all used port 4000 (Phoenix default) and port 8080 (generic HTTP default).
Reassigned to unique ports above. Config changes needed in each project.

| Project | Old port | New port | Config file |
|---------|---------|---------|-------------|
| stapeln | 4000 | 4010 | backend/config/runtime.exs |
| burble | 4000 | 4020 | server/config/runtime.exs |
| idaptik | 4000 | 4030 | sync-server/config/runtime.exs |
| gossamer | 4000 | 4040 | gossamer.conf.json |
| ambientops | 4000 | 4050 | composer/config/runtime.exs |
| reposystem | 4000 | 4060 | gui/config (if Phoenix) |

## Ephemeral Port Strategy

For services that run infrequently (batch jobs, one-shot scans, training runs):

1. **Reserve range 9100-9199** for ephemeral use
2. Acquire port via lockfile: `/tmp/hyper-port-<port>.lock`
3. Open firewall rule on acquire, close on release
4. Services announce availability via Groove protocol (port probe)

This reduces permanent firewall surface from ~20 ports to ~10 persistent + dynamic.

## VeriSimDB Instance Policy

Each project gets its own VeriSimDB instance on a unique port:
- 8080: standalone/default
- 8093: Stapeln
- 8094: Kategoria
- 8095: Project-M
- 8096: Work (RAM disk)
- 8097: standards innervation instance (live per standards/.verisimdb/config.toml)
- 8098-8099: reserved for future projects

NEVER store one project's data in another project's VeriSimDB instance.
