# Port Registry — hyperpolymath ecosystem
# Last audited: 2026-03-28
#
# RULE: Every project MUST use a unique port. No two projects share a port.
# Check this file BEFORE assigning ports in new projects.

## Assigned Ports (CANONICAL — use these)

| Port | Project | Service | Protocol |
|------|---------|---------|----------|
| 4010 | stapeln | Phoenix backend | HTTP |
| 4020 | burble | Phoenix voice server | HTTP |
| 4030 | idaptik | Sync server | HTTP |
| 4040 | gossamer | Dev server | HTTP |
| 4050 | ambientops | Composer | HTTP |
| 4060 | reposystem | GUI backend | HTTP |
| 6480 | vext | Verifiable comms protocol | HTTP |
| 7500 | gitbot-fleet | Bot orchestration | HTTP |
| 7600 | panic-attacker | Static analysis server | HTTP |
| 7700 | boj-server | Main MCP API | HTTP |
| 7701 | boj-server | Browser bridge | HTTP |
| 7800 | boj-server | Cartridge runner | HTTP |
| 8080 | verisimdb | Default API (standalone) | HTTP |
| 8081 | echidna | Neural/prover API | HTTP |
| 8090 | idaptik | Game server (dev) | HTTP |
| 8093 | verisimdb | Stapeln instance | HTTP |
| 8094 | verisimdb | Kategoria instance | HTTP |
| 8095 | verisimdb | Project-M instance | HTTP |
| 8096 | verisimdb | Work instance (RAM disk) | HTTP |
| 9090 | hypatia | Web + Groove endpoints | HTTP |
| 50051 | verisimdb | gRPC | gRPC |

## Commonly Blocked Ports (AVOID)

These are frequently blocked by corporate firewalls, ISPs, or proxies:

| Port | Why blocked | Our status |
|------|-----------|-----------|
| 25 | SMTP — spam prevention | Not used |
| 80 | HTTP — requires root | Not used (dev) |
| 443 | HTTPS — requires root | Not used (dev) |
| 1080 | SOCKS proxy — abuse | Not used |
| 3128 | Squid proxy | Not used |
| 8080 | Common proxy port — **often intercepted** | **VeriSimDB default — consider changing** |
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
- 8096-8099: reserved for future projects

NEVER store one project's data in another project's VeriSimDB instance.
