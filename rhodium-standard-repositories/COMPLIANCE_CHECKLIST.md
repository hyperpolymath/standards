# RSR Compliance Checklist

**Version**: 1.0.0
**Last Updated**: 2025-11-22
**Purpose**: Validate Rhodium Standard Repository (RSR) compliance

This checklist provides **pass/fail criteria** for all 11 RSR compliance categories. Use this for self-assessment, audits, and CI/CD automation.

---

## How to Use This Checklist

1. **Self-Assessment**: Review each section manually
2. **Automation**: Run `just validate` to execute automated checks
3. **Audit**: Use for third-party compliance verification
4. **CI/CD Integration**: Embed checks in `.gitlab-ci.yml`

**Scoring**:
- ✅ **Pass**: Requirement met
- ⚠️ **Partial**: Partially implemented, needs work
- ❌ **Fail**: Requirement not met
- N/A: Not applicable to this project

---

## Category 1: Foundational Infrastructure

### 1.1 Reproducibility & Configuration

- [ ] **Nix flakes**: `flake.nix` and `flake.lock` present and functional
- [ ] **Nickel configs**: Infrastructure-as-code in Nickel format (or CUE if complex)
- [ ] **Justfile**: Task runner with comprehensive recipes (`just --list` shows 15+ tasks)
- [ ] **Podman**: Container configuration uses Podman (never Docker)
- [ ] **Chainguard Wolfi**: Base images use `cgr.dev/chainguard/wolfi-base`

**Automation**: `nix flake check`, `just validate`

---

### 1.2 Version Control & Automation

- [ ] **GitLab**: Repository hosted on GitLab (not GitHub)
- [ ] **Git hooks**: Pre-commit and pre-push hooks configured
- [ ] **RVC (Robot Vacuum Cleaner)**: Automated tidying via hooks
- [ ] **SaltRover**: Offline repository management configured (if applicable)
- [ ] **Salt states**: Configuration management in place (temporary, migrating to Nickel)

**Automation**: Check `.git/hooks/`, verify GitLab remote

---

## Category 2: Documentation Standards

### 2.1 Required Files (Exact Naming)

Core documentation must exist with exact filenames:

- [ ] `README.md` or `README.adoc`
- [ ] `LICENSE.txt` (plain text, not `.md`)
- [ ] `SECURITY.md` (not `.txt` or `.adoc`)
- [ ] `CODE_OF_CONDUCT.md` or `CODE_OF_CONDUCT.adoc`
- [ ] `CONTRIBUTING.md` or `CONTRIBUTING.adoc`
- [ ] `FUNDING.yml` (not `.yaml`)
- [ ] `GOVERNANCE.adoc`
- [ ] `MAINTAINERS.md`
- [ ] `.gitignore` (not `gitignore`)
- [ ] `.gitattributes`

**Automation**: `test -f README.md || test -f README.adoc`

---

### 2.2 Well-Known Directory

- [ ] `.well-known/security.txt` (RFC 9116 compliant)
- [ ] `.well-known/ai.txt` (AI crawling policies)
- [ ] `.well-known/consent-required.txt` (HTTP 430 protocol)
- [ ] `.well-known/provenance.json` (provenance chains)
- [ ] `.well-known/humans.txt` (attribution)

**Automation**: `test -d .well-known && ls .well-known/`

---

### 2.2.5 AI Context Files

Machine-readable context for AI assistants:

- [ ] `META.scm` — Architecture decisions, development practices, design rationale

- [ ] `ECOSYSTEM.scm` — Project ecosystem position, relationships, boundaries

- [ ] `STATE.scm` — Current project state, progress, blockers, session history

- [ ] SPDX headers present in all three files

- [ ] Guile Scheme syntax valid (parseable)

**Purpose**: These files enable AI assistants to maintain context across sessions, understand architectural decisions, and track project state. They work alongside `.claude/CLAUDE.md` or similar instruction files.

**Specification Repositories**:

- [META.scm](https://github.com/hyperpolymath/META.scm)

- [ECOSYSTEM.scm](https://github.com/hyperpolymath/ECOSYSTEM.scm)

- [STATE.scm](https://github.com/hyperpolymath/state.scm)

**Automation**: `test -f META.scm && test -f ECOSYSTEM.scm && test -f STATE.scm`

---
### 2.3 Structural Requirements

README must contain:
- [ ] Project overview (1-2 paragraphs)
- [ ] Installation instructions
- [ ] Usage examples
- [ ] License reference

SECURITY must define:
- [ ] Vulnerability reporting channel
- [ ] Response SLA (e.g., "24-hour acknowledgement")
- [ ] Supported versions table

LICENSE must be:
- [ ] SPDX-identified (e.g., `MIT`, `Apache-2.0`, `Palimpsest-0.8`)
- [ ] Plain text format (`.txt`)
- [ ] Single file (or dual: `LICENSE-MIT` + `LICENSE-PALIMPSEST`)

**Automation**: `rg "SPDX-License-Identifier" LICENSE.txt`

---

### 2.4 Link Integrity

- [ ] All outbound links validated (no 404s)
- [ ] All internal anchors resolve
- [ ] All images have alt text
- [ ] Cross-references consistent (e.g., `README → FUNDING.yml`)

**Automation**: `lychee --verbose docs/ *.md *.adoc`

---

### 2.5 DocGementer Compliance

- [ ] Canonical heading synonyms normalized
- [ ] Metadata extracted and validated
- [ ] Anchor resolution automated
- [ ] Lychee link validation in CI/CD
- [ ] Codespell/vale for prose quality

**Automation**: `just check-links`

---

## Category 3: Security Architecture (10+ Dimensions)

### 3.1 Type Safety

- [ ] Primary language provides compile-time type safety (Ada/Chapel/Elixir/Haskell/Julia/ReScript/Rust)
- [ ] No TypeScript (unsound gradual typing)
- [ ] No Python (except SaltStack, temporary)
- [ ] No JavaScript (being actively eliminated)

**Automation**: Check `Cargo.toml`, `mix.exs`, `*.gpr`, `package.json` absence

---

### 3.2 Memory Safety

- [ ] Rust ownership model **or** Ada SPARK proofs **or** GC-based (Elixir/Haskell)
- [ ] No manual memory management in C/C++ without verification
- [ ] WASM compilation targets available (if applicable)

**Automation**: `cargo audit`, `gnatcheck` (for Ada)

---

### 3.3 Data Security

- [ ] CRDTs used for distributed state (no distributed locking)
- [ ] No cache invalidation complexity (offline-first design)
- [ ] Deno KV or equivalent for persistent CRDT storage (if applicable)

**Automation**: `rg "CRDT|LWW|AWSet|ORSet" src/`

---

### 3.4 Process Security

- [ ] Deno permissions model: explicit `--allow-*` flags (if using Deno)
- [ ] Podman rootless containers (no privileged daemon)
- [ ] Software-Defined Perimeter (SDP) for network access
- [ ] Zero Trust architecture (no default internal trust)

**Automation**: Check Containerfile for `USER` directive, no `--privileged`

---

### 3.5 Platform Security

- [ ] Chainguard Wolfi base images (minimal attack surface)
- [ ] RISC-V consideration documented (if hardware-specific)
- [ ] Supply chain auditing: SPDX headers on every source file
- [ ] `just audit-licence` command available

**Automation**: `just audit-licence`

---

### 3.6 Network Security

- [ ] IPv6 native support (no IPv4-only)
- [ ] QUIC protocol (HTTP/3) preferred over HTTP/2
- [ ] DoQ (DNS over QUIC) or oDNS (Oblivious DNS)
- [ ] DNSSEC validation mandatory
- [ ] Security headers configured (CSP, HSTS, X-Frame-Options, COOP, COEP, CORP)
- [ ] HTTP header minimization (Maximum Principal Reduction)

**Automation**: `rg "Content-Security-Policy|Strict-Transport-Security"` in configs

---

### 3.7 Privacy & Data Minimization

- [ ] Necessary processing only (Ada philosophy: if it exists, it has a reason)
- [ ] Cookie minimization or none at all
- [ ] No tracking scripts
- [ ] Privacy-respecting analytics (or none)
- [ ] GDPR/CCPA compliance by design
- [ ] Data retention policies documented

**Automation**: `rg -i "cookie|analytics|tracking" src/` (should find none)

---

### 3.8 Fault Tolerance

- [ ] Elixir supervision trees **or** equivalent fault isolation
- [ ] OTP patterns (let it crash, restart cleanly)
- [ ] Circuit breakers for external dependencies
- [ ] Graceful degradation (offline mode, partial functionality)

**Automation**: `rg "GenServer|Supervisor|supervisor" src/` (for Elixir)

---

### 3.9 Self-Healing

- [ ] CRDT conflict resolution (automatic, deterministic)
- [ ] Supervision tree restarts (automatic process recovery)
- [ ] Health checks and automatic remediation
- [ ] RVC automated cleanup (preventive maintenance)

**Automation**: Check for health check endpoints

---

### 3.10 Kernel Security

- [ ] Podman (no Docker daemon)
- [ ] cgroups v2 resource limits
- [ ] SELinux/AppArmor mandatory access control
- [ ] Seccomp syscall filtering

**Automation**: `podman info | rg "cgroupVersion: 2"`

---

### 3.11 Supply Chain Security

- [ ] SPDX audit on every source file
- [ ] Dependency vendoring for critical components
- [ ] Pinned versions (no floating ranges: `~`, `^`)
- [ ] SBOM (Software Bill of Materials) generation

**Automation**: `just audit-licence`, `just sbom-generate`

---

## Category 4: Architecture Principles

### 4.1 Distributed-First Design

- [ ] CRDTs for state (no coordination needed)
- [ ] Event sourcing where appropriate
- [ ] Blockchain consideration for audit trails (if applicable)
- [ ] Peer-to-peer capabilities (not always client-server)

**Automation**: `rg "EventSourcing|CRDT" docs/architecture/`

---

### 4.2 Offline-First

- [ ] SaltRover offline repository management **or** equivalent
- [ ] Local-first software principles
- [ ] Intermittent connectivity never blocks work
- [ ] Sync when online (not required for operation)

**Automation**: Test `git clone` + `just build` works without network

---

### 4.3 Reversibility

- [ ] Every operation can be undone
- [ ] No destructive defaults
- [ ] Confirmation for risky operations
- [ ] Git history + RVC tidying = safe experimentation
- [ ] `REVERSIBILITY.md` document present

**Automation**: `test -f REVERSIBILITY.md`

---

### 4.4 Reflexivity

- [ ] Systems that can reason about themselves
- [ ] Meta-programming where beneficial (Elixir macros, Nickel contracts)
- [ ] Homoiconicity (code-as-data, Lisp-style where appropriate)

**Automation**: N/A (architectural review)

---

### 4.5 Interoperability (iSOS: Integrated Stack of Stacks)

- [ ] FFI layers documented (e.g., Rust ↔ Ada)
- [ ] WASM targets available
- [ ] Standard protocols: HTTP/3, QUIC, WebRTC
- [ ] Semantic web integration (Schema.org, RDF, JSON-LD)

**Automation**: Check for FFI bindings, WASM build targets

---

## Category 5: Web Standards & Protocols

### 5.1 DNS Configuration (if web-facing)

- [ ] DNSSEC validation
- [ ] CAA records (Certificate Authority Authorization)
- [ ] SPF/DKIM/DMARC for email domains
- [ ] DANE (DNS-based Authentication of Named Entities)
- [ ] SVCB/HTTPS records (service binding)

**Automation**: `dig +dnssec example.com`

---

### 5.2 TLS/SSL Best Practices (if web-facing)

- [ ] TLS 1.3 only (no TLS 1.2 or earlier)
- [ ] Certificate pinning where appropriate
- [ ] OCSP stapling
- [ ] HSTS preload
- [ ] Certificate transparency monitoring

**Automation**: `testssl.sh example.com`

---

### 5.3 HTTP Security Headers (if web-facing)

Must include:
- [ ] `Content-Security-Policy: default-src 'self'`
- [ ] `X-Frame-Options: DENY`
- [ ] `X-Content-Type-Options: nosniff`
- [ ] `Referrer-Policy: no-referrer`
- [ ] `Permissions-Policy: geolocation=(), microphone=(), camera=()`
- [ ] `Cross-Origin-Opener-Policy: same-origin`
- [ ] `Cross-Origin-Embedder-Policy: require-corp`
- [ ] `Cross-Origin-Resource-Policy: same-origin`

**Automation**: `curl -I https://example.com | rg "Content-Security-Policy"`

---

## Category 6: Semantic Web & IndieWeb

### 6.1 Vocabularies & Linked Data (if applicable)

- [ ] Schema.org markup where appropriate
- [ ] RDF for interrelated datasets
- [ ] JSON-LD for structured data
- [ ] Microformats (h-card, h-entry)

**Automation**: `rg "@context|@type|h-entry" docs/`

---

### 6.2 IndieWeb Principles (if applicable)

- [ ] Own your data (not platform-dependent)
- [ ] Webmention support for federated comments
- [ ] Micropub for publishing
- [ ] POSSE (Publish Own Site, Syndicate Elsewhere)
- [ ] RelMeAuth for identity verification

**Automation**: Check for Webmention endpoints

---

## Category 7: FOSS & Licensing

### 7.1 License Clarity

- [ ] `LICENSE.txt` present (plain text, SPDX-identified)
- [ ] SPDX headers in every source file
- [ ] `just audit-licence` passes
- [ ] Dependency license audit (no GPL contamination if incompatible)

**Automation**: `just audit-licence`

---

### 7.2 Contributor Rights

- [ ] Palimpsest License **or** clear attribution framework
- [ ] DCO (Developer Certificate of Origin) **or** CLA
- [ ] Clear attribution in `MAINTAINERS.md`

**Automation**: `test -f LICENSE-PALIMPSEST || test -f DCO`

---

### 7.3 Funding Transparency

- [ ] `FUNDING.yml` present
- [ ] OpenCollective **or** Liberapay **or** sponsor links
- [ ] Solidarity economics framework documented

**Automation**: `test -f FUNDING.yml`

---

## Category 8: Cognitive Ergonomics & Human Factors

### 8.1 Information Architecture

- [ ] Consistent directory structure across repos
- [ ] Canonical heading synonyms (no confusion)
- [ ] Progressive disclosure (simple → complex)

**Automation**: N/A (manual review)

---

### 8.2 Accessibility

- [ ] WCAG 2.1 AA compliance minimum (if web-facing)
- [ ] Semantic HTML (not div soup)
- [ ] Alt text on all images
- [ ] Keyboard navigation
- [ ] Screen reader testing

**Automation**: `pa11y-ci --sitemap https://example.com/sitemap.xml`

---

### 8.3 Internationalization

- [ ] i18n from the start (not an afterthought)
- [ ] UTF-8 everywhere
- [ ] Language tags (HTML lang attribute, if web)
- [ ] Right-to-left (RTL) support consideration

**Automation**: `rg "lang=\"en\"|i18n|gettext" src/`

---

## Category 9: Lifecycle Management

### 9.1 Upstream Dependencies

- [ ] Vendoring critical dependencies
- [ ] Pin specific versions (no floating ranges)
- [ ] Supply chain security (SPDX, SBOM)
- [ ] Dependency update policy documented

**Automation**: `rg "~|\\^" Cargo.toml mix.exs` (should fail)

---

### 9.2 Downstream Impact

- [ ] Semantic versioning (SemVer 2.0)
- [ ] Deprecation warnings (one version ahead)
- [ ] Migration guides for breaking changes
- [ ] API stability guarantees documented

**Automation**: Check `CHANGELOG.md` for SemVer compliance

---

### 9.3 End-of-Life Planning

- [ ] Sunset policy documented
- [ ] Archive strategy (not just deletion)
- [ ] Data export capabilities
- [ ] Succession planning (who maintains after you?)

**Automation**: `test -f ROADMAP.md` (should mention EOL)

---

## Category 10: Community & Governance

### 10.1 Tri-Perimeter Contribution Framework (TPCF)

- [ ] Perimeter 1 (Core): Maintainers-only access defined
- [ ] Perimeter 2 (Expert): Trusted contributor pathway defined
- [ ] Perimeter 3 (Community): Open contribution sandbox defined
- [ ] `CONTRIBUTING.adoc` documents TPCF

**Automation**: `test -f CONTRIBUTING.adoc && rg "Perimeter" CONTRIBUTING.adoc`

---

### 10.2 Code of Conduct

- [ ] Explicit CoC (Contributor Covenant or custom)
- [ ] Enforcement procedures documented
- [ ] Reporting mechanisms clear
- [ ] Conflict resolution process

**Automation**: `test -f CODE_OF_CONDUCT.md || test -f CODE_OF_CONDUCT.adoc`

---

### 10.3 Governance Model

- [ ] `GOVERNANCE.adoc` defining decision-making
- [ ] Maintainer succession process
- [ ] Voting procedures (if applicable)
- [ ] Financial transparency (if funded)

**Automation**: `test -f GOVERNANCE.adoc`

---

## Category 11: Mutually Assured Accountability (MAA)

### 11.1 Framework Integration

- [ ] MAA principles embedded in architecture
- [ ] RMR (Reputation, Merit, Rights) utilities
- [ ] RMO (Responsibility, Monitoring, Obligations) utilities
- [ ] Formal verification of accountability properties

**Automation**: `rg "RMR|RMO|MAA" docs/architecture/`

---

### 11.2 Audit Trails

- [ ] Immutable logs (blockchain where appropriate)
- [ ] Provenance chains (`.well-known/provenance.json`)
- [ ] Change attribution (Git history + SPDX)

**Automation**: `test -f .well-known/provenance.json`

---

## Summary Scoring

**Total Categories**: 11
**Total Checkpoints**: 150+

**Pass Thresholds**:
- **RSR Gold** (Full Compliance): 100% pass
- **RSR Silver** (Strong Compliance): 90-99% pass
- **RSR Bronze** (Basic Compliance): 75-89% pass
- **Non-Compliant**: < 75% pass

**Automated Validation**:
```bash
just validate  # Runs all automated checks
```

---

## Continuous Compliance

**Frequency**:
- **Pre-commit**: SPDX headers, format, lint
- **Pre-push**: Tests, security scan
- **CI/CD**: Full `just validate` on every MR
- **Weekly**: Dependency audit
- **Quarterly**: Manual compliance review
- **Annual**: Third-party audit (if applicable)

---

## Contact

- **Compliance questions**: Open issue with `compliance` label
- **Audit requests**: {compliance-email}
- **RSR specification**: See `CLAUDE.md`

---

*"Compliance is not a checklist—it's a continuous commitment to excellence."*

— The Rhodium Standard
