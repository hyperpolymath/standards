# CLAUDE.md

**Rhodium Standard Repository (RSR) Guidance for Claude AI**

This document provides comprehensive guidance for Claude AI when working with Rhodium Standard Repositories, which embody the principles of the Campaign for Cooler Coding and Programming (CCCP).

---

## AI CLI Defaults (Workspace Standard)

- Primary repos root: `$REPOS_DIR` (typically `~/Documents/hyperpolymath-repos`)
- Remote mapping: `https://github.com/hyperpolymath/<name>`; names are lowercase and dots become `-`
- If a repo is missing locally, check the remote and sync it
- Include `ai-cli-crash-capture/` and `1-formats/contractiles/` from the template in new repos
- Mirror 6SCM files into `.machine_readable/6scm/`
- Before any code generation, check `$REPOS_DIR/proven` for the current Idris “unbreakable” library status
- If a project uses that Idris library, add an *Idris Inside* badge to the repo
- If new bindings are required, add them in `$REPOS_DIR/proven` as `idris2-zig-<language>` during the work
- If new code suggests additional “unbreakable” Idris modules, add them under `$REPOS_DIR/proven` and update binders across all supported languages

## What is a Rhodium Standard Repository?

A Rhodium Standard Repository is not just a code repository—it is a **comprehensive framework for emotionally safe, technically excellent, politically autonomous software development** that considers the entire lifecycle from upstream dependencies to downstream human impact.

### Why "Rhodium"?

Rhodium is the **rarest and most precious** of all metals, chosen for specific symbolic properties:

- **Rarest precious metal**: Excellence over ubiquity
- **Catalytic role**: Transforms toxic outputs into safer alternatives (like transforming brittle JavaScript ecosystems into resilient multi-language stacks)
- **Wedding ring plating**: Durability for long-term commitments and maintainability
- **Corrosion resistance**: Systems that don't decay under pressure

---

## The CCCP: Campaign for Cooler Coding and Programming

The CCCP is the ideological and aesthetic framework that gives RSR its purpose.

### "Cooler" as Multi-Dimensional

1. **Emotional Temperature**: Reduced anxiety through reversibility, safe experimentation, no shame in mistakes
2. **Computational Temperature**: Efficient systems, no unnecessary cycles, minimal runtime overhead
3. **Social Temperature**: Warm community with graduated trust, cool-headed technical decisions
4. **Political Temperature**: Resistance to monopolistic tooling, anti-corporate lock-in, autonomy through offline-first design

### Core CCCP Principles

1. **Post-JavaScript Liberation**: Rejecting the npm/node ecosystem's fragility
2. **Offline-First as Autonomy**: Work continues without constant connectivity
3. **Distributed State Without Coordination**: CRDTs eliminate locking and cache invalidation
4. **Formal Verification as Mutual Aid**: Correctness is an act of solidarity
5. **Community Over Ego**: Architecture enforces collaborative patterns
6. **Language Polyglotism as Resistance**: Using Ada, Rust, Elixir, Haskell, AffineScript instead of JavaScript/Python monoculture

---

## The Citadel of Code Creation

The Citadel is where RSR meets CCCP—the actual implementation pattern that embodies both.

### What Makes It a "Citadel"?

- **Defensible Position**: Local development environment can't be taken away by service outages or corporate decisions
- **Self-Sufficient**: Nix + Podman + SaltRover = complete autonomy
- **Collective Defense**: Architecture makes mutual aid easier than isolation
- **Safe Haven**: Emotionally and technically safe to experiment

---

## RSR Architecture

```
┌───────────────────────────────────────────────────────────────────────────────┐
│   🌍 COMMUNITY: "Friction-Free, Reversible, Emotionally Safe Development"      │
└───────────────────────────┬───────────────────────────────────┬───────────────┘
                            │                                   │
                            ▼                                   ▼
┌─────────────────┐       ┌─────────────────┐           ┌─────────────────────┐
│   📡 GitLab     │◄──────┤  SaltRover      │◄──────────┤  Local Dev          │
│  (CI/CD)        │       │ (Offline Repo   │           │  (Podman + Salt)    │
└─────────┬───────┘       │  Manager)       │           └──────────┬──────────┘
          │               └─────────┬───────┘                      │
          ▼                         ▼                              ▼
┌─────────────────┐       ┌─────────────────┐           ┌─────────────────────┐
│  Haskell        │       │  Nickel Config  │           │  Podman Compose     │
│  Registry       │       │  (Infra as Code)│           │  (Elixir, Ada,      │
│  (Validation)   │       │                 │           │  Rust, AffineScript)│
└─────────┬───────┘       └─────────┬───────┘           └──────────┬──────────┘
          │                         │                              │
          ▼                         ▼                              ▼
┌───────────────────────────────────────────────────────────────────────────────┐
│  🚀 POST-JAVASCRIPT STACK (Podman Orchestration)                              │
│                                                                               │
│  ┌──────────────────────────────────────────────────────────────────┐        │
│  │  Frontend: AffineScript → typed-wasm (affine/linear)             │        │
│  └────────────────────────────────┬─────────────────────────────────┘        │
│                                   │                                          │
│  ┌──────────────────────────────────────────────────────────────────┐        │
│  │  Router: CADRE (AffineScript + Bun + CRDTs)                      │        │
│  │  - Affine/linear type safety                                     │        │
│  │  - Sandboxed at the typed-wasm boundary                          │        │
│  │  - Conflict-free distributed state                               │        │
│  └────────────────────────────────┬─────────────────────────────────┘        │
│                                   │                                          │
│  ┌──────────────────────────────────────────────────────────────────┐        │
│  │  Backend: Elixir GenServers                                      │        │
│  │  - Supervision trees (fault tolerance)                           │        │
│  │  - OTP patterns (self-healing)                                   │        │
│  └────────────────────────────────┬─────────────────────────────────┘        │
│                                   │                                          │
│  ┌──────────────────────────────────────────────────────────────────┐        │
│  │  FFI Layer: Rust ←→ Ada                                          │        │
│  │  - Memory safety (Rust)                                          │        │
│  │  - SPARK verification (Ada)                                      │        │
│  │  - WASM compilation targets                                      │        │
│  └────────────────────────────────┬─────────────────────────────────┘        │
│                                   │                                          │
│  ┌──────────────────────────────────────────────────────────────────┐        │
│  │  Validation: Haskell Registry                                    │        │
│  │  - Pure functional plugin validation                             │        │
│  └──────────────────────────────────────────────────────────────────┘        │
└───────────────────────────────────────────────────────────────────────────────┘
       ▲                      ▲                      ▲
       │                      │                      │
┌──────┴───────┐      ┌───────┴───────┐      ┌────────┴────────┐
│  📦 Nix      │      │  📦 Podman    │      │  📦 SaltStack   │
│  Flakes      │      │  (Chainguard  │      │  (Config Mgmt)  │
│              │      │   Wolfi)      │      │                 │
└──────────────┘      └───────────────┘      └─────────────────┘
```

### Key Components

#### GitLab (Never GitHub)
- Source of truth for canonical repository state
- CI/CD pipeline orchestration
- Not a bottleneck—SaltRover provides offline capability

#### SaltRover
- **Offline-first repository manager**
- Syncs with GitLab when online, fully functional offline
- Triggers CI/CD pipelines but doesn't depend on them
- Creates buffer between individual developers and centralized infrastructure
- **Philosophy**: Intermittent connectivity should never block creative work

#### Robot Vacuum Cleaner (RVC)
- **Automated repository tidying and optimization**
- Rust or Elixir only. Python is FULLY banned - its "SaltStack only" exception was removed 2026-01-03
- Triggered by Git hooks (pre-commit, pre-push)
- Operates during offline work, before push
- Preventive maintenance—keeps repos clean without manual intervention

#### CADRE Router
- **Replaces traditional HTTP servers (including Bandit)**
- AffineScript compilation (affine/linear types → typed-wasm)
- Bun runtime (tier 1; `bunx <tool>` for one-off tooling)
- CRDTs for conflict-free distributed state
- No databases + locks + cache invalidation complexity

---

## RSR Compliance Criteria

A repository is Rhodium Standard compliant when it meets the following comprehensive criteria:

### 1. Foundational Infrastructure

#### Reproducibility & Configuration
- ✅ **Nix flakes** for hermetic builds
- ✅ **Nickel configs** for all documentation, CI/CD, infrastructure-as-code
- ✅ **CUE** when validation complexity demands it
- ✅ **Justfile** with comprehensive, well-flagged CLI operations
- ✅ **Podman** (never Docker) with **Chainguard Wolfi** base images
  - Ultra-minimal, supply-chain-secure, APK-based distro
  - No legacy package bloat

#### Version Control & Automation
- ✅ **GitLab** (never GitHub) for source control
- ✅ Git hooks triggering local automation
- ✅ **RVC** for automated tidying
- ✅ **SaltRover** for offline-first repository management
- ✅ Nickel configs + Bash for configuration management (the Salt/Python exception was removed 2026-01-03)

### 2. Documentation Standards

#### Required Files (Exact Naming)

```
repository-root/
├── README.md                  # or .adoc
├── LICENSE.txt                # MUST be .txt (plain text)
├── 3-practice/SECURITY.md                # MUST be .md
├── CODE_OF_CONDUCT.md         # or .adoc
├── CONTRIBUTING.md            # or .adoc
├── FUNDING.yml                # MUST be .yml
├── 0-canon/GOVERNANCE.adoc
├── MAINTAINERS.md
├── .gitignore                 # NOT "gitignore"
├── .gitattributes
├── .well-known/
│   ├── security.txt
│   ├── ai.txt                 # AI crawling policies
│   ├── consent-required.txt   # HTTP 430 consent protocol
│   ├── provenance.json        # Content provenance chains
│   └── humans.txt
├── docs/                      # Long-form documentation
├── reference/                 # API documentation
└── policies/                  # Security, governance
```

#### Structural Requirements
- **README** must contain: Overview, Installation, Usage, License reference
- **SECURITY** must define: Vulnerability reporting, response SLA, supported versions
- **LICENSE** must be: SPDX-identified, plain text (.txt), single file

#### Link Integrity
- All outbound links validated via **Lychee**
- All internal anchors must resolve
- All images must have alt text
- Cross-references between docs must be consistent

#### DocGementer Pattern
- **Scanner**: ripgrep + git ls-files + tree-sitter
- **Normalizer**: mdast (Markdown) + Asciidoctor (AsciiDoc) → unified AST
- **Schema**: Nickel contracts for required docs, naming, structure
- **Rules**: miniKanren/Datalog for cross-file assertions
- **Quality**: Lychee, codespell/vale, anchor resolution
- **Reporter**: SARIF/JSON for CI, human reports (AsciiDoc/Markdown)

### 3. Security Architecture (10+ Dimensions)

#### Type Safety
- ✅ **AffineScript** (affine/linear types, compiles to typed-wasm) for frontend
- ✅ **Rust** for systems programming
- ✅ **Ada + SPARK** for safety-critical paths
- ✅ **Elixir** (Erlang VM) for fault-tolerant services
- ✅ **Haskell** for pure functional validation
- ❌ **No TypeScript** (unsound gradual typing) - use AffineScript. Not a fallback tier.
- ❌ **No Python** (fully banned; the SaltStack exception was removed 2026-01-03)
- ❌ **No JavaScript** (actively being eliminated)

#### Memory Safety
- ✅ Rust ownership model
- ✅ Ada SPARK proofs
- ✅ No garbage collection pauses (Rust/Ada preferred)
- ✅ WASM compilation targets for sandboxed execution

#### Data Security
- ✅ **CRDTs** (Conflict-free Replicated Data Types) for distributed state
- ✅ No distributed locking
- ✅ No cache invalidation issues
- ✅ Offline-first by design
- ✅ An embedded KV store for persistent CRDT state (Bun ships a built-in SQLite driver)

#### Process Security
- ✅ **Capability boundaries enforced OUTSIDE the runtime**: no file, network or
  environment access beyond what the deployment grants
  - ⚠ This replaced the Deno permission model when Deno was banned (2026-09-22).
    Bun is the estate runtime and has **no granular permission flags**, so the
    boundary cannot live in the runtime any more. Do not read "Bun" as a
    drop-in for `--allow-net` / `--allow-read`; it has no such thing.
  - The boundary is therefore the typed-wasm sandbox (AffineScript compiles to
    it), the rootless Podman container, and the SDP network perimeter below.
- ✅ Podman rootless containers
- ✅ **Software-Defined Perimeter (SDP)** for network access
- ✅ Zero Trust architecture

#### Platform Security
- ✅ Chainguard Wolfi base images (minimal attack surface)
- ✅ **RISC-V** consideration (open hardware, no backdoors)
- ✅ Supply chain auditing: **SPDX headers on every file**
- ✅ `just audit-license` command for automated compliance

#### Network Security
- ✅ **IPv6 native** (no IPv4 legacy cruft)
- ✅ **QUIC protocol** (HTTP/3, reduced latency, improved security)
- ✅ **DoQ** (DNS over QUIC) replacing DoH/DoT
- ✅ **oDNS** (Oblivious DNS) for privacy
- ✅ **DNSSEC** validation mandatory
- ✅ Security headers configured by default (CSP, HSTS, X-Frame-Options, etc.)
- ✅ **HTTP header minimization** (Maximum Principal Reduction):
  - Only necessary headers exposed
  - No verbose error messages to clients
  - Strict CORS policies

#### Privacy & Data Minimization
- ✅ **Necessary processing only** (Ada philosophy: if it exists, it has a reason)
- ✅ Cookie minimization (or none at all)
- ✅ No tracking scripts
- ✅ Privacy-respecting analytics (if any)
- ✅ GDPR/CCPA compliance by default
- ✅ Data retention policies documented

#### Fault Tolerance
- ✅ Elixir supervision trees (let it crash, restart cleanly)
- ✅ OTP patterns (battle-tested Erlang reliability)
- ✅ Circuit breakers for external dependencies
- ✅ Graceful degradation (offline mode, partial functionality)

#### Self-Healing
- ✅ CRDT conflict resolution (automatic, deterministic)
- ✅ Supervision tree restarts (automatic process recovery)
- ✅ Health checks and automatic remediation
- ✅ RVC automated cleanup (preventive maintenance)

#### Kernel Security
- ✅ Podman (no privileged daemon)
- ✅ cgroups v2 resource limits
- ✅ SELinux/AppArmor mandatory access control
- ✅ Seccomp syscall filtering

#### Supply Chain Security
- ✅ SPDX audit on every file
- ✅ Dependency vendoring for critical components
- ✅ Pinned versions (no floating ranges)
- ✅ SBOM (Software Bill of Materials) generation

### 4. Architecture Principles

#### Distributed-First Design
- ✅ CRDTs for state (no coordination needed)
- ✅ Event sourcing where appropriate
- ✅ Blockchain consideration for audit trails (not for everything!)
- ✅ Peer-to-peer capabilities (not always client-server)

#### Offline-First
- ✅ SaltRover offline repository management
- ✅ Local-first software principles
- ✅ Intermittent connectivity never blocks work
- ✅ Sync when online (not required for operation)

#### Reversibility
- ✅ Every operation can be undone
- ✅ No destructive defaults
- ✅ Confirmation for risky operations
- ✅ Git history + RVC tidying = safe experimentation

#### Reflexivity
- ✅ Systems that can reason about themselves
- ✅ Meta-programming where beneficial (Elixir macros, Nickel contracts)
- ✅ Homoiconicity (code-as-data, Lisp-style where appropriate)

#### Interoperability (iSOS: Integrated Stack of Stacks)
- ✅ FFI layers: Rust ↔ Ada ↔ Elixir
- ✅ WASM targets for polyglot integration
- ✅ Standard protocols: HTTP/3, QUIC, WebRTC
- ✅ Semantic web: Vocabularies, linked data, IndieWeb principles

### 5. Web Standards & Protocols

#### .well-known/ Directory

```
.well-known/
├── security.txt          # Security contact, PGP keys
├── ai.txt                # AI crawling policies
├── consent-required.txt  # HTTP 430 consent protocol
├── provenance.json       # Content provenance chains
└── humans.txt            # Attribution, credits
```

#### DNS Configuration
- ✅ DNSSEC validation
- ✅ CAA records (Certificate Authority Authorization)
- ✅ SPF/DKIM/DMARC for email domains
- ✅ DANE (DNS-based Authentication of Named Entities)
- ✅ SVCB/HTTPS records (service binding)

#### TLS/SSL Best Practices
- ✅ TLS 1.3 only (no legacy protocol support)
- ✅ Certificate pinning where appropriate
- ✅ OCSP stapling
- ✅ HSTS preload
- ✅ Certificate transparency monitoring

#### HTTP Security Headers (Mandatory)

```http
Content-Security-Policy: default-src 'self'; script-src 'none'
X-Frame-Options: DENY
X-Content-Type-Options: nosniff
Referrer-Policy: no-referrer
Permissions-Policy: geolocation=(), microphone=(), camera=()
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
Cross-Origin-Resource-Policy: same-origin
```

### 6. Semantic Web & IndieWeb

#### Vocabularies & Linked Data
- ✅ Schema.org markup where appropriate
- ✅ RDF for interrelated datasets
- ✅ JSON-LD for structured data
- ✅ Microformats (h-card, h-entry, etc.)

#### IndieWeb Principles
- ✅ Own your data (not platform-dependent)
- ✅ Webmention support for federated comments
- ✅ Micropub for publishing
- ✅ POSSE (Publish Own Site, Syndicate Elsewhere)
- ✅ RelMeAuth for identity verification

### 7. FOSS & Licensing

#### License Clarity
- ✅ **LICENSE.txt** (plain text, SPDX-identified)
- ✅ SPDX headers in every source file
- ✅ `just audit-license` validation
- ✅ Dependency license audit (no GPL contamination if incompatible)

#### Contributor Rights
- ✅ **Palimpsest License** (versioning/attribution/transformation rights framework)
- ✅ DCO (Developer Certificate of Origin) or CLA
- ✅ Clear attribution in MAINTAINERS.md

#### Funding Transparency
- ✅ **FUNDING.yml** (sponsor links)
- ✅ OpenCollective or Liberapay integration
- ✅ Solidarity economics framework

### 8. Cognitive Ergonomics & Human Factors

#### Information Architecture
- ✅ Consistent directory structure across repos
- ✅ Canonical heading synonyms (no confusion)
- ✅ Progressive disclosure (simple → complex)

#### Accessibility
- ✅ WCAG 2.1 AA compliance minimum
- ✅ Semantic HTML (not div soup)
- ✅ Alt text on all images
- ✅ Keyboard navigation
- ✅ Screen reader testing

#### Internationalization
- ✅ i18n from the start (not an afterthought)
- ✅ UTF-8 everywhere
- ✅ Language tags (HTML lang attribute)
- ✅ Right-to-left (RTL) support consideration

### 9. Lifecycle Management

#### Upstream Dependencies
- ✅ Vendoring critical dependencies
- ✅ Pin specific versions (no floating ranges)
- ✅ Supply chain security (SPDX, SBOM)
- ✅ Dependency update policy (automated PRs reviewed by humans)

#### Downstream Impact
- ✅ Semantic versioning (SemVer 2.0)
- ✅ Deprecation warnings (one version ahead)
- ✅ Migration guides for breaking changes
- ✅ API stability guarantees

#### End-of-Life Planning
- ✅ Sunset policy documented
- ✅ Archive strategy (not just deletion)
- ✅ Data export capabilities
- ✅ Succession planning (who maintains after you?)

### 10. Community & Governance

#### Tri-Perimeter Contribution Framework (TPCF)

**🔒 Perimeter 1: Core Systems (Maintainers Only)**
- **Languages**: Rust, Nickel, Bash, C++
- **Scope**: Shell runtime, build systems, protocol emitters, SPDX audit, CI/CD
- **Contribution**: Closed. Direct commits by maintainers only.
- **Rationale**: Architectural integrity > open contribution here

**🧠 Perimeter 2: Expert Extensions (Trusted Contributors)**
- **Languages**: Rust, Nickel, Bash
- **Scope**: Protocol extensions, shell plugins, compliance validators
- **Contribution**: Apply via issue template → review → merge under `extensions/` or `emit/`
- **Requirements**: Unit tests, docs, examples, SPDX headers

**🌱 Perimeter 3: Community Sandbox (Open to All)**
- **Languages**: Shell, Markdown, AsciiDoc, JSON
- **Scope**: Docs, spec tests, compliance proposals
- **Contribution**: Fork → add to `doc/`, `spec/`, `.well-known/` → PR
- **Validation**: `just validate` locally, CI enforces SPDX/schema/formatting

This is **graduated trust without gatekeeping**—everyone can contribute, but scope matches expertise.

#### Code of Conduct
- ✅ Explicit CoC (Contributor Covenant or custom)
- ✅ Enforcement procedures documented
- ✅ Reporting mechanisms clear
- ✅ Conflict resolution process

#### Governance Model
- ✅ 0-canon/GOVERNANCE.adoc defining decision-making
- ✅ Maintainer succession process
- ✅ Voting procedures if applicable
- ✅ Financial transparency if funded

### 11. Mutually Assured Accountability (MAA)

#### Framework Integration
- ✅ MAA principles embedded in architecture
- ✅ RMR (Reputation, Merit, Rights) utilities
- ✅ RMO (Responsibility, Monitoring, Obligations) utilities
- ✅ Formal verification of accountability properties

#### Audit Trails
- ✅ Immutable logs (blockchain where appropriate)
- ✅ Provenance chains (`.well-known/provenance.json`)
- ✅ Change attribution (Git history + SPDX)

---

## Language Policy

> **Updated 2026-09-22.** This section is the seed copy of the estate language
> policy; its canonical source is `spec/LANGUAGE-POLICY.adoc` (rev 1.6.0) and its
> machine-readable twin is `spec.scm/language-policy.scm` (MODULE-VERSION 2.0.0).
> Where they disagree, the `.adoc` wins and this file is the defect. Four bans
> ratified after this document was written were missing from it entirely, so it
> was still recommending ReScript and Deno as destinations:
>
> | Banned | On | Replacement |
> |---|---|---|
> | Python (incl. the "SaltStack only" exception) | 2026-01-03 | AffineScript / Rust / Julia |
> | ReScript | 2026-04-30 | AffineScript (directly - not via ReScript) |
> | TypeScript | 2026-08-27 | AffineScript. Not a fallback tier. |
> | Deno | 2026-09-22 | Bun (tier 1; `bunx` for one-off tooling) |

### Prohibited Languages

❌ **ReScript**: Banned 2026-04-30
- Replace with: AffineScript. Migrate `.res` directly to `.affine` - do not route new work through ReScript.

❌ **TypeScript**: Banned 2026-08-27
- Replace with: AffineScript. Owner ruling: *"no typescript ... that should not exist at all."*
- It is **not** a fallback tier. Carve-outs are limited to `**/*.d.ts`, `**/bindings/ts/**` and `**/vscode/**`.

❌ **Deno**: Banned 2026-09-22
- Replace with: Bun. Owner ruling: *"deno is over, we're prioritising bun, and using bunx."*
- Bun is Node-compatible and reads `package.json` + `bun.lock`. ⚠ `bun run` does **not** read a `deno.json` `tasks` map - those become Justfile recipes.

❌ **JavaScript**: Actively being eliminated
- Replace with: AffineScript → typed-wasm, run under Bun. TypeScript is NOT a fallback.
- Build tools: Use Rust alternatives (rspack, turbopack)
- npm scripts: Replace with Justfile commands

❌ **Python**: Fully banned (the SaltStack exception was removed 2026-01-03)
- RVC rewrite in progress: Target is Rust or Elixir
- SaltStack replacement: Nickel configs → Bash scripts directly

### Approved Languages

✅ **AffineScript** (affine/linear types) - Frontend, compiles to typed-wasm
✅ **Bun** - JS runtime & package management (tier 1). `package.json` + `bun.lock`; `bunx <tool>` for one-off tooling
✅ **Rust** - Systems programming, memory safety
✅ **Julia** - Scientific computing, CLI tools, high-performance
✅ **Ada + SPARK** - Safety-critical, formal verification
✅ **Elixir** - Fault-tolerant services, OTP patterns
✅ **Haskell** - Pure functional validation, registry logic
✅ **Nickel** - Configuration, infrastructure-as-code
✅ **Bash** - Build orchestration, CLI tooling (with discipline)

---

## Working with RSR Repositories

### When Asked to Add Features

1. **Understand the existing structure first**
   - Check for similar patterns in the codebase
   - Verify compliance with RSR criteria
   - Identify which Perimeter the change affects (TPCF)

2. **Follow established conventions**
   - Use approved languages only
   - Maintain offline-first capability
   - Ensure reversibility of operations

3. **Update documentation alongside code**
   - Maintain DocGementer compliance
   - Update SPDX headers
   - Validate links with Lychee

4. **Add tests for new functionality**
   - Unit tests required
   - Integration tests for complex features
   - Security tests for privileged operations

5. **Verify compliance before committing**
   - Run `just validate` locally
   - Check `just audit-license`
   - Ensure RVC passes

### When Asked to Fix Issues

1. **Reproduce the issue if possible**
   - Test offline-first (disconnect network)
   - Verify across different Podman containers
   - Check CRDT conflict resolution

2. **Identify root cause**
   - Check logs (structured, auditable)
   - Examine supervision tree restarts
   - Review SPARK proofs if in Ada code

3. **Propose solution before implementing**
   - Consider reversibility
   - Evaluate security implications
   - Check downstream impact

4. **Test thoroughly**
   - Offline mode
   - Concurrent operations (CRDT conflicts)
   - Security boundaries (container + typed-wasm sandbox)

5. **Document the fix**
   - Update 3-practice/SECURITY.md if vulnerability
   - Add regression test
   - Update CHANGELOG (SemVer)

### When Asked to Explain Code

1. **Provide context about the file's purpose**
   - Which architectural layer? (Frontend/Router/Backend/FFI/Validation)
   - Which TPCF perimeter?
   - Security sensitivity level

2. **Explain the overall structure first**
   - How it fits in iSOS (Integrated Stack of Stacks)
   - Dependencies and dependents
   - Offline-first considerations

3. **Then dive into specific details**
   - Type safety guarantees (AffineScript/Rust/Ada)
   - CRDT operations if applicable
   - Supervision tree structure if Elixir

4. **Use examples to clarify complex concepts**
   - Show CADRE router request flow
   - Demonstrate CRDT conflict resolution
   - Illustrate SPARK proof techniques

5. **Reference related standards/documentation**
   - Link to relevant `.well-known/` protocols
   - Cite IndieWeb specifications
   - Point to formal verification proofs

### When Asked to Review Code

1. **Check for adherence to RSR standards**
   - Language policy compliance
   - Documentation completeness
   - SPDX header presence

2. **Look for potential issues or bugs**
   - Memory safety (if not Rust/Ada)
   - Concurrency issues (if not using CRDTs)
   - Offline-first violations

3. **Suggest improvements for clarity**
   - Type annotations (AffineScript/Rust/Haskell)
   - Error handling (Elixir supervision, Rust Result)
   - Security boundaries (container + typed-wasm sandbox)

4. **Verify documentation is complete**
   - DocGementer compliance
   - Link integrity (Lychee validation)
   - Accessibility (alt text, semantic HTML)

5. **Ensure tests are adequate**
   - Unit test coverage
   - Security test scenarios
   - Offline mode testing

---

## Development Workflow

### Local Development Setup

1. **Prerequisites**
   - Nix with flakes enabled
   - Podman (rootless mode)
   - SaltRover configured
   - RVC installed

2. **Initialize repository**
   ```bash
   nix develop          # Enter Nix shell
   just setup           # Run setup tasks
   just validate        # Verify RSR compliance
   ```

3. **Iterate with reversibility**
   ```bash
   just build           # Build project
   just test            # Run tests
   just lint            # Check style/security
   git commit           # RVC runs pre-commit hooks
   ```

4. **Offline work**
   - SaltRover syncs when online
   - Full functionality offline
   - RVC tidies during offline work
   - Push when connectivity returns

### CI/CD Pipeline

- GitLab CI/CD (never GitHub Actions)
- Triggered by SaltRover
- Runs in Podman containers (Chainguard Wolfi)
- SPDX audit (`just audit-license`)
- Link validation (Lychee)
- Security scanning
- Accessibility tests
- CRDT invariant checks

### Git Commit Conventions

Use conventional commit format:
- `feat:` for new features
- `fix:` for bug fixes
- `docs:` for documentation changes
- `refactor:` for code refactoring
- `test:` for test additions/changes
- `security:` for security improvements
- `perf:` for performance optimizations
- `build:` for build system changes

---

## Common Tasks & Examples

### Validating RSR Compliance

```bash
just validate         # Run all compliance checks
just audit-license    # SPDX header validation
just check-links      # Lychee link validation
just check-security   # Security header checks
just check-offline    # Offline-first capability
```

### Adding a New Standard

1. Research existing similar standards
2. Define clear objectives and scope (in Nickel)
3. Create schema/definition files
4. Write comprehensive documentation (DocGementer compliant)
5. Provide example implementations
6. Add validation tests (unit + security)
7. Update `.well-known/` manifests if applicable
8. Run `just validate` before committing

### Migrating from JavaScript/Python

#### JavaScript → AffineScript/Rust

```bash
# 1. Identify JS files
fd -e js -e jsx

# 2. For frontend: Convert to AffineScript
# (Affine/linear types; compiles to typed-wasm)

# 3. For Node scripts: Convert to Bun or Justfile tasks
# Bun is Node-compatible: run the code, drop the runtime

# 4. For build tools: Replace with Rust alternatives
# webpack → rspack
# esbuild → turbopack

```

#### Python → AffineScript/Rust/Elixir/Julia/Nickel

```bash
# 1. Identify Python files (no exclusions - Python is fully banned)
fd -e py

# 2. For scripts: Convert to Nickel or Bash
# Nickel for configuration/validation
# Bash for simple automation

# 3. For services: Convert to Elixir
# OTP supervision, fault tolerance

# 4. For performance-critical: Convert to Rust
# Memory safety, no GC pauses

```

### Implementing CRDT State

```elixir
# Elixir GenServer with CRDT state
defmodule MyApp.CRDTServer do
  use GenServer

  # State is a CRDT (e.g., AWSet, LWWMap)
  # Conflict-free, no coordination needed

  def handle_call({:merge, remote_state}, _from, local_state) do
    # Deterministic merge, commutative & associative
    merged = CRDT.merge(local_state, remote_state)
    {:reply, :ok, merged}
  end
end
```

### Setting Up Runtime Capability Boundaries

```javascript
// CADRE router under Bun.
// bun run server.js
//
// ⚠ Bun has NO per-process permission flags. There is no `--allow-net`
// equivalent, so the boundary is declared by the deployment, not the command:
//   - bind only the port you serve on
//   - mount only the paths you read (rootless Podman, read-only volumes)
//   - pass only the environment variables you need
// Application logic that must be sandboxed compiles to typed-wasm instead.

Bun.serve({
  port: 8000,
  fetch(req) {
    return new Response("ok");
  },
});
```

### Writing SPARK Proofs (Ada)

```ada
-- Ada with SPARK verification
procedure Process_Data (Input : in String; Output : out String)
  with Pre  => Input'Length > 0,
       Post => Output'Length = Input'Length
is
begin
  -- Implementation with provable memory safety
  -- SPARK proves no buffer overflows, no null dereferences
end Process_Data;
```

---

## Resources & Further Reading

### Official Documentation
- **Rhodium Standard Repositories**: Hyperpolymath/rhodium-standard-repositories
- **CCCP Manifesto**: (to be published)
- **Palimpsest License**: (to be published)

### Technologies
- **Nickel**: https://nickel-lang.org/
- **AffineScript**: https://github.com/hyperpolymath/affinescript
- **Bun**: https://bun.sh/
- **CRDTs**: https://crdt.tech/
- **SPARK**: https://www.adacore.com/about-spark
- **Chainguard Wolfi**: https://chainguard.dev/unchained/introducing-wolfi-the-first-linux-un-distro
- **IndieWeb**: https://indieweb.org/

### Security Standards
- **OWASP**: https://owasp.org/
- **DNSSEC**: https://www.icann.org/resources/pages/dnssec-what-is-it-why-important-2019-03-05-en
- **Security Headers**: https://securityheaders.com/
- **WCAG**: https://www.w3.org/WAI/WCAG21/quickref/

---

## Notes for Claude AI

### Core Principles
1. **Emotionally Safe Development**: Reversibility, no shame, safe experimentation
2. **Offline-First**: Intermittent connectivity never blocks work
3. **Formally Verified**: Correctness is care, use SPARK/Coq where critical
4. **Community Over Ego**: TPCF graduated trust model
5. **Post-JavaScript**: Eliminate JS/Python, use AffineScript/Rust/Elixir/Ada/Haskell
6. **Holistic Lifecycle**: Consider upstream dependencies to downstream human impact
7. **Maximum Principal Reduction**: Only necessary processing, minimal exposure
8. **Mutually Assured Accountability**: MAA framework embedded in architecture

### Always Check
- ✅ Is this offline-first capable?
- ✅ Is this reversible?
- ✅ Does this use approved languages?
- ✅ Are SPDX headers present?
- ✅ Is documentation DocGementer compliant?
- ✅ Are security headers configured?
- ✅ Is this accessible (WCAG 2.1 AA)?
- ✅ Which TPCF perimeter does this affect?

### Never Do
- ❌ Add Python, TypeScript, ReScript or Deno at all - all four are banned outright
- ❌ Use Docker (always Podman)
- ❌ Use GitHub (always GitLab)
- ❌ Add dependencies without vendoring/pinning
- ❌ Create online-only features
- ❌ Skip SPDX headers
- ❌ Ignore accessibility
- ❌ Bypass the sandbox or capability boundary

### When in Doubt
- Ask for clarification (don't assume)
- Check existing patterns (consistency matters)
- Consider reversibility (can this be undone?)
- Evaluate security implications (defense in depth)
- Think holistically (upstream → downstream impact)
- Prioritize clarity and maintainability (future-you will thank you)

---

*This document evolves with the project. Last updated: 2025-11-22*

*"We build systems that don't decay under pressure, communities that support experimentation without shame, and infrastructure that respects human autonomy."*

— The Rhodium Standard
