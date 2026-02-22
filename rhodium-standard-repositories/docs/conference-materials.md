# RSR Conference Materials

**Rhodium Standard Repository (RSR) - Conference Talks & Abstracts**

This document provides ready-to-use abstracts, talk outlines, and submission materials for presenting RSR/CCCP at various conferences.

---

## 📋 Table of Contents

1. [Technical Conference Abstracts](#technical-conference-abstracts)
2. [Security Conference Abstracts](#security-conference-abstracts)
3. [Open Source Conference Abstracts](#open-source-conference-abstracts)
4. [Academic Conference Abstracts](#academic-conference-abstracts)
5. [Talk Outlines](#talk-outlines)
6. [Target Venues](#target-venues)
7. [Speaker Bio](#speaker-bio)
8. [Slides Preview](#slides-preview)

---

## Technical Conference Abstracts

### Abstract 1: "Post-JavaScript Liberation: Building Safer Systems with RSR"

**Target**: FOSDEM, RustConf, ElixirConf, Strange Loop
**Duration**: 30-45 minutes
**Level**: Intermediate

**Abstract**:

The JavaScript/npm ecosystem has normalized fragility: weekly breaking changes, dependency hell, and gradual typing that provides false security. The Rhodium Standard Repository (RSR) offers a radical alternative: type-safe, memory-safe, offline-first development using ReScript, Rust, Ada, Elixir, and Haskell.

In this talk, we'll explore:

1. **Why JavaScript Must Go**: The technical and political costs of npm dependency sprawl
2. **Type Safety Without Compromise**: How ReScript's OCaml foundation provides 10-100x faster compilation than TypeScript with sound types
3. **Memory Safety as Default**: Rust ownership vs Ada SPARK proofs vs Elixir OTP supervision
4. **Offline-First Architecture**: CRDTs eliminate cache invalidation and distributed locking
5. **The CCCP Framework**: Campaign for Cooler Coding and Programming - reducing emotional, computational, social, and political temperature
6. **Live Demo**: Building a Bronze-level RSR project from scratch with `rhodium-init`

**Key Takeaways**:
- Practical migration path from JavaScript to ReScript/Rust
- Understanding CRDTs for conflict-free distributed state
- How offline-first design creates political autonomy
- Tri-Perimeter Contribution Framework (TPCF) for graduated trust

**Repository**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories

---

### Abstract 2: "Distributed State Without Coordination: CRDTs and the RSR Stack"

**Target**: ElixirConf, Erlang/OTP Conference, PolyConf
**Duration**: 45-60 minutes
**Level**: Advanced

**Abstract**:

Traditional distributed systems require coordination: locks, consensus protocols, cache invalidation. The Rhodium Standard Repository (RSR) eliminates this complexity using Conflict-free Replicated Data Types (CRDTs), Elixir supervision trees, and offline-first architecture.

This talk presents:

1. **The Three Hard Problems**: Naming, cache invalidation, and off-by-one errors - and how CRDTs solve #2
2. **CRDT Fundamentals**: LWW-Registers, AWSet, ORSet, and when to use each
3. **CADRE Router**: ReScript + Deno + CRDTs replacing traditional HTTP servers
4. **Elixir OTP Integration**: Supervision trees + CRDT state = self-healing systems
5. **Offline-First Patterns**: SaltRover for repository sync, Deno KV for persistent storage
6. **Production War Stories**: Where CRDTs shine and where they struggle

**Code Examples**:
- Implementing AWSet (Add-Wins Set) in Elixir
- CRDT merge semantics for eventual consistency
- Handling CRDT conflicts deterministically

**Target Audience**: Backend engineers, distributed systems architects, Elixir/Erlang developers

**Repository**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories

---

### Abstract 3: "Formal Verification for Everyday Developers: Ada SPARK in Production"

**Target**: Ada-Europe, HILT (High-Integrity Language Technology), Safety-Critical Systems Conference
**Duration**: 30-45 minutes
**Level**: Intermediate to Advanced

**Abstract**:

Formal verification is no longer academic luxury - it's production necessity. The Rhodium Standard Repository (RSR) integrates Ada SPARK proofs with Rust FFI, bringing mathematical correctness to safety-critical components.

This talk covers:

1. **Why Ada in 2025**: Type safety beyond Rust, SPARK proofs, deterministic execution
2. **SPARK Proof Obligations**: Pre/post conditions, loop invariants, absence of runtime errors
3. **Rust ↔ Ada FFI**: Combining Rust's ergonomics with Ada's verification
4. **WASM Compilation**: Ada + SPARK to WebAssembly for sandboxed execution
5. **Real-World Example**: Formally verified cryptographic primitives (SHAKE256)
6. **Tool Integration**: gnatprove, SPARK Pro, automated CI/CD verification

**Who Should Attend**:
- Developers in aerospace, automotive, medical devices
- Engineers seeking provable correctness
- Anyone frustrated by runtime errors that could have been caught at compile-time

**Repository**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories

---

## Security Conference Abstracts

### Abstract 4: "10+ Dimensions of Security: The RSR Security Architecture"

**Target**: DEF CON, Black Hat, ShmooCon, BSides
**Duration**: 45-60 minutes
**Level**: Intermediate

**Abstract**:

Most security discussions focus on 2-3 dimensions: authentication, encryption, network security. The Rhodium Standard Repository (RSR) defines 10+ security dimensions for defense-in-depth:

1. **Type Safety**: Compile-time verification (Rust, ReScript, Ada)
2. **Memory Safety**: Ownership models, SPARK proofs, no unsafe code
3. **Data Security**: CRDTs, no cache invalidation complexity
4. **Process Security**: Deno permissions, Podman rootless, SDP (Software-Defined Perimeter)
5. **Platform Security**: Chainguard Wolfi, RISC-V consideration, SPDX audit
6. **Network Security**: IPv6, QUIC (HTTP/3), DoQ (DNS over QUIC), oDNS, DNSSEC, security headers
7. **Privacy & Data Minimization**: Cookie-free, no tracking, GDPR by design
8. **Fault Tolerance**: Elixir supervision, OTP "let it crash", circuit breakers
9. **Self-Healing**: CRDT conflict resolution, supervision restarts, RVC cleanup
10. **Kernel Security**: Podman, cgroups v2, SELinux/AppArmor, Seccomp
11. **Supply Chain Security**: SPDX headers, dependency vendoring, pinned versions, SBOM

**Live Demos**:
- Exploiting TypeScript's unsound types (vs ReScript's sound types)
- CRDT conflict resolution (no coordination needed)
- Deno permission model (explicit, granular, auditable)
- Chainguard Wolfi attack surface reduction

**Target Audience**: Security engineers, DevSecOps, penetration testers, security researchers

**Repository**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories

---

### Abstract 5: "Software-Defined Perimeter & Zero Trust: Practical Implementation with RSR"

**Target**: RSA Conference, Gartner Security Summit, SANS conferences
**Duration**: 30-45 minutes
**Level**: Intermediate

**Abstract**:

Traditional perimeter security is dead. The Rhodium Standard Repository (RSR) implements Software-Defined Perimeter (SDP) and Zero Trust architecture using Deno permissions, Podman rootless containers, and graduated trust (TPCF).

Key Topics:

1. **Zero Trust Principles**: Verify explicitly, least privilege, assume breach
2. **Software-Defined Perimeter**: Network-level isolation, dynamic access control
3. **Deno Permissions Model**: Explicit `--allow-*` flags, no ambient authority
4. **Podman Rootless**: No privileged daemon, user namespaces, cgroups v2
5. **TPCF Security Model**: Tri-Perimeter Contribution Framework as defense-in-depth
   - Perimeter 1 (Core): Maintainers-only, critical infrastructure
   - Perimeter 2 (Expert): Trusted contributors, reviewed extensions
   - Perimeter 3 (Community): Open sandbox, safe experimentation
6. **Compliance Integration**: How SDP maps to NIST, ISO 27001, SOC 2

**Practical Outcomes**:
- Implement SDP without enterprise products
- Zero Trust for open-source projects
- Security-by-design architectural patterns

**Target Audience**: CISOs, security architects, compliance officers, enterprise security teams

**Repository**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories

---

## Open Source Conference Abstracts

### Abstract 6: "Emotional Safety in Open Source: The CCCP Manifesto"

**Target**: OSCON, All Things Open, FOSDEM (Community Track), SCALE
**Duration**: 30-45 minutes
**Level**: All levels

**Abstract**:

Open source burnout is an epidemic. The Campaign for Cooler Coding and Programming (CCCP) reduces temperature across four dimensions: emotional, computational, social, and political.

This talk explores:

1. **Emotional Temperature**: Reversibility, no shame in mistakes, safe experimentation
   - Git + RVC (Robot Vacuum Cleaner) = friction-free iteration
   - "Let it crash" philosophy from Erlang/OTP
   - Formal verification as mutual aid (correctness is care)

2. **Computational Temperature**: Efficient systems, no wasteful cycles
   - Rust/Ada vs JavaScript/Python overhead
   - No proof-of-work blockchain where unnecessary
   - Compile-time verification vs runtime checks

3. **Social Temperature**: Warm community, cool-headed decisions
   - TPCF (Tri-Perimeter Contribution Framework): graduated trust
   - Code of Conduct: inclusive by design
   - Conflict resolution: evidence-based, not reactionary

4. **Political Temperature**: Autonomy, anti-monopoly, offline-first
   - Resist vendor lock-in (GitLab not GitHub, Podman not Docker)
   - Offline-first: intermittent connectivity never blocks work
   - SaltRover: buffer against centralized infrastructure

**Personal Stories**:
- How reversibility reduces anxiety
- Community over ego: TPCF success stories
- Political autonomy through offline-first design

**Target Audience**: Maintainers, contributors, community managers, anyone experiencing burnout

**Repository**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories

---

### Abstract 7: "Tri-Perimeter Contribution Framework: Graduated Trust Without Gatekeeping"

**Target**: FOSDEM (Community Dev Room), CommunityBridge Summit, Sustain OSS
**Duration**: 20-30 minutes (Lightning talk or short session)
**Level**: All levels

**Abstract**:

Open source faces a dilemma: total openness invites spam and malicious contributions, but gatekeeping alienates newcomers. The Tri-Perimeter Contribution Framework (TPCF) solves this with graduated trust.

**Three Perimeters**:

1. **Perimeter 3 (Community Sandbox)**: Open to ALL
   - Languages: Shell, Markdown, AsciiDoc, JSON
   - Scope: Documentation, examples, tests
   - Process: Fork → PR, automated validation
   - Trust: None required, safe experimentation

2. **Perimeter 2 (Expert Extensions)**: Trusted Contributors
   - Languages: Rust, Nickel, Bash, controlled Python
   - Scope: Protocol extensions, build system
   - Process: Apply via issue → Review → Approval
   - Trust: Earned through Perimeter 3 contributions

3. **Perimeter 1 (Core Systems)**: Maintainers Only
   - Languages: Rust, Nickel, Bash, C++
   - Scope: Critical infrastructure, security-sensitive
   - Process: Direct commits by maintainers
   - Trust: Long-term commitment demonstrated

**Benefits**:
- Newcomers can contribute immediately (Perimeter 3)
- Expertise recognized and rewarded (Perimeter 2)
- Critical systems protected (Perimeter 1)
- Clear progression path (3 → 2 → 1)

**Case Study**: rhodium-minimal example (Perimeter 3 project)

**Target Audience**: FOSS maintainers, community managers, governance researchers

**Repository**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories

---

## Academic Conference Abstracts

### Abstract 8: "Rhodium Standard Repository: A Framework for Politically Autonomous Software Development"

**Target**: PLDI (Programming Language Design), OOPSLA, ESEC/FSE, ICSE
**Duration**: 20 minutes (Research paper presentation)
**Level**: Academic/Research

**Abstract**:

We present the Rhodium Standard Repository (RSR), a comprehensive framework addressing software engineering's socio-technical challenges through architectural constraints. RSR integrates formal methods, distributed systems theory, and community governance into a cohesive standard achieving 11-category compliance.

**Research Contributions**:

1. **Tri-Perimeter Contribution Framework (TPCF)**: Formal model for graduated trust in open-source collaboration, balancing openness with architectural integrity

2. **10+ Dimensional Security Architecture**: Taxonomy spanning type safety, memory safety, data security, process isolation, platform security, network security, privacy, fault tolerance, self-healing, kernel security, and supply chain security

3. **Offline-First Formal Semantics**: Mathematical model for CRDT-based state management eliminating coordination requirements (formalized using operational semantics)

4. **Emotionally Safe Development Metrics**: Quantifiable measures of "emotional temperature" through reversibility guarantees, error recovery mechanisms, and blame-free architecture

5. **Multi-Language Verification Strategy**: Integration patterns for Rust, Ada SPARK, ReScript, Elixir, and Haskell achieving compositional correctness

**Evaluation**:

- **rhodium-minimal**: Bronze-level (75-89%) reference implementation (100 LOC Rust)
- **Compliance Automation**: 150+ checkpoints across 11 categories
- **Migration Case Studies**: JavaScript → ReScript, Python → Rust/Elixir/Nickel
- **TPCF Validation**: Security analysis of three-perimeter trust model

**Related Work**: Comparison with conventional DevSecOps, ISO 27001, NIST frameworks; formal methods (SPARK, Coq, TLA+); distributed systems (CRDTs, eventual consistency); social/technical systems

**Availability**: Open-source implementation at https://gitlab.com/hyperpolymath/rhodium-standard-repositories

**Keywords**: Software engineering, formal verification, distributed systems, CRDTs, community governance, security architecture, offline-first systems

---

### Abstract 9: "CRDTs for Everyday Developers: Conflict-Free State Without Coordination"

**Target**: ICFP (Functional Programming), ECOOP, SPLASH
**Duration**: 20-30 minutes
**Level**: Academic/Practitioner

**Abstract**:

Conflict-free Replicated Data Types (CRDTs) solve distributed consistency without coordination, yet remain underutilized due to perceived complexity. We present practical CRDT patterns from the Rhodium Standard Repository (RSR), demonstrating real-world applicability.

**Theoretical Foundation**:

- **Strong Eventual Consistency (SEC)**: Mathematical guarantees for convergence
- **Semilattice Properties**: Commutativity, associativity, idempotence
- **CRDT Taxonomy**: State-based (CvRDT) vs Operation-based (CmRDT)

**Practical Implementations**:

1. **CADRE Router**: ReScript + Deno + CRDTs replacing traditional HTTP servers
2. **SaltRover**: Offline repository management with eventual sync
3. **Deno KV Integration**: Persistent CRDT storage patterns

**Case Studies**:

- **AWSet (Add-Wins Set)**: Collaborative editing without central authority
- **LWW-Register (Last-Writer-Wins)**: Configuration management
- **ORSet (Observed-Remove Set)**: Distributed task lists

**Performance Analysis**:

- Merge complexity: O(n) for state-based CRDTs
- Network overhead: Comparison with consensus protocols (Raft, Paxos)
- Storage requirements: CRDT metadata vs operational logs

**Contributions**:

1. Elixir OTP + CRDT design patterns
2. Type-safe CRDT implementations in ReScript/Rust
3. Integration with supervision trees for self-healing

**Evaluation**: Production deployments, performance benchmarks, developer experience surveys

**Repository**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories

**Keywords**: CRDTs, eventual consistency, distributed systems, offline-first, functional programming

---

## Talk Outlines

### Outline 1: "Post-JavaScript Liberation" (45 minutes)

**Introduction (5 min)**
- The npm ecosystem crisis: 30M packages, weekly breaking changes
- False security of gradual typing (TypeScript unsoundness)
- Political implications of vendor lock-in

**The RSR Alternative (10 min)**
- Rhodium as catalytic converter: transforming toxic outputs
- 11 compliance categories overview
- CCCP framework: 4-dimensional "Cooler"

**Type Safety Deep Dive (10 min)**
- ReScript's OCaml foundation: sound types, 10-100x faster compilation
- Rust ownership model: memory safety without garbage collection
- Ada SPARK: formal verification for safety-critical paths
- Live comparison: TypeScript vs ReScript type errors

**Offline-First Architecture (10 min)**
- CRDTs eliminate coordination
- SaltRover for repository sync
- Political autonomy: work continues without connectivity
- Live demo: Offline CRDT merge

**TPCF & Community (5 min)**
- Tri-Perimeter Contribution Framework
- Graduated trust without gatekeeping
- Bronze→Silver→Gold compliance progression

**Q&A (5 min)**

---

### Outline 2: "10+ Dimensions of Security" (60 minutes)

**Introduction (5 min)**
- Traditional security: 2-3 dimensions
- RSR: 10+ dimensions for defense-in-depth

**Dimensions 1-3: Safety (15 min)**
- Type safety: Rust/ReScript/Ada compile-time verification
- Memory safety: Ownership, SPARK proofs, no unsafe code
- Data security: CRDTs, no cache invalidation
- Live exploit: TypeScript unsoundness

**Dimensions 4-6: Process & Platform (15 min)**
- Process security: Deno permissions, Podman rootless, SDP
- Platform security: Chainguard Wolfi, RISC-V, SPDX audit
- Network security: IPv6, QUIC, DoQ, oDNS, DNSSEC
- Live demo: Deno permission model

**Dimensions 7-9: Resilience (10 min)**
- Privacy: Cookie-free, no tracking, GDPR by design
- Fault tolerance: Elixir supervision, circuit breakers
- Self-healing: CRDT conflict resolution, automatic restarts

**Dimensions 10-11: Infrastructure (10 min)**
- Kernel security: cgroups v2, SELinux, Seccomp
- Supply chain: SPDX, vendoring, SBOM generation

**Compliance Automation (5 min)**
- 150+ automated checkpoints
- `just validate` command demo

**Q&A (5 min)**

---

### Outline 3: "Emotional Safety in Open Source" (30 minutes)

**Introduction (3 min)**
- Open source burnout epidemic
- CCCP: Reducing temperature, not creating cold systems

**Emotional Temperature (8 min)**
- Reversibility: Git + RVC, no destructive defaults
- "Let it crash": Erlang/OTP philosophy
- Formal verification as mutual aid
- Personal story: How reversibility reduced anxiety

**Social Temperature (8 min)**
- TPCF: Graduated trust model
- Code of Conduct: Inclusive by design
- Conflict resolution: Evidence-based decisions
- Success story: TPCF in action

**Political Temperature (8 min)**
- Offline-first as autonomy
- Vendor lock-in resistance
- SaltRover: Buffer against centralized infrastructure
- Community over corporate control

**Actionable Takeaways (3 min)**
- Implement reversibility in your projects
- Consider TPCF for your community
- Prioritize offline-first where possible

**Q&A (open)**

---

## Target Venues

### Technical Conferences (2025-2026)

| Conference | Location | Dates (2025) | Submission Deadline | Focus Tracks |
|-----------|----------|--------------|---------------------|--------------|
| **FOSDEM** | Brussels, Belgium | Feb 1-2 | Dec 15, 2024 | Community, Rust, Ada, Security |
| **RustConf** | Online/Hybrid | TBA | ~May 2025 | Rust, Systems Programming |
| **ElixirConf** | USA | Aug | ~Mar 2025 | Elixir, OTP, Distributed Systems |
| **Strange Loop** | St. Louis, USA | Sep | ~Apr 2025 | Languages, Architecture |
| **Ada-Europe** | Europe | Jun | ~Jan 2025 | Ada, Safety-Critical Systems |
| **ICFP** | Various | Sep | ~Mar 2025 | Functional Programming |
| **PolyConf** | Online | TBA | TBA | Polyglot Programming |

### Security Conferences

| Conference | Location | Dates (2025) | Submission Deadline | Focus Tracks |
|-----------|----------|--------------|---------------------|--------------|
| **DEF CON** | Las Vegas, USA | Aug | ~Apr 2025 | All security topics |
| **Black Hat USA** | Las Vegas, USA | Aug | ~Feb 2025 | Enterprise security |
| **ShmooCon** | Washington DC, USA | Jan (2026) | ~Oct 2025 | Hacker community |
| **BSides (various)** | Global | Year-round | Rolling | Community-driven security |
| **RSA Conference** | San Francisco, USA | Apr | ~Sep 2024 | Enterprise security |

### Open Source Conferences

| Conference | Location | Dates (2025) | Submission Deadline | Focus Tracks |
|-----------|----------|--------------|---------------------|--------------|
| **OSCON** | TBA | TBA | TBA | Open source strategy |
| **All Things Open** | Raleigh, USA | Oct | ~Jun 2025 | FOSS community |
| **SCALE** | Pasadena, USA | Mar | ~Nov 2024 | Linux, FOSS |
| **Sustain OSS** | Online/Hybrid | TBA | TBA | Sustainability |
| **CommunityBridge Summit** | Online | TBA | TBA | Community management |

### Academic Conferences

| Conference | Location | Dates (2025) | Submission Deadline | Focus Areas |
|-----------|----------|--------------|---------------------|-------------|
| **PLDI** | TBA | Jun | ~Nov 2024 | Programming languages |
| **OOPSLA** | TBA | Oct | ~Apr 2025 | Object-oriented systems |
| **ICSE** | Ottawa, Canada | Apr | ~Aug 2024 | Software engineering |
| **ESEC/FSE** | TBA | Sep | ~Mar 2025 | Software engineering |
| **ECOOP** | TBA | Jun | ~Jan 2025 | Object-oriented programming |

---

## Speaker Bio

### Short Bio (100 words)

Jonathan D.A. Jewell (hyperpolymath) is the creator of the Rhodium Standard Repository (RSR) and the Campaign for Cooler Coding and Programming (CCCP). With expertise spanning Rust, Ada, Elixir, ReScript, and Haskell, Jonathan advocates for type-safe, memory-safe, offline-first software development. Their work focuses on reducing emotional, computational, social, and political "temperature" in software engineering through formal verification, distributed systems (CRDTs), and community governance (TPCF). Jonathan contributes to open-source projects emphasizing security, accessibility, and political autonomy.

### Long Bio (250 words)

Jonathan D.A. Jewell, known as hyperpolymath, is a polyglot software engineer and advocate for emotionally safe, technically excellent, politically autonomous software development. As the architect of the Rhodium Standard Repository (RSR) and author of the Campaign for Cooler Coding and Programming (CCCP) manifesto, Jonathan challenges industry norms around JavaScript dependency culture, unsound type systems, and centralized infrastructure.

Jonathan's technical work spans multiple paradigms: Rust for systems programming and memory safety, Ada with SPARK for formal verification, Elixir/OTP for fault-tolerant distributed systems, ReScript for type-safe web development, and Haskell for pure functional validation. This polyglot approach informs RSR's "iSOS" (Integrated Stack of Stacks) architecture, demonstrating that language diversity is resistance against monoculture.

The CCCP framework addresses software engineering holistically: reducing emotional temperature through reversibility and safe experimentation, computational temperature through efficient type systems, social temperature through the Tri-Perimeter Contribution Framework (TPCF), and political temperature through offline-first design and vendor lock-in resistance.

Jonathan's contributions to open source emphasize accessibility (WCAG 2.1 AA compliance), supply chain security (SPDX headers, SBOM generation), and ethical AI training (Palimpsest License). They advocate for GitLab over GitHub, Podman over Docker, and CRDTs over distributed locking as political and technical choices.

When not writing code or manifestos, Jonathan explores intersections of software architecture, community governance, and formal methods, always asking: "How can we build systems that don't decay under pressure?"

---

## Slides Preview

### Slide Deck Structure: "Post-JavaScript Liberation" (45 min, ~25 slides)

**Title Slide**
- "Post-JavaScript Liberation: Building Safer Systems with RSR"
- Jonathan D.A. Jewell (hyperpolymath)
- FOSDEM 2025

**Slide 1-2: The Problem**
- npm ecosystem crisis (30M packages)
- TypeScript's unsound gradual typing
- Weekly breaking changes, dependency hell

**Slide 3-5: The RSR Solution**
- Rhodium as catalytic converter
- 11 compliance categories
- Bronze (75%) → Silver (90%) → Gold (100%)

**Slide 6-8: CCCP Framework**
- 4-dimensional "Cooler"
  - Emotional: Reversibility, no shame
  - Computational: Efficiency
  - Social: TPCF graduated trust
  - Political: Offline-first autonomy

**Slide 9-12: Type Safety**
- ReScript: Sound types, OCaml foundation
- Rust: Ownership model
- Ada: SPARK formal verification
- Live comparison demo

**Slide 13-16: Offline-First Architecture**
- CRDTs: Conflict-free state
- SaltRover: Repository sync
- No distributed locking
- Political autonomy

**Slide 17-19: Security Dimensions**
- 10+ dimensions overview
- Deno permissions model
- Chainguard Wolfi, IPv6, QUIC

**Slide 20-22: TPCF**
- Perimeter 3: Community Sandbox
- Perimeter 2: Expert Extensions
- Perimeter 1: Core Systems
- Graduated trust diagram

**Slide 23: rhodium-minimal Demo**
- Live walkthrough
- Bronze compliance (75-89%)
- `just validate` command

**Slide 24: Call to Action**
- Try rhodium-minimal
- Join CCCP movement
- Contribute to RSR

**Slide 25: Q&A**
- Links, contact, resources

---

## Submission Materials Checklist

### Required for Most CFPs

- [ ] **Title** (catchy, descriptive)
- [ ] **Abstract** (150-300 words)
- [ ] **Description** (500-1000 words, detailed)
- [ ] **Speaker bio** (100-250 words)
- [ ] **Speaker photo** (headshot, 500x500px minimum)
- [ ] **Session level** (Beginner/Intermediate/Advanced)
- [ ] **Target audience** (Who should attend)
- [ ] **Key takeaways** (3-5 bullet points)
- [ ] **Talk format** (Presentation/Workshop/Panel)
- [ ] **A/V requirements** (Projector, mic, internet)
- [ ] **Previous talks** (Links to videos, if available)

### Optional but Helpful

- [ ] **Slide deck preview** (First 5 slides)
- [ ] **Code repository link** (GitHub/GitLab)
- [ ] **Social media handles** (Twitter, Mastodon, LinkedIn)
- [ ] **Testimonials** (From previous attendees)
- [ ] **Video trailer** (1-2 minute teaser)

---

## Contact & Links

- **GitLab Repository**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories
- **CCCP Manifesto**: [CCCP-MANIFESTO.md](../CCCP-MANIFESTO.md)
- **Compliance Checklist**: [COMPLIANCE_CHECKLIST.md](../COMPLIANCE_CHECKLIST.md)
- **Email**: [See .well-known/security.txt for contact]
- **Social**: @hyperpolymath (Mastodon, preferred)

---

*"From fragility to resilience, from isolation to community, from vendor lock-in to autonomy."*

— The Rhodium Standard, CCCP Manifesto
