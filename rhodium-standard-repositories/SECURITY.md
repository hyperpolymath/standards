# SECURITY.md

<!--
SPDX-License-Identifier: MPL-2.0-or-later
SPDX-FileCopyrightText: 2025 The Rhodium Standard Contributors
-->

# Security Policy

## Supported Versions

We actively maintain security updates for the following versions:

| Version | Supported          | End of Support |
| ------- | ------------------ | -------------- |
| main    | :white_check_mark: | N/A (rolling)  |
| 1.x     | :white_check_mark: | 2026-12-31     |
| 0.x     | :x:                | 2025-06-30     |

## Reporting a Vulnerability

### Priority Response Times

| Severity | Acknowledgement | Initial Triage | Fix Target |
| -------- | --------------- | -------------- | ---------- |
| Critical | 4 hours         | 8 hours        | 24 hours   |
| High     | 12 hours        | 24 hours       | 72 hours   |
| Medium   | 24 hours        | 48 hours       | 1 week     |
| Low      | 48 hours        | 1 week         | 1 month    |

### How to Report

We use a multi-channel security reporting system:

#### 1. Private Security Issue (Preferred)

1. Go to https://gitlab.com/hyperpolymath/rhodium-standard-repositories/-/issues/new
2. Check "This issue is confidential"
3. Add label: `~security`
4. Provide details using the template below

#### 2. Encrypted Email

Send to: `security@rhodium-standard.org`

PGP Key:
```
-----BEGIN PGP PUBLIC KEY BLOCK-----
[Key will be published once project email is established]
-----END PGP PUBLIC KEY BLOCK-----
```

#### 3. Security.txt (RFC 9116)

Our canonical security contact is maintained in `.well-known/security.txt`:

```
Contact: mailto:security@rhodium-standard.org
Contact: https://gitlab.com/hyperpolymath/rhodium-standard-repositories/-/issues/new?issuable_template=security
Expires: 2026-12-31T23:59:59Z
Preferred-Languages: en, es, fr, de
Canonical: https://rhodium-standard.org/.well-known/security.txt
Encryption: https://keys.openpgp.org/search?q=security@rhodium-standard.org
Policy: https://gitlab.com/hyperpolymath/rhodium-standard-repositories/-/blob/main/SECURITY.md
Acknowledgments: https://gitlab.com/hyperpolymath/rhodium-standard-repositories/-/blob/main/SECURITY.md#hall-of-fame
```

### Report Template

Please include the following information:

```markdown
**Vulnerability Type**: [e.g., XSS, SQL Injection, Buffer Overflow, CRDT Conflict]

**Affected Component**: [e.g., rsr-audit.sh, Haskell registry, example code]

**Severity Assessment**: [Critical/High/Medium/Low]

**Description**:
[Clear description of the vulnerability]

**Steps to Reproduce**:
1. [First Step]
2. [Second Step]
3. [...]

**Expected Behavior**:
[What should happen]

**Actual Behavior**:
[What actually happens]

**Proof of Concept**:
[Code, screenshots, or reproduction repository]

**Impact**:
[Who is affected? What's the worst-case scenario?]

**Suggested Fix** (optional):
[If you have ideas for remediation]

**Discoverer**: [Your name/pseudonym for credit, or "Anonymous"]
```

## Security Architecture

The Rhodium Standard Repository framework employs defense-in-depth across 10+ security dimensions:

### 1. Type Safety

- **ReScript** (OCaml soundness) for frontend
- **Rust** for systems programming
- **Ada + SPARK** for safety-critical paths
- **Elixir** for fault-tolerant services
- **Haskell** for pure functional validation
- ❌ No TypeScript (unsound gradual typing)
- ❌ No Python (except SaltStack, temporary)
- ❌ No JavaScript (actively being eliminated)

### 2. Memory Safety

- Rust ownership model (compile-time guarantees)
- Ada SPARK proofs (mathematical verification)
- No garbage collection pauses (Rust/Ada preferred)
- WASM compilation targets for sandboxed execution

### 3. Data Security

- **CRDTs** (Conflict-free Replicated Data Types) for distributed state
- No distributed locking vulnerabilities
- No cache invalidation race conditions
- Offline-first by design (no network dependency attacks)

### 4. Process Security

- **Deno permissions model**: Explicit, granular, auditable
  - No file access by default
  - No network access by default
  - No environment variable access by default
- Podman rootless containers (no privileged daemon)
- **Software-Defined Perimeter (SDP)** for network access
- Zero Trust architecture

### 5. Platform Security

- Chainguard Wolfi base images (minimal attack surface)
- **RISC-V** consideration (open hardware, no backdoors)
- Supply chain auditing: **SPDX headers on every file**
- `just audit-license` command for automated compliance

### 6. Network Security

- **IPv6 native** (no IPv4 legacy cruft)
- **QUIC protocol** (HTTP/3, reduced latency, improved security)
- **DoQ** (DNS over QUIC) replacing DoH/DoT
- **oDNS** (Oblivious DNS) for privacy
- **DNSSEC** validation mandatory

### 7. Privacy & Data Minimization

- **Necessary processing only** (Ada philosophy)
- Cookie minimization (or none at all)
- No tracking scripts
- Privacy-respecting analytics (if any)
- GDPR/CCPA compliance by default

### 8. Fault Tolerance

- Elixir supervision trees (let it crash, restart cleanly)
- OTP patterns (battle-tested Erlang reliability)
- Circuit breakers for external dependencies
- Graceful degradation (offline mode, partial functionality)

### 9. Self-Healing

- CRDT conflict resolution (automatic, deterministic)
- Supervision tree restarts (automatic process recovery)
- Health checks and automatic remediation
- RVC automated cleanup (preventive maintenance)

### 10. Kernel Security

- Podman (no privileged daemon)
- cgroups v2 resource limits
- SELinux/AppArmor mandatory access control
- Seccomp syscall filtering

## Security Testing

We employ multiple layers of security validation:

### Automated Testing (CI/CD)

- **SPDX header validation**: `just audit-license`
- **Dependency scanning**: `cargo audit`, `mix audit`
- **Static analysis**: `cargo clippy`, `dialyzer`, `gnatcheck`
- **Secret scanning**: `gitleaks`, `trufflehog`
- **License compliance**: REUSE specification
- **Link validation**: `lychee` (prevents phishing/typosquatting)

### Manual Testing (Pre-Release)

- Security-focused code review (all MRs)
- Threat modeling sessions (quarterly)
- Penetration testing (annually, if applicable)
- Third-party security audit (for 1.0 release)

### Fuzzing (Where Applicable)

- `cargo fuzz` for Rust components
- Property-based testing (QuickCheck, PropEr)
- CRDT invariant testing

## Disclosure Policy

### Coordinated Disclosure

1. **Report received**: Acknowledgement within 24 hours
2. **Triage**: Severity assessment within 48 hours
3. **Fix development**: Timeline communicated to reporter
4. **Testing**: Fix verified by reporter (if willing)
5. **Release**: Security advisory published with fix
6. **Public disclosure**: 90 days after fix release (or sooner if agreed)

### Public Disclosure Timeline

- **Critical vulnerabilities**: Immediate disclosure after fix available
- **High severity**: 30-day embargo (coordinated with distributors)
- **Medium/Low**: 90-day embargo (standard disclosure timeline)

### Security Advisories

Published at:
- GitLab Security Advisories: https://gitlab.com/hyperpolymath/rhodium-standard-repositories/-/security/advisories
- Mailing list: `security-announce@rhodium-standard.org`
- RSS feed: https://rhodium-standard.org/security.rss

Format: [GHSA-XXXX-YYYY-ZZZZ](https://github.com/advisories) compatible

## Secure Development Practices

### Code Review Requirements

All changes must:
1. Pass automated security tests
2. Be reviewed by at least one maintainer
3. Include security impact assessment (for risky changes)
4. Update threat model (if architecture changes)

### Dependency Management

- **Pinned versions**: No floating ranges (`~`, `^`)
- **Vendoring**: Critical dependencies vendored
- **SBOM generation**: `just sbom-generate`
- **Quarterly updates**: Dependency refresh cycle
- **CVE monitoring**: Automated alerts for known vulnerabilities

### Secret Management

- **No secrets in repository**: Pre-commit hooks enforce
- **Environment variables**: `.env.example` templates only
- **Credential rotation**: 90-day maximum lifetime
- **Least privilege**: Minimal permissions for all services

## Security Champions

We maintain a distributed security team:

- **Security Lead**: Responsible for overall security posture
- **Component Owners**: Security for their components
- **Community Reviewers**: Security-focused code review
- **External Researchers**: Bug bounty participants (planned)

## Bug Bounty Program

**Status**: Planned for 1.0 release

Anticipated rewards:
- **Critical**: $500-$2000
- **High**: $250-$500
- **Medium**: $100-$250
- **Low**: $50-$100

Eligibility:
- Follow responsible disclosure
- No social engineering
- No DoS attacks on production
- No automated scanning without permission

## Hall of Fame

We thank the following security researchers:

<!-- Add researchers as vulnerabilities are responsibly disclosed -->

_Be the first! Report a security issue to get listed here._

## Compliance & Certifications

- **OWASP Top 10**: All categories addressed in RSR framework
- **CWE Top 25**: Mitigations documented
- **NIST Cybersecurity Framework**: Alignment documented (planned)
- **SOC 2 Type II**: Target for hosted services (planned)

## Security Resources

- [OWASP Secure Coding Practices](https://owasp.org/www-project-secure-coding-practices-quick-reference-guide/)
- [Rust Security Guidelines](https://anssi-fr.github.io/rust-guide/)
- [Erlang/OTP Security](https://www.erlang.org/doc/apps/ssl/ssl_protocol.html)
- [SPARK Proof Tutorials](https://learn.adacore.com/courses/intro-to-spark/index.html)

## Contact

- **Security Team**: security@rhodium-standard.org
- **PGP Key**: [Published once email established]
- **Matrix**: `#rhodium-standard-security:matrix.org` (planned)
- **GitLab**: Use confidential issues with `~security` label

---

**Last Updated**: 2025-11-28
**Next Review**: 2026-01-28
**Policy Version**: 1.0.0

_"Security is not a feature—it's a continuous commitment to protecting our community."_

— The Rhodium Standard Security Team
