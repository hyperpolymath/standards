# Tri-Perimeter Contribution Framework (TPCF)

## Palimpsest License Project - Perimeter Classification

The Palimpsest License project follows the **Tri-Perimeter Contribution Framework (TPCF)**, a graduated trust model for managing contributions based on security, cultural sensitivity, and governance requirements.

---

## Current Perimeter: **Perimeter 3 (Community Sandbox)**

**Rationale**: The Palimpsest License is an open licensing framework designed for broad community adoption. Legal texts benefit from diverse review, and documentation improvements help creators worldwide protect their work.

**Access Policy**: Open contribution with maintainer review and Stewardship Council oversight for substantive changes.

---

## TPCF Perimeter Definitions

### Perimeter 1: Trusted Core (Closed)
**Access**: 1-3 trusted maintainers with cryptographic signing keys
**Use Cases**: Critical security infrastructure, cryptographic keys, production secrets
**Palimpsest Example**: Not applicable (no secrets in a public license framework)

### Perimeter 2: Verified Contributors (Gated)
**Access**: Contributors with verified identity, signed CLA, background checks
**Use Cases**: Financial systems, medical data, high-assurance software
**Palimpsest Example**: Not currently used; could apply to:
- Stewardship Council-only license text changes
- Legal opinion documents requiring attorney verification
- Private vulnerability coordination before public disclosure

### Perimeter 3: Community Sandbox (Open with Review)
**Access**: Any GitHub/GitLab user can fork, PR, discuss
**Use Cases**: Open source projects, documentation, public standards
**Palimpsest Example**: **Current tier** for:
- Documentation improvements (guides, examples, FAQs)
- Translation contributions (Dutch, English, other languages)
- Tool development (validators, integration scripts)
- Metadata schemas and examples
- Outreach materials (presentations, letters, campaigns)

**Review Process**:
1. Community member submits PR
2. Maintainer reviews within 7 days
3. For significant changes: second maintainer approval required
4. For license text changes: Stewardship Council governance process
5. Automated checks: linting, link validation, RSR compliance

---

## Contribution Categories & Perimeter Assignment

| Category | Perimeter | Approval Required | Examples |
|----------|-----------|-------------------|----------|
| **Typo fixes, formatting** | P3 | 1 maintainer | README spelling, markdown formatting |
| **Documentation improvements** | P3 | 1 maintainer | User guide clarifications, new examples |
| **Code contributions (tools)** | P3 | 1-2 maintainers | Haskell validator enhancements, ReScript components |
| **Metadata schemas** | P3 | 1-2 maintainers | JSON-LD improvements, new Protocol Buffer fields |
| **Translation additions** | P3 | 1 maintainer + native speaker review | Dutch, French, Spanish versions |
| **License text clarifications** | P2/Council | Stewardship Council (4/7 vote) | Clause wording improvements |
| **New license clauses** | P2/Council | Stewardship Council (5/7 vote) + 30-day review | Adding new protections |
| **Major version changes** | P2/Council | Stewardship Council (6/7 vote) + 90-day review | v1.0 release |
| **Security vulnerabilities** | P2 (private) | Security team + Council | Pre-disclosure coordination |
| **Governance changes** | P2/Council | Stewardship Council (5/7 vote) | GOVERNANCE.md modifications |

---

## Access Control Mechanisms

### GitHub Repository Settings
- **Public repository**: Open read access
- **Write access**: Maintainers only (see MAINTAINERS.md)
- **Branch protection**:
  - `main` branch requires PR + 1 approval minimum
  - `main` branch requires passing CI checks
  - No direct commits to `main` (except automated tools)
- **Required reviews**:
  - Documentation: 1 maintainer
  - Code: 1-2 maintainers (depending on scope)
  - License text: Stewardship Council vote

### Contributor License Agreement (CLA)
**Not required** for Perimeter 3 contributions under these terms:
- Contributions licensed under same terms as project (CC BY-SA 4.0 for docs, MIT for code)
- Contributors retain copyright but grant irrevocable license to project
- No CLA signing process (reduces friction for casual contributors)

**May be required** for Perimeter 2 (Stewardship Council governance changes):
- Legal opinion documents
- Formal policy proposals
- Contractual relationships with organizations

### Identity Verification
- **P3 (Community Sandbox)**: GitHub account sufficient
- **P2 (Verified Contributors)**: GPG-signed commits, verified email, ORCID (for academics)
- **Council Members**: Full identity verification, background check (for legal/fiduciary roles)

---

## Perimeter Escalation & De-escalation

### When to Escalate to P2
If the project handles:
- Legally binding contracts or financial instruments
- Personal data requiring GDPR compliance
- Cryptographic key material or secrets
- High-value intellectual property requiring stricter control

**Current Status**: Not anticipated for Palimpsest License (open public framework)

### When to De-escalate to P3
**Already at P3** - maximum openness appropriate for public licensing framework

---

## Security Boundaries

### What P3 (Community Sandbox) Protects
✅ **Code review catches**:
- Malicious links in documentation
- Misleading legal guidance
- Inappropriate examples (offensive content)
- XSS in web components
- Broken metadata schemas

✅ **Transparency**:
- All changes visible in public git history
- Community can audit and challenge changes
- Forks and alternative implementations encouraged

### What P3 Cannot Protect
❌ **Advanced persistent threats**:
- Nation-state actors compromising maintainer accounts
- Supply chain attacks on dependencies (npm, Haskell packages)
- Social engineering of Stewardship Council

**Mitigation**:
- GPG-signed commits for maintainers (encouraged)
- Two-factor authentication required for all maintainers
- Security.txt for coordinated vulnerability disclosure
- Regular security audits of tooling dependencies

---

## Emotional Safety Considerations

TPCF for Palimpsest includes **emotional perimeter** alongside technical perimeter:

### Cultural Sensitivity Review
For contributions involving:
- Diaspora narratives or trauma examples
- Indigenous knowledge or sacred content
- Marginalized community representations

**Additional review required**:
- Cultural Heritage Advocate (Stewardship Council member) approval
- Community consultation for culturally specific content
- Trauma-informed language review

**Example**: A PR adding vignette about [specific cultural event] requires:
1. Standard P3 maintainer code review
2. Cultural Heritage Advocate review for sensitivity
3. Optional: Consultation with represented community

### Tone and Language Standards
See [CODE_OF_PRACTICE.md](CODE_OF_PRACTICE.md) for community norms:
- Respectful, inclusive language
- British English spelling (organisational standard)
- Trauma-informed content warnings where appropriate
- No ableist, racist, sexist, or marginalizing language

---

## Automated Perimeter Enforcement

### CI/CD Checks (GitHub Actions)
```yaml
# .github/workflows/rsr-compliance.yml
- Markdown linting (markdownlint)
- Link validation (broken link checker)
- SPDX license identifier verification
- RSR compliance self-check
- Spell check (British English dictionary)
- Cultural sensitivity scan (prohibited terms list)
```

### Pre-commit Hooks (Optional for Contributors)
```bash
# .git/hooks/pre-commit
- Run `just validate` (RSR compliance)
- Check for TODO/FIXME in license text
- Verify bilingual clause alignment (Dutch ↔ English)
```

---

## Perimeter Metrics & Monitoring

### Community Health Metrics
- **PR Response Time**: <7 days (target: 2 days)
- **Issue Response Time**: <7 days (target: 3 days)
- **Contributor Diversity**: Track geographic, cultural, professional backgrounds
- **Emotional Temperature**: Quarterly contributor satisfaction surveys

### Security Metrics
- **Dependency Vulnerabilities**: Weekly scan via `npm audit`, `cargo audit`
- **SAST (Static Analysis)**: ESLint, Prettier, Haskell linter
- **Secret Scanning**: GitHub secret scanning enabled
- **Outdated Dependencies**: Monthly dependency updates

### Governance Metrics
- **Council Vote Participation**: >80% of eligible voters
- **Community Proposal Rate**: Tracking proposals submitted vs. adopted
- **Stewardship Turnover**: Healthy rotation (target: 2-3 members/year)

---

## Exceptions & Edge Cases

### Emergency Security Fixes
- **Process**: Bypass normal review for active exploitation
- **Authority**: Security team + 1 Council member
- **Post-hoc review**: Within 48 hours of emergency commit
- **Transparency**: Public disclosure after fix deployed (90-day coordinated disclosure)

### Typo Fixes by New Contributors
- **Process**: Fast-track approval for trivial changes
- **Authority**: Any maintainer can merge
- **Example**: Spelling correction in README (no substantive change)

### Translations by Native Speakers
- **Process**: Maintainer review + native speaker verification
- **Authority**: Translation area maintainer + community native speaker
- **Example**: Adding French translation of User Guide

---

## TPCF Compliance Checklist

The Palimpsest License project maintains TPCF compliance through:

- [x] **Perimeter classification documented** (this file)
- [x] **Access control policies defined** (GitHub branch protection, review requirements)
- [x] **Contribution guidelines published** (CONTRIBUTING.md)
- [x] **Maintainer roster maintained** (MAINTAINERS.md)
- [x] **Security disclosure process** (SECURITY.md, security.txt)
- [x] **Code of Practice** (CODE_OF_PRACTICE.md)
- [x] **Governance model** (GOVERNANCE.md)
- [x] **CI/CD security checks** (.github/workflows/)
- [x] **Emergency procedures** (documented above)
- [x] **Emotional safety protocols** (trauma-informed review, cultural sensitivity)

---

## Further Reading

- **Rhodium Standard Repository (RSR)**: [Link to RSR documentation]
- **TPCF Original Paper**: [Link to academic paper on graduated trust]
- **Palimpsest Governance**: [GOVERNANCE.md](GOVERNANCE.md)
- **Contributing Guide**: [CONTRIBUTING.md](CONTRIBUTING.md)
- **Security Policy**: [SECURITY.md](SECURITY.md)

---

**Document Version**: 1.0
**Last Updated**: 2025-11-22
**Review Cycle**: Annually or when changing perimeter tier
**Next Review**: 2026-11-22
**Maintained by**: Palimpsest Stewardship Council
**License**: CC BY-SA 4.0
