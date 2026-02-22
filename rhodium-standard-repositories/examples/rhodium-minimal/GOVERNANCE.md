# Governance

**rhodium-minimal - Project Governance Structure**

---

## 📋 Overview

This project follows the **Tri-Perimeter Contribution Framework (TPCF)** with **Perimeter 3 (Community Sandbox)** governance model.

---

## 🎯 Project Status

- **Status**: Example Project / Reference Implementation
- **TPCF Perimeter**: 3 (Community Sandbox)
- **Governance Model**: Lightweight, open contribution
- **Decision Making**: Maintainer consensus with community input

---

## 👥 Roles

### Maintainers

**Current Maintainers**:
- The Rhodium Standard Contributors

**Responsibilities**:
- Review and merge contributions
- Maintain code quality and RSR compliance
- Release management
- Community moderation
- Security response

**Authority**:
- Direct commit access
- Merge request approval
- Release creation
- Community moderation decisions

### Contributors

**Anyone can contribute!** This is a Perimeter 3 project.

**Responsibilities**:
- Follow Code of Conduct
- Write tests for new features
- Maintain SPDX headers
- Follow RSR coding standards

**Rights**:
- Submit merge requests
- Report issues
- Participate in discussions
- Propose new features

---

## 🔄 Decision Making

### Perimeter 3 Governance Model

As a **Community Sandbox** project, governance is lightweight:

1. **Technical Decisions**:
   - Small changes: Single maintainer approval
   - Medium changes: Two maintainer consensus
   - Large changes: Community discussion + maintainer consensus

2. **Non-Technical Decisions**:
   - Documentation: Open contribution with review
   - Community standards: Maintainer consensus
   - Code of Conduct enforcement: Maintainer majority vote

### Decision Process

```
Proposal → Discussion → Review → Decision → Implementation
```

1. **Proposal**: Issue or merge request
2. **Discussion**: Community feedback (min 48 hours for significant changes)
3. **Review**: Maintainer technical review
4. **Decision**: Maintainer consensus (majority vote if needed)
5. **Implementation**: Merge and deploy

### Voting (if needed)

- **Quorum**: 2/3 of active maintainers
- **Majority**: >50% for standard decisions
- **Supermajority**: 2/3 for governance changes
- **Veto**: Project lead can veto (with public explanation)

---

## 📊 Contribution Tiers

### Tier 1: Community Contributors

- **Access**: Fork and submit MRs
- **Scope**: Documentation, examples, tests, bug fixes
- **Process**: Standard MR review

### Tier 2: Regular Contributors

After 3+ quality contributions:
- **Recognition**: Listed in MAINTAINERS.md
- **Access**: Faster review cycle
- **Scope**: All Tier 1 + feature development

### Tier 3: Trusted Contributors

After 10+ quality contributions + demonstrated expertise:
- **Consideration**: May be invited to Tier 3
- **Access**: Triage permissions
- **Scope**: All Tier 2 + issue triage, limited merge rights

### Tier 4: Maintainers

By invitation only (rare):
- **Access**: Direct commit, full merge rights
- **Scope**: All aspects of project
- **Responsibility**: Long-term stewardship

---

## 🔐 Security Governance

### Vulnerability Reporting

See [SECURITY.md](SECURITY.md) for process.

### Security Response Team

- All current maintainers
- Response SLA: 24 hours acknowledgement
- Private disclosure until patch available

### Security Decisions

- **Critical vulnerabilities**: Emergency maintainer decision
- **High severity**: Rapid consensus (24-48 hours)
- **Medium/Low**: Standard process

---

## 🚀 Release Process

### Versioning

- **SemVer 2.0**: MAJOR.MINOR.PATCH
- **MAJOR**: Breaking changes
- **MINOR**: New features, backwards-compatible
- **PATCH**: Bug fixes, backwards-compatible

### Release Authority

- Any maintainer can cut patch releases
- Minor releases require 2 maintainer consensus
- Major releases require all maintainer consensus + community notice

### Release Checklist

1. Update CHANGELOG.md
2. Update version in Cargo.toml
3. Run `just validate` (100% pass required)
4. Create git tag
5. Build release artifacts
6. Publish release notes
7. Update documentation

---

## 📝 Amendments

### Changing This Document

1. **Proposal**: Issue or MR with rationale
2. **Discussion**: Minimum 7 days community feedback
3. **Vote**: 2/3 maintainer supermajority required
4. **Documentation**: Update CHANGELOG, notify community

### Amendment History

| Date | Version | Changes | Rationale |
|------|---------|---------|-----------|
| 2025-11-22 | 1.0.0 | Initial governance document | Establish TPCF Perimeter 3 governance |

---

## 🤝 Conflict Resolution

### Process

1. **Direct Communication**: Parties attempt to resolve directly
2. **Mediation**: Uninvolved maintainer mediates
3. **Escalation**: All maintainers review and vote
4. **Final Decision**: Majority maintainer vote is binding

### Appeals

- Appeals allowed within 7 days
- Requires new information or evidence of bias
- Different maintainer panel reviews
- Final decision is binding

---

## 🌐 Community Standards

### Code of Conduct

See [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

**Enforcement**:
- Warnings for first offense (minor)
- Temporary ban for repeated or serious offenses
- Permanent ban for severe violations

### Contributing Guidelines

See [CONTRIBUTING.md](CONTRIBUTING.md).

**Key Principles**:
- TPCF Perimeter 3 (open contribution)
- RSR compliance required
- SPDX headers mandatory
- Tests for new features

---

## 📊 Transparency

### Public Information

- All technical decisions documented in issues/MRs
- Maintainer meetings (if held) have public notes
- Votes are recorded with rationale
- Governance changes announced publicly

### Private Information

- Security vulnerabilities (until patched)
- Code of Conduct reports
- Personal contributor information

---

## 🔄 Succession Planning

### Maintainer Departure

- **Planned**: 30-day notice preferred
- **Emergency**: Best effort transition
- **Knowledge Transfer**: Documentation and handoff
- **Access Revocation**: Timely and complete

### Maintainer Addition

**Criteria**:
- Consistent quality contributions (10+ merged MRs)
- Deep understanding of RSR principles
- Community respect and trust
- Long-term commitment demonstrated

**Process**:
1. Existing maintainer nomination
2. Candidate accepts nomination
3. 7-day community feedback period
4. Maintainer vote (2/3 supermajority)
5. Onboarding and access provisioning

---

## 📞 Contact

### Governance Questions

- **Issues**: https://gitlab.com/hyperpolymath/rhodium-standard-repositories/-/issues
- **Email**: See `.well-known/security.txt`
- **Discussion**: GitLab project discussions

---

## 📚 References

- **TPCF Framework**: [CONTRIBUTING.md](CONTRIBUTING.md)
- **RSR Specification**: [../../CLAUDE.md](../../CLAUDE.md)
- **Code of Conduct**: [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md)
- **Security Policy**: [SECURITY.md](SECURITY.md)

---

*"Transparent governance, community empowerment, maintainer stewardship."*

— The Rhodium Standard, Governance Principles
