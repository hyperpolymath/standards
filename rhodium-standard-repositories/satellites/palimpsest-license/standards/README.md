# CWCF Standards Documentation

This directory contains comprehensive standards documentation for the **Creative Works Consent Framework (CWCF)**, a machine-readable protocol for expressing creator consent for AI and automated systems.

## Directory Structure

```
standards/
├── README.md                           # This file
├── ROADMAP.md                          # Standards track roadmap (IETF/W3C)
├── irtf-drafts/                       # IRTF/IETF Internet-Drafts
│   └── draft-creative-works-consent-00.md
├── w3c/                               # W3C Community Group materials
│   ├── community-group-charter.md
│   └── community-group-proposal.md
└── implementation/                     # Supporting documentation
    ├── implementation-guide.md
    ├── use-cases-requirements.md
    ├── threat-model.md
    └── comparison-existing-approaches.md
```

## Quick Start

### For Standards Body Participants

1. **IETF participants**: Start with [`irtf-drafts/draft-creative-works-consent-00.md`](irtf-drafts/draft-creative-works-consent-00.md)
2. **W3C participants**: Start with [`w3c/community-group-proposal.md`](w3c/community-group-proposal.md)
3. **Both**: Review the [`ROADMAP.md`](ROADMAP.md) for parallel engagement strategy

### For Implementers

1. **Implementation Guide**: [`implementation/implementation-guide.md`](implementation/implementation-guide.md)
2. **Use Cases**: [`implementation/use-cases-requirements.md`](implementation/use-cases-requirements.md)
3. **Security**: [`implementation/threat-model.md`](implementation/threat-model.md)

### For Researchers

1. **Problem Analysis**: [`implementation/comparison-existing-approaches.md`](implementation/comparison-existing-approaches.md)
2. **Requirements**: [`implementation/use-cases-requirements.md`](implementation/use-cases-requirements.md)
3. **Threat Model**: [`implementation/threat-model.md`](implementation/threat-model.md)

## Document Summaries

### ROADMAP.md

**Path to Standardisation**

Comprehensive 3-5 year roadmap from Palimpsest License project to internationally-recognised web standards:

* Parallel IETF (HTTP protocol) and W3C (semantic web) engagement
* 5 phases: Foundation → Community Building → Implementation → Standardisation → Adoption
* Resource requirements, funding strategies, risk mitigation
* Success metrics and long-term vision

**Target Audience**: Standards body participants, project funders, strategic partners

---

### irtf-drafts/draft-creative-works-consent-00.md

**IRTF Internet-Draft (RFC Format)**

Formal specification for machine-readable consent framework:

* **Abstract**: Problem statement, consent declaration protocol, AI system types
* **Technical Specification**: JSON-LD, Protocol Buffers, RDF formats
* **Discovery**: Well-known URIs, HTTP headers, HTML metadata
* **Security**: Cryptographic signatures, tamper resistance, privacy protections
* **IANA**: Well-known URI, link relation, HTTP header registrations

**Format**: RFC-compliant (xml2rfc ready)
**Status**: Draft for IRTF submission
**Target Audience**: IETF participants, protocol implementers

---

### w3c/community-group-charter.md

**W3C Community Group Charter**

Formal charter for Creative Works Consent Framework Community Group:

* **Mission**: Develop machine-readable consent standards for creative works in AI context
* **Scope**: JSON-LD vocabulary, HTTP integration, lineage, governance, cultural heritage
* **Deliverables**: Core specification, supporting specs, implementation guide, test suite
* **Governance**: Co-chairs (creator + technical), consensus decision-making
* **Timeline**: 24 months to Community Group Final Report
* **Success Criteria**: 100+ websites, 10+ AI systems, 2+ interoperable implementations

**Status**: Ready for W3C submission
**Target Audience**: W3C Community Group participants, potential supporters

---

### w3c/community-group-proposal.md

**Accessible Proposal Summary**

Executive summary for W3C Community Group proposal:

* **Problem**: Unauthorised AI training, inadequate existing mechanisms
* **Solution**: Machine-readable consent with granular AI type permissions
* **Why W3C**: Web-native, JSON-LD/RDF expertise, global reach
* **Deliverables**: Specifications, tools, implementations
* **Impact**: Protect millions of creators, enable responsible AI

**Format**: Accessible prose (non-technical stakeholders)
**Target Audience**: W3C members, potential supporters, media

---

### implementation/implementation-guide.md

**Comprehensive Implementation Guide**

Practical guidance for all stakeholders (150+ pages):

* **For Creators**: 5-minute setup, consent options, hosting, CMS integration
* **For Developers**: Server-side (Node.js, Python, Nginx), client-side JavaScript, validation
* **For AI Operators**: Bulk verification, lineage generation, compliance reporting
* **For Platforms**: CDN integration, hosting platform configuration
* **Advanced Topics**: DAO governance, cryptographic signatures, quantum-proof lineage

**Includes**: Code examples, integration guides, troubleshooting, tooling
**Target Audience**: Creators, developers, AI companies, platform providers

---

### implementation/use-cases-requirements.md

**Use Cases and Requirements**

Foundation for specification design:

* **Personas**: 6 stakeholder types (individual creators, collectives, AI companies, etc.)
* **Use Cases**: 10 detailed scenarios with step-by-step workflows
* **Functional Requirements**: 50+ requirements derived from use cases
* **Non-Functional Requirements**: Performance, security, privacy, accessibility (40+)
* **Priority Matrix**: P0 (MVP), P1 (important), P2 (future)
* **Acceptance Criteria**: Success metrics for MVP and long-term

**Target Audience**: Specification authors, implementers, product managers

---

### implementation/threat-model.md

**Security Threat Model Analysis**

Comprehensive security analysis using STRIDE methodology:

* **Threat Categories**: Tampering, spoofing, information disclosure, DoS
* **Detailed Analysis**: 10 major threats with attack vectors and mitigations
* **Attack Scenarios**: High-profile creator, cultural heritage, DAO governance
* **Mitigations**: Technical (signatures, HTTPS), legal (enforcement), operational
* **Residual Risks**: Accepted and mitigated risks
* **Recommendations**: For creators, AI companies, spec authors, platforms

**Target Audience**: Security researchers, implementers, risk assessors

---

### implementation/comparison-existing-approaches.md

**Comparison with Existing Standards**

How CWCF relates to and extends existing mechanisms:

* **robots.txt**: Binary allow/deny → CWCF adds AI granularity
* **Creative Commons**: Human-readable → CWCF adds machine layer
* **TDM Reservation**: Limited scope → CWCF broader and conditional
* **Platform TOS**: Platform-specific → CWCF creator-controlled
* **W3C ODRL**: General rights → CWCF AI-specific extensions
* **Blockchain**: Complex/expensive → CWCF hybrid (optional attestation)

**Includes**: Detailed comparison matrix, integration strategies, migration paths
**Target Audience**: Standards experts, stakeholders evaluating CWCF vs. alternatives

---

## Key Features

### Technical Highlights

* **Machine-Readable**: JSON-LD, Protocol Buffers, RDF formats
* **AI-Specific**: Granular consent for LLM, vision, autonomous, agentic, neural interface, quantum AI
* **Conditional Consent**: "Allow if attribution provided" or "Allow if revenue shared"
* **Lineage Tracking**: Provenance metadata for AI-generated outputs
* **Governance Support**: Individual, collective, DAO, cultural stewardship
* **Privacy-Preserving**: GDPR compliance, pseudonymous creators, sensitive content protection
* **Cryptographically Verifiable**: Signatures, blockchain attestations (optional)
* **Decentralised**: No central authority, creator-controlled

### Stakeholder Benefits

**Creators**:
* Granular control over AI use
* Attribution in AI outputs
* Revenue opportunities (conditional consent)
* Cultural heritage protection

**AI Companies**:
* Legal clarity and risk reduction
* Ethical AI compliance
* Standardised protocol (vs. fragmentation)
* Audit and transparency mechanisms

**Platforms**:
* Competitive advantage ("We protect creators")
* Standards compliance (W3C/IETF)
* Reduced liability
* Creator retention

**Civil Society**:
* Creator rights protection
* AI ethics enforcement
* Cultural preservation
* Democratic governance

## Timeline

| Phase | Timeline | Key Milestones |
|-------|----------|----------------|
| **Phase 1: Foundation** | Months 1-6 (Q1-Q2 2026) | IRTF publication, W3C CG launch, reference implementations |
| **Phase 2: Community** | Months 7-12 (Q3-Q4 2026) | CG specs, 100+ participants, 5+ CMS integrations |
| **Phase 3: Implementation** | Months 13-24 (2027) | 1,000+ websites, 10+ AI systems, CG Final Report |
| **Phase 4: Standardisation** | Months 25-36 (2028) | W3C CR, IETF RFC, IANA registrations, 10,000+ sites |
| **Phase 5: Adoption** | Months 37-48+ (2029+) | W3C Recommendation, mainstream adoption, v2.0 planning |

## Getting Involved

### Standards Bodies

* **IETF**: Submit to IRTF, propose WG creation, engage with httpapi/dispatch
* **W3C**: Join Community Group, participate in specifications, advocate for WG

### Implementation

* **Creators**: Implement consent declarations, provide feedback
* **Developers**: Build libraries, CMS plugins, validation tools
* **AI Companies**: Pilot consent verification, publish compliance reports
* **Platforms**: Integrate CWCF support, offer creator tools

### Community

* **Mailing List**: public-cwcf@w3.org (once CG established)
* **GitHub**: https://github.com/w3c/creative-works-consent (proposed)
* **Website**: https://palimpsestlicense.org/standards/

## License

All standards documentation licensed under **CC-BY 4.0** (Creative Commons Attribution 4.0 International).

* You are free to: Share, adapt for any purpose
* You must: Give appropriate credit, indicate changes

**Attribution**: Palimpsest Stewardship Council and CWCF Community Group Contributors

## Contact

* **Project**: Palimpsest License Project
* **Email**: stewardship@palimpsestlicense.org
* **Website**: https://palimpsestlicense.org/

---

**Last Updated**: 2025-11-22
**Maintained By**: Palimpsest Stewardship Council
