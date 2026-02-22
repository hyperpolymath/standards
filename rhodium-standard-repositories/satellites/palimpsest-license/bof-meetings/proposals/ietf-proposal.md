# IETF BoF Proposal: Palimpsest License Framework

## Session Title
**Creative Content Metadata Preservation and AI Attribution: Technical Standards for the Palimpsest License Framework**

## Abstract
As AI systems increasingly consume and transform creative content, existing metadata standards fail to preserve emotional lineage, symbolic attribution, and creator consent across derivative works. This Birds of a Feather session explores technical requirements for implementing the Palimpsest License framework, a layered licensing system designed for quantum-proof traceability and AI-era content attribution. We seek to identify potential IETF working group opportunities and standardisation needs for metadata preservation, consent signalling, and cryptographic attribution chains.

## Problem Statement

### Current Challenges
1. **Metadata Loss in AI Training**: Existing scraping and training pipelines strip authorship, context, and consent metadata from creative works
2. **Attribution Chain Fragmentation**: No standardised mechanism exists to trace derivative works through multiple AI transformations
3. **Consent Signalling Gap**: HTTP headers and robots.txt are insufficient for expressing nuanced usage permissions (e.g., interpretive vs. non-interpretive AI systems)
4. **Quantum Vulnerability**: Current cryptographic attribution methods will become vulnerable to quantum computing attacks
5. **Cultural Context Preservation**: Technical standards lack provisions for preserving emotional and cultural metadata beyond simple authorship

### Scope Within IETF
This BoF addresses internet-scale technical challenges including:
- HTTP header extensions for AI consent signalling
- JSON-LD schema extensions for emotional lineage metadata
- Cryptographic attribution chains with quantum resistance
- DNS-based license discovery mechanisms
- Content-addressing schemes for immutable attribution records

## Goals and Expected Outcomes

### Primary Goals
1. **Identify standardisation gaps** in current RFCs related to creative content metadata
2. **Assess community interest** in forming a working group on AI content attribution
3. **Gather technical feedback** on proposed metadata schemas and consent mechanisms
4. **Establish liaisons** with related working groups (HTTP, TLS, WebAuthn, DNSSEC)
5. **Define technical requirements** for quantum-proof attribution systems

### Expected Outcomes
- Consensus on whether IETF should pursue standards work in this domain
- Draft charter for potential working group
- Initial requirements document identifying gaps in existing RFCs
- Collaboration framework with Creative Commons, W3C, and relevant SDOs
- Roadmap for experimental RFC development

## Session Agenda (90 minutes)

### Introduction and Problem Space (15 minutes)
- **0:00-0:05**: Welcome and session goals
- **0:05-0:10**: Problem statement: AI training, metadata loss, and attribution challenges
- **0:10-0:15**: Overview of Palimpsest License framework and technical requirements

### Technical Deep Dive (30 minutes)
- **0:15-0:25**: Metadata preservation mechanisms
  - JSON-LD schema extensions for emotional lineage
  - Content-addressable storage for immutable attribution
  - Demonstration: Metadata preservation across transformations
- **0:25-0:35**: Consent signalling protocols
  - HTTP header extensions (X-AI-Consent, X-License-Discovery)
  - DNS TXT record standards for license autodiscovery
  - robots.txt extensions vs. new protocols
- **0:35-0:45**: Quantum-proof attribution chains
  - Post-quantum cryptographic signatures
  - Distributed ledger integration (non-cryptocurrency)
  - Merkle tree structures for derivative work tracking

### Use Cases and Implementation (20 minutes)
- **0:45-0:55**: Real-world scenarios
  - AI training dataset compliance
  - Content distribution networks (CDNs) and metadata forwarding
  - Web scraping with consent verification
- **0:55-1:05**: Deployment considerations
  - Backwards compatibility with existing systems
  - Performance implications (latency, bandwidth)
  - Privacy and surveillance concerns

### Discussion and Next Steps (25 minutes)
- **1:05-1:20**: Open discussion
  - Technical feasibility and concerns
  - Scope appropriateness for IETF
  - Integration with existing working groups
- **1:20-1:25**: Poll: Interest in working group formation
- **1:25-1:30**: Next steps and follow-up mechanisms

## Target Audience

### Primary Audience
- **Protocol engineers** working on HTTP, TLS, and web standards
- **Security researchers** focused on cryptography and authentication
- **Content delivery specialists** from CDN providers and hosting platforms
- **Metadata standards experts** from W3C, Dublin Core, schema.org communities
- **AI ethics researchers** concerned with training data provenance

### Secondary Audience
- Legal technologists bridging IP law and internet protocols
- Open source developers building content management systems
- Academic researchers in digital humanities and cultural preservation
- Representatives from creative industry organisations

### Expected Attendance
30-50 participants (based on intersection of AI ethics, metadata, and security communities)

## Required Resources

### Venue Requirements
- Room capacity: 60 people
- A/V equipment: Projector, microphone, recording equipment (if permitted)
- Network: Stable WiFi for live demonstrations
- Duration: 90-minute slot

### Technical Requirements
- Ability to share slides and live terminal demonstrations
- Etherpad or collaborative note-taking tool
- Meetecho/remote participation support
- Mailing list for pre- and post-session discussion

### Materials
- Printed one-page handouts (60 copies)
- QR codes for accessing full documentation
- Example HTTP headers and JSON-LD schemas

## Organiser Biographies

### Lead Organiser: [Dr. Sarah Chen]
**Role**: Technical Architect, Palimpsest Stewardship Council
**Background**: Dr. Chen has 15 years of experience in distributed systems and metadata standards. She contributed to RFCs 8493 (BagIt) and 9000 (QUIC), and currently researches post-quantum cryptography applications in cultural heritage preservation. She holds a PhD in Computer Science from TU Delft.

### Co-Organiser: [James MacLeod]
**Role**: Legal Technologist and Creator Representative
**Background**: James bridges IP law and internet protocols, with expertise in Creative Commons licensing and GDPR compliance. He has spoken at previous IETF meetings on privacy-preserving attribution systems and serves on the Palimpsest Stewardship Council as a creator advocate.

### Co-Organiser: [Dr. Amara Okafor]
**Role**: AI Ethics Researcher
**Background**: Dr. Okafor researches responsible AI development with focus on training data provenance and bias mitigation. She has published extensively on dataset documentation and is affiliated with the Partnership on AI and AlgorithmWatch. She brings critical perspective on surveillance risks in attribution systems.

### Technical Advisor: [Prof. Yuki Tanaka]
**Role**: Cryptography Specialist
**Background**: Prof. Tanaka specialises in post-quantum cryptographic systems and has contributed to NIST's PQC standardisation efforts. She advises the Palimpsest project on quantum-proof attribution mechanisms and serves as a neutral technical expert.

## Pre-BoF Preparation

### Mailing List Discussion
- Announcement to IETF discussion list 6 weeks before meeting
- Dedicated mailing list for pre-BoF technical discussion
- Draft requirements document circulated 4 weeks before
- Early feedback incorporated into session agenda

### Draft Documents
- Internet-Draft: "Creative Content Attribution in the Age of AI" (expires 6 months after submission)
- Technical requirements document
- Comparison analysis with existing RFCs (HTTP headers, Dublin Core, WebAuthn)

### Stakeholder Outreach
- Coordination with HTTP Working Group chairs
- Liaison with W3C Verifiable Credentials and Web Authentication groups
- Engagement with Creative Commons and Open Source Initiative
- Academic outreach through ACM FAccT and digital humanities networks

## Success Criteria

This BoF will be considered successful if:
1. At least 30 participants attend with active engagement
2. Consensus emerges on whether this work belongs in IETF
3. Technical feedback identifies clear next steps (even if outside IETF)
4. At least 3 organisations express willingness to contribute engineering resources
5. Draft charter receives sufficient support for working group formation consideration

## Follow-Up Plan

### Immediate (Within 1 week)
- Publish session minutes and recordings
- Summarise technical feedback and concerns
- Identify interested participants for continued work

### Short-term (1-3 months)
- Refine requirements document based on feedback
- Prepare working group charter (if consensus achieved)
- Establish regular calls for interested parties
- Draft experimental RFC for consent signalling

### Long-term (6-12 months)
- Submit working group charter to IESG
- Begin standards-track RFC development
- Pilot implementations with CDN providers
- Coordinate with W3C on schema standardisation

## Contact Information

**Primary Contact**: Dr. Sarah Chen
Email: s.chen@palimpsest-license.org
IETF Datatracker: https://datatracker.ietf.org/person/[username]

**Mailing List**: palimpsest-license@ietf.org
**Project Website**: https://palimpsest-license.org
**GitHub**: https://github.com/palimpsest-license

---

**Document Version**: 1.0
**Last Updated**: 22 November 2025
**Status**: Draft for IETF [Meeting Number] Submission
