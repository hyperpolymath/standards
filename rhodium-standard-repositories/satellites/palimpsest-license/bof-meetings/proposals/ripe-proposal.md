# RIPE Meeting BoF Proposal: Palimpsest License Framework

## Session Title
**DNS-Based License Discovery and Infrastructure for Creative Content Attribution**

## Abstract
The Palimpsest License framework addresses AI-era challenges in creative content protection through technical infrastructure including DNS-based license discovery, DNSSEC-authenticated attribution records, and distributed metadata preservation. This BoF explores infrastructure requirements for implementing quantum-proof content attribution at internet scale, with particular focus on DNS extensions, registry-level metadata, and European regulatory compliance (DSA, AI Act). We seek community feedback on proposed DNS record types, RPKI-style validation mechanisms for content licensing, and infrastructure deployment strategies.

## Problem Statement

### Current Challenges for Internet Infrastructure
1. **License Discovery**: No standardised DNS-based mechanism for discovering licensing terms for web content
2. **Registry-Level Attribution**: WHOIS and RDAP lack fields for creative content licensing metadata
3. **Validation Infrastructure**: No trust anchor system for verifying content attribution claims
4. **European Regulatory Compliance**: DSA and upcoming AI Act require traceable content provenance
5. **Automated Scraping**: Large-scale AI training operations ignore existing license indicators

### Relevance to RIPE Community
This work intersects with several RIPE community interests:
- DNS operations and standards extensions
- European internet governance and regulatory compliance
- Registry data models and RDAP extensions
- Trust infrastructure analogous to RPKI
- Operational security for content platforms

## Goals and Expected Outcomes

### Primary Goals
1. **Gather operational feedback** on DNS-based license discovery mechanisms
2. **Assess feasibility** of DNSSEC-authenticated attribution records
3. **Explore registry integration** for licensing metadata in RDAP
4. **Discuss European regulatory requirements** (DSA, AI Act, GDPR)
5. **Identify deployment challenges** for content hosting providers

### Expected Outcomes
- Technical consensus on DNS record format for license discovery
- Operational guidance on DNSSEC signing for attribution metadata
- Roadmap for RDAP extension proposal
- Collaboration framework with RIPE NCC and other RIRs
- Pilot deployment commitments from hosting providers

## Session Agenda (90 minutes)

### Introduction and Regulatory Context (15 minutes)
- **0:00-0:05**: Welcome and session objectives
- **0:05-0:10**: Problem statement: AI training, content scraping, and infrastructure gaps
- **0:10-0:15**: European regulatory landscape (DSA, AI Act) and compliance requirements

### DNS-Based License Discovery (25 minutes)
- **0:15-0:25**: Proposed DNS record format
  - TXT vs. new RRTYPE for license metadata
  - Example: `_license.example.com TXT "palimpsest-v0.4 https://..."`
  - DNSSEC signing requirements
  - Caching and TTL considerations
- **0:25-0:35**: Operational considerations
  - Zone file management at scale
  - Anycast DNS and consistency
  - Performance impact analysis
  - Deployment case study: .nl ccTLD
- **0:35-0:40**: Live demonstration: DNS query for license metadata

### Registry and Trust Infrastructure (25 minutes)
- **0:40-0:50**: RDAP extensions for creative content licensing
  - New object class: "content_license"
  - Integration with existing WHOIS/RDAP infrastructure
  - Privacy considerations (vs. GDPR requirements)
- **0:50-1:00**: Trust anchor mechanisms
  - RPKI-inspired validation system for attribution claims
  - Certificate transparency for license records
  - Distributed ledger integration (non-cryptocurrency)
- **1:00-1:05**: European registry perspectives: RIPE NCC, SIDN, Nominet

### Deployment and Operations (15 minutes)
- **1:05-1:15**: Deployment scenarios
  - Large-scale hosting providers (WordPress, GitHub, Cloudflare)
  - National libraries and cultural heritage institutions
  - CDN integration and caching
  - Backwards compatibility and gradual rollout
- **1:15-1:20**: Monitoring and compliance verification

### Discussion and Next Steps (10 minutes)
- **1:20-1:28**: Open discussion and Q&A
- **1:28-1:30**: Next steps and follow-up coordination

## Target Audience

### Primary Audience
- **DNS operators** from ISPs, hosting providers, and registries
- **ccTLD and gTLD registry operators** interested in policy extensions
- **RIPE NCC staff** working on registry services and RDAP
- **European hosting providers** subject to DSA compliance
- **Security operators** familiar with DNSSEC and RPKI

### Secondary Audience
- Legal experts in European internet regulation
- Cultural heritage IT administrators
- Academic researchers in internet measurement
- Policy makers concerned with AI regulation

### Expected Attendance
40-60 participants (RIPE working group session attendance range)

## Required Resources

### Venue Requirements
- Working group meeting room (60-person capacity)
- Standard RIPE meeting A/V setup
- Recording and streaming equipment
- Duration: 90 minutes

### Technical Requirements
- Live DNS query demonstrations (requires internet access)
- Access to RIPE Atlas probes for measurement demonstration
- Presentation materials with technical diagrams
- Collaborative note-taking (RIPE meeting tools)

### Materials
- Printed technical specification summary (60 copies)
- DNS record format examples
- RDAP extension proposal draft
- QR codes to documentation repository

## Organiser Biographies

### Lead Organiser: [Marieke van der Berg]
**Role**: Infrastructure Architect, Palimpsest Stewardship Council
**Background**: Marieke has 12 years of experience in DNS operations and served as technical lead at SIDN (.nl registry). She contributed to DNSSEC deployment across European ccTLDs and has presented at five previous RIPE meetings on DNS security and registry operations. She holds CISSP and is active in the DNS-OARC community.

### Co-Organiser: [Dr. Klaus Hoffmann]
**Role**: Legal Technologist, European Internet Governance
**Background**: Dr. Hoffmann specialises in European digital regulation and technical compliance. He advised on DSA implementation and serves on RIPE's policy development process. He brings expertise on GDPR, DSA, and the upcoming AI Act requirements for content traceability.

### Co-Organiser: [Elena Popescu]
**Role**: Security Researcher, RPKI and Trust Infrastructure
**Background**: Elena researches internet trust infrastructure with focus on RPKI, certificate transparency, and DNS security. She has published on BGP security and contributed to MANRS (Mutually Agreed Norms for Routing Security). She advises the Palimpsest project on validation mechanisms.

### Technical Advisor: [Jan Kowalski]
**Role**: RDAP Specialist, RIPE NCC
**Background**: Jan works on registry data services at RIPE NCC and contributed to RDAP standardisation efforts. He provides operational perspective on registry infrastructure scalability and privacy considerations. (Note: Pending confirmation of RIPE NCC participation)

## Pre-BoF Preparation

### Mailing List Discussion
- Announcement to RIPE DNS Working Group 6 weeks before meeting
- Draft DNS record specification circulated 4 weeks before
- RDAP extension proposal shared with Registration Services WG
- Early feedback from ccTLD operators and hosting providers

### Draft Documents
- DNS RRTYPE specification (TXT-based and new RRTYPE comparison)
- RDAP extension proposal
- Operational deployment guide
- European regulatory compliance mapping

### Stakeholder Outreach
- Coordination with SIDN, Nominet, DENIC, AFNIC (European ccTLDs)
- Engagement with Cloudflare, OVH, Hetzner (major European hosts)
- Liaison with CENTR (Council of European National Top-Level Domain Registries)
- Academic collaboration through NLnet Labs, SIDN Labs

## RIPE-Specific Considerations

### Alignment with RIPE Working Groups
- **DNS Working Group**: Primary venue for DNS record format discussion
- **Registration Services WG**: RDAP extension proposals
- **Anti-Abuse WG**: Compliance verification mechanisms
- **IPv6 WG**: IPv6 readiness for attribution infrastructure

### RIPE NCC Services Integration
- Potential RIPE Atlas measurement campaign for adoption tracking
- Integration with RIPEstat for license metadata visibility
- RIPE Database extensions for licensing information

### European Policy Context
- DSA compliance timeline and requirements
- AI Act preliminary guidance on training data documentation
- GDPR considerations for registry metadata
- National implementations (Dutch AI law, German NetzDG)

## Success Criteria

This BoF will be considered successful if:
1. At least 40 participants attend with DNS/registry operational experience
2. Technical consensus on DNS record format emerges
3. At least 2 ccTLD operators express interest in pilot deployment
4. RDAP extension proposal receives positive feedback
5. Clear next steps identified for standardisation and deployment

## Follow-Up Plan

### Immediate (Within 1 week)
- Publish session minutes on RIPE Labs
- Summarise technical feedback and concerns
- Identify pilot deployment candidates

### Short-term (1-3 months)
- Refine DNS record specification based on feedback
- Submit RDAP extension to IETF for standardisation
- Coordinate with CENTR on European ccTLD adoption
- Begin pilot with 2-3 ccTLD registries

### Long-term (6-12 months)
- Measure adoption using RIPE Atlas
- Publish operational best practices on RIPE Labs
- Integrate with RIPE NCC registry services
- Expand to other RIRs (ARIN, APNIC, AFRINIC, LACNIC)

## European Regulatory Alignment

### Digital Services Act (DSA)
- Article 14: Traceability requirements for content
- Article 24: Transparency for recommender systems
- Reporting obligations for VLOPs (Very Large Online Platforms)

### AI Act (Proposed)
- Training data documentation requirements
- Transparency obligations for generative AI
- Copyright and fundamental rights safeguards

### GDPR Compliance
- Minimal personal data in licensing metadata
- Privacy-by-design in attribution systems
- Data subject rights (access, erasure, rectification)

## Contact Information

**Primary Contact**: Marieke van der Berg
Email: m.vandeberg@palimpsest-license.org
RIPE NCC Access: marieke.vandeberg

**Mailing List**: palimpsest-dns@lists.ripe.net (to be created)
**Project Website**: https://palimpsest-license.org
**Technical Documentation**: https://github.com/palimpsest-license

---

**Document Version**: 1.0
**Last Updated**: 22 November 2025
**Status**: Draft for RIPE [Meeting Number] Submission
**Submitted to**: DNS Working Group (Primary), Registration Services WG (Secondary)
