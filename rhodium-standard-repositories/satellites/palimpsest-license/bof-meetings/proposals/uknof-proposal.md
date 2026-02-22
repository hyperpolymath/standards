# UKNOF BoF Proposal: Palimpsest License Framework

## Session Title
**Creative Content Attribution Infrastructure: UK Operator Perspectives on AI, Licensing, and Metadata**

## Abstract
As AI systems increasingly scrape UK-hosted content for training data, network operators face new challenges in preserving creator rights whilst maintaining operational efficiency. This BoF examines practical deployment of the Palimpsest License framework—a technical infrastructure for DNS-based license discovery, HTTP consent signalling, and metadata preservation. We explore UK-specific regulatory considerations (Online Safety Act, upcoming AI regulation), operational feasibility for ISPs and hosting providers, and alignment with British creative industry needs. The session emphasises pragmatic implementation strategies suitable for UK network scale and regulatory environment.

## Problem Statement

### The UK Context
British creators, cultural institutions, and digital platforms face unique challenges:
1. **Cultural Heritage**: Extensive digital archives (British Library, National Archives) require robust attribution
2. **Creative Industries**: £126 billion sector needs protection from unauthorised AI training
3. **Regulatory Pressure**: Online Safety Act and forthcoming AI regulation mandate content traceability
4. **Scottish Law**: Unique jurisdiction requires consideration alongside English law
5. **Brexit Implications**: Post-EU regulatory landscape and international interoperability

### Technical Gaps for UK Operators
- No standardised mechanism for signalling AI training consent
- DNS infrastructure lacks license discovery capabilities
- CDN and caching layers strip creative metadata
- Compliance monitoring lacks operational tooling
- Cross-border attribution tracking (UK ↔ EU ↔ US)

### Operational Impact
- Customer complaints from creators reporting AI scraping violations
- Regulatory compliance costs without clear technical standards
- Liability concerns for platforms hosting derivative works
- Bandwidth consumed by aggressive AI training scrapers
- Need for UK-specific guidance on European and domestic law

## Goals and Expected Outcomes

### Primary Goals
1. **Assess UK operator readiness** for license infrastructure deployment
2. **Gather feedback** on DNS, HTTP, and metadata proposals
3. **Address regulatory compliance** specific to UK legal framework
4. **Identify deployment challenges** for UK hosting and ISP sector
5. **Build UK community consensus** on implementation approach

### Expected Outcomes
- Technical validation of proposed mechanisms for UK deployment
- Regulatory compliance roadmap for UK operators
- Pilot deployment commitments from UK hosting providers
- Liaison with UK government (DCMS, IPO, Ofcom)
- Collaboration framework with UK creative industry bodies

## Session Agenda (90 minutes)

### Introduction and UK Context (12 minutes)
- **0:00-0:03**: Welcome and session objectives
- **0:03-0:07**: Problem statement: AI scraping impact on UK creators
- **0:07-0:12**: UK regulatory landscape
  - Online Safety Act traceability requirements
  - Proposed AI regulation and creative rights
  - IPO consultation on AI and copyright
  - Ofcom enforcement considerations

### Technical Proposals (28 minutes)
- **0:12-0:20**: DNS-based license discovery
  - `_license.example.co.uk TXT` record format
  - .uk ccTLD implications and Nominet coordination
  - DNSSEC deployment status in UK (Ofcom guidance)
  - Caching and resolver configuration
- **0:20-0:28**: HTTP header extensions
  - `X-AI-Consent` and `X-License-URI` headers
  - Integration with existing UK hosting platforms
  - CDN considerations (Cloudflare, Fastly UK presence)
- **0:28-0:35**: Metadata preservation
  - British Library digital preservation standards
  - Dublin Core and UK cultural heritage metadata
  - Image EXIF/IPTC preservation through CDNs
- **0:35-0:40**: Live demonstration: UK deployment example

### Operational Considerations (25 minutes)
- **0:40-0:48**: UK hosting provider perspectives
  - Deployment feasibility for SME hosting sector
  - Cost implications for smaller operators
  - Wordpress.com, GitHub, Automattic UK presence
  - Jisc (UK academic network) considerations
- **0:48-0:56**: Performance and scale
  - Latency impact on UK network speeds
  - Bandwidth overhead analysis
  - Monitoring and alerting for compliance
- **0:56-1:03**: Security and privacy
  - GDPR compliance (UK GDPR post-Brexit)
  - DDoS mitigation and DNS amplification
  - Abuse prevention and rate limiting
- **1:03-1:05**: Case study: Pilot deployment at UK university

### Regulatory and Industry Alignment (15 minutes)
- **1:05-1:12**: UK government and regulator perspectives
  - DCMS (Department for Culture, Media and Sport) position
  - IPO (Intellectual Property Office) guidance
  - Ofcom enforcement mechanisms
  - Scottish Government cultural policy
- **1:12-1:20**: Creative industry coordination
  - Publishers Association, BPI, UK Music
  - National Union of Journalists
  - British Library and national archives
  - UK creative commons community

### Discussion and Next Steps (10 minutes)
- **1:20-1:27**: Open discussion and Q&A
- **1:27-1:29**: Poll: Interest in UK working group
- **1:29-1:30**: Next steps and contact information

## Target Audience

### Primary Audience
- **ISP network engineers** from major UK providers (BT, Virgin Media, Sky, TalkTalk)
- **Hosting provider operators** (Krystal, Memset, Bytemark, etc.)
- **University IT administrators** (Jisc members, Russell Group, post-92)
- **CDN engineers** with UK operations
- **DNS operators** including Nominet (.uk registry)

### Secondary Audience
- DCMS and IPO policy staff
- Ofcom technical regulation team
- UK creative industry representatives
- Legal technologists specialising in UK IP law
- Academic researchers in digital humanities

### Expected Attendance
35-50 participants (typical UKNOF BoF size)

## Required Resources

### Venue Requirements
- UKNOF meeting room (50-person capacity)
- Standard UKNOF A/V equipment
- Recording for YouTube archive
- Duration: 90 minutes

### Technical Requirements
- Live demonstration capability (internet access)
- DNS query and HTTP header examples
- Network diagrams for UK infrastructure
- Collaborative note-taking tool

### Materials
- Printed one-page summary (50 copies)
- UK regulatory compliance checklist
- Configuration examples for common platforms
- QR codes to GitHub repository

## Organiser Biographies

### Lead Organiser: [James MacLeod]
**Role**: Technical Lead and Creator Representative, Palimpsest Stewardship Council
**Background**: James is a Scottish-based network engineer with 14 years of experience in UK ISP and hosting operations. He has contributed to UKNOF sessions on DNS security and IPv6 deployment, and bridges technical operations with creator advocacy. James holds a BSc from Edinburgh and is actively involved in Scottish cultural preservation initiatives.

### Co-Organiser: [Dr. Aisha Patel]
**Role**: Legal Technologist, UK Internet Governance
**Background**: Dr. Patel specialises in UK digital regulation, having advised on Online Safety Act implementation and DCMS AI consultations. She has presented at previous UKNOFs on regulatory compliance for network operators and maintains relationships with Ofcom, IPO, and DCMS. She is based in London and holds a PhD in Technology Law from LSE.

### Co-Organiser: [Robert Chen]
**Role**: DNS and Registry Specialist
**Background**: Robert has worked extensively with .uk DNS infrastructure and has liaised with Nominet on DNSSEC deployment and registry policy. He brings operational experience from UK academic networks (Jisc) and commercial DNS hosting. He is a regular contributor to UK DNS operations discussions.

### Technical Advisor: [Sarah Williams]
**Role**: Cultural Heritage IT Specialist
**Background**: Sarah manages digital infrastructure at a major UK cultural institution and understands the intersection of preservation, attribution, and technical operations. She has implemented metadata standards for millions of digitised artefacts and provides the creative sector perspective on operational requirements.

## Pre-BoF Preparation

### Mailing List Discussion
- Announcement to UKNOF list 6 weeks before meeting
- Technical proposal shared 4 weeks before
- Coordination with Jisc community mailing lists
- Pre-BoF calls with interested UK operators

### Draft Documents
- UK deployment guide with regulatory context
- Nominet .uk DNS integration proposal
- Configuration examples for common UK platforms
- Compliance checklist for Online Safety Act

### Stakeholder Outreach
- Coordination with Nominet (.uk registry operator)
- Engagement with major UK hosting providers
- Liaison with DCMS, IPO, Ofcom
- Outreach to UK creative industry bodies (Publishers Association, BPI, NUJ)
- Academic collaboration through Jisc and British Library

## UK Regulatory Context

### Online Safety Act 2023
- **Duty of Care**: Platforms must consider creator rights in content management
- **Traceability**: Requirements for understanding content provenance
- **Ofcom Enforcement**: Technical standards for compliance verification
- **Codes of Practice**: Forthcoming guidance on AI and automated systems

### AI Regulation (Proposed)
- **Transparency Requirements**: AI systems must document training data sources
- **Creator Consent**: Mechanisms for opt-out from AI training
- **Copyright Protection**: Technical measures to prevent unauthorised use
- **Enforcement**: IPO and Ofcom coordination on complaints

### UK GDPR
- **Personal Data Minimisation**: Licensing metadata must avoid excessive personal data
- **Data Subject Rights**: Access, rectification, erasure for attribution records
- **Privacy by Design**: Attribution infrastructure must consider privacy implications

### Scottish Legal Considerations
- **Separate Jurisdiction**: Scots law differences in IP and contract
- **Cultural Heritage**: Scottish Government's digital culture strategy
- **Gaelic Language**: Considerations for multilingual metadata

## Technical Implementation for UK

### DNS Configuration for .uk Domains

```
# License discovery for .uk domains
_license.example.co.uk. 3600 IN TXT "palimpsest-v0.4 https://example.co.uk/license"
_license.example.co.uk. 3600 IN TXT "ai-consent=interpretive-only"

# DNSSEC signing (aligned with Nominet guidance)
example.co.uk. 3600 IN DNSKEY 256 3 13 [public-key]
_license.example.co.uk. 3600 IN RRSIG TXT 13 4 3600 [signature]
```

### HTTP Headers for UK Compliance

```
# Online Safety Act traceability
X-License-URI: https://example.co.uk/palimpsest.html
X-AI-Consent: non-interpretive-prohibited
X-Content-Origin: creator-generated
X-Attribution-Chain: sha256:abc123def456
X-UK-Copyright-Status: protected

# GDPR-compliant minimal metadata
X-License-URI: https://example.co.uk/license.txt
```

### Performance Benchmarks

Based on preliminary testing across UK networks:

| Mechanism | Latency (London) | Latency (Edinburgh) | Bandwidth |
|-----------|------------------|---------------------|-----------|
| HTTP headers | <1ms | <1ms | ~200 bytes |
| DNS TXT (uncached) | 8-15ms | 12-20ms | ~150 bytes |
| DNS TXT (cached) | <1ms | <1ms | ~150 bytes |
| Attribution DB | 3-8ms | 5-12ms | ~500 bytes |

## UK Industry Alignment

### Creative Sector Bodies
- **Publishers Association**: Book publishing industry representation
- **BPI**: Recorded music industry
- **UK Music**: Live music and composer organisations
- **PACT**: Independent TV and film producers
- **National Union of Journalists**: Working journalists' advocacy

### Cultural Heritage
- **British Library**: National library and digital archive
- **The National Archives**: UK government records
- **National Museums**: V&A, British Museum, National Gallery digitisation
- **Jisc**: UK academic and research network

### Technical Community
- **Nominet**: .uk registry operator
- **Ispa UK**: Internet Services Providers Association
- **TechUK**: Technology industry trade association
- **Open Rights Group**: Digital rights advocacy

## Success Criteria

This BoF will be considered successful if:
1. At least 35 UK operators participate actively
2. Technical proposals validated for UK deployment
3. At least 2 UK hosting providers commit to pilots
4. Clear regulatory compliance pathway identified
5. UK working group established for ongoing coordination

## Follow-Up Plan

### Immediate (Within 2 weeks)
- Publish session notes and recording on UKNOF website
- Summarise feedback for UK working group
- Coordinate with Nominet on .uk DNS integration
- Engage Ofcom and DCMS on regulatory alignment

### Short-term (1-3 months)
- Develop UK deployment guide with regulatory context
- Pilot with 2-3 UK hosting providers
- Present findings to DCMS AI and copyright consultation
- Coordinate with IPO on copyright guidance

### Long-term (6-12 months)
- Expand UK pilot to 10+ operators
- Integrate with Jisc network infrastructure
- Coordinate with British Library on cultural heritage
- Align with EU developments (RIPE, EuroDIG)

## UK-Specific Resources

### Regulatory Contacts
- **DCMS**: Digital and Creative Industries team
- **IPO**: Policy and International team
- **Ofcom**: Online Safety technical standards
- **Scottish Government**: Culture and Digital Economy

### Technical Infrastructure
- **Nominet**: .uk registry and DNS services
- **Jisc**: Academic network and digital infrastructure
- **Janet**: UK education and research network
- **UK DNS providers**: Cloudflare UK, Memset, Bytemark

### Support and Funding
- Potential for Innovate UK funding
- UKRI digital infrastructure grants
- Creative Industries Council support
- Arts Council England digital programmes

## Contact Information

**Primary Contact**: James MacLeod
Email: j.macleod@palimpsest-license.org
UKNOF Profile: jamesmacleod
Location: Edinburgh, Scotland

**Mailing List**: uk-operators@palimpsest-license.org
**Project Website**: https://palimpsest-license.org
**GitHub**: https://github.com/palimpsest-license

---

**Document Version**: 1.0
**Last Updated**: 22 November 2025
**Status**: Draft for UKNOF [Meeting Number] Submission
**Location**: [City, UK]
