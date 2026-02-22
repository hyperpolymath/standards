# Proposal for Creative Works Consent Framework Community Group

## Executive Summary

We propose establishing a **W3C Community Group** to develop open web standards for machine-readable consent declarations that enable creators to express granular permissions for AI and automated systems accessing their creative works.

**Problem**: AI systems lack standardised mechanisms to verify creator consent before training on or generating content from creative works, leading to widespread unauthorised use.

**Solution**: A web standards framework providing machine-readable consent declarations, discovery protocols, and verification mechanisms that respect creator rights whilst enabling responsible AI development.

**Impact**: Protect millions of creators from unauthorised AI exploitation whilst establishing transparent, enforceable consent mechanisms for the AI industry.

## The Problem in Detail

### Unauthorised AI Training

AI companies are training large language models and generative AI systems on vast quantities of creative works without creator consent or compensation:

* **Authors**: Books scraped from pirate sites and used for LLM training
* **Artists**: Artwork used to train image generation systems without permission
* **Journalists**: Articles used in AI training, threatening media sustainability
* **Musicians**: Compositions used in audio generation systems without licensing
* **Indigenous communities**: Traditional knowledge exploited without cultural protocols

### Inadequate Existing Mechanisms

Current tools fail to address AI-specific consent needs:

| Mechanism | Limitation |
|-----------|-----------|
| **robots.txt** | Binary allow/deny; no AI system granularity |
| **Creative Commons** | Human-readable; no machine consent layer |
| **Terms of Service** | Platform-specific; not portable |
| **Copyright notices** | Not machine-readable; enforcement unclear |
| **Meta tags** | Non-standard; easily ignored |

### Real-World Harms

The lack of standardised consent mechanisms has caused:

* **Economic harm**: Creators lose revenue as AI systems replace their work
* **Attribution erasure**: AI outputs obscure original creator contributions
* **Cultural exploitation**: Indigenous and marginalised communities see sacred content misused
* **Privacy violations**: Personal narratives and sensitive content used without consent
* **Market disruption**: Creative industries undermined by unauthorised AI competition

## The Proposed Solution

### Machine-Readable Consent Framework

A comprehensive web standard enabling creators to declare:

* **Granular permissions**: Different consent for different AI types (LLM, generative, autonomous, neural interface, quantum)
* **Conditional access**: "Yes if you attribute me" or "Yes if you pay revenue share"
* **Collective governance**: Support for DAOs, cooperatives, and cultural bodies
* **Lineage requirements**: Mandate provenance tracking in AI outputs
* **Revocation mechanisms**: Withdraw consent dynamically
* **Cultural protections**: Special protocols for sensitive and heritage content

### Technical Architecture

Built on proven W3C technologies:

* **JSON-LD**: Semantic, linked data format for consent declarations
* **Well-known URIs**: `/.well-known/creative-works-consent.json`
* **HTTP integration**: Headers, status codes, link relations
* **Verifiable Credentials**: Cryptographic verification of consent
* **RDF/SPARQL**: Semantic web integration for complex queries

### Example Consent Declaration

```json
{
  "@context": "https://palimpsestlicense.org/cwcf/v1",
  "@type": "CreativeWork",
  "name": "My Novel",
  "author": {"@type": "Person", "name": "Jane Author"},
  "license": "https://creativecommons.org/licenses/by-nc-nd/4.0/",
  "consent": {
    "training": "deny",
    "generation": "deny",
    "analysis": "allow"
  },
  "aiTypes": {
    "llm": {"consent": "deny"},
    "search": {"consent": "allow"}
  },
  "lineage": {
    "required": true,
    "format": ["json-ld"]
  }
}
```

## Why W3C?

### Strategic Alignment

W3C is the ideal venue because:

* **Web-native**: AI systems primarily access content via HTTP/HTTPS
* **Existing infrastructure**: JSON-LD, RDF, Verifiable Credentials already standardised
* **Multi-stakeholder**: Creators, platforms, AI companies, civil society
* **Global reach**: International participation and adoption
* **Open standards**: Royalty-free, implementable by anyone

### Coordination Opportunities

The CWCF CG will work with:

* **Verifiable Credentials WG**: Trust and verification
* **DID WG**: Creator and governance body identification
* **Privacy Interest Group**: Privacy review
* **Accessibility Guidelines WG**: Accessibility compliance
* **Internationalization WG**: Multilingual support

### Complementary to IETF Work

Parallel work at IETF addresses:

* HTTP status codes (430 Consent Required)
* IANA registrations for headers and well-known URIs
* Protocol-level enforcement mechanisms

W3C work focuses on:

* Semantic web integration
* Web platform APIs
* Browser and CMS integration
* User-facing standards

## Deliverables and Timeline

### Year 1: Foundation

**Deliverables**:
* Use Cases and Requirements document
* Core specification (first public working draft)
* JSON-LD context and vocabulary
* Initial reference implementation

**Milestones**:
* Community Group established
* 50+ participants recruited
* First implementations deployed

### Year 2: Maturation

**Deliverables**:
* HTTP Integration specification
* Lineage and Provenance specification
* Test suite and validator
* Implementation guide
* Security and privacy analysis

**Milestones**:
* Community Group Final Report status
* 100+ websites implementing
* 10+ AI systems verifying consent
* Multiple interoperable implementations

### Year 3+: Standards Track

**Path forward**:
* Evaluate W3C Working Group creation
* Submit specifications to W3C Recommendation track
* Or coordinate IETF standards track for protocol aspects
* Expand adoption across web platform

## Success Metrics

### Adoption

* **Creators**: 1,000+ websites with CWCF declarations
* **Platforms**: 10+ CMSs with native CWCF support
* **AI systems**: 20+ systems verifying consent
* **Organisations**: Adoption by creators' unions, cultural heritage institutions

### Technical

* **Interoperability**: 5+ independent implementations
* **Test coverage**: 90%+ of normative requirements
* **Performance**: <50ms consent verification latency
* **Accessibility**: WCAG 2.1 AA compliant

### Impact

* **Creator satisfaction**: Measurable reduction in unauthorised use complaints
* **Industry adoption**: AI companies implementing consent verification
* **Legal recognition**: Referenced in court cases and legislation
* **Cultural protection**: Indigenous and marginalised communities report improved control

## Stakeholder Benefits

### For Creators

* **Control**: Granular control over AI use of works
* **Attribution**: Ensure credit in AI-generated content
* **Revenue**: Enable compensation for AI training
* **Protection**: Legal and technical enforcement mechanisms

### For AI Companies

* **Legal clarity**: Clear signals replacing legal ambiguity
* **Risk reduction**: Avoid copyright litigation
* **Ethical compliance**: Meet responsible AI commitments
* **Interoperability**: Standard protocol across all content

### For Platforms

* **Value-add**: Offer creators consent management tools
* **Liability protection**: Demonstrate good-faith compliance
* **Differentiation**: Attract creators with better protections
* **Standards compliance**: Implement open, non-proprietary standard

### For Civil Society

* **Creator rights**: Protect vulnerable and marginalised creators
* **Cultural heritage**: Preserve Indigenous and traditional knowledge
* **AI ethics**: Enforce consent and transparency in AI
* **Democratic participation**: Open standards process

## Initial Support

### Proposers

* **Palimpsest Stewardship Council**: License and consent framework developers

### Expressions of Interest

*(To be gathered during proposal socialisation)*

Seeking participation from:

* **Creator organisations**: Authors Guild, Musicians Union, National Union of Journalists
* **Cultural institutions**: Indigenous communities, libraries, archives, museums
* **Technology companies**: CMS vendors, browser developers, AI companies
* **Civil society**: EFF, Creative Commons, Public Knowledge, Access Now
* **Academic institutions**: AI ethics researchers, digital humanities scholars

## How to Get Involved

### Joining the Community Group

1. Sign the W3C Community Contributor License Agreement
2. Join the mailing list: public-cwcf@w3.org
3. Introduce yourself and your interest area
4. Participate in weekly calls (Thursdays 15:00 UTC)

### Contributing

We need:

* **Creator perspective**: Ensure standards meet real creator needs
* **Technical expertise**: JSON-LD, RDF, HTTP, web APIs
* **Legal insight**: Copyright, licensing, AI regulation
* **Cultural knowledge**: Indigenous protocols, heritage protection
* **Implementation**: Build tools, validators, libraries
* **Documentation**: Write guides, examples, tutorials
* **Testing**: Develop test cases, validate implementations
* **Outreach**: Recruit participants, advocate for adoption

### Resources Required

* **Time**: Weekly 1-hour calls, async mailing list participation
* **Expertise**: Share knowledge in your domain
* **Implementation**: (Optional) Build tools and deploy standards
* **Advocacy**: (Optional) Promote adoption in your networks

## Frequently Asked Questions

### Q: How does this relate to copyright law?

**A**: This standard provides a technical layer for expressing consent, not a replacement for copyright. It enables creators to declare machine-readable permissions that complement legal rights.

### Q: Won't AI companies just ignore this?

**A**: Multi-layer enforcement:
* **Technical**: CDN/server-level blocking of non-compliant systems
* **Legal**: License breach provisions and copyright infringement claims
* **Reputational**: Transparency about which systems respect consent
* **Regulatory**: Potential incorporation into AI governance frameworks

### Q: Is this compatible with Creative Commons?

**A**: Yes! CWCF adds a machine-readable layer to CC licenses. A CC-BY work can declare `consent.training = "conditional", requirements = "attribution"`.

### Q: What about fair use/fair dealing?

**A**: Consent declarations don't override fair use rights. They provide clarity for uses outside fair use exceptions.

### Q: How does this handle collective governance?

**A**: The standard supports DAO integration, multi-stakeholder approval, and delegation to cultural stewards through verifiable credentials and governance body identifiers.

### Q: What if an AI system is developed after standards are published?

**A**: The framework is extensible. New AI types can be added to the registry without breaking existing implementations.

### Q: How do you prevent consent declaration spoofing?

**A**: Cryptographic signatures, verifiable credentials, and optional blockchain attestations provide tamper-resistance. Well-known URIs ensure authentic discovery.

### Q: Is this just for web content?

**A**: Initial focus is web-hosted works (the primary AI training source), but the framework can extend to distributed systems, APIs, and offline content with persistent identifiers.

## Call to Action

We invite you to:

1. **Review** the charter and technical specifications
2. **Provide feedback** via public-cwcf@w3.org
3. **Express support** by signing the supporter list
4. **Join the Community Group** when established
5. **Implement the standard** in your tools and platforms
6. **Advocate** for adoption in your communities

Together, we can build a web where creator consent is respected, AI development is responsible, and cultural heritage is protected.

---

## Next Steps

1. **Socialise proposal**: Share with W3C community, potential participants
2. **Gather support**: Recruit initial participants and supporters
3. **Refine charter**: Incorporate feedback from community review
4. **Submit to W3C**: Formally propose Community Group creation
5. **Launch**: Hold inaugural meeting, elect chairs, begin work

---

## Contact

* **Email**: stewardship@palimpsestlicense.org
* **Website**: https://palimpsestlicense.org/standards/
* **GitHub**: https://github.com/palimpsest-license/cwcf-standards

---

**Document Version**: 1.0
**Date**: 2025-11-22
**Status**: Draft Proposal
