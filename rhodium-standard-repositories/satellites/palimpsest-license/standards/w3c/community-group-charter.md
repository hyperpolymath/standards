# Creative Works Consent Framework Community Group Charter

## 1. Group Name

**Creative Works Consent Framework Community Group** (CWCF CG)

## 2. Mission

The Creative Works Consent Framework Community Group develops open web standards for machine-readable consent declarations that enable creators to express granular permissions for AI and automated systems accessing their creative works. The group's mission is to protect creator rights whilst enabling responsible AI development through transparent, standardised consent mechanisms.

## 3. Background and Motivation

### 3.1. The Problem

The proliferation of AI systems—particularly large language models, generative AI, and autonomous agents—has created an urgent need for standardised mechanisms allowing creators to declare consent for automated processing of their works. Current approaches suffer from critical limitations:

* **Robots.txt** provides only binary allow/deny signals without AI-specific granularity
* **Creative Commons licenses** are human-readable but lack machine-verifiable consent layers
* **Platform terms of service** are fragmented, platform-specific, and not creator-controlled
* **Metadata standards** do not address AI training consent or synthetic output lineage

This gap has led to widespread unauthorised AI training on creative works, lack of attribution for AI-generated content, and erosion of creator rights in the AI era.

### 3.2. The Opportunity

Web standards are uniquely positioned to address this challenge by:

* Leveraging existing W3C technologies (JSON-LD, RDF, Verifiable Credentials)
* Integrating with web infrastructure (HTTP headers, well-known URIs, HTML metadata)
* Enabling decentralised, creator-controlled consent mechanisms
* Supporting both individual and collective governance models
* Providing machine-readable, legally-enforceable declarations

### 3.3. Prior Art

This work builds upon:

* The **Palimpsest License Project**, which pioneered emotional lineage protection and AI consent frameworks
* **AI Boundary Declaration Protocol (AIBDP)**, addressing HTTP-level consent enforcement
* **W3C Verifiable Credentials**, providing trust and verification mechanisms
* **Schema.org and JSON-LD**, offering semantic web foundations
* **Creative Commons**, establishing creator-centric licensing culture

## 4. Scope

### 4.1. In Scope

The Community Group will develop specifications and supporting materials for:

#### 4.1.1. Core Technical Specifications

* **Consent Declaration Format**: JSON-LD, RDF, and Protocol Buffers schemas for expressing consent
* **Discovery Mechanisms**: Well-known URIs, HTTP headers, HTML metadata, and link relations
* **AI Type Taxonomy**: Standardised categories for AI systems (LLM, generative, autonomous, agentic, neural interface, quantum)
* **Lineage Requirements**: Metadata schemas for provenance tracking in synthetic outputs
* **Attribution Specifications**: Machine-readable attribution requirements preserving emotional and cultural context
* **Verification Protocols**: Cryptographic signatures, blockchain attestations, and validation mechanisms

#### 4.1.2. Governance and Rights Management

* **Collective Governance**: DAO integration, multi-stakeholder consent, and benefit-sharing frameworks
* **Cultural Heritage Protection**: Protocols for Indigenous knowledge, traditional works, and sensitive content
* **Revocation Mechanisms**: Dynamic consent withdrawal and notice periods
* **Privacy Controls**: GDPR compliance, sensitive content handling, and anonymisation

#### 4.1.3. Web Integration

* **HTTP Status Codes**: Integration with HTTP 430 (Consent Required) and related error handling
* **CDN Integration**: Edge enforcement of consent declarations
* **Browser APIs**: JavaScript APIs for consent verification
* **Web Components**: Reusable UI components for consent display

#### 4.1.4. Supporting Deliverables

* **Implementation Guide**: Practical guidance for creators, developers, and AI system operators
* **Use Cases and Requirements**: Documented scenarios and functional requirements
* **Test Suite**: Validation tools and conformance tests
* **Best Practices**: Security, privacy, accessibility, and internationalization guidance

### 4.2. Out of Scope

The following are explicitly out of scope (but may be addressed by coordination with other groups):

* **Copyright law reform**: Legal frameworks remain the domain of legislatures and courts
* **AI model architecture**: How AI systems are built internally
* **Content filtering technologies**: Technical mechanisms for content moderation (separate from consent)
* **Digital Rights Management (DRM)**: Encryption-based access control systems
* **Payment processing**: Financial transaction protocols (though benefit-sharing mechanisms are in scope)

### 4.3. Coordination with Existing Groups

The CWCF CG will coordinate with:

* **W3C Verifiable Credentials Working Group**: For trust and verification mechanisms
* **W3C Decentralized Identifier (DID) Working Group**: For creator and governance body identification
* **W3C Web of Things (WoT) Working Group**: For ambient AI consent considerations
* **W3C Dataset Exchange Working Group (DXWG)**: For DCAT integration
* **W3C Privacy Interest Group (PING)**: For privacy review
* **W3C Accessibility Guidelines Working Group (AG WG)**: For accessibility compliance
* **W3C Internationalization (i18n) Working Group**: For multilingual support
* **IETF HTTP Working Group**: For HTTP header and status code standardisation
* **IETF Security Area**: For cryptographic and security review
* **Creative Commons**: For license compatibility and integration
* **Schema.org Community Group**: For vocabulary extensions

## 5. Deliverables

### 5.1. Specifications (Normative)

1. **Creative Works Consent Framework Core Specification**
   * Status: Community Group Specification
   * Expected: Q2 2026
   * Format: ReSpec/Bikeshed

2. **CWCF JSON-LD Context and Vocabulary**
   * Status: Community Group Specification
   * Expected: Q2 2026
   * Format: JSON-LD context document

3. **CWCF HTTP Integration Specification**
   * Status: Community Group Specification
   * Expected: Q3 2026
   * Format: ReSpec/Bikeshed

4. **CWCF Lineage and Provenance Specification**
   * Status: Community Group Specification
   * Expected: Q4 2026
   * Format: ReSpec/Bikeshed

### 5.2. Supporting Documents (Non-Normative)

1. **Implementation Guide**
   * Expected: Q2 2026
   * Audience: Creators, developers, AI operators

2. **Use Cases and Requirements**
   * Expected: Q1 2026
   * Purpose: Document driving use cases

3. **Security and Privacy Considerations**
   * Expected: Q3 2026
   * Purpose: Threat model and mitigations

4. **Cultural Heritage Best Practices**
   * Expected: Q4 2026
   * Audience: Cultural institutions, Indigenous communities

5. **Test Suite and Validator**
   * Expected: Q3 2026
   * Format: JavaScript/Python reference implementation

### 5.3. Living Documents

1. **AI System Type Registry**
   * Purpose: Catalog of recognised AI system types
   * Maintenance: Ongoing

2. **Implementation Report**
   * Purpose: Track deployments and conformance
   * Maintenance: Quarterly updates

## 6. Success Criteria

The Community Group will be considered successful if:

### 6.1. Adoption Metrics

* **100+ websites** implement CWCF declarations within 18 months
* **10+ AI systems** verify CWCF consent within 18 months
* **5+ content management systems** integrate CWCF support
* **3+ browser extensions** display CWCF consent status

### 6.2. Specification Maturity

* Core specification reaches Community Group Final Report status
* At least 2 independent, interoperable implementations
* Test suite achieves 80%+ coverage of normative requirements

### 6.3. Standards Track Progress

* Specification submitted to W3C Working Group (if appropriate interest exists)
* OR submitted to IETF for standards track (in coordination with IRTF work)
* Positive review from W3C TAG (Technical Architecture Group)

### 6.4. Community Impact

* Measurable reduction in creator complaints about unauthorised AI training
* Adoption by major creator organisations (e.g., Authors Guild, Musicians Union)
* Integration with cultural heritage organisations and Indigenous communities
* Recognition by AI ethics frameworks and governance bodies

## 7. Participation

### 7.1. Membership

Participation is open to all under the W3C Community Contributor License Agreement (CLA). We particularly encourage participation from:

* **Individual creators**: Authors, artists, musicians, journalists, researchers
* **Creator organisations**: Unions, guilds, cooperatives, collectives
* **Cultural heritage institutions**: Museums, libraries, archives, Indigenous communities
* **Technology providers**: Content platforms, AI companies, CMS vendors, browser developers
* **Legal and ethics experts**: Copyright scholars, AI ethicists, digital rights advocates
* **Standards experts**: W3C participants, IETF contributors, semantic web developers

### 7.2. Communication Channels

* **Mailing list**: public-cwcf@w3.org (primary discussion forum)
* **GitHub**: https://github.com/w3c/creative-works-consent (specification development)
* **Weekly calls**: Thursdays 15:00 UTC (rotating for global participation)
* **Face-to-face meetings**: Twice yearly (virtual and/or in-person at TPAC)
* **Chat**: IRC #cwcf on irc.w3.org (day-to-day coordination)

### 7.3. Decision Making

The Community Group operates by consensus:

* **Consensus**: General agreement with no sustained objections
* **Chair discretion**: Chairs determine when consensus is reached
* **Formal objections**: Documented objections trigger extended discussion
* **Voting**: Used only as last resort (simple majority of active participants)
* **Appeals**: Can be made to W3C Community Group Coordinator

### 7.4. Chairs

The group will be co-chaired by:

* **One creator representative**: Ensures creator perspective is centred
* **One technical expert**: Ensures specification quality and web standards alignment

Initial chairs to be elected at first meeting. Terms are 1 year, renewable.

### 7.5. Editors

Specifications will have designated editors responsible for:

* Incorporating agreed changes into specification text
* Maintaining editorial quality and consistency
* Managing specification publication process

Editors are appointed by chairs with group consensus.

## 8. Intellectual Property

### 8.1. Licensing

All specifications and supporting documents will be published under:

* **W3C Community Group Contributor License Agreement**: Covers patent commitments
* **W3C Software and Document License**: For specification text
* **CC0 or CC-BY**: For non-normative supporting materials (group decision)

### 8.2. Patent Policy

Participants agree to the W3C Community Contributor License Agreement, which includes:

* Royalty-free licensing commitment for implemented specifications
* Patent disclosure requirements
* Good-faith participation in standards development

### 8.3. Copyright

Contributions to specifications are made under the W3C CLA. Copyright for final specifications rests with W3C and contributors as specified in the CLA.

## 9. Relationship to W3C Process

### 9.1. Community Group Status

This is a W3C Community Group, not a Working Group. Specifications produced are Community Group Specifications, not W3C Recommendations, unless/until transferred to a Working Group.

### 9.2. Path to Recommendation Track

If specifications mature and demonstrate wide implementation and support, the group may:

1. Propose creation of a W3C Working Group to standardise the work
2. Transfer specifications to an existing Working Group (if appropriate)
3. Continue as a Community Group if W3C membership support is insufficient

### 9.3. W3C Review

The group will seek review from:

* **W3C Technical Architecture Group (TAG)**: Architecture and design review
* **W3C Privacy Interest Group (PING)**: Privacy review
* **W3C Accessibility Guidelines Working Group**: Accessibility review
* **W3C Internationalization Working Group**: I18n review

## 10. Timeline

### Phase 1: Foundation (Months 1-6)

* Establish group, elect chairs, recruit participants
* Develop use cases and requirements document
* Create initial vocabulary and JSON-LD context
* Begin core specification drafting

### Phase 2: Specification Development (Months 7-12)

* Complete draft core specification
* Develop HTTP integration specification
* Create reference implementation and test suite
* Conduct first round of external review

### Phase 3: Implementation and Testing (Months 13-18)

* Refine specifications based on implementation experience
* Expand test suite coverage
* Develop supporting documentation
* Conduct security and privacy review

### Phase 4: Maturation (Months 19-24)

* Achieve Community Group Final Report status
* Document multiple interoperable implementations
* Evaluate path to W3C Recommendation track
* Expand adoption and advocacy

## 11. Resources

### 11.1. Required Resources

* **W3C Community Group infrastructure**: Mailing list, wiki, GitHub
* **Hosting**: For validator, test suite, and documentation
* **Meeting tools**: Teleconference and screen-sharing services

### 11.2. Contributed Resources

Participants are expected to contribute:

* **Time**: Participation in calls, review of documents, implementation work
* **Expertise**: Technical knowledge, creator perspective, legal insight
* **Tools**: Development of implementations, test cases, documentation

### 11.3. Funding

The Community Group operates on a volunteer basis. Organisations may choose to:

* Sponsor member participation (employee time)
* Fund implementation development
* Support outreach and documentation efforts
* Host face-to-face meetings

## 12. Diversity and Inclusion

The Community Group is committed to:

* **Geographic diversity**: Global participation across time zones
* **Stakeholder diversity**: Creators, technologists, legal experts, cultural advocates
* **Cultural diversity**: Indigenous communities, diaspora groups, marginalised creators
* **Accessibility**: Meetings and documents accessible to participants with disabilities
* **Language**: English as working language, with multilingual support for outreach
* **Economic diversity**: Ensuring individual creators can participate alongside organisations

We will actively recruit participants from underrepresented communities and provide support (documentation, mentoring) to enable meaningful participation.

## 13. Code of Conduct

All participants must adhere to the W3C Code of Ethics and Professional Conduct (CEPC):

* **Respectful**: Treat all participants with respect and courtesy
* **Inclusive**: Welcome diverse perspectives and experiences
* **Collaborative**: Work together constructively toward shared goals
* **Transparent**: Conduct work in public, document decisions
* **Accountable**: Take responsibility for words and actions

Violations will be addressed through W3C CEPC reporting and enforcement mechanisms.

## 14. Amendments

This charter may be amended by:

1. Proposal by any participant
2. Discussion on mailing list (minimum 14 days)
3. Consensus decision (or vote if necessary)
4. Notification to W3C Community Group Coordinator

Substantive changes require re-confirmation of participant commitments under the W3C CLA.

## 15. Contact Information

* **Chairs**: TBD (to be elected at inaugural meeting)
* **W3C Staff Contact**: TBD (if assigned)
* **Mailing List**: public-cwcf@w3.org
* **GitHub**: https://github.com/w3c/creative-works-consent
* **Website**: https://www.w3.org/community/creative-works-consent/

---

## Charter History

* **2025-11-22**: Initial draft charter created
* **YYYY-MM-DD**: Charter approved by W3C (pending)

---

**Proposers**: Palimpsest Stewardship Council

**Supporters** (initial expressions of interest):
* [To be completed as support is gathered]

---

*This charter is based on the W3C Community Group template and incorporates best practices from successful W3C Community Groups including the Credentials CG, Web Payments CG, and WICG.*
