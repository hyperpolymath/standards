# Palimpsest License Infrastructure Initiative
## Supporting Creative Rights at the Network Layer

---

## Overview

This directory contains comprehensive materials for engaging Internet infrastructure providers—ISPs, CDNs, cloud providers, and network operators—in supporting the Palimpsest License at the technical infrastructure layer.

**Mission:** Make creative licensing metadata visible, preservable, and enforceable throughout Internet infrastructure.

## Directory Structure

```
infrastructure/
├── README.md                      # This file
├── technical/                     # Technical specifications
│   ├── bgp-license-propagation.md
│   ├── dns-txt-specifications.md
│   ├── edns-opt-metadata.md
│   ├── csp-license-enforcement.md
│   └── http-license-headers.md
├── isp-letters/                   # Provider-specific outreach
│   ├── TEMPLATE-isp-letter.md
│   ├── hurricane-electric.md
│   ├── cloudflare.md
│   ├── aws.md
│   ├── bt-british-telecom.md
│   └── [others to be added]
└── guides/                        # Educational materials
    ├── why-isps-should-care.md
    ├── infrastructure-role-in-creative-rights.md
    └── route-object-integration.md
```

## For Infrastructure Providers

### Getting Started

1. **Understand the opportunity:** Read [Why ISPs Should Care](./guides/why-isps-should-care.md)
2. **Explore technical options:** Review [Technical Specifications](#technical-specifications)
3. **See your use case:** Check [ISP Letters](#isp-letters) for providers similar to yours
4. **Contact us:** Reach out to discuss pilot programmes and partnerships

### Quick Implementation Paths

| Provider Type | Quickest Win | Effort | Impact |
|--------------|--------------|--------|--------|
| **Transit ISP** | BGP community preservation | Low | High |
| **CDN** | HTTP header preservation | Medium | Very High |
| **Cloud Provider** | Object metadata support | Medium | Very High |
| **Telecom** | BGP + enterprise services | Medium | Medium |
| **Hosting** | DNS TXT + HTTP headers | Low | Medium |

### Business Benefits

- **New revenue:** £50M-500M ARR potential (scale-dependent)
- **Differentiation:** "Ethical infrastructure" positioning
- **Risk mitigation:** Proactive regulatory compliance
- **Customer retention:** Unique value proposition
- **Industry leadership:** Shape emerging standards

## Technical Specifications

### 1. BGP License Propagation
**File:** [`technical/bgp-license-propagation.md`](./technical/bgp-license-propagation.md)

How BGP communities and extended communities can signal licensing requirements at the routing layer.

**Key concepts:**
- Standard communities (ASN:1000-1010)
- Extended communities for richer metadata
- Route object annotations in IRR databases
- Policy enforcement in route filters

**Who needs this:** Transit providers, ISPs, peering coordinators

### 2. DNS TXT Record Specifications
**File:** [`technical/dns-txt-specifications.md`](./technical/dns-txt-specifications.md)

Standardised DNS TXT records for declaring Palimpsest License metadata.

**Key concepts:**
- `_license.domain.tld` subdomain convention
- Structured key-value format
- DNSSEC signing for authenticity
- Integration with .well-known URIs

**Who needs this:** All providers, easy to implement

### 3. EDNS/OPT Metadata
**File:** [`technical/edns-opt-metadata.md`](./technical/edns-opt-metadata.md)

Using EDNS options to carry license metadata in DNS queries/responses.

**Key concepts:**
- EDNS option code allocation (experimental: 65400)
- Wire format specification
- Resolver-level policy enforcement
- Real-time license verification

**Who needs this:** DNS operators, resolver developers, advanced implementations

### 4. Content-Security-Policy for Licensing
**File:** [`technical/csp-license-enforcement.md`](./technical/csp-license-enforcement.md)

Extending CSP headers to enforce Palimpsest License requirements in browsers.

**Key concepts:**
- Custom CSP directives (`palimpsest-*`)
- Browser-level enforcement
- Violation reporting
- Client-side policy checking

**Who needs this:** Web servers, CDNs, application developers

### 5. HTTP Header Specifications
**File:** [`technical/http-license-headers.md`](./technical/http-license-headers.md)

Standardised HTTP headers for declaring and preserving license metadata.

**Key concepts:**
- `X-Palimpsest-*` header family
- Request and response headers
- Proxy and cache preservation
- Per-object granularity

**Who needs this:** Web servers, CDNs, proxies, caches

## ISP Letters

Personalised outreach letters to major infrastructure providers:

### Completed Letters

1. **[Hurricane Electric](./isp-letters/hurricane-electric.md)** (HE.net)
   - Target: Transit provider / BGP leadership
   - Focus: BGP communities, IRR integration, technical innovation

2. **[Cloudflare](./isp-letters/cloudflare.md)**
   - Target: CDN / Edge network
   - Focus: Workers, edge enforcement, compliance dashboards

3. **[Amazon Web Services](./isp-letters/aws.md)**
   - Target: Cloud platform
   - Focus: S3, CloudFront, SageMaker, Bedrock integration

4. **[BT (British Telecom)](./isp-letters/bt-british-telecom.md)**
   - Target: Telecom / National infrastructure
   - Focus: UK creative industries, enterprise services, Openreach

### Template for Additional Letters

**[Template ISP Letter](./isp-letters/TEMPLATE-isp-letter.md)** provides a comprehensive framework for adapting outreach to other providers.

**Remaining providers to target:**
- Akamai, Fastly (CDNs)
- Google Cloud Platform, Microsoft Azure (Cloud)
- Digital Ocean, Linode (Hosting/Cloud)
- Level3/Lumen, Cogent (Transit)
- Virgin Media, Vodafone (Telecoms)
- JANET, GÉANT (Academic networks)

## Educational Guides

### 1. Why ISPs Should Care About Creative Licensing
**File:** [`guides/why-isps-should-care.md`](./guides/why-isps-should-care.md)

Executive-level overview of why infrastructure providers should engage with creative licensing.

**Covers:**
- Changing regulatory landscape (EU AI Act, etc.)
- Business opportunities and revenue models
- Risk mitigation and compliance
- Competitive differentiation
- Common objections and responses

**Audience:** Executives, business development, policy teams

### 2. The Role of Infrastructure in Protecting Creative Rights
**File:** [`guides/infrastructure-role-in-creative-rights.md`](./guides/infrastructure-role-in-creative-rights.md)

Deep dive into how infrastructure shapes creative rights outcomes.

**Covers:**
- How infrastructure fails creators today
- Technical solutions at each layer
- Layered protection model (DNS, BGP, HTTP, content, crypto)
- Business models for infrastructure
- Policy and regulatory context
- Real-world case studies

**Audience:** Technical staff, policymakers, academic researchers

### 3. Route Object Integration Guide
**File:** [`guides/route-object-integration.md`](./guides/route-object-integration.md)

Practical guide for annotating IRR route objects with license metadata.

**Covers:**
- RPSL syntax and conventions
- Complete examples for different use cases
- Querying via WHOIS
- Automation scripts
- IRR database-specific instructions
- Troubleshooting

**Audience:** Network operators, IRR administrators

## Implementation Roadmap

### Phase 1: Quick Wins (0-3 months)

**For providers:**
1. Implement DNS TXT records (1 day)
2. Preserve HTTP headers in caching (1 week)
3. Document in network policy (1 week)

**Expected outcomes:**
- Basic license visibility
- Demonstrable good faith
- Customer feedback

### Phase 2: Enhanced Services (3-6 months)

**For providers:**
1. BGP community support (1-2 months)
2. AI scraper blocking rules (2-4 weeks)
3. Compliance logging (1 month)
4. Customer dashboard (2 months)

**Expected outcomes:**
- Premium service offering
- Initial revenue
- Case studies

### Phase 3: Advanced Features (6-12 months)

**For providers:**
1. EDNS integration (3-4 months)
2. Blockchain attribution (2-3 months)
3. API for third parties (2 months)
4. Standards participation (ongoing)

**Expected outcomes:**
- Industry leadership
- Significant revenue
- Market differentiation

### Phase 4: Industry Standard (12-24 months)

**For providers:**
1. IETF RFC publication
2. Native software support (BIND, Nginx, etc.)
3. Ecosystem partnerships
4. Global adoption

**Expected outcomes:**
- De facto standard
- Competitive necessity
- Regulatory alignment

## Success Metrics

### For Individual Providers

- **Adoption rate:** % of customers using license features
- **Revenue impact:** ARR from license-aware services
- **Customer retention:** Churn reduction in target segments
- **Brand sentiment:** Media coverage, awards, recognition
- **Regulatory positioning:** Cited in policy documents

### For the Ecosystem

- **Provider count:** Number of ISPs/CDNs/clouds supporting
- **Traffic coverage:** % of Internet traffic with license awareness
- **Creator adoption:** Number of creators using infrastructure features
- **Violation reduction:** Measurable decrease in unauthorised use
- **Standards progress:** IETF/W3C standardisation milestones

## Frequently Asked Questions

### For Infrastructure Providers

**Q: Are we liable if licenses are still violated?**

A: No. You're providing technical tools, not making legal determinations. Safe harbour protections apply. You're actually **reducing** risk by demonstrating good faith.

**Q: Will customers pay for this?**

A: Market research suggests yes—especially content platforms, publishers, cultural institutions, and AI companies needing compliant data sources. Pilot programmes will validate willingness to pay.

**Q: Is this technically feasible?**

A: Yes. Most implementations use existing functionality (BGP communities, HTTP headers, DNS TXT records) with minimal new development. Reference implementations provided.

**Q: What if no standard emerges?**

A: Early adopters shape standards. You have first-mover advantage and can influence the direction. Waiting means following others' designs.

**Q: How does this interact with RPKI, DNSSEC, etc.?**

A: Complementary. License metadata can be signed with DNSSEC, linked to RPKI ROAs, and integrated with existing security infrastructure.

### For Rights Holders

**Q: Will this really stop AI scraping?**

A: No single solution is perfect, but infrastructure-level enforcement is far more effective than relying on AI companies' voluntary compliance. It creates technical and legal barriers.

**Q: Do I need to pay ISPs for this?**

A: Not necessarily. Basic metadata preservation should be standard practice (like not stripping other headers). Premium services (advanced analytics, compliance reports) may cost extra.

**Q: What if my ISP doesn't support this?**

A: Demand drives supply. Ask your ISP, include in RFPs, consider switching to supporting providers. This guide provides ammunition for those conversations.

**Q: Is this only for the Palimpsest License?**

A: No. The infrastructure specifications can support any licensing framework. We advocate for open standards that benefit all creators.

## Getting Involved

### For Infrastructure Providers

1. **Contact us:** Email infrastructure@palimpsest.example
2. **Join pilot programme:** Limited slots available
3. **Attend working group:** Monthly calls (schedule TBD)
4. **Contribute to standards:** IETF, W3C, etc.

### For Rights Holders / Creators

1. **Adopt infrastructure-friendly licensing:** Use DNS TXT, HTTP headers, etc.
2. **Demand ISP support:** Include in contracts and RFPs
3. **Provide use cases:** Help us understand your needs
4. **Test and feedback:** Try reference implementations

### For Policymakers

1. **Educate:** Use these materials for briefings
2. **Consult:** We can participate in policy consultations
3. **Pilot programmes:** Government can seed adoption
4. **Procurement:** Prefer providers with license support

### For Researchers / Academics

1. **Study effectiveness:** Research needed on impact
2. **Formal verification:** Security and privacy analysis
3. **Economic modeling:** Business case refinement
4. **Legal analysis:** Jurisdiction-specific implications

## Resources

### Technical Resources

- **Reference implementations:** github.com/palimpsest-license/reference
- **Testing tools:** github.com/palimpsest-license/test-suite
- **Configuration examples:** github.com/palimpsest-license/examples
- **API documentation:** docs.palimpsest.example/api

### Business Resources

- **ROI calculator:** roi.palimpsest.example
- **Market research:** Available on request
- **Customer case studies:** (In development)
- **RFP templates:** templates.palimpsest.example

### Legal Resources

- **Jurisdiction analyses:** By request (UK, EU, US, etc.)
- **Terms of service templates:** legal.palimpsest.example
- **Compliance checklists:** Available on request
- **Expert network:** Connect with legal specialists

### Community

- **Mailing list:** infrastructure-discuss@palimpsest.example
- **Monthly calls:** TBD, all welcome
- **Slack workspace:** palimpsest-infra.slack.com
- **Conference track:** Submissions welcome

## Roadmap

### Q1 2026: Launch and Pilots

- ✅ Technical specifications published
- ✅ ISP outreach letters sent
- ⏳ Pilot programmes initiated (target: 20-30 providers)
- ⏳ Reference implementations released

### Q2 2026: Standards and Expansion

- ⏳ IETF Internet-Draft submitted
- ⏳ W3C Community Group formed
- ⏳ First commercial deployments
- ⏳ Academic partnerships established

### Q3-Q4 2026: Scaling and Adoption

- ⏳ 100+ providers supporting
- ⏳ Native software support (BIND, Nginx, etc.)
- ⏳ Industry consortium formation
- ⏳ Regulatory engagement (EU, UK, US)

### 2027+: Industry Standard

- ⏳ RFC publication
- ⏳ Global adoption
- ⏳ Integration with AI governance frameworks
- ⏳ Proven impact on creator protection

## Contact

### General Inquiries

- **Email:** infrastructure@palimpsest.example
- **Website:** https://palimpsest.example/infrastructure
- **GitHub:** github.com/palimpsest-license/infrastructure

### Specific Contacts

- **For ISPs/operators:** isp@palimpsest.example
- **For cloud providers:** cloud@palimpsest.example
- **For CDNs:** cdn@palimpsest.example
- **For technical questions:** tech@palimpsest.example
- **For business development:** partnerships@palimpsest.example

### Working Group Leads

- **Technical standards:** [Name], tech-lead@palimpsest.example
- **ISP engagement:** [Name], isp-lead@palimpsest.example
- **Policy and regulatory:** [Name], policy-lead@palimpsest.example
- **Academic research:** [Name], research-lead@palimpsest.example

## Acknowledgements

This initiative builds on decades of work by:

- Internet Engineering Task Force (IETF)
- Regional Internet Registries (RIPE, ARIN, APNIC, etc.)
- Creative Commons and open licensing communities
- Network operator forums (NANOG, RIPE meetings, etc.)
- Academic researchers in network architecture and digital rights

We stand on the shoulders of giants. Our contribution is applying these foundations to the urgent challenge of protecting creative rights in the AI era.

## License

All materials in this directory are licensed under the **Palimpsest License v0.4** unless otherwise specified.

You may:
- **Share** these materials freely with attribution
- **Adapt** them for your specific use case
- **Implement** the technical specifications without restriction
- **Reference** them in standards documents and policies

You must:
- **Attribute** the Palimpsest License project
- **Preserve** license notices when sharing
- **Link** back to the original specifications when adapting

The ideas and technical specifications are freely available. We want these to become industry standards, not proprietary lock-in.

---

**Let's build an Internet infrastructure that respects and protects creative voices.**

---

*Last updated: 2025-11-22*
*Version: 1.0*
*Maintainer: Palimpsest License Infrastructure Working Group*
