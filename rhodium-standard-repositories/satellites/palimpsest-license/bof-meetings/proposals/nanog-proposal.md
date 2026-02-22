# NANOG BoF Proposal: Palimpsest License Framework

## Session Title
**Operational Challenges in Content Attribution: Deploying License Discovery at CDN and Hosting Scale**

## Abstract
AI training systems scrape billions of web pages daily, often stripping licensing metadata and ignoring creator consent. This BoF addresses operational challenges in preserving and enforcing creative content licenses at network scale. We present the Palimpsest License framework's infrastructure requirements—DNS-based license discovery, HTTP header extensions, and CDN metadata forwarding—and seek feedback from operators on deployment feasibility, performance impact, and operational best practices. This session focuses on practical implementation for hosting providers, CDNs, and large-scale content platforms.

## Problem Statement

### The Operational Challenge
Network operators and hosting providers face increasing pressure to:
1. **Comply with AI-related regulations** without clear technical guidance
2. **Preserve licensing metadata** through caching, compression, and transformation layers
3. **Signal creator consent** for different types of AI usage
4. **Track attribution chains** for derivative content
5. **Scale solutions** to handle billions of requests per day

### Current Gaps
- **HTTP headers**: No standard for AI usage consent (User-Agent filtering is insufficient)
- **CDN caching**: Aggressive optimisation strips metadata from HTML and images
- **DNS**: License discovery requires manual website inspection
- **Monitoring**: No operational metrics for license compliance
- **Automation**: Scraping bots ignore robots.txt license directives

### Why This Matters to Network Operators
- Regulatory compliance (DMCA, EU AI Act, upcoming state laws)
- Bandwidth costs from aggressive AI scraping
- Customer support burden from creators reporting violations
- Liability concerns for hosting infringing derivative works
- Competitive differentiation through ethical AI practices

## Goals and Expected Outcomes

### Primary Goals
1. **Validate deployment feasibility** of proposed technical mechanisms
2. **Identify performance bottlenecks** and operational concerns
3. **Gather requirements** from CDN, hosting, and platform operators
4. **Share operational experience** with content licensing systems
5. **Build consensus** on practical implementation approaches

### Expected Outcomes
- Operational feedback on HTTP header and DNS proposals
- Performance impact analysis for CDN metadata forwarding
- Deployment roadmap with rollout strategies
- Operator commitments for pilot deployments
- Best practices documentation for configuration

## Session Agenda (90 minutes)

### Introduction and Problem Space (10 minutes)
- **0:00-0:03**: Welcome and session goals
- **0:03-0:07**: Problem statement: AI scraping, metadata loss, operational impact
- **0:07-0:10**: Overview of Palimpsest License framework

### Technical Mechanisms (30 minutes)
- **0:10-0:18**: HTTP header extensions for AI consent
  - `X-AI-Consent: interpretive` / `non-interpretive-prohibited`
  - `X-License-URI: https://example.com/license`
  - `X-Attribution-Chain: [hash-based-lineage]`
  - Integration with existing CSP, CORS, security headers
- **0:18-0:26**: DNS-based license discovery
  - `_license.example.com TXT "palimpsest-v0.4"`
  - Caching considerations and TTL recommendations
  - Anycast DNS compatibility
  - DNSSEC signing requirements
- **0:26-0:34**: CDN and caching infrastructure
  - Metadata preservation through Varnish, nginx, Cloudflare
  - Image optimization without stripping EXIF/IPTC
  - HTML minification and metadata comments
  - Edge computing for license validation
- **0:34-0:40**: Live demonstration: License discovery and header propagation

### Operational Considerations (25 minutes)
- **0:40-0:48**: Performance and scale
  - Latency impact of additional DNS lookups
  - Header size and bandwidth overhead
  - Caching efficiency and hit rates
  - Database queries for attribution chains
- **0:48-0:56**: Deployment strategies
  - Backwards compatibility with legacy systems
  - Gradual rollout approaches
  - Configuration management (Ansible, Terraform, etc.)
  - Monitoring and alerting
- **0:56-1:04**: Security and abuse prevention
  - DDoS amplification via DNS lookups
  - Header injection attacks
  - False attribution claims
  - Rate limiting aggressive scrapers
- **1:04-1:05**: Operator experiences: Case studies from early adopters

### Discussion and Next Steps (25 minutes)
- **1:05-1:22**: Open discussion
  - Operator concerns and questions
  - Alternative approaches and trade-offs
  - Integration with existing compliance systems
  - Cost-benefit analysis
- **1:22-1:25**: Poll: Interest in pilot deployment
- **1:25-1:28**: Next steps and working group formation
- **1:28-1:30**: Wrap-up and contact information

## Target Audience

### Primary Audience
- **Network operators** from ISPs and hosting providers
- **CDN engineers** from Cloudflare, Fastly, Akamai, and regional providers
- **Platform operators** from content hosting services (WordPress.com, GitHub, etc.)
- **DevOps engineers** managing web infrastructure at scale
- **Security teams** concerned with compliance and abuse

### Secondary Audience
- Legal teams from hosting providers
- Product managers for content platforms
- Open source developers of web servers and caching software
- Academic researchers measuring internet-wide adoption

### Expected Attendance
50-70 participants (based on typical NANOG BoF attendance)

## Required Resources

### Venue Requirements
- BoF session room (80-person capacity)
- Standard NANOG meeting A/V setup
- Recording and livestream (NANOG YouTube)
- Duration: 90 minutes

### Technical Requirements
- Live demonstrations (require internet access and test infrastructure)
- Terminal access for showing DNS queries and HTTP headers
- Network diagram visualisations
- Real-time collaborative notes (Etherpad or similar)

### Materials
- One-page technical summary (80 copies)
- Configuration examples (nginx, Varnish, Apache)
- QR codes to GitHub repository with deployment tools
- Performance benchmark data

## Organiser Biographies

### Lead Organiser: [Alex Rivera]
**Role**: Network Architect, Palimpsest Stewardship Council
**Background**: Alex has 18 years of experience in large-scale network operations, including 8 years at a major CDN provider. They have presented at NANOG on topics including DDoS mitigation, anycast DNS operations, and HTTP/2 deployment. Alex holds CCIE certification and contributes to open source web server projects.

### Co-Organiser: [Dr. Jennifer Wu]
**Role**: Performance Engineering Researcher
**Background**: Dr. Wu researches web performance and caching infrastructure, with publications on CDN optimization and edge computing. She has collaborated with browser vendors on HTTP header standardization and brings quantitative analysis of performance trade-offs. She previously worked at Fastly and is now at UC Berkeley.

### Co-Organiser: [Marcus Thompson]
**Role**: Security Operations Engineer
**Background**: Marcus specialises in web application security and compliance automation. He has deployed large-scale header injection prevention systems and leads the security working group for a major hosting provider. He brings practical experience with DMCA compliance automation and abuse prevention.

### Technical Advisor: [Priya Sharma]
**Role**: DNS Operations Expert
**Background**: Priya operates authoritative DNS infrastructure at global scale and has extensive experience with DNSSEC deployment. She has presented at DNS-OARC and contributes to open source DNS software. She provides operational perspective on DNS-based license discovery.

## Pre-BoF Preparation

### Mailing List Discussion
- Announcement to NANOG mailing list 6 weeks before meeting
- Technical proposal circulated 4 weeks before
- Early feedback from CDN and hosting operators
- Pre-BoF calls with interested operators

### Draft Documents
- Technical deployment guide
- Performance benchmark results
- Configuration examples for popular web servers
- Security considerations and best practices

### Stakeholder Outreach
- Engagement with Cloudflare, Fastly, Akamai, AWS CloudFront
- Coordination with major hosting providers (OVH, DigitalOcean, Linode)
- Collaboration with web server projects (nginx, Apache, Caddy)
- Academic partnerships for measurement studies

## Operational Implementation Details

### HTTP Header Specification

```
# Example: Full license metadata
X-AI-Consent: non-interpretive-prohibited
X-License-URI: https://example.com/palimpsest-v0.4.html
X-Attribution-Chain: sha256:abc123...
X-Emotional-Lineage: protest-song; cultural-heritage
X-License-Expires: 2050-12-31T23:59:59Z

# Example: Minimal implementation
X-License-URI: https://example.com/license.txt
```

### DNS Record Format

```
# TXT record approach
_license.example.com. 3600 IN TXT "palimpsest-v0.4 https://example.com/license.html"
_license.example.com. 3600 IN TXT "ai-consent=interpretive-only"

# Or under main domain
example.com. 3600 IN TXT "license=palimpsest-v0.4"
```

### nginx Configuration Example

```nginx
# Add license headers to all responses
add_header X-License-URI "https://example.com/palimpsest.html" always;
add_header X-AI-Consent "non-interpretive-prohibited" always;

# Preserve existing metadata in proxied responses
proxy_pass_header X-Attribution-Chain;
proxy_set_header X-Forwarded-License $http_x_license_uri;
```

### Performance Considerations

| Mechanism | Latency Impact | Bandwidth Overhead | Cache Impact |
|-----------|----------------|-------------------|--------------|
| HTTP headers | <1ms | ~200 bytes/request | Negligible |
| DNS TXT lookup | 5-50ms (cached: <1ms) | ~150 bytes | Separate cache |
| Attribution chain | 2-10ms (DB query) | ~500 bytes | High (invalidation) |

## Regulatory and Compliance Context

### United States
- DMCA Section 512 safe harbour requirements
- State-level AI regulations (California, Texas, New York proposals)
- Copyright Office AI training data guidance (2024)

### International
- EU AI Act training data documentation
- UK Online Safety Act content traceability
- Australian Online Safety Act

### Industry Standards
- Creative Commons license compatibility
- Open Source Initiative licence metadata
- Schema.org CreativeWork extensions

## Success Criteria

This BoF will be considered successful if:
1. At least 50 operators attend with active participation
2. Clear consensus on feasibility of proposed mechanisms
3. At least 3 operators commit to pilot deployments
4. Performance concerns identified with mitigation strategies
5. Follow-up working group has sufficient interest

## Follow-Up Plan

### Immediate (Within 2 weeks)
- Publish session recording and transcript
- Summarise operator feedback and concerns
- Create GitHub repository with configuration examples
- Set up mailing list for continued discussion

### Short-term (1-3 months)
- Develop deployment playbooks for common platforms
- Coordinate pilot deployments with 3-5 operators
- Measure performance impact in production
- Create monitoring dashboard templates

### Long-term (6-12 months)
- Publish operational best practices document
- Submit performance data to academic conferences
- Expand pilot to 20+ operators
- Integrate with IETF standardisation efforts

## Measurement and Validation

### Proposed Metrics
- **Adoption rate**: DNS record deployment across top 1M sites
- **Header propagation**: Cache hit rates with metadata preservation
- **Performance impact**: P50, P95, P99 latency changes
- **Compliance rate**: AI scraper adherence to consent signals
- **Operational cost**: Engineering hours and infrastructure expenses

### Tools
- RIPE Atlas measurements for DNS adoption
- HTTP Archive analysis for header prevalence
- Custom scrapers to test compliance
- CDN analytics for performance validation

## Contact Information

**Primary Contact**: Alex Rivera
Email: a.rivera@palimpsest-license.org
NANOG Profile: alexrivera

**Mailing List**: operators@palimpsest-license.org
**Project Website**: https://palimpsest-license.org
**GitHub**: https://github.com/palimpsest-license/operators

---

**Document Version**: 1.0
**Last Updated**: 22 November 2025
**Status**: Draft for NANOG [Meeting Number] Submission
