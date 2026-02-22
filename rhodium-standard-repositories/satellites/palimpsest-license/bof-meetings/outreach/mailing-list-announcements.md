# Mailing List Announcement Drafts
## Palimpsest License BoF Sessions

Template announcements for various mailing lists. Customize for each venue and list culture/norms.

---

## IETF Mailing Lists

### IETF Discussion List (General Announcement)

**Subject**: BoF Proposal: Creative Content Attribution Infrastructure (IETF [NUMBER])

---

The Palimpsest License project is proposing a Birds of a Feather session at IETF [NUMBER] to discuss technical standards for creative content attribution in the age of AI.

**Problem Statement**:
AI training systems scrape billions of creative works, stripping metadata and ignoring licensing terms. Current internet infrastructure lacks standardized mechanisms for:
- License autodiscovery (DNS-based)
- Consent signalling (HTTP headers for AI usage permissions)
- Attribution preservation (cryptographic chains through derivatives)

**Proposed Solutions**:
1. DNS TXT record standard for license discovery (_license.example.com)
2. HTTP header extensions (X-AI-Consent, X-License-URI, X-Attribution-Chain)
3. Post-quantum cryptographic attribution mechanisms
4. JSON-LD schema extensions for emotional lineage metadata

**BoF Session**:
- **When**: [DAY], [TIME]
- **Where**: [ROOM]
- **Goal**: Assess community interest in forming a working group

**Documents**:
- Internet-Draft (expires [DATE]): https://datatracker.ietf.org/doc/draft-[name]
- Full proposal: https://github.com/palimpsest-license/bof-meetings/proposals/ietf
- Mailing list: palimpsest-license@ietf.org (to be created if sufficient interest)

**Scope for IETF**:
This work intersects with HTTP, DNS, TLS, and WebAuthn working groups. We're seeking feedback on:
- Whether this work belongs in IETF
- Which existing WGs should be coordinated with
- Technical feasibility and standards gaps
- Potential for experimental or standards-track RFCs

Looking forward to productive discussions.

Dr. Sarah Chen
(On behalf of the Palimpsest organizing team)
s.chen@palimpsest-license.org

---

### HTTP Working Group

**Subject**: Creative Content Licensing Headers - Request for Feedback

---

Dear HTTP WG,

I'm writing to request feedback on a proposal for HTTP header extensions related to creative content licensing and AI consent signalling.

**Context**:
Large-scale AI training scrapes web content, often ignoring licensing terms. We're proposing standardized headers to signal creator consent and licensing information.

**Proposed Headers**:

```
X-License-URI: https://example.com/license.html
X-AI-Consent: interpretive-only | non-interpretive-prohibited | unrestricted
X-Attribution-Chain: sha256:[hash]
X-Emotional-Lineage: [semicolon-separated tags]
```

**Questions for HTTP WG**:
1. Should these be X- prefixed or follow different naming convention?
2. Any conflicts with existing header semantics?
3. Caching implications (Vary, Cache-Control interactions)?
4. Security considerations (header injection, etc.)?
5. Is HTTP WG the right venue for standardization?

**More information**:
- Technical specification draft: [GITHUB LINK]
- BoF session at IETF [NUMBER]: [DAY, TIME, ROOM]
- Related DNS work: [LINK TO DNS PROPOSAL]

I'm planning to attend the HTTP WG session at IETF [NUMBER] and would appreciate any preliminary feedback before then.

Thank you for your consideration.

Best regards,
Dr. Sarah Chen
s.chen@palimpsest-license.org

---

## RIPE Mailing Lists

### DNS Working Group

**Subject**: DNS-based License Discovery - Proposal for RIPE [NUMBER]

---

Dear DNS WG,

We're proposing a BoF session at RIPE [NUMBER] on DNS-based license discovery for creative content.

**Problem**:
AI scrapers need a lightweight mechanism to check licensing terms before fetching content. HTTP-only solutions require fetching the resource first. robots.txt is insufficient and frequently ignored.

**Proposed Solution**:
TXT records at `_license.domain.tld` for license autodiscovery:

```
_license.example.com. 3600 IN TXT "palimpsest-v0.4 https://example.com/license"
_license.example.com. 3600 IN TXT "ai-consent=interpretive-only"
```

**Operational Considerations**:
- Minimal zone file impact (~200 bytes per domain)
- Standard DNS caching applies (<1ms cached latency)
- DNSSEC signing recommended for authentication
- Compatible with existing DNS infrastructure

**Feedback Requested**:
- Operational feasibility for DNS operators
- Potential for new RRTYPE vs. TXT record
- Security and abuse considerations
- Integration with ccTLD registries

**BoF Session**:
- **When**: [DATE, TIME]
- **Where**: [ROOM]
- **Materials**: https://github.com/palimpsest-license/bof-meetings

**Pilot Coordination**:
We're seeking 2-3 ccTLD registries for pilot deployment. If your registry is interested, please contact m.vandeberg@palimpsest-license.org.

Looking forward to discussions at RIPE [NUMBER].

Best regards,
Marieke van der Berg
Infrastructure Architect, Palimpsest License
m.vandeberg@palimpsest-license.org

---

## NANOG Mailing List

**Subject**: BoF: Operational Challenges in Content Attribution and License Enforcement

---

Operators,

We're organizing a BoF at NANOG [NUMBER] on practical deployment challenges for creative content attribution infrastructure. If you run hosting, CDN, or ISP networks, this might be relevant to you.

**The Problem** (in operational terms):
- Aggressive AI scrapers consuming bandwidth
- Customer complaints about scraped content in AI outputs
- Regulatory pressure (EU DSA, upcoming AI Act) with no clear technical standards
- License metadata stripped by caching/optimization pipelines

**What We're Proposing**:
- DNS TXT records for license autodiscovery (RFC-able standard)
- HTTP headers for consent signalling (~200 bytes overhead)
- CDN configuration to preserve attribution metadata
- Monitoring tools for compliance validation

**BoF Session**:
- **When**: [DATE, TIME]
- **Where**: [ROOM]
- **Focus**: Performance impact, deployment strategies, operational best practices

**What We're NOT Proposing**:
- Breaking changes to existing infrastructure
- Mandatory implementations
- Solving copyright through technology alone

**Live Demos**:
- DNS queries and caching behaviour
- nginx/Apache/Cloudflare Worker configurations
- Performance benchmarking (latency, bandwidth)

**Pre-BoF Materials**:
- Deployment guide: [GITHUB LINK]
- Performance benchmarks: [LINK]
- Configuration examples: [LINK]

**Pilot Program**:
Seeking 5-10 operators for pilot deployment. Benefits:
- Early input into standards development
- Technical support from our team
- Visibility as ethical AI infrastructure leader

Interested? Email a.rivera@palimpsest-license.org

See you in [CITY],
Alex Rivera
Network Architect, Palimpsest License

---

## EuroDIG Mailing List

**Subject**: Workshop Proposal: Protecting European Creators in the AI Age

---

Dear EuroDIG Community,

We're proposing a multi-stakeholder workshop for EuroDIG 2025 on creative content attribution and governance in the AI era.

**The Challenge**:
European creators—artists, writers, musicians, cultural institutions—face unprecedented extraction of their work by AI systems. The AI Act and DSA mandate transparency and traceability, but lack concrete implementation mechanisms.

**Workshop Theme**:
"Protecting European Creators in the AI Age: Multi-Stakeholder Governance for Content Attribution"

**Multi-Stakeholder Panel** (confirmed):
- **Creator perspective**: [CREATOR REPRESENTATIVE]
- **Platform perspective**: [CDN/HOSTING PROVIDER]
- **Policy perspective**: [EUROPEAN COMMISSION DG CNECT - invited]
- **Civil society perspective**: [EDRi/ACCESS NOW REPRESENTATIVE]
- **Technical perspective**: [DNS/WEB STANDARDS EXPERT]

**Interactive Component**:
Breakout groups on:
1. Technical implementation and standards
2. Governance models and coordination
3. Regulatory alignment across Member States
4. Cultural diversity and fundamental rights

**Expected Outcomes**:
- Multi-stakeholder statement of principles
- Input to AI Act delegated acts
- Coordination mechanism for European implementation
- National-level adaptation templates

**Why EuroDIG?**:
This exemplifies internet governance challenges requiring:
- Cross-border coordination (EU institutions, Member States, global platforms)
- Balance between regulation and innovation
- Cultural and linguistic diversity
- Fundamental rights considerations (privacy, expression, culture)

**Background Materials**:
- Framework overview: https://palimpsest-license.org
- EU regulatory alignment analysis: [LINK]
- Case studies from Member States: [LINK]

**Contact**:
Dr. Klaus Hoffmann (k.hoffmann@palimpsest-license.org)
Elena Popescu (e.popescu@palimpsest-license.org)

Looking forward to collaborative discussions.

Best regards,
The Palimpsest organizing team

---

## RightsCon Mailing List / Community Calls

**Subject**: Session Proposal: From Extraction to Consent - Protecting Marginalised Creators

---

Dear RightsCon Community,

We're proposing a session that centers marginalized creator voices in discussions of AI ethics and creative rights:

**"From Extraction to Consent: Protecting Marginalised Creators' Rights in the AI Training Era"**

**The Issue** (in human terms):
- Diaspora storytellers' cultural narratives scraped without consent
- Trauma survivors' testimony commodified in chatbot training data
- Protest artists' resistance imagery used to train surveillance systems
- Indigenous knowledge extracted and sold back without attribution

This is **cultural extraction at scale**, and it's a human rights issue.

**Session Approach**:
- **Trauma-informed facilitation** (trained facilitators, content warnings)
- **Creator testimonials** (with informed consent)
- **Community breakout groups** by identity/experience:
  - Diaspora and immigrant creators
  - Trauma survivors and therapeutic storytelling
  - Indigenous knowledge keepers
  - Protest artists and activists
  - Marginalised language communities

**Not Just Tech**:
We're realistic: technical solutions can't solve political problems. But they're part of the toolkit alongside:
- Advocacy and organizing
- Regulatory pressure (EU AI Act, etc.)
- Platform accountability
- Collective action

**Accessibility Commitments**:
- Live CART captioning
- Sign language interpretation
- Multilingual support (Spanish, French, Arabic - TBC)
- Trauma-informed practices
- Content warnings
- Quiet room available

**Solidarity and Coalition**:
We're working with:
- Access Now, EFF, EDRi (digital rights)
- Algorithmic Justice League, AI Now (AI accountability)
- Tech Workers Coalition (labor organizing)
- Indigenous-led tech organisations
- Diaspora and cultural heritage groups

**Get Involved**:
- Pre-session consultation: [LINK TO SURVEY]
- Mailing list: creators-justice@palimpsest-license.org
- Background materials: https://palimpsest-license.org/rights

**Facilitators**:
Elena Popescu (diaspora advocate), Makara Chea (trauma storytelling), James MacLeod (technical), Dr. Amara Okafor (AI ethics, Global South)

In solidarity,
The Palimpsest organizing team

**Content Warning**: This session will discuss trauma, extraction, colonialism, and cultural erasure.

---

## Creative Commons Mailing Lists

### CC Community List

**Subject**: Creative Commons Summit Session: Beyond BY-SA - Open Licensing for AI Era

---

Dear CC Community,

We're proposing a session at the Creative Commons Global Summit exploring how open licensing can evolve to address AI-specific challenges whilst preserving CC's core values.

**"Beyond BY-SA: Evolving Open Licensing for the AI Training Era"**

**The Tension**:
Many creators love CC licenses but feel vulnerable to AI extraction:
- Attribution (BY) fails when AI outputs don't credit training data
- Share-Alike (SA) doesn't propagate to AI-generated derivatives
- Non-Commercial (NC) violated by billion-dollar AI companies
- Metadata stripped in AI training pipelines

**Not Criticism - Collaboration**:
We deeply respect CC's 20+ years of success. We're proposing **complementary** approaches:
- CC provides baseline sharing permissions
- Additional mechanisms address AI-specific challenges
- Dual-licensing: CC-BY-SA + [AI-specific protections]

**Session Format**:
- **Panel**: CC perspective, Palimpsest perspective, creator perspective, platform perspective
- **Workshops**: Breakout groups on technical mechanisms, consent layers, commons protection, cultural dimensions
- **Synthesis**: Areas for collaboration and coordination

**Questions We're Exploring**:
- Should CC licenses be extended for AI contexts?
- Can additional layers coexist without fragmenting the licensing landscape?
- What role should CC play in setting standards for AI-era licensing?
- How do we balance openness with creator protection?

**Complementarity Example**:

```html
<!-- Dual licensing -->
<a href="https://creativecommons.org/licenses/by-sa/4.0/">CC BY-SA 4.0</a>
<a href="https://palimpsest-license.org/v0.4">Palimpsest v0.4</a>
<!-- CC for sharing, Palimpsest for AI-specific protections -->
```

**We're Seeking**:
- CC community feedback on our approach
- Technical collaboration on JSON-LD interoperability
- Joint advocacy on AI regulation
- Guidance on dual-licensing best practices

**Resources**:
- Comparison analysis: [CC vs. Palimpsest complementarity]
- Technical interoperability docs: [GitHub link]
- Project overview: https://palimpsest-license.org

**Contact**:
Dr. Sarah Chen (s.chen@palimpsest-license.org) - former CC technical contributor
James MacLeod (j.macleod@palimpsest-license.org) - creator representative

Looking forward to collaborative discussions.

In the spirit of open culture,
The Palimpsest team

---

## Mailing List Etiquette and Best Practices

### Before Posting

- [ ] Review list archives to avoid duplication
- [ ] Check list guidelines and culture
- [ ] Get approval from list moderators if required
- [ ] Prepare for questions and pushback
- [ ] Have links and resources ready

### Posting Guidelines

**Subject lines**:
- Clear and specific
- Include conference name/number
- Use [BoF] or [Proposal] prefix if standard

**Content**:
- Lead with problem statement (why should readers care?)
- Be concise but provide enough detail
- Include clear calls-to-action
- Provide links for more information
- Offer contact information

**Tone**:
- Professional and respectful
- Humble, not promotional
- Acknowledge limitations and uncertainties
- Invite feedback and collaboration

**Length**:
- Target: 200-400 words for announcement
- Provide "more information" links for details
- Use formatting (headers, bullets) for readability

### After Posting

- [ ] Monitor for responses within 24-48 hours
- [ ] Respond promptly to questions
- [ ] Thank people for feedback
- [ ] Incorporate suggestions into proposals
- [ ] Follow up closer to conference date

### Common Mistakes to Avoid

- ❌ Overly long announcements (>500 words)
- ❌ Promotional or sales-y language
- ❌ Dismissing concerns or criticisms
- ❌ Cross-posting to irrelevant lists
- ❌ Not responding to questions
- ❌ Promising things you can't deliver

---

**Last Updated**: 22 November 2025
**Maintained By**: Outreach Coordination Team
**Contact**: outreach@palimpsest-license.org
