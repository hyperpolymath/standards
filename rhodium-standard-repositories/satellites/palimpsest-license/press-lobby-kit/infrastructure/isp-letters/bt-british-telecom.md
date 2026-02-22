# Letter to BT (British Telecom)

**To:** BT Group plc
**Attn:** Philip Jansen, Chief Executive
**Cc:** BT Enterprise, Openreach Leadership
**Subject:** Partnership Opportunity: Infrastructure Support for UK Creative Industries

---

## Dear BT Leadership Team,

As the United Kingdom's largest telecommunications provider and a cornerstone of British digital infrastructure, BT is uniquely positioned to support the UK's thriving creative industries. The Palimpsest License project offers BT an opportunity to demonstrate leadership in protecting British creative talent whilst positioning BT as the network of choice for content creators and cultural institutions.

## Who We Are

The Palimpsest License is a UK-developed framework for protecting creative work in the age of AI:

- **Non-interpretive AI consent:** Control over AI training and interpretation
- **Metadata preservation:** Attribution that survives distribution
- **Emotional lineage:** Protection for cultural narratives and community voices
- **Quantum-proof traceability:** Future-resistant cryptographic attribution
- **DAO governance:** Community-led oversight of creative rights

**Developed in consultation with UK legal experts** and aligned with British values of fairness, cultural preservation, and creative freedom.

## Why This Matters to BT

### 1. UK Creative Industries Support

The UK's creative industries contribute **£116 billion annually** to the economy:

- Film, television, music, publishing, gaming, design
- Museums, galleries, archives, libraries
- Individual creators, artists, photographers, writers
- **These are BT's customers** and deserve infrastructure support

### 2. Regulatory Compliance

UK Government initiatives BT must navigate:

- **Online Safety Act:** Platform responsibilities for content
- **AI Regulation (proposed):** Transparency on training data sources
- **Data Protection Act 2018:** Consent and privacy requirements
- **Copyright, Designs and Patents Act:** Enhanced digital protections expected

**Proactive infrastructure support** demonstrates BT's commitment to responsible innovation.

### 3. Enterprise and Public Sector Opportunities

**Target customers:**

- **BBC, ITV, Channel 4:** Need license-aware content delivery
- **British Library, British Museum:** Require ethical archival infrastructure
- **Universities (via JANET):** Research institutions with licensing concerns
- **Government departments:** Public sector content protection
- **Publishing houses:** Penguin Random House, HarperCollins, others

**BT can offer license-aware connectivity as a premium service.**

### 4. Competitive Differentiation

Distinguish BT from Virgin Media, Vodafone, and international players:

- **"The Network that Protects British Creativity"**
- **Ethical infrastructure positioning**
- **Support for UK cultural institutions**
- **Enterprise service differentiation**

## Technical Implementation

### Phase 1: Transit and Peering (3-6 months)

#### BGP Community Support

Implement license-aware routing in BT's AS5400 and AS2856:

```cisco
! BT IOS configuration example
ip community-list standard PALIMPSEST permit 64512:1000-64512:1010

route-map PRESERVE_LICENSE permit 10
 match community PALIMPSEST
 set community additive
 continue 20

router bgp 5400
 neighbor UPSTREAM_PEER route-map PRESERVE_LICENSE in
 neighbor DOWNSTREAM_PEER route-map PRESERVE_LICENSE out
```

**Impact:** Negligible performance overhead, demonstrates technical leadership

#### IRR Database Updates

Update RIPE database entries for BT routes:

```rpsl
route: 2.0.0.0/8
origin: AS5400
descr: BT Group networks
remarks: license-preservation-enabled: true
remarks: palimpsest-support: v0.4
mnt-by: BT-MNT
source: RIPE
```

### Phase 2: Wholesale and Enterprise Services (6-12 months)

#### Openreach Fiber Integration

Metadata preservation across Openreach network:

- **FTTP/FTTC** installations with license-aware CPE firmware
- **Business broadband** tiers with enhanced license support
- **Leased lines** with SLA guarantees for metadata preservation

#### BT Enterprise Solutions

**"BT Creative Connect" service tier:**

- License-aware routing and transit
- CDN integration with license preservation
- Compliance reporting for enterprise customers
- **Pricing:** +10-20% over standard enterprise connectivity

### Phase 3: Consumer Services (12-24 months)

#### BT Broadband Integration

Consumer-facing features:

- **BT Hub** firmware with license detection and display
- **BT TV** integration (respect content licensing)
- **BT Cloud** with license metadata support
- **MyBT app** showing license information for accessed content

**Marketing:** "Respect for creators built into your connection"

## Business Case

### Revenue Opportunities

**Enterprise services:**

- 500,000 UK businesses × 1% adoption × £50/month = £3M/month (£36M/year)
- Cultural institutions: 100 × £5000/month = £500K/month (£6M/year)
- Government contracts: £10-50M/year

**Wholesale/Openreach:**

- Premium FTTP tier: +£5/month × 500,000 customers = £2.5M/month (£30M/year)
- Business leased lines: Enhanced SLA pricing

**Total conservative estimate:** £50-100M ARR within 3 years

### Strategic Value

Beyond direct revenue:

- **Brand differentiation:** "BT supports British creativity"
- **Government relations:** Demonstrates responsible innovation
- **Enterprise retention:** Unique value proposition for content businesses
- **Public sector contracts:** Preferential positioning for cultural/creative bids

### Cost-Benefit Analysis

**Implementation costs:**

- Phase 1 (BGP): £100K-500K (network engineering, testing)
- Phase 2 (Enterprise): £1-5M (product development, sales enablement)
- Phase 3 (Consumer): £5-10M (CPE firmware, marketing)

**Total investment:** £6-15M over 2-3 years

**Expected returns:** £50-100M ARR = 3-15x ROI within 5 years

## UK-Specific Advantages

### National Infrastructure Provider

BT's unique position:

- **Openreach:** Reaches 97% of UK premises
- **BT Wholesale:** Powers most UK ISPs
- **BT Enterprise:** Serves major institutions
- **BT Global:** International connectivity for UK businesses

**Can implement license awareness end-to-end.**

### Regulatory Alignment

Work with UK Government:

- **DCMS (Digital, Culture, Media & Sport):** Support for creative industries
- **Ofcom:** Potential future requirements for ISPs
- **UK IPO (Intellectual Property Office):** Copyright modernisation
- **ARIA (Advanced Research and Invention Agency):** AI ethics research

### Cultural Institutions Partnerships

Collaborate with:

- **British Library:** Archival content protection
- **BBC:** Public service broadcasting licensing
- **British Museum:** Cultural heritage digitisation
- **Arts Council England:** Support for creative sector
- **UK Music, BPI, Publishers Association:** Industry representation

### Academic Collaboration

Partner with:

- **University College London (UCL):** Centre for Blockchain Technologies
- **Oxford Internet Institute:** Digital ethics research
- **Cambridge Computer Laboratory:** Security and cryptography
- **JANET (Jisc):** UK academic network integration

## Implementation Roadmap

### Q1 2026: Pilot Programme

- Select 10-15 pilot customers (mix of enterprises, institutions)
- Implement BGP community preservation in BT core network
- Deploy license-aware configurations on pilot customer connections
- Gather metrics and feedback

### Q2 2026: Enterprise Launch

- Announce "BT Creative Connect" service tier
- Sales training and go-to-market materials
- Partner with Creative Commons UK, others
- Initial government/public sector pitches

### Q3 2026: Openreach Integration

- Fiber product tier with license support
- CPE firmware updates for business customers
- Wholesale partner enablement
- RIPE database updates complete

### Q4 2026: Public Awareness

- Consumer marketing campaign
- BT TV integration
- MyBT app updates
- Industry awards and recognition

### 2027+: Industry Standard

- Work with Ofcom on potential requirements
- Expand to BT Global international routes
- Partnership with other UK ISPs
- Open-source reference implementations

## Support We Provide

### Technical Resources

- BGP configuration examples for BT infrastructure
- Openreach integration specifications
- CPE firmware requirements
- Testing and validation tools

### Marketing Collaboration

- Co-branded announcements
- "Protected by BT" badges for creative businesses
- Case studies with pilot customers
- Industry event presentations (Internet UK, Digital Leaders Week)

### Business Development

- Customer referrals from UK creative community
- Introductions to cultural institutions
- Government contract support letters
- Legal community endorsements

### Standards Participation

- RIPE working group involvement (BT is already active)
- UK Internet Governance Forum presentations
- Academic research partnerships
- Open-source community leadership

## Competitive Context

### UK ISP Landscape

- **Virgin Media:** Focus on consumer broadband, not enterprise creative
- **Sky Broadband:** Residential focus, limited enterprise
- **Vodafone UK:** Mobile-first strategy
- **TalkTalk:** Budget positioning

**BT can own the "creative industries ISP" category.**

### International Comparison

- **Hurricane Electric (US):** Leading in BGP innovation
- **Level3/Lumen (US):** Enterprise transit focus
- **Deutsche Telekom (DE):** European infrastructure leader

**BT can be the UK and European pioneer.**

## Addressing Potential Concerns

### "Is there really demand?"

**Evidence:**

- £116B UK creative economy
- 2.3M creative sector jobs
- Growing AI concerns from creators
- Government interest in AI regulation

**Market research available** (we can provide)

### "What's the liability risk?"

**Mitigation:**

- BT is technical facilitator, not legal enforcer
- Safe harbour protections apply (similar to hosting safe harbours)
- Customers set policies (like firewall rules)
- Terms of service clarify responsibilities

### "Is this technically feasible?"

**Yes:**

- BGP communities already supported
- Firmware updates are routine
- Openreach already handles complex requirements
- Incremental rollout minimises risk

### "Will customers pay more?"

**Evidence suggests yes:**

- Creative businesses value IP protection
- Institutions require compliance tools
- Premium positioning aligns with BT Enterprise brand
- Pilot will validate pricing assumptions

## Call to Action

We would welcome the opportunity to:

1. **Present detailed proposal** to BT Enterprise and BT Technology leadership (45-60 minute meeting)
2. **Technical deep-dive** with BT network engineering (90 minutes)
3. **Identify pilot customers** from BT's enterprise and public sector base
4. **Discuss partnership terms** (technical, commercial, marketing)

### Contact Information

**Palimpsest License Project**
UK Infrastructure Working Group

- **Email:** uk-isp@palimpsest.example
- **Website:** https://palimpsest.example
- **GitHub:** github.com/palimpsest-license
- **Address:** [UK registered office]

**Key Contacts:**

- **Executive sponsor:** [Name, UK-based]
- **Technical lead:** [Name, network engineering background]
- **Business development:** [Name, enterprise sales experience]
- **Legal/policy:** [Name, UK qualified solicitor]

**Meeting availability:**
- **London office:** Can meet at BT Centre or BT Tower
- **Remote:** Flexible scheduling for any BT location

## Closing Remarks

BT has a proud history of supporting British innovation, from laying the first undersea cables to pioneering digital television. The company's commitment to connecting people, businesses, and communities has made BT synonymous with reliable, responsible infrastructure.

**Now, the UK creative industries need BT's support.**

As AI companies train models on British creative work—often without consent or attribution—creators need infrastructure allies who understand the value of cultural preservation and fair attribution. BT's network touches millions of British homes and businesses daily. That infrastructure can be part of the solution.

By implementing Palimpsest License support, BT can:

- **Support UK creative industries** (£116B sector, 2.3M jobs)
- **Differentiate BT services** in competitive markets
- **Align with UK Government priorities** (creative sector support, AI regulation)
- **Strengthen relationships** with cultural institutions and enterprises
- **Demonstrate technical leadership** in ethical infrastructure
- **Create new revenue streams** (£50-100M ARR potential)

**This is an opportunity for BT to lead—not just in connectivity, but in protecting what makes Britain creative.**

We believe this partnership can make BT the network of choice for the UK creative economy, whilst advancing Internet standards that respect and protect cultural work.

We look forward to your response and the opportunity to work together.

**Yours sincerely,**

The Palimpsest License Stewardship Council
UK Infrastructure Partnership Group

---

## Attachments

1. Technical Specification: BT Network Integration Guide
2. Business Case: UK Market Analysis and Revenue Projections
3. Pilot Programme Proposal: Customer Selection and Success Metrics
4. Legal Analysis: UK Regulatory Context and Safe Harbours
5. Letters of Support: UK Creative Industry Organisations

---

*P.S.: We note BT's commitment to digital inclusion and responsible technology. This partnership extends that commitment to protecting the creators whose work enriches British culture and economy.*

---

*This letter is licensed under the Palimpsest License v0.4. You may share, quote, and reference this document with attribution.*
