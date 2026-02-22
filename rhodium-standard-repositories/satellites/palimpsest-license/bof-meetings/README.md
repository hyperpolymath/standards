# Birds of a Feather (BoF) Meeting Materials
## Palimpsest License Framework

This directory contains comprehensive materials for organizing and conducting Birds of a Feather sessions at major technical, policy, and rights conferences.

---

## Quick Navigation

- **[Proposals](#proposals)** - Complete BoF session proposals for each conference
- **[Presentations](#presentations)** - Slide decks, handouts, and demo scripts
- **[Stakeholders](#stakeholders)** - Key individuals and organizations to engage
- **[Outreach](#outreach)** - Email templates, mailing list drafts, social media plans
- **[Follow-Up](#follow-up)** - Post-BoF action plan and coordination

---

## Directory Structure

```
bof-meetings/
├── README.md                          # This file
├── follow-up-action-plan.md          # Systematic post-BoF follow-up
├── proposals/                         # Conference-specific proposals
│   ├── ietf-proposal.md              # IETF (Internet standards)
│   ├── ripe-proposal.md              # RIPE (European internet registry)
│   ├── nanog-proposal.md             # NANOG (North American operators)
│   ├── uknof-proposal.md             # UKNOF (UK operators)
│   ├── eurodig-proposal.md           # EuroDIG (European governance)
│   ├── rightscon-proposal.md         # RightsCon (Digital rights)
│   └── cc-summit-proposal.md         # Creative Commons Summit
├── presentations/                     # Slide decks and materials
│   ├── README.md                     # Presentation usage guide
│   ├── master-template.html          # Reveal.js base presentation
│   ├── handout-technical.md          # Technical one-pager
│   ├── handout-general.md            # General/rights one-pager
│   ├── demo-dns-discovery.sh         # Live DNS demo script
│   ├── demo-http-headers.sh          # Live HTTP headers demo
│   └── demo-README.md                # Demo script usage guide
│   └── qa-preparation.md             # Comprehensive Q&A prep
├── stakeholders/                      # Stakeholder identification
│   └── key-stakeholders.md           # Individuals and orgs by conference
└── outreach/                          # Communication materials
    ├── email-invitation-tier1.md     # Personal invitations (VIPs)
    ├── email-templates-collection.md # All email templates
    ├── mailing-list-announcements.md # Conference list drafts
    └── social-media-campaign.md      # Complete social strategy
```

---

## Target Conferences

### Technical and Network Operations

| Conference | Focus | Dates | Proposal |
|------------|-------|-------|----------|
| **IETF** | Internet standards | 3x/year | [ietf-proposal.md](proposals/ietf-proposal.md) |
| **RIPE** | European internet infrastructure | 2x/year | [ripe-proposal.md](proposals/ripe-proposal.md) |
| **NANOG** | North American network operators | 3x/year | [nanog-proposal.md](proposals/nanog-proposal.md) |
| **UKNOF** | UK network operators | 2x/year | [uknof-proposal.md](proposals/uknof-proposal.md) |

**Key Focus**: DNS-based license discovery, HTTP header extensions, CDN metadata preservation, operational deployment

### Policy and Governance

| Conference | Focus | Dates | Proposal |
|------------|-------|-------|----------|
| **EuroDIG** | European internet governance | Annual | [eurodig-proposal.md](proposals/eurodig-proposal.md) |

**Key Focus**: Multi-stakeholder governance, EU regulatory alignment (AI Act, DSA), cultural diversity

### Rights and Justice

| Conference | Focus | Dates | Proposal |
|------------|-------|-------|----------|
| **RightsCon** | Digital rights and human rights | Annual | [rightscon-proposal.md](proposals/rightscon-proposal.md) |

**Key Focus**: Creator rights, marginalized communities, extraction and consent, trauma-informed approaches

### Open Culture and Licensing

| Conference | Focus | Dates | Proposal |
|------------|-------|-------|----------|
| **Creative Commons Summit** | Open licensing and culture | Annual | [cc-summit-proposal.md](proposals/cc-summit-proposal.md) |

**Key Focus**: Complementarity with CC licenses, AI-era licensing evolution, technical interoperability

---

## Using These Materials

### For Conference Organizers

1. **Select relevant proposal** from `proposals/` directory
2. **Customize for specific conference** (dates, numbers, local context)
3. **Submit according to conference deadlines** (typically 6-12 weeks before)
4. **Coordinate with stakeholders** using lists in `stakeholders/`
5. **Prepare presentations** using templates in `presentations/`

### For Presenters

1. **Review proposal** to understand session objectives
2. **Study Q&A preparation** document thoroughly
3. **Customize presentation** template for your conference
4. **Test demonstration scripts** before the session
5. **Prepare handouts** for distribution

### For Outreach Coordinators

1. **Use email templates** from `outreach/email-templates-collection.md`
2. **Send mailing list announcements** 4-6 weeks before conference
3. **Execute social media campaign** per `outreach/social-media-campaign.md`
4. **Track engagement** using stakeholder database
5. **Follow up** using `follow-up-action-plan.md`

---

## Timeline for BoF Organization

### 12 Weeks Before Conference

- [ ] Draft initial proposal
- [ ] Identify co-organizers and speakers
- [ ] Review conference submission requirements
- [ ] Begin stakeholder identification

### 8 Weeks Before

- [ ] Submit proposal to conference
- [ ] Begin Tier 1 outreach (personal invitations)
- [ ] Prepare pre-BoF materials (specs, docs)
- [ ] Schedule pre-BoF coordination calls

### 6 Weeks Before

- [ ] Tier 2 outreach (targeted invitations)
- [ ] Mailing list announcements
- [ ] Pre-BoF webinar (optional)
- [ ] Finalize presentation materials

### 4 Weeks Before

- [ ] Tier 3 outreach (broad announcements)
- [ ] Social media campaign launch
- [ ] Confirm speakers and logistics
- [ ] Test demonstration infrastructure

### 2 Weeks Before

- [ ] Reminder emails to confirmed attendees
- [ ] Final presentation review
- [ ] Print handouts and materials
- [ ] Social media reminder posts

### 1 Week Before

- [ ] Final logistics confirmation
- [ ] Arrival and setup at conference
- [ ] Pre-session organizer meeting
- [ ] Final social media push

### During Conference

- [ ] Deliver BoF session
- [ ] Live social media coverage
- [ ] Collect attendee contacts
- [ ] Take notes and photos
- [ ] Immediate follow-up conversations

### 1 Week After

- [ ] Thank you emails to attendees
- [ ] Publish session notes and recording
- [ ] Blog post and social media summary
- [ ] Identify follow-up actions
- [ ] Schedule working group kick-off

### 1-3 Months After

- [ ] Execute follow-up action plan
- [ ] Form working groups
- [ ] Pilot deployments
- [ ] Standards submissions
- [ ] Prepare for next conferences

---

## Key Messages by Audience

### For Technical Audiences (IETF, RIPE, NANOG, UKNOF)

**Core message**: "We're proposing technical standards for creative content attribution that integrate with existing infrastructure with minimal performance overhead."

**Key points**:
- DNS TXT records: ~150 bytes, <1ms cached latency
- HTTP headers: ~200 bytes, negligible processing time
- Backwards compatible, graceful degradation
- Addresses regulatory requirements (EU AI Act, DSA)

**Tone**: Professional, data-driven, implementation-focused

### For Policy Audiences (EuroDIG)

**Core message**: "Multi-stakeholder governance can balance AI innovation with creator rights through technical standards and participatory decision-making."

**Key points**:
- EU regulatory alignment (AI Act, DSA, Copyright Directive)
- Creator-majority governance (3/7 Council seats)
- Cultural diversity and linguistic support
- Cross-border coordination mechanisms

**Tone**: Collaborative, inclusive, policy-oriented

### For Rights Audiences (RightsCon)

**Core message**: "Marginalized creators deserve technical tools and governance structures that center their voices and protect their work from extraction."

**Key points**:
- Emotional lineage protection (not just legal attribution)
- Trauma-informed design and facilitation
- Privacy-by-design (pseudonymous, collective attribution)
- Coalition building for creator justice

**Tone**: Justice-oriented, empowering, solidarity-focused

### For Open Culture Audiences (CC Summit)

**Core message**: "Palimpsest complements Creative Commons by extending open licensing principles to address AI-specific challenges."

**Key points**:
- Dual-licensing supported (CC + Palimpsest)
- Technical interoperability (JSON-LD schemas)
- Shared values (attribution, sharing, openness)
- Collaboration, not competition

**Tone**: Respectful, collaborative, solution-oriented

---

## Common Questions and Answers

### "Isn't this impossible to enforce?"

**Answer**: Enforcement is multi-layered: technical detection (headers make violations provable), legal recourse (standard IP law), regulatory compliance (EU AI Act requirements), platform policies (Terms of Service), and social pressure (reputation costs). Not perfect, but better than current state.

### "Won't this break the web?"

**Answer**: Backwards compatible design. Old tools ignore new headers. Optional DNS records. Graceful degradation. No breaking changes to existing infrastructure.

### "What about performance?"

**Answer**: Negligible impact. HTTP headers: ~200 bytes, <1ms. DNS queries: <1ms cached, 5-50ms uncached. Comparable to existing security headers (CSP, CORS).

### "Is this anti-AI?"

**Answer**: No—pro-ethical-AI. Interpretive AI (search, recommendations, accessibility) generally permitted. Non-interpretive AI (generative models) requires consent. AI built on consent, not extraction.

### "How does this work with Creative Commons?"

**Answer**: Complementary, not competitive. Dual-license with CC for baseline sharing + Palimpsest for AI-specific protections. Actively coordinating with CC on interoperability.

See [presentations/qa-preparation.md](presentations/qa-preparation.md) for comprehensive Q&A guidance.

---

## Success Metrics

### Immediate (Per BoF Session)

- **Attendance**: 30-80 participants (varies by conference)
- **Engagement**: Active Q&A, substantive feedback
- **Commitments**: Pilot deployments, working group participation
- **Media**: Session coverage, social media reach

### Short-term (3-6 Months)

- **Working groups**: Formed with regular participation
- **Standards**: Internet-Drafts submitted, technical specs published
- **Pilots**: 5-10 deployment partners
- **Adoption**: DNS records and HTTP headers on pilot sites

### Long-term (12+ Months)

- **Standards**: RFCs published, W3C recommendations
- **Regulatory**: Referenced in AI Act guidance, policy documents
- **Platform adoption**: Major CDNs, hosting providers, CMS platforms
- **Community**: Active contributor base, geographic diversity

---

## Resources and Support

### Documentation

- **Project website**: https://palimpsest-license.org
- **GitHub repository**: https://github.com/palimpsest-license
- **Technical specifications**: https://github.com/palimpsest-license/specs
- **Governance model**: [../GOVERNANCE.md](../GOVERNANCE.md)

### Mailing Lists

- **General**: community@palimpsest-license.org
- **Technical**: technical@palimpsest-license.org
- **Policy**: policy@palimpsest-license.org
- **Creators**: creators@palimpsest-license.org

### Contact

**BoF Coordination**: bof-coordination@palimpsest-license.org

**Conference-specific contacts**:
- IETF/RIPE/NANOG/UKNOF: technical@palimpsest-license.org
- EuroDIG: policy@palimpsest-license.org
- RightsCon: rights@palimpsest-license.org
- CC Summit: cc-collaboration@palimpsest-license.org

### Contributing

We welcome contributions to these materials:
- Improvements to proposals and presentations
- Additional conference targets
- Translation into other languages
- Success stories and case studies

Submit via GitHub pull requests or email proposed changes to bof-coordination@palimpsest-license.org.

---

## Acknowledgments

These materials were developed collaboratively by the Palimpsest Stewardship Council and community contributors. Special thanks to:

- Technical experts who reviewed DNS and HTTP proposals
- Conference organizers who provided guidance on submission requirements
- Creator advocates who shaped rights-centered messaging
- Policy experts who mapped regulatory landscape

---

## License

**Proposals and outreach materials**: CC-BY-SA 4.0
**Presentation templates**: CC-BY-SA 4.0
**Demonstration scripts**: MIT License
**Documentation**: CC-BY-SA 4.0

Use, adapt, and share these materials. Attribution appreciated but focused on advancing creator rights, not individual credit.

---

## Version History

**v1.0** (22 November 2025)
- Initial comprehensive BoF materials package
- Seven conference proposals
- Complete outreach and presentation suite
- Stakeholder identification and follow-up planning

---

**Questions?** Contact bof-coordination@palimpsest-license.org

**Good luck with your BoF sessions!** 🎉
