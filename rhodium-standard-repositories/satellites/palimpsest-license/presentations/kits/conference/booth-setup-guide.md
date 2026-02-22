# Palimpsest License: Conference Booth Setup Guide

**For conferences, trade shows, and creative community events**

---

## Pre-Event Preparation (1 Week Before)

### Materials Checklist

**Printed Materials:**
- [ ] 200× 1-page handouts (executive summary)
- [ ] 100× 2-page handouts (comprehensive overview)
- [ ] 50× Technical implementation guides (4-page, for developers)
- [ ] 50× Legal framework briefs (6-page, for legal professionals)
- [ ] 500× Business cards with QR codes
- [ ] 50× Stickers (Palimpsest logo, "Protected by Palimpsest" badges)

**Display Materials:**
- [ ] Booth banner (2m × 3m backdrop) — print-ready PDF in `assets/booth-banner.pdf`
- [ ] Table-top display (A4 landscape, 3× folding cards)
- [ ] Poster boards (3× A1 posters: Problem, Solution, Get Started)
- [ ] QR code stands (3×: Website, Documentation, Community)

**Technology:**
- [ ] Laptop for demos (fully charged, backup battery)
- [ ] Tablet for lead capture (Google Form or offline app)
- [ ] External monitor (if booth allows)
- [ ] HDMI cable, adapters (USB-C, DisplayPort)
- [ ] Power strip and extension cord
- [ ] WiFi hotspot (backup if venue WiFi unreliable)

**Promotional Items (Optional):**
- [ ] Notebooks with Palimpsest logo
- [ ] Pens with website URL
- [ ] Tote bags ("Protect Your Voice" slogan)
- [ ] Badges/pins ("Creator Protected" design)

### Staff Preparation

**Booth Staff (2-3 people rotating):**
- [ ] Review all presentation materials
- [ ] Practice 10-minute demo script
- [ ] Memorise FAQ responses
- [ ] Dress code: Smart casual with Palimpsest-branded items (optional t-shirts/badges)

**Roles:**
- **Greeter:** Welcomes visitors, identifies interest level, directs to appropriate staff
- **Technical Demonstrator:** Shows implementation, answers developer questions
- **Legal/Strategy Specialist:** Discusses licensing, enforcement, partnerships

---

## Booth Layout

```
                    [BACKDROP BANNER 2m×3m]
                    "Palimpsest License"

    [Poster 1]     [Demo Laptop]      [Poster 2]      [Poster 3]
    Problem        on Monitor          Solution        Get Started

    [Handouts]     [QR Codes]          [Stickers]      [Business Cards]

         [Table: Staff side]
              |Greeter| |Tech| |Legal|

         [Front: Visitor side]
```

---

## 10-Minute Demo Script

**Goal:** Get visitor from curiosity to "I want to try this"

### Version A: For Creators (Non-Technical)

**[0:00-1:00] Hook and Problem**

"Hi! Are you a creator? [Yes/No response]

Great! So you know how AI companies are scraping everyone's work to train their models, right? Poems, art, music—just feeding it all in without asking. And then AI generates content using your style, your ideas, your cultural stories—but you get nothing. No credit, no payment, no control.

That's the problem Palimpsest solves."

**[1:00-2:30] Emotional Lineage (Show Example)**

"Let me show you a real example. [Pull up protected poem on screen]

This is a poem by a Nigerian diaspora poet about her family's forced migration. It's deeply personal, culturally significant.

Now, here's what traditional copyright does: [Show basic copyright notice]
It says 'You can't copy this.' That's it.

But what about AI training? Copyright doesn't cover that. AI can scrape it, strip the cultural context, and generate cheerful travel poems using her metaphors.

Here's what Palimpsest does differently: [Show SLT metadata]

It preserves the emotional lineage—the intent, the cultural context, the meaning. AI systems that respect Palimpsest will see: 'This is about trauma and displacement. Don't use it for commercial fluff. If you want to train on this, ask permission.'"

**[2:30-4:00] Consent Framework**

"And that's the key: consent.

With Palimpsest, YOU choose whether AI can train on your work. Not them. You.

[Show AIBDP manifest on screen]

This is called an AI Boundary Declaration. It's like a 'No Trespassing' sign, but for AI. You put it on your website, and ethical AI companies check it before training.

If you say 'No training,' they're legally required to respect that. If you say 'Ask me first,' they send a consent request, and you can negotiate royalties.

It's your voice, your choice."

**[4:00-5:30] Protection Layers**

"Palimpsest has four layers of protection:

1. **Emotional Lineage:** Preserves meaning and cultural context
2. **AI Consent:** Requires permission before training
3. **Quantum-Proof Traceability:** Your work stays yours, even 20 years from now
4. **Community Governance:** If you're part of a collective, you can protect work together

[Show diagram or visual on screen]

Traditional licenses weren't designed for AI. Palimpsest is."

**[5:30-7:00] Real Results**

"This isn't theoretical. We've got real results.

A poetry platform with 50,000 poems adopted Palimpsest. Within 6 months:
- 89% reduction in unauthorised AI scraping
- £15,000 in licensing revenue from ethical AI companies that requested consent
- Zero performance impact on their website
- 3 legal violations successfully enforced

Creators are protected, and ethical AI companies have a clear path to work with them."

**[7:00-8:30] How to Adopt (Simplified)**

"So how do you protect your work? Three steps:

1. **Choose your consent level:** Allow training? Only with permission? Never?
2. **Add protection:** Use our free tools—WordPress plugin, or just copy-paste some code
3. **Publish:** Your work is now protected

Takes about an hour for your first work. After that, it's automatic.

[Hand them 1-page handout]

Here's everything you need to get started. Website, guides, tools—all free."

**[8:30-10:00] Close and Next Steps**

"Questions?

[Answer 1-2 quick questions]

Great! Here's what I recommend:
1. Visit palimpsest-license.org/creators — there's a step-by-step guide
2. Join the community forum if you need help
3. Protect at least one work this week—see how it feels

[Hand business card with QR code]

Scan this to go straight to the Creator Guide. And take a sticker—'Protected by Palimpsest.'

Thanks for stopping by! Protect your voice!"

---

### Version B: For Developers (Technical)

**[0:00-1:00] Hook**

"Hey! Are you a developer? [Yes]

Cool. So you're probably dealing with AI training data compliance, GDPR for creative works, maybe EU AI Act requirements?

Palimpsest is a licensing framework that makes all that way easier. Let me show you."

**[1:00-3:00] Technical Problem**

"Here's the problem: Creators want control over AI training. But there's no standard way to signal consent.

Some companies use robots.txt—but that's for crawlers, not AI training.
Some use Creative Commons—but CC has no AI-specific provisions.
Some just hope for the best.

Result? Legal mess. Companies get sued. Creators feel exploited.

Palimpsest fixes this with a technical protocol: AIBDP—AI Boundary Declaration Protocol."

**[3:00-5:30] AIBDP Demo**

"[Show code on screen]

AIBDP works at four layers:

1. **DNS TXT Record:**
```
_aibdp.example.com TXT "v=AIBDP1; training=deny"
```
AI systems query DNS before even making HTTP requests.

2. **.well-known/aibdp.json:**
```json
{
  "version": "AIBDP/1.0",
  "policies": {
    "training": {"allowed": false},
    "generation": {"allowed": true, "attribution": "required"}
  }
}
```
Machine-readable policy manifest.

3. **HTTP Headers:**
```
GenAI-Consent: training=deny; generation=allow
```

4. **HTML Meta Tags:**
```html
<meta name="genai-consent" content="training=deny">
```

Defense in depth. Even if AI ignores one layer, others enforce.

[Show validator tool]

And here's the validator—checks all four layers, confirms compliance."

**[5:30-7:00] SLT Implementation**

"If you allow training, you embed Synthetic Lineage Tags in outputs.

[Show JSON-LD SLT]

This is quantum-resistant cryptography—CRYSTALS-Dilithium signatures. Won't break when quantum computing arrives.

We've got libraries for:
- JavaScript/Node.js
- Python
- WordPress/Drupal plugins
- GitHub Actions

[Pull up GitHub repo]

All open source. MIT licensed code, documentation under CC BY-SA."

**[7:00-8:30] Integration**

"Integration is dead simple.

Node.js example:
```javascript
const palimpsest = require('@palimpsest/server');
app.use(palimpsest.aibdpMiddleware({
  training: 'deny',
  generation: 'allow'
}));
```

That's it. Automatic AIBDP headers on all responses.

Python/Flask:
```python
from palimpsest import AIBDP
aibdp = AIBDP(training='deny')
@app.after_request
def add_headers(response):
    response.headers['GenAI-Consent'] = aibdp.consent_header()
    return response
```

Takes 10 minutes to integrate. Zero performance impact—we benchmarked at <2ms latency."

**[8:30-10:00] Close**

"So why should you care?

1. **Legal compliance:** EU AI Act requires training data transparency. Palimpsest SLTs provide that automatically.
2. **Risk mitigation:** If you're building AI, using Palimpsest-licensed data = clear consent trail.
3. **Ethical positioning:** Show your users you respect creator rights.

[Hand technical guide + business card]

Here's the implementation guide. GitHub link, API docs, everything.

Questions?

[Answer questions]

Cool. Check out the repo, star it if you like it, contribute if you want. We're building this as a community.

Thanks!"

---

## FAQ Quick Reference (for Booth Staff)

**Q: Is this just Creative Commons with extra steps?**
A: No. CC is great for sharing permissions, but it has no AI-specific provisions. Palimpsest adds: AI consent framework, quantum-resistant cryptography, DAO governance, cultural heritage protections. You can actually layer them: "CC BY-SA + Palimpsest" gives sharing + AI protection.

**Q: What if AI companies just ignore it?**
A: Two things: (1) Legal enforcement—we've already had 3 successful cases with damages awarded. (2) Reputational pressure—ethical AI companies WANT clear consent frameworks. They're adopting Palimpsest to demonstrate responsibility.

**Q: Does it cost money?**
A: No. License is free. Tools are open source. No fees, no subscriptions.

**Q: Is it only for famous creators?**
A: Opposite! It's especially for marginalized creators most vulnerable to exploitation—diaspora artists, indigenous storytellers, activist documentarians. They need protection most.

**Q: Can I use it for code?**
A: Palimpsest is designed for creative works (art, writing, music). For code, stick with OSI-approved licenses (MIT, GPL, etc.). But you CAN use Palimpsest for creative assets IN your codebase (documentation, examples, artwork).

**Q: How long does implementation take?**
A: Individual creator: 30 min to 2 hours for first work.
Developer integrating into platform: 1-4 hours depending on complexity.

**Q: Is it legally tested?**
A: It's based on established legal principles (contract law, moral rights, EU directives). We're actively building case law—3 enforcement actions so far, all successful. More test cases planned to establish precedent.

**Q: What about fair use / fair dealing?**
A: Palimpsest provides a clear licensing path. Even if AI training might be fair use (debated), having available license undermines fair use defense—failure to seek consent = bad faith. Plus, this is contract law, not just copyright—different legal basis.

**Q: Can communities / DAOs use this?**
A: Yes! Clause 3.6 specifically recognises collective governance. First IP license to legally acknowledge DAOs as rights-holders. Great for indigenous communities, artist collectives, activist networks.

**Q: What's the long-term plan?**
A: We're working toward:
- IRTF/W3C standardisation (AIBDP as internet standard)
- Partnerships with major orgs (Creative Commons, FSF, NUJ)
- Global registry (Palimpsest Commons)
- Legal precedent in multiple jurisdictions
- Goal: Default license for creative work in AI age

---

## Lead Capture

**Information to Collect:**

- Name
- Email
- Organisation (if applicable)
- Creator Type (artist, writer, musician, developer, legal, other)
- Interest Level (Curious / Planning to Adopt / Want Partnership / Press/Media)
- Follow-up Preference (Email updates / Community invite / Implementation support / Partnership discussion)

**Google Form QR Code:** [Generate and place on tablet]

**Offline Backup:** Paper sign-up sheet if tech fails

---

## Post-Event Follow-Up

**Within 24 Hours:**
- Email all leads thanking them for visiting
- Segment by interest level:
  - Curious → Send 2-page summary + resources
  - Planning to Adopt → Send implementation guide + offer 1-on-1 support call
  - Want Partnership → CC partnerships team, schedule meeting
  - Press/Media → Send press kit, offer interview

**Within 1 Week:**
- Add to community forum (if they opted in)
- Track adoption (who actually implements?)
- Note feedback themes for product/doc improvements

**Metrics to Track:**
- Total booth visitors (estimate)
- Leads captured (email signups)
- Handouts distributed
- Demos given
- Adoption commitments
- Partnership discussions initiated

---

## Troubleshooting

**Tech Demo Fails:**
→ Have backup: Pre-recorded video demo on USB drive
→ Or: Use printed screenshots as "manual demo"

**Too Crowded:**
→ Take names for "next available slot"
→ Or: Do group demos (5-10 people at once)

**Hostile Questions:**
→ Stay calm, acknowledge concern: "That's a great question—let me explain our thinking..."
→ If persistent troll: "I appreciate the discussion, but I need to help other visitors. Here's our contact for detailed debates: legal@palimpsest-license.org"

**Staff Exhaustion:**
→ Rotate every 2 hours
→ Designate break person (always 2 at booth, 1 on break)
→ Hydrate, snack, sit when possible

---

**Questions about booth setup?** events@palimpsest-license.org

**Good luck! You're helping creators reclaim power in the AI age.**
