# Palimpsest License: Half-Day Introduction Workshop

**Facilitator Guide**

---

## Workshop Overview

**Duration:** 4 hours (with breaks)

**Audience:** Creators, cultural workers, community organisations (15-30 participants)

**Objectives:**
1. Understand AI exploitation risks to creative work
2. Learn Palimpsest License core concepts
3. Adopt Palimpsest for at least one creative work (hands-on)
4. Join the Palimpsest creator community

**Prerequisites:** None (no technical expertise required)

---

## Required Materials

### For Facilitator:
- Laptop with projector (presentations/reveal-js/creative-community/index.html)
- Whiteboard or flipchart
- Participant workbooks (printed, 1 per person)
- Handouts (1-page and 2-page summaries)
- USB drives with offline resources (for low-connectivity situations)
- Example works with Palimpsest protection (show real implementations)

### For Participants:
- Laptop or tablet (optional but recommended for hands-on session)
- One creative work to protect (digital file: poem, image, audio, etc.)
- Notebook and pen

### Room Setup:
- Tables arranged in small groups (4-5 people per table)
- Power outlets accessible
- WiFi available (provide credentials)
- Break area with refreshments

---

## Detailed Schedule

### Session 1: Introduction and Problem Framing (30 minutes)

**9:00-9:15 — Welcome and Icebreaker (15 min)**

*Facilitator Script:*

"Good morning! Thank you for joining this workshop on protecting your creative work in the age of AI. Before we dive in, let's go around the room. Please share:
- Your name
- What type of creative work you do
- One thing you're worried about regarding AI and creativity"

*Tips:*
- Keep icebreaker moving (1 min per person max)
- Note common themes for later discussion
- Validate concerns—these fears are real and justified

**9:15-9:30 — The Problem: AI Exploitation (15 min)**

*Presentation:* Slides 1-3 from Creative Community deck

*Key Points:*
- AI companies scraping millions of creative works without consent
- Training data becomes "laundered"—attribution lost, context stripped
- Current copyright inadequate for AI-era challenges
- Real harm to real creators (share Amara's story from slides)

*Interactive Element:*
"Has anyone experienced or heard of AI using creative work without permission? [Allow 2-3 brief shares]"

*Facilitator Notes:*
- Avoid technical jargon
- Focus on emotional and cultural impact, not just legal/technical issues
- Validate participants' experiences and concerns

---

### Session 2: Core Concepts (60 minutes)

**9:30-9:50 — Emotional Lineage (20 min)**

*Presentation:* Slides 5-7 from Creative Community deck

*Definition:*
"Emotional lineage is the cultural and narrative DNA of your work—the intent, the context, the meaning that makes it yours."

*Exercise: Mapping Your Emotional Lineage (10 min)*

1. Participants choose one of their creative works
2. In workbook, answer:
   - What was your intent in creating this?
   - What cultural context does it carry?
   - What symbols or metaphors are important?
   - How should someone experience this emotionally?
3. Pair-share (5 min)

*Facilitator Tips:*
- Walk around and offer encouragement
- Share your own example if participants hesitate
- Emphasize: There are no wrong answers—your intent is valid

**9:50-10:10 — AI Consent Framework (20 min)**

*Presentation:* Slides 8-10

*Key Concept:*
"You get to choose whether AI trains on your work. Consent = your power."

*Discussion:*
- What would you require from an AI company to grant consent?
- Would you ever say yes? Under what conditions?
- What would automatic "no" look like for your work?

*Capture on Flipchart:*
Create three columns: "Always Allow", "Consent Required", "Never Allow"
Note participants' criteria for each

**10:10-10:30 — DAO Governance & Cultural Heritage (20 min)**

*Presentation:* Slides 11-13

*Concept:*
"You don't have to protect your work alone. Communities can govern collectively."

*Group Exercise: Collective Protection (10 min)*

1. Divide into small groups (4-5 people)
2. Imagine you're a community with shared cultural heritage
3. Discuss:
   - What works would you protect collectively?
   - Who would make consent decisions?
   - How would you share benefits (royalties)?
4. Brief report-backs (2 min per group)

*Facilitator Notes:*
- Some participants may be from actual communities (Indigenous, diaspora)—invite their perspectives
- Explain DAO concept simply: "A group that makes decisions together, often using smart contracts"
- Emphasize: Palimpsest supports both individual and collective protection

**10:30-10:45 — BREAK (15 min)**

*During break:*
- Ensure all laptops/tablets charged
- Verify WiFi working
- Troubleshoot any technical issues for hands-on session

---

### Session 3: Hands-On Implementation (90 minutes)

**10:45-11:00 — Technical Overview (15 min)**

*Presentation:* Slides 14-16

*Simplified Explanation:*
"Palimpsest uses three layers of protection:
1. **Metadata:** Information embedded in your file (like EXIF in photos)
2. **AIBDP:** A signal to AI systems saying 'check my license first!'
3. **Cryptographic Signature:** Mathematical proof it's yours and unchanged"

*Show Real Example:*
- Display a work with Palimpsest protection
- Show the metadata (use online validator: palimpsest-license.org/tools/validator)
- Demonstrate how AI systems check AIBDP

**11:00-12:00 — Hands-On: Protect Your Work (60 min)**

*Activity: Adopt Palimpsest for One Work*

**Option A: Web-Based Works (websites, portfolios, blogs)**

1. **Choose Consent Level (5 min)**
   - Workbook: Fill out consent questionnaire
   - Decide: training allowed? generation allowed? conditions?

2. **Generate AIBDP Manifest (10 min)**
   - Go to: palimpsest-license.org/tools/aibdp-generator
   - Input consent preferences
   - Download aibdp.json file
   - (If using WordPress: Install plugin instead)

3. **Embed in Website (20 min)**
   - Upload aibdp.json to `.well-known/` directory
   - Add HTML meta tags to pages (copy-paste from tool)
   - Verify with validator

4. **Test and Celebrate (5 min)**
   - Run validator: palimpsest-license.org/tools/validator
   - Take screenshot of success message
   - Share with group!

**Option B: Standalone Files (documents, images, audio, video)**

1. **Choose Consent Level (5 min)** [Same as above]

2. **Generate Synthetic Lineage Tag (15 min)**
   - Go to: palimpsest-license.org/tools/slt-generator
   - Fill in work details:
     - Title, creator name
     - Emotional lineage (from earlier exercise!)
     - Consent preferences
   - Download SLT (JSON-LD file)

3. **Embed in File (15 min)**
   - **For images:** Use metadata editor (or upload to tool)
   - **For documents:** Embed JSON-LD in header/footer
   - **For audio/video:** Add to ID3 tags or container metadata
   - Facilitator assists with platform-specific steps

4. **Verify and Celebrate (5 min)** [Same as above]

**Facilitator Role During Hands-On:**
- Circulate among participants
- Troubleshoot technical issues
- Answer questions about consent choices
- Celebrate each success ("You just future-proofed your work!")
- Assist those without laptops (pair with someone who has one)

**Common Issues:**
- **No website access:** Use test sandbox (palimpsest-license.org/sandbox)
- **Connectivity issues:** Use offline tools (USB drives)
- **Platform not supported:** Document in "feature request" list for later

**12:00-12:15 — Share and Celebrate (15 min)**

*Gallery Walk:*
- Each participant briefly shows their protected work
- What did you protect?
- How did you set consent?
- How do you feel now vs. beginning of workshop?

*Facilitator:*
"You are now part of the Palimpsest community—creators who refuse to let AI erase our voices!"

---

### Session 4: Q&A and Next Steps (40 minutes)

**12:15-12:35 — Q&A (20 min)**

*Common Questions:*

Q: "What if AI companies ignore Palimpsest?"
A: "You have legal recourse—contract breach, damages, injunctions. Plus, monitoring tools log violations. Already 3 successful enforcements!"

Q: "Is this only for famous creators?"
A: "No! It's especially for marginalized, diaspora, and cultural creators most vulnerable to exploitation."

Q: "What if I change my mind about consent?"
A: "You can revoke consent (Clause 5) if terms are breached. Update your AIBDP manifest to change policies."

Q: "How much does this cost?"
A: "Palimpsest License is free. Tools are open-source. No fees."

Q: "Can I use this with Creative Commons?"
A: "Yes! You can layer: 'CC BY-SA + Palimpsest' gives sharing freedoms + AI protections."

*Facilitator Tips:*
- Write questions on flipchart if you don't know answer
- "Great question—let me find out and email everyone"
- Encourage peer answers: "Has anyone dealt with this?"

**12:35-12:55 — Community and Resources (20 min)**

*Next Steps:*

1. **Join Community Forum:** community.palimpsest-license.org
   - Share protected works
   - Get technical help
   - Participate in governance (vote for Council members!)

2. **Protect More Works:**
   - Apply to existing portfolio (bulk tools available)
   - Make Palimpsest default for new works

3. **Spread the Word:**
   - Tell fellow creators
   - Share on social media (#PalimpsestProtected)
   - Write about your experience

4. **Get Involved:**
   - Volunteer for community support
   - Share your story for case studies
   - Advocate for adoption in your sector

*Resource Handout:*
- 1-page and 2-page summaries
- Cheat sheet: Quick reference for consent decisions
- Community directory: Who to contact for what
- Event calendar: Upcoming workshops, webinars

**12:55-1:00 — Closing Circle (5 min)**

*Final Round:*
"One word: How do you feel about AI and your creative work now?"

*Facilitator Closing:*
"Thank you for your courage in protecting your work. Remember: Your stories matter. Your voice deserves to be heard on your terms. The Palimpsest community stands with you. Stay in touch!"

---

## Post-Workshop Follow-Up

### Within 24 Hours:
- Email participants:
  - Thank you message
  - Link to recording (if permitted and recorded)
  - Resource packet (PDFs of all materials)
  - Community forum invitation
  - Answers to questions noted during workshop

### Within 1 Week:
- Check-in survey:
  - Did you protect additional works?
  - What support do you need?
  - Would you recommend this workshop?
- Share survey results with Council
- Connect highly engaged participants with community roles

### Ongoing:
- Invite to future events
- Feature success stories (with permission)
- Provide advanced workshops for interested participants

---

## Troubleshooting Guide

**Participant says: "I don't understand the technical stuff"**
→ "You don't need to! The tools do the technical work. You just make creative decisions—like choosing a filter on Instagram."

**Participant says: "AI companies will ignore this anyway"**
→ "Some might, and we're building enforcement. But many AI companies WANT clear consent frameworks. We've already seen ethical AI companies using Palimpsest to demonstrate responsibility."

**Participant says: "I want to share my work freely, not restrict it"**
→ "Palimpsest isn't about restriction—it's about consent. You can allow AI training if you want! But YOU choose, not them."

**Technical failure during hands-on:**
→ Have offline fallback: Paper-based SLT template, manual metadata embedding guide

**Emotional participant (trauma narratives):**
→ Acknowledge: "Your story deserves protection. That's exactly why Palimpsest exists—to honor the emotional weight of your work."

**Skeptical participant:**
→ Invite curiosity: "What would convinced you this is worth trying?" Address specific concerns. Offer to follow up individually.

---

## Adaptation Notes

**For Different Audiences:**

**Indigenous Communities:**
- Emphasize collective governance and cultural heritage protections
- Invite Elders to co-facilitate
- Adapt examples to specific cultural context
- Allow longer time for community discussion and consensus

**Youth/Students:**
- More interactive exercises, less lecture
- Use contemporary examples (TikTok, Instagram, YouTube)
- Gamify: "Level up your creative protection!"
- Connect to social justice frameworks

**Professional Artists:**
- Focus on economic benefits (licensing revenue)
- Legal enforceability and precedent
- Integration with existing copyright/contracts
- Tax and financial implications

**Activist Groups:**
- Emphasize protecting protest art, documentation
- Preventing co-optation and defanging
- Collective action and solidarity
- Rapid response to violations

**For Different Durations:**

**2-Hour Version (Condensed):**
- Skip Session 2 exercises (present concepts only)
- Hands-on: Choose one option (web OR file)
- Limit Q&A to 10 minutes

**Full-Day Version (8 hours):**
- Add: Deep dive on legal framework (guest lawyer)
- Add: Advanced technical session (SLT customization)
- Add: Group project (protect a collective work together)
- Add: Strategy session (advocacy in your sector)

---

## Facilitator Self-Care

This work is emotional. You're helping creators protect deeply personal, often traumatic, cultural narratives.

**Remember:**
- Take breaks when you need them
- Debrief with co-facilitators after workshop
- Don't absorb participants' pain—hold space, don't carry
- Celebrate the wins: Every protected work is a victory

**Resources for Facilitators:**
- Facilitator community: facilitators@palimpsest-license.org
- Monthly peer support calls
- Advanced facilitation training (quarterly)

---

## Feedback and Improvement

After each workshop:
1. Participant survey (Google Form link in resources)
2. Facilitator reflection (what worked? what didn't?)
3. Share insights with Council (governance@palimpsest-license.org)

Palimpsest workshops evolve based on facilitator and participant feedback. You're not just delivering—you're co-creating this movement.

---

**Questions?** facilitators@palimpsest-license.org

**This guide is licensed CC BY-SA 4.0 — adapt and improve it!**

---

*Thank you for facilitating. Your work helps creators reclaim power in the AI age.*
