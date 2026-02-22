# Palimpsest License Presentation Materials

Comprehensive presentation resources for introducing, explaining, and advocating for the Palimpsest License across different audiences and contexts.

## Directory Structure

```
presentations/
├── reveal-js/              # HTML-based presentations (reveal.js framework)
│   ├── executive-summary/      # 10 slides for decision-makers
│   ├── technical-deep-dive/    # 30 slides for developers/engineers
│   ├── legal-framework/        # 25 slides for lawyers/legal scholars
│   ├── creative-community/     # 20 slides for artists/writers/creators
│   ├── academic-research/      # 35 slides for university presentations
│   └── investor-pitch/         # 15 slides for fundraising/investors
├── beamer/                 # LaTeX Beamer versions (academic contexts)
├── handouts/               # 1-page and 2-page summaries
├── workshops/              # Interactive workshop materials
├── kits/                   # Conference, trade show, training materials
│   ├── conference/
│   ├── trade-show/
│   ├── workshop/
│   └── train-the-trainer/
├── assets/                 # Shared visual assets
├── scripts/                # Automation scripts for presentation generation
└── templates/              # Reusable presentation templates
```

## Available Presentations

### 1. Executive Summary (10 slides, 30 minutes)

**Audience:** CEOs, decision-makers, policy makers, organisational leaders

**Key Messages:**
- Problem: AI exploitation of creative works
- Solution: Palimpsest's layered protection framework
- Real-world impact and beneficiaries
- Technical innovation (AIBDP, SLTs)
- Legal robustness (dual-jurisdiction model)
- Implementation simplicity
- Vision and roadmap
- Call to action for adoption/partnership

**Timing Guide:**
- Slide 1 (Title): 2 min
- Slide 2 (Problem): 3 min
- Slide 3 (Solution): 3 min
- Slide 4 (Core Features): 4 min
- Slide 5 (Beneficiaries): 3 min
- Slide 6 (Technical): 3 min
- Slide 7 (Legal): 3 min
- Slide 8 (Implementation): 2 min
- Slide 9 (Vision): 2 min
- Slide 10 (CTA): 2 min + Q&A
- **Total: 27 min + Q&A**

**How to Use:**
```bash
# Open in browser
open presentations/reveal-js/executive-summary/index.html

# Present mode: Press 'S' for speaker notes
# Navigate: Arrow keys or space
# Overview: Press 'Esc'
```

### 2. Technical Deep Dive (30 slides, 90 minutes)

**Audience:** Software engineers, AI/ML developers, system architects, DevOps

**Key Messages:**
- Complete technical stack overview
- Synthetic Lineage Tags (JSON-LD, XML, NDJSON formats)
- AIBDP implementation (DNS, .well-known, HTTP headers, HTML)
- Quantum-resistant cryptography (CRYSTALS-Dilithium)
- Validation and compliance tools
- Integration guides (Node.js, Python, JavaScript, WordPress, GitHub)
- AI system compliance requirements
- Audit and monitoring infrastructure
- Performance and scalability analysis
- Testing and CI/CD integration
- Security best practices
- Standards interoperability
- Case studies and implementation examples

**Timing: 90 min (3 min per slide average)**

**Code Examples:** All code snippets are production-ready and tested

### 3. Legal Framework (25 slides, 90 minutes)

**Audience:** Solicitors, barristers, legal scholars, IP attorneys, policy makers

**Key Messages:**
- Legal innovation context (copyright inadequacy for AI)
- Dual-jurisdiction model (Dutch law, Scottish courts)
- Core provisions analysis (Clauses 1.2, 2.3)
- Emotional lineage legal basis
- AIBDP legal enforceability
- AGI consent framework
- DAO governance recognition
- Cultural heritage protections
- Enforcement mechanisms and remedies
- Evidentiary standards
- Jurisdictional comparison
- EU AI Act compliance
- Tax and royalty implications
- Legal precedent-building strategy

**Timing: 90 min (includes time for legal discussion/debate)**

**Citations:** All case law and statutory references included in speaker notes

### 4. Creative Community (20 slides, 60 minutes)

**Audience:** Artists, writers, musicians, filmmakers, cultural storytellers

**Key Messages:**
- Story-driven approach (real creator experiences)
- Emotional lineage protection (preserving cultural context)
- Simple adoption process (no technical expertise required)
- Community support and solidarity
- Examples of protected works (diaspora narratives, protest songs, cultural heritage)
- Empowerment and control over AI use
- Revenue opportunities (consent-based licensing)
- DAO and collective governance for communities

**Timing: 60 min (more interactive, discussion-heavy)**

**Accessibility:** Plain language, minimal jargon, emotionally resonant

### 5. Academic Research (35 slides, 120 minutes)

**Audience:** University faculty, graduate students, research institutions

**Key Messages:**
- Research methodology and theoretical foundations
- Comparative legal analysis across jurisdictions
- Technical architecture and cryptographic innovation
- Sociological impact on creative communities
- Economic models (licensing markets, royalty structures)
- Ethical frameworks (AI ethics, cultural heritage)
- Future research directions
- Open questions and collaborative opportunities

**Timing: 120 min (includes extended Q&A and discussion)**

**Academic Rigour:** Full citations, methodological transparency, research agenda

### 6. Investor/Funder Pitch (15 slides, 30 minutes)

**Audience:** Venture capitalists, grant foundations, institutional investors

**Key Messages:**
- Market opportunity (creator economy, AI ethics market)
- Traction and adoption metrics
- Revenue model and financial projections
- Competitive advantage (first-mover, technical moat)
- Team and governance structure
- Use of funds and milestones
- Exit strategy and impact metrics
- Investment thesis

**Timing: 30 min (15 min presentation + 15 min Q&A)**

**Financial Focus:** ROI, market size, scalability, sustainability

## Using the Presentations

### Viewing Presentations

All reveal.js presentations can be opened directly in a web browser:

```bash
# Executive Summary
firefox presentations/reveal-js/executive-summary/index.html

# Technical Deep Dive
chromium presentations/reveal-js/technical-deep-dive/index.html

# Any presentation
open presentations/reveal-js/[presentation-name]/index.html
```

### Keyboard Controls

| Key | Action |
|-----|--------|
| Space / → | Next slide |
| ← | Previous slide |
| S | Speaker notes window |
| F | Full screen |
| Esc | Overview mode |
| B / . | Blackout screen |
| Alt+Click | Zoom into element |

### Presenting Tips

1. **Preparation:**
   - Read all speaker notes thoroughly
   - Familiarise yourself with timing guides
   - Test all demos and code examples
   - Prepare for common questions (see FAQ sections)

2. **Delivery:**
   - Use speaker notes window (press 'S')
   - Monitor time with built-in slide timer
   - Pause for questions at marked slides
   - Adjust pacing based on audience engagement

3. **Customisation:**
   - Edit HTML files directly for custom branding
   - Update visual descriptions with actual images
   - Modify speaker notes for your context
   - Add or remove slides as needed

### Technical Requirements

- **Browser:** Modern browser (Chrome, Firefox, Safari, Edge)
- **Screen Resolution:** 1920×1080 recommended (presentations optimised for this)
- **Internet Connection:** Required for CDN-hosted reveal.js libraries (or download locally)
- **Projector:** HDMI connection, 1080p minimum

## Handout Materials

### 1-Page Summary

**File:** `handouts/executive-summary-1page.pdf`

Covers:
- What is Palimpsest? (2 sentences)
- Key features (4 bullet points)
- Who benefits? (creators, communities, institutions, enterprises)
- How to adopt (3 steps)
- Contact information

**Use Cases:** Conference booth giveaways, email attachments, quick reference

### 2-Page Summary

**File:** `handouts/executive-summary-2page.pdf`

Covers:
- Problem statement (1 paragraph)
- Solution overview (1 paragraph)
- Core features (detailed list with explanations)
- Technical innovation (SLT, AIBDP)
- Legal framework (dual-jurisdiction, enforcement)
- Implementation process
- Case studies (1-2 examples)
- Resources and next steps

**Use Cases:** Follow-up materials, leave-behinds for decision-makers, email attachments after meetings

### Technical Handout (4-Page)

**File:** `handouts/technical-implementation-guide.pdf`

Covers:
- Architecture diagram
- Integration steps for common platforms
- Code snippets (quick reference)
- Validation checklist
- Troubleshooting guide
- Resources and documentation links

**Use Cases:** Developer workshops, hackathons, technical training

### Legal Brief (6-Page)

**File:** `handouts/legal-framework-brief.pdf`

Covers:
- Legal basis and precedents
- Clause-by-clause analysis
- Enforcement mechanisms
- Jurisdictional considerations
- Risk mitigation
- FAQs for legal practitioners

**Use Cases:** Legal consultations, academic seminars, policy briefings

## Workshop Materials

### Half-Day Workshop (4 hours)

**File:** `workshops/half-day-introduction/`

**Structure:**
1. Introduction (30 min): Problem framing, Palimpsest overview
2. Core Concepts (60 min): Emotional lineage, AIBDP, DAO governance
3. Hands-On Session (90 min): Adopt Palimpsest for your work, Generate SLTs, Configure AIBDP
4. Q&A and Next Steps (40 min): Community discussion, resources, action planning

**Materials Included:**
- Facilitator guide with timing
- Participant workbook
- Exercise templates
- Technical setup instructions
- Resource list

### Full-Day Workshop (8 hours)

**File:** `workshops/full-day-comprehensive/`

Adds:
- Deep dive on technical implementation
- Legal framework session with case studies
- Creative exercises (protecting cultural heritage works)
- Group work on consent agreements
- Network building and community formation

### Train-the-Trainer

**File:** `workshops/train-the-trainer/`

For organisations wanting to deliver Palimpsest workshops themselves. Includes:
- Trainer certification process
- Presentation skills guidance
- Technical troubleshooting
- Legal FAQ responses
- Community building strategies
- Materials customisation guide

## Conference and Trade Show Materials

### Conference Booth Kit

**Location:** `kits/conference/`

**Includes:**
- Booth banner designs (print-ready PDF, 2m × 3m)
- Table-top displays (A4, landscape)
- Handout materials (1-page, 2-page summaries)
- QR code cards (link to website, documentation, community)
- Demo script (10-minute booth demonstration)
- FAQ quick reference (for booth staff)
- Swag ideas (stickers, badges, notebooks)

### Trade Show Package

**Location:** `kits/trade-show/`

**Includes:**
- Large-format displays (3m × 2m backdrop)
- Product demo stations (laptop setup guide)
- Lead capture forms
- Follow-up email templates
- Presentation scheduler (15-min lightning talks)
- Meeting space materials

### Academic Conference Package

**Location:** `kits/academic-conference/`

**Includes:**
- Poster presentations (A0, portrait)
- Abstract templates (500-word, 1000-word)
- Slide deck for 20-min conference talk
- Proceedings paper template (LaTeX)
- Networking materials

## LaTeX Beamer Versions

### Academic Template

**File:** `beamer/academic-template.tex`

Professional LaTeX Beamer template optimised for academic contexts:
- Clean, readable design
- University-neutral branding (easily customisable)
- Bibliography integration (BibTeX)
- Mathematical notation support
- Code listing environments
- Theorem/proof blocks

**Compilation:**
```bash
cd presentations/beamer/academic-template
pdflatex academic-template.tex
bibtex academic-template
pdflatex academic-template.tex
pdflatex academic-template.tex
```

**Customisation:**
- Edit `config.tex` for university branding
- Modify `content.tex` for your presentation
- Update `references.bib` for citations

## Accessibility Features

All presentation materials include:

1. **Visual Descriptions:** Detailed descriptions of all visual elements for screen readers
2. **Keyboard Navigation:** Full keyboard control (no mouse required)
3. **High Contrast:** Readable colour schemes with sufficient contrast ratios
4. **Alternative Formats:** Plain text versions of all slides available
5. **Captioning Support:** Speaker notes can be used for live captioning
6. **Font Sizing:** Large, readable fonts throughout

## Customisation Guide

### Branding

To add your organisation's branding:

1. **Colours:**
   ```css
   /* Edit in <style> section of HTML */
   :root {
       --primary-color: #YourColor;
       --secondary-color: #YourColor;
   }
   ```

2. **Logo:**
   ```html
   <!-- Replace logo placeholder -->
   <div class="logo-placeholder">
       <img src="path/to/your/logo.png" alt="Your Organisation">
   </div>
   ```

3. **Contact Information:**
   Update all instances of:
   - `info@palimpsest-license.org`
   - `palimpsest-license.org`
   - Social media handles

### Content Adaptation

For specific contexts:

1. **Local Examples:** Replace case studies with local creator stories
2. **Jurisdiction Focus:** Emphasise legal frameworks relevant to your region
3. **Language Translation:** Translate speaker notes and slide text
4. **Cultural Sensitivity:** Adapt examples for cultural appropriateness

## Translation Workflow

For translating presentations:

1. Extract text from HTML (use `scripts/extract-text.sh`)
2. Translate using professional translation service
3. Replace text in HTML maintaining structure
4. Test rendering and layout
5. Have native speaker review technical/legal terminology
6. Commit translated version to `presentations/[language-code]/`

## Feedback and Improvements

We welcome feedback on presentation materials:

- **Content Suggestions:** presentations@palimpsest-license.org
- **Technical Issues:** GitHub Issues
- **Speaker Stories:** Share your presentation experiences with the community

## License

These presentation materials are licensed under:
- **Code/Technical Examples:** Palimpsest License v0.4
- **Educational Content:** CC BY-SA 4.0
- **Visual Assets:** See `assets/LICENSE.md`

## Version History

- **v0.4.0 (2025-11-22):** Initial comprehensive presentation suite
  - Six reveal.js presentations
  - Supporting materials and handouts
  - LaTeX Beamer templates
  - Workshop and conference kits

## Credits

**Primary Authors:**
- Presentation Content: Palimpsest Stewardship Council
- Technical Examples: Development Team Contributors
- Legal Analysis: Legal Expert Members
- Visual Concepts: Community Contributors

**Design:**
- reveal.js framework: Hakim El Hattab
- LaTeX Beamer: Till Tantau and contributors
- Custom styling: Palimpsest Design Team

## Getting Help

- **General Questions:** info@palimpsest-license.org
- **Technical Support:** developers@palimpsest-license.org
- **Speaker Support:** presentations@palimpsest-license.org
- **Community Forum:** community.palimpsest-license.org

---

**Remember:** These presentations represent real creators, real cultural heritage, and real protections. Present with respect, authenticity, and conviction.
