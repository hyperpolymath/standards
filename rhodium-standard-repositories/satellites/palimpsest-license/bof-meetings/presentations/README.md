# BoF Presentations

This directory contains reveal.js HTML presentations for Palimpsest License BoF sessions at various conferences.

## Master Template

`master-template.html` provides a comprehensive base presentation that can be customised for specific conferences. The template includes:

- **Problem statement**: AI scraping, metadata loss, consent gaps
- **Solution overview**: Palimpsest License framework and core principles
- **Technical mechanisms**: DNS, HTTP headers, metadata preservation
- **Deployment strategies**: Web servers, CDNs, content platforms
- **Governance model**: Stewardship Council composition and decision-making
- **Use cases**: Diaspora storytelling, protest art, cultural heritage
- **Regulatory alignment**: EU, UK, and international frameworks
- **Next steps and contact information**

## Customisation Guide

### For Technical Conferences (IETF, RIPE, NANOG, UKNOF)

**Emphasise:**
- Technical specifications and RFCs
- Performance benchmarks and latency impact
- Deployment configurations and examples
- Standards alignment and interoperability
- Operational considerations

**Adjust slides:**
1. Expand "Technical Mechanisms" section with detailed specs
2. Add performance data and benchmarks
3. Include more code examples and configurations
4. Add network diagrams and architecture slides
5. Reference relevant RFCs and standards documents

**Tone:** Professional, precise, implementation-focused

### For Policy/Governance Conferences (EuroDIG)

**Emphasise:**
- Multi-stakeholder governance
- European regulatory context (AI Act, DSA)
- Policy alignment and implementation
- Cross-border coordination
- Cultural and linguistic diversity

**Adjust slides:**
1. Expand regulatory alignment section
2. Add multi-stakeholder governance details
3. Include Member State coordination mechanisms
4. Highlight cultural diversity considerations
5. Add policy recommendation slides

**Tone:** Collaborative, policy-oriented, inclusive

### For Rights-Focused Conferences (RightsCon)

**Emphasise:**
- Creator testimonials and lived experience
- Power asymmetries and extraction
- Human rights frameworks (UDHR, UNDRIP)
- Marginalised communities and advocacy
- Justice and solidarity

**Adjust slides:**
1. Lead with creator stories and testimonials
2. Frame as human rights issue
3. Highlight extraction and harm
4. Center marginalised voices
5. Focus on advocacy and action

**Tone:** Justice-oriented, empowering, trauma-informed

### For Open Culture Conferences (Creative Commons Summit)

**Emphasise:**
- Complementarity with CC licenses
- Open culture values and tensions
- Collaboration opportunities
- Technical interoperability
- Community governance

**Adjust slides:**
1. Add CC comparison and complementarity analysis
2. Highlight shared principles
3. Include dual-licensing examples
4. Emphasise collaboration not competition
5. Focus on community consensus

**Tone:** Collaborative, respectful, solution-focused

## Technical Details

### Using the Presentations

1. **Open locally**: Simply open the HTML file in a web browser
2. **Navigation**: Arrow keys, space bar, or on-screen controls
3. **Speaker notes**: Press 'S' to open speaker view
4. **Full screen**: Press 'F'
5. **Overview**: Press 'Esc' or 'O'

### Dependencies

All presentations use CDN-hosted reveal.js libraries (no local installation required):
- reveal.js 4.5.0
- reveal.js highlight plugin (for code syntax highlighting)
- reveal.js notes plugin (for speaker notes)

### Customisation

Edit the HTML directly to:
- Change title and conference name
- Modify slide content
- Add or remove sections
- Adjust styling in the `<style>` block
- Add speaker notes within `<aside class="notes">` tags

### Responsive Design

Presentations automatically adapt to different screen sizes and orientations. Test on:
- Laptop/desktop projector (1920×1080 or 1024×768)
- Tablet (for remote presenting)
- Mobile (for reference)

## Creating Conference-Specific Presentations

### Quick Start

1. Copy `master-template.html` to `[conference]-presentation.html`
2. Update title slide with conference name and date
3. Adjust content based on customisation guide above
4. Add conference-specific organiser information
5. Test presentation in browser

### Adding Speaker Notes

Speaker notes appear in presenter view but not in main presentation:

```html
<section>
    <h2>Slide Title</h2>
    <p>Slide content...</p>
    <aside class="notes">
        Speaker notes here. Mention timing, emphasise key points,
        anticipate questions, etc.
    </aside>
</section>
```

### Performance Optimisation

- Keep images optimised (max 1920px width)
- Limit animated GIFs and videos
- Use fragments sparingly (`.fragment` class)
- Test presentation load time on conference WiFi speeds

## Conference-Specific Considerations

### IETF/RIPE/NANOG/UKNOF
- Include live terminal demonstrations (separate window)
- Prepare code examples in syntax-highlighted format
- Have network diagrams ready
- Test DNS queries and HTTP header examples beforehand

### EuroDIG
- Prepare multilingual slides if needed
- Include charts showing cross-country coordination
- Have regulatory citation references ready

### RightsCon
- Obtain consent for creator testimonials
- Include content warnings if discussing trauma
- Prepare accessible versions (screen reader compatible)
- Have quiet, text-heavy slides for sensitive content

### Creative Commons Summit
- Include CC license icons and branding (with permission)
- Prepare comparative charts (CC vs. Palimpsest)
- Have technical integration examples ready

## Accessibility

All presentations should be accessible:
- **Colour contrast**: Minimum 4.5:1 ratio
- **Alt text**: Describe all images in speaker notes
- **Keyboard navigation**: All features accessible without mouse
- **Screen reader**: Semantic HTML structure
- **Captions**: For any video or audio content

## Printing and PDF Export

To create PDF handouts:

1. Open presentation in Chrome/Chromium
2. Add `?print-pdf` to URL (e.g., `file:///path/to/presentation.html?print-pdf`)
3. Print to PDF using browser print dialogue
4. Result: One slide per page, suitable for handouts

## Version Control

When creating conference-specific versions:
- Use descriptive filenames: `ietf-120-presentation.html`
- Include conference number/year if applicable
- Document changes in commit messages
- Keep master template up to date with improvements

## Contact

For questions about presentations:
- **Technical issues**: [Technical contact]
- **Content questions**: [Content contact]
- **Accessibility**: accessibility@palimpsest-license.org

---

**Last Updated**: 22 November 2025
