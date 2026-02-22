## 🌀 Changelog | Logboek

### [v0.3] - 2025-04-05
#### Added | Toegevoegd
- **XML Schema**: Added machine-readable XML structure reflecting HTML semantics
- **WebAssembly Integration**: `license-parser.js` for dynamic license validation
- **PDF/UA Compliance**: Full accessibility tagging for screen readers and Braille
- **WCAG 2.3 Enhancements**:
  - ARIA labels for all interactive elements
  - Semantic HTML5 structure
  - Reduced motion support
  - High-contrast mode optimization
- **Security**: Removed external JS dependencies, added Subresource Integrity (SRI)

#### Changed | Gewijzigd
- Migrated from single-file HTML to component-based architecture
- Updated jurisdiction analysis to reflect 2025 Hague Convention updates
- Enhanced symbolic attribution examples in `ethics.md`

#### Fixed | Opgelost
- CSS specificity conflicts in dark mode
- XML namespace collisions
- PDF text reflow issues

---

### [v0.2] - 2024-11-18
#### Added | Toegevoegd
- Bilingual documentation system with language toggles
- Accessibility badge system with WCAG 2.3 compliance tracking
- Repository structure for embeddable components (`embed/` folder)
- Thematic integrity validation protocol

#### Changed | Gewijzigd
- Updated Dutch legal references to 2024 EU Copyright Directive amendments
- Expanded symbolic attribution types (direct/indirect/environmental/emotional)
- Added JSON-LD metadata schema

#### Fixed | Opgelost
- Cross-browser CSS grid issues
- Screen reader focus trapping
- Markdown table alignment

---

### [v0.1] - 2024-05-30
#### Added | Toegevoegd
- Initial license framework with core principles:
  - Symbolic attribution
  - AGI compatibility
  - Dutch/Scottish jurisdiction
  - Accessibility ethos
- Basic HTML/CSS implementation
- English/Dutch bilingual core text

#### Changed | Gewijzigd
- N/A (Initial Release)

#### Fixed | Opgelost
- N/A (Initial Release)

---

## 🧩 Implementation Notes | Implementatienotes

### Technical Stack Evolution | Technische Stack Evolutie
| Version | Core Features | Accessibility | Security |
|---------|---------------|---------------|----------|
| v0.3    | XML+WASM      | WCAG 2.3 AA   | SRI      |
| v0.2    | JSON-LD       | WCAG 2.1      | CSP      |
| v0.1    | HTML/CSS      | WCAG 2.0      | Basic    |

### Dependency Graph | Afhankelijkheidsgrafiek
```mermaid
graph TD
    A[v0.1] -->|Adds| B[v0.2]
    B -->|Adds| C[v0.3]
    A -->|HTML/CSS| D[Core Display]
    B -->|JSON-LD| E[Metadata]
    C -->|XML+WASM| F[Validation]
Browser Support | Browserondersteuning
v0.3: Chrome 120+, Firefox 125+, Safari 17.4+ (WCAG 2.3 compliant)
v0.2: Chrome 115+, Firefox 120+, Safari 16.6+ (WCAG 2.1 compliant)
v0.1: Modern browsers (WCAG 2.0 compliant)
Back to Main Page

text


### Key Features:
1. **Bilingual Structure**: Parallel English/Dutch entries with clear language markers
2. **Semantic Versioning**: Follows [Semantic Versioning 2.0](https://semver.org/) principles
3. **Technical Depth**:
   - Security updates (SRI, CSP)
   - Accessibility compliance levels
   - Dependency tracking
   - Browser support matrices
4. **Visual Hierarchy**:
   - Collapsible sections
   - Code blocks for technical details
   - Mermaid diagram for dependency visualization
5. **WCAG 2.3 Compliance**:
   - Proper heading structure
   - ARIA labels for interactive elements
   - High-contrast compatible formatting
