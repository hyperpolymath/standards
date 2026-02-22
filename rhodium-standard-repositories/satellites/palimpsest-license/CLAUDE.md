# CLAUDE.md - Palimpsest License Project

## Project Overview

The **Palimpsest License** is a future-proof, layered licensing framework designed for creative work in the age of AI. It protects creators who work with narrative, trauma, mythology, and symbolic expression while addressing modern challenges like AI training, emotional lineage, and quantum-proof traceability.

**Current Version:** v0.4 (transitioning from v0.3)

**Core Purpose:**
- Protects emotional lineage (e.g., protest songs, cultural narratives)
- Enforces quantum-proof traceability for AI outputs
- Supports DAO governance for collective assets
- Balances remix culture with exploitation prevention

## Project Structure

<!-- Run `just generate-structure` to regenerate this -->
```
palimpsest-license/
├── LICENSES/                    # License texts (v0.3, v0.4)
├── LICENSE_CORE/                # Core agreements (AGI consent, etc.)
├── GUIDES_v0.4/                 # User guides and professional integration
├── TOOLKIT_v0.4/                # Compliance audit tools
├── METADATA_v0.4/               # Machine-readable metadata schemas
├── RESEARCH/                    # Research documents (neurosymbolic, OCanren, etc.)
├── PROJECT_MANAGEMENT/          # Roadmaps, governance, campaigns
├── OUTREACH/                    # Podcast series, communications
├── docs/                        # Additional documentation
├── examples/                    # Usage vignettes and scenarios
├── assets/                      # Visual assets (badges, images)
├── embed/                       # Embeddable HTML/JS snippets
├── press-lobby-kit/             # Advocacy and press materials
├── TOOLS/                       # Validation tools (Haskell, future OCaml)
├── ARCHIVE/                     # Historical versions and experiments
├── ocaml/                       # OCaml implementation (primary)
├── standards/                   # RSR compliance, TPCF, reversibility docs
├── scripts/                     # Shell scripts (setup, hooks, generation)
├── config/                      # Configuration files (Nickel, Deno)
├── .well-known/                 # Web standards (security.txt, robots.txt, etc.)
├── .github/                     # CI/CD workflows
│
├── README.md                    # Project overview (English)
├── README.nl.md                 # Project overview (Dutch)
├── LICENSE.md                   # License pointer
├── CHANGELOG.md                 # Version history
├── CONTRIBUTING.md              # Contribution guidelines
├── CODE_OF_CONDUCT.md           # Community standards
├── GOVERNANCE.md                # Decision-making process
├── MAINTAINERS.md               # Project maintainers
├── SECURITY.md                  # Security policy
├── FUNDING.md                   # Funding strategy
├── CLAUDE.md                    # AI assistant context
│
├── flake.nix                    # Nix reproducible builds
├── guix.scm                     # Guix package definition
├── Containerfile                # Wolfi container definition
├── justfile                     # Build automation recipes
├── package.json                 # Node.js dependencies
└── Makefile                     # Asset conversion targets
```

## Key Principles

### 1. Bilingual Framework
The license operates in **Dutch (primary legal)** and **English (informative)**:
- Dutch version is legally binding in the Netherlands
- English version provides global accessibility
- Both versions must be kept in sync
- See `docs/bilingual-map.md` for clause alignment

### 2. Layered Protection
The license has multiple protection layers:
- **Clause 1.2:** Non-Interpretive (NI) systems require explicit consent
- **Clause 2.3:** Metadata preservation is mandatory
- **Emotional Lineage:** Protects narrative intent and cultural context
- **Quantum-Proof Traceability:** Future-proofs attribution mechanisms

### 3. Governance
Managed by the **Palimpsest Stewardship Council** (7 members):
- 3 Creator Representatives
- 2 Legal Experts (IP, Dutch, or Scottish law)
- 1 Technologist (AI ethics/decentralized systems)
- 1 Cultural Heritage Advocate

Decision-making requires transparent community input and Council votes.

## Development Guidelines

### Version Management
- **Major versions (v1.0, v2.0):** Fundamental changes, 90-day review, 6/7 Council vote
- **Minor versions (v0.3, v0.4):** Protections, clarifications, tech adaptations
- Always update `CHANGELOG.md` when making significant changes
- Keep archived versions in `ARCHIVE/` or versioned directories

### File Naming Conventions
- License files: `palimpsest-v{version}.{lang}.md` (e.g., `palimpsest-v0.4.md`)
- Bilingual files: Use `.nl.md` for Dutch, `.md` or `.en.md` for English
- Guides/Toolkits: Prefix with version when applicable (e.g., `GUIDES_v0.4/`)

### Documentation Standards
- Use clear, accessible language
- Include both legal precision and creative/ethical context
- Provide practical examples in `examples/vignettes/`
- Cross-reference related documents
- Maintain bilingual parity where applicable

### Common Pitfalls (from EXPLAINME_ROOT.md)
- ❌ **Stripping metadata:** Breach of Clause 2.3
- ❌ **Using NI systems without consent:** Breach of Clause 1.2
- Always preserve emotional and symbolic texture in derivatives

## Testing and Validation

### Available Scripts
```bash
# Run tests
./test.sh
npm test

# Setup environment
./setup.sh

# Linting and formatting
npm run lint
npm run format

# Build/validation (Makefile)
make [target]
```

### Before Committing
1. Validate markdown syntax
2. Check bilingual consistency (Dutch ↔ English)
3. Update `CHANGELOG.md` if needed
4. Run `npm run format` to ensure consistent formatting
5. Verify links and cross-references
6. Test any code snippets or integration examples

## Working with Different Components

### License Text (LICENSES/)
- **Critical:** Legal precision is paramount
- Changes require Council review (see GOVERNANCE.md)
- Always maintain versioned copies
- Update corresponding guides when clauses change

### Guides (GUIDES_v0.4/)
- User-facing, accessible language
- Include practical scenarios
- Cross-reference license clauses
- Update when license evolves

### Toolkit (TOOLKIT_v0.4/)
- Practical compliance tools
- Audit templates and checklists
- Integration examples
- Machine-readable formats when possible

### Examples (examples/)
- Creative vignettes illustrate license in action
- Legal scenarios demonstrate edge cases
- Both positive (compliant) and negative (violation) examples
- Keep culturally sensitive and inclusive

### Press/Outreach (press-lobby-kit/)
- Advocacy materials for broader adoption
- Proposals to organizations (Creative Commons, Ubuntu, etc.)
- Social media snippets
- Maintain professional, principled tone

## Technology Stack

- **Documentation:** Markdown and AsciiDoc (.adoc)
- **Primary Implementation:** OCaml (Melange for browser JS, OCanren for logic)
- **Legacy Validation:** Haskell (TOOLS/validation/haskell/, migrating to OCaml)
- **Metadata:** JSON-LD, Dublin Core, Protocol Buffers, VoID RDF
- **Build System:** Nix flake (primary), Guix (alternative), justfile recipes
- **Configuration:** Nickel (.ncl) for validated config schemas
- **Containers:** Wolfi-based (Containerfile) with nerdctl
- **Web:** HTML/SCSS for presentation

## Contributing

### For Claude AI Assistants
When working on this project:
1. **Respect the ethical framework:** This license exists to protect creators
2. **Maintain bilingual integrity:** Dutch and English must align
3. **Preserve emotional context:** Technical changes shouldn't lose narrative depth
4. **Follow governance:** Major changes need community/Council input
5. **Document thoroughly:** This project values transparency
6. **Test comprehensively:** Legal documents must be precise

### Proposal Process
1. Submit via GitHub Issue with "Proposal" template
2. 30-day community discussion minimum
3. Council review (4/7 votes to draft)
4. Working group creates draft
5. Final Council vote (5/7 for ratification)

## Special Considerations

### AI Ethics
This license is deeply concerned with AI's impact on creative work:
- How AI systems use creative works for training
- Attribution in AI-generated content
- Consent for interpretive AI systems
- Future-proofing against quantum computing and AGI

### Cultural Sensitivity
- Protects diaspora narratives, trauma stories, cultural heritage
- Symbolic attribution (not just legal credit)
- Accessibility is core (format diversity, plain language options)
- Indigenous knowledge and collective ownership considerations

### Legal Context
- Primary jurisdiction: Netherlands (Dutch law)
- Comparative analysis with other jurisdictions in `docs/`
- Designed to be symbolically meaningful beyond strict legal enforcement
- Balances legal protection with ethical principles

## Quick Reference

### Key Files to Read First
1. `LICENSES/v0.4/palimpsest-v0.4.md` - Current license text
2. `docs/EXPLAINME_ROOT.md` - Quick orientation
3. `GOVERNANCE.md` - How decisions are made
4. `CONTRIBUTING.md` - How to participate
5. `GUIDES_v0.4/User_Guide.md` - Practical usage

### Common Tasks
- **Add new clause:** Update license text + guides + changelog + bilingual map
- **Fix typo in docs:** Edit, run formatter, verify links
- **Create example:** Add to `examples/vignettes/`, ensure culturally appropriate
- **Update metadata:** Modify JSON-LD in `METADATA_v0.4/` or `metadata/`
- **Add translation:** Create `.{lang}.md` file, update bilingual documentation

## Mission Statement

From the README:
> "This is a license born not just from copyright, but from care. From narrative debt. From cultural refusal. From emotional fidelity."

When working on this project, remember: you're helping protect the voices, stories, and creative expressions of vulnerable communities and individual creators in an age where AI and technology can easily exploit or erase them.

---

**For questions or clarification:** Consult GOVERNANCE.md for decision-making processes, or review the extensive documentation in `docs/` and the version-specific guides.
