# Professional Community Integration Guide

**Version:** 0.4
**Purpose:** Integration pathways for archival, cultural, academic, and professional communities

---

## Why Professional Integration Matters

The Palimpsest License exists to protect creative work in an era of AI. But protection without integration is mere aspiration. For this license to fulfill its purpose, it must seamlessly integrate with the workflows, standards, and repositories that professional communities already use.

This guide provides integration pathways for:

- **Memory Institutions:** Archives, libraries, museums
- **Cultural Professionals:** Curators, conservators, anthropologists
- **Legal & Forensic:** IP specialists, digital forensicists
- **Academic:** Researchers, publishers, data managers
- **Creative:** Artists, writers, musicians, performers
- **Technical:** Developers, AI practitioners, standards bodies
- **Social Science:** Psychologists, economists, sociologists

---

## Integration Principles

### 1. Meet Them Where They Are

Each community has established:
- Metadata standards
- Repository systems
- Professional practices
- Ethical frameworks

Palimpsest should integrate with these, not replace them.

### 2. Add Value, Don't Add Burden

Integration should:
- Extend existing workflows naturally
- Provide clear benefits for adoption
- Minimize additional work
- Automate where possible

### 3. Respect Professional Expertise

These communities have deep knowledge about:
- Provenance and authenticity
- Cultural sensitivity
- Long-term preservation
- Ethical handling

Palimpsest should learn from and support this expertise.

---

## Quick Reference: Standards & Repositories

### Metadata Standards to Integrate With

| Standard | Domain | Integration Status |
|----------|--------|-------------------|
| Dublin Core | Universal | Mapping defined |
| METS/MODS | Libraries/Archives | Planned |
| EAD | Archival description | Planned |
| LIDO | Museums | Planned |
| PREMIS | Preservation | Planned |
| ISAD(G) | Archives | Planned |
| CCO/VRA Core | Visual resources | Planned |
| PBCore | Broadcast | Planned |
| CDWA | Art documentation | Planned |
| SPECTRUM | Museum procedures | Planned |
| OSCOLA | Legal citation | Primary |
| SPDX | Software licensing | Submitted |

### Major Repositories & Indices

| Repository | Type | Integration Path |
|------------|------|------------------|
| OCLC WorldCat | Library holdings | MARC record extension |
| Getty Vocabularies | Art terminology | Controlled vocabulary alignment |
| VIAF | Authority records | Creator linking |
| Wikidata | Knowledge base | Property proposal |
| Internet Archive | Web/media archive | Metadata enhancement |
| HathiTrust | Digital library | Rights metadata |
| Europeana | Cultural heritage | EDM mapping |
| DPLA | US digital library | DPLA MAP extension |
| JSTOR/ITHAKA | Academic archive | Rights metadata |
| arXiv | Preprints | License field option |
| Zenodo | Research data | License integration |
| ORCID | Researcher IDs | Consent linking |
| DOI/Handle | Persistent IDs | Metadata association |

### Professional Organizations to Engage

| Organization | Constituency | Engagement Path |
|--------------|--------------|-----------------|
| SAA | Archivists | Standards committee |
| ALA | Librarians | IP/licensing groups |
| AAM | Museums | Ethics committee |
| AIC | Conservators | Documentation standards |
| AAA | Anthropologists | Ethics committee |
| CILIP | UK Librarians | IP working group |
| IFLA | International libraries | Copyright committee |
| ICOM | International museums | Ethics committee |
| ICA | International archives | Standards committee |
| CC | Creative Commons | License compatibility |

---

## Community-Specific Guides

Detailed guides for each professional community:

1. [Archivists Guide](./ARCHIVISTS.md)
2. [Museum Professionals Guide](./MUSEUMS.md)
3. [Librarians Guide](./LIBRARIANS.md)
4. [Conservators Guide](./CONSERVATORS.md)
5. [Digital Forensics Guide](./FORENSICS.md)
6. [Anthropologists Guide](./ANTHROPOLOGISTS.md)
7. [Artists & Creators Guide](./ARTISTS.md)
8. [Researchers & Academics Guide](./RESEARCHERS.md)
9. [Technologists Guide](./TECHNOLOGISTS.md)
10. [Psychologists Guide](./PSYCHOLOGISTS.md)
11. [Economists Guide](./ECONOMISTS.md)
12. [Legal Professionals Guide](./LEGAL.md)

---

## Cross-Cutting Integration Features

### Provenance Chain

All communities care about provenance. Palimpsest provides:

```
PROVENANCE INTEGRATION
──────────────────────

Palimpsest Provenance → Maps to:
├── Archival: Custodial history (ISAD(G) 3.2.3)
├── Museum: Provenance/Ownership (CDWA)
├── Library: Provenance note (MARC 561)
├── Digital: PREMIS event chain
└── Forensic: Chain of custody

The emotional lineage concept extends provenance to include:
├── Emotional transmission
├── Cultural significance
├── Trauma markers
└── Consent history
```

### Rights Management

```
RIGHTS INTEGRATION
──────────────────

Palimpsest Rights → Maps to:
├── Library: MARC 540, 542 (Use/Copyright)
├── Archive: EAD <userestrict>, <accessrestrict>
├── Museum: LIDO rightsWorkWrap
├── Digital: PREMIS rights
├── Research: DataCite Rights
└── Standards: ODRL policies

AI-specific extensions:
├── Training consent (new field)
├── NI-system permissions (new field)
├── Emotional context preservation (new field)
└── Consent registry link (new field)
```

### Controlled Vocabularies

```
VOCABULARY ALIGNMENT
────────────────────

Palimpsest Terms → Align with:
├── Getty AAT: Art & Architecture Thesaurus
├── LCSH: Library of Congress Subject Headings
├── MeSH: Medical Subject Headings (trauma, psychology)
├── UNESCO Thesaurus: Cultural heritage
├── Wikidata: Properties and items
└── FAST: Faceted Application of Subject Terminology

New vocabulary contributions:
├── Emotional lineage taxonomy
├── AI consent categories
├── Cultural sensitivity markers
├── Trauma classification (aligned with clinical standards)
```

---

## Technical Integration Points

### API Endpoints for Professional Tools

```
/api/v1/validate          # Validate Palimpsest metadata
/api/v1/convert/marc      # Convert to/from MARC
/api/v1/convert/dc        # Convert to/from Dublin Core
/api/v1/convert/ead       # Convert to/from EAD
/api/v1/convert/mets      # Convert to/from METS
/api/v1/consent/verify    # Verify consent status
/api/v1/lineage/trace     # Trace emotional lineage
/api/v1/report/compliance # Generate compliance report
```

### Integration Libraries

```
Planned libraries:
├── palimpsest-marc      # MARC record integration
├── palimpsest-oai       # OAI-PMH harvesting support
├── palimpsest-iiif      # IIIF manifest integration
├── palimpsest-archivematica  # Archivematica plugin
├── palimpsest-omeka     # Omeka plugin
├── palimpsest-dspace    # DSpace integration
├── palimpsest-fedora    # Fedora integration
├── palimpsest-collective-access # CA plugin
├── palimpsest-resourcespace    # ResourceSpace plugin
```

---

## Contribution Pathways

How professionals can contribute:

### 1. Standards Mapping

Help map Palimpsest concepts to domain-specific standards:
- Identify equivalent fields
- Propose extensions where needed
- Review mapping accuracy

### 2. Use Case Documentation

Document how Palimpsest applies in real situations:
- Archival accessioning of digital-born content
- Museum acquisition of AI-generated art
- Library handling of trauma narratives
- Forensic analysis of AI-manipulated media

### 3. Vocabulary Development

Contribute to controlled vocabularies:
- Cultural context terms
- Emotional lineage categories
- Professional handling requirements

### 4. Tool Development

Build integrations for professional tools:
- Collection management systems
- Digital preservation platforms
- Citation managers
- Research data platforms

### 5. Policy Templates

Create templates for institutional adoption:
- Collection policies
- Donor agreements
- Access policies
- Deaccession criteria

---

## Implementation Roadmap

### Phase 1: Foundation
- Dublin Core mapping (complete)
- OSCOLA citation integration (complete)
- SPDX submission (in progress)

### Phase 2: Memory Institutions
- MARC21 extension proposal
- EAD/EAC-CPF extension
- LIDO extension
- Archivematica plugin

### Phase 3: Research Infrastructure
- DataCite integration
- ORCID consent linking
- arXiv license option
- Zenodo integration

### Phase 4: Professional Tools
- Omeka/DSpace/Fedora plugins
- CollectiveAccess integration
- ArchivesSpace integration
- Preservica integration

### Phase 5: Discovery Layers
- WorldCat record enhancement
- Europeana EDM mapping
- DPLA integration
- Discovery API endpoints

---

## Resources for Professionals

### Training Materials
- Webinar series (planned)
- Workshop curricula (planned)
- Case study collection (building)
- FAQ by profession (see guides)

### Community Spaces
- Professional working groups (forming)
- Mailing lists (planned)
- Annual summit (aspirational)
- Office hours (TBD)

### Documentation
- Technical specifications
- Policy templates
- Implementation guides
- Best practices

---

## Contact & Contribution

To contribute professional expertise:

1. **Standards mapping:** Open issue on GitHub with domain tag
2. **Use cases:** Submit to case-study collection
3. **Tool integration:** Contact technical leads
4. **Policy review:** Join working group

Professional liaisons (TBD):
- Archives: [position open]
- Libraries: [position open]
- Museums: [position open]
- Research: [position open]
- Legal: [position open]

---

## Document Review Log

| Date | Reviewer | Status | Notes |
|------|----------|--------|-------|
| 2025-12-08 | AI | Current | Initial creation |

**Next Review Due:** 2026-03-08

