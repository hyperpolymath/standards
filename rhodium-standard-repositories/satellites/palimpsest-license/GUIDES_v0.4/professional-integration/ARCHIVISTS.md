# Palimpsest License: Guide for Archivists

**Audience:** Archivists, records managers, digital preservation specialists
**Version:** 0.4

---

## Why Archivists Should Care

Archives are witnessing an inflection point: the materials being accessioned today include AI-generated content, AI-influenced derivatives, and works whose relationship to AI training is unclear. Traditional archival description wasn't designed for:

- **AI provenance:** How do you describe a work that was "influenced by" thousands of training examples?
- **Consent chains:** What consent was given for works that predate AI?
- **Emotional lineage:** How do you preserve not just the content but its cultural significance?
- **Training use:** Can donated materials be used to train AI systems?

The Palimpsest License provides a framework for these questions.

---

## Integration with Archival Standards

### ISAD(G) Mapping

```
ISAD(G) → Palimpsest

3.1.1 Reference code       → Work identifier (compatible)
3.1.4 Dates                → Creation/consent dates
3.2.1 Name of creator      → Creator + consent status
3.2.2 Admin/Biographical   → Extended for emotional context
3.2.3 Archival history     → Consent chain history
3.2.4 Immediate source     → Donor consent + AI status
3.3.1 Scope and content    → Includes AI provenance
3.4.1 Conditions of access → Palimpsest access terms
3.4.2 Conditions of repro  → AI training restrictions
3.4.3 Language             → Bilingual license notes
3.4.4 Physical char        → Digital provenance
3.4.5 Finding aids         → Consent registry links
3.5.3 Related materials    → Emotional lineage links
```

### EAD Extension

```xml
<ead>
  <archdesc level="item">
    <!-- Standard EAD elements -->
    <did>
      <unittitle>Protest Photograph Collection</unittitle>
      <unitdate>2020</unitdate>
    </did>

    <!-- Palimpsest extension -->
    <palimpsest:licensing xmlns:palimpsest="https://palimpsestlicense.org/ead">
      <palimpsest:version>0.4</palimpsest:version>

      <palimpsest:consent>
        <palimpsest:donor-consent>full</palimpsest:donor-consent>
        <palimpsest:ai-training>prohibited</palimpsest:ai-training>
        <palimpsest:ni-systems>explicit-only</palimpsest:ni-systems>
        <palimpsest:consent-registry>
          https://consent.palimpsestlicense.org/verify/xyz789
        </palimpsest:consent-registry>
      </palimpsest:consent>

      <palimpsest:emotional-lineage>
        <palimpsest:context>social-justice-movement</palimpsest:context>
        <palimpsest:trauma-markers>
          <palimpsest:marker type="collective">protest-suppression</palimpsest:marker>
        </palimpsest:trauma-markers>
        <palimpsest:cultural-significance>
          Photographs from 2020 protests documenting police response.
          Contains sensitive imagery. Community consent obtained for
          archival preservation but not AI training use.
        </palimpsest:cultural-significance>
      </palimpsest:emotional-lineage>
    </palimpsest:licensing>

    <!-- Standard access restrictions enhanced -->
    <accessrestrict>
      <p>Access restricted per Palimpsest License v0.4.
         AI training use prohibited without explicit community consent.
         Research access permitted with ethical review.</p>
    </accessrestrict>

    <userestrict>
      <p>Reproduction for AI/ML training datasets prohibited.
         NI (Non-Interpretive) system use requires consent verification.
         See consent registry for current status.</p>
    </userestrict>
  </archdesc>
</ead>
```

### EAC-CPF for Consent Authorities

```xml
<eac-cpf>
  <control>
    <recordId>consent-authority-001</recordId>
  </control>
  <cpfDescription>
    <identity>
      <entityType>corporateBody</entityType>
      <nameEntry>
        <part>Community Consent Council</part>
      </nameEntry>
    </identity>
    <description>
      <existDates>
        <dateRange>
          <fromDate>2020</fromDate>
        </dateRange>
      </existDates>
      <functions>
        <function>
          <term>AI training consent authority</term>
          <descriptiveNote>
            <p>Authorized to grant or withhold consent for AI training
               use of community-created materials.</p>
          </descriptiveNote>
        </function>
      </functions>
    </description>
  </cpfDescription>
</eac-cpf>
```

---

## Practical Applications

### 1. Accessioning Digital-Born Materials

When accessioning born-digital materials:

```
ACCESSIONING CHECKLIST (PALIMPSEST-ENHANCED)
────────────────────────────────────────────

Standard accessioning:
☐ Provenance verification
☐ Rights documentation
☐ Physical/digital inventory
☐ Condition assessment

Palimpsest additions:
☐ AI provenance inquiry
   • Was AI involved in creation?
   • Is creator aware of AI influence?
   • Are there AI-generated elements?

☐ Consent documentation
   • Does donor consent to archival use?
   • Does donor consent to AI training use?
   • Are there community stakeholders who need consent?

☐ Emotional lineage capture
   • What is the cultural context?
   • Are there trauma markers?
   • What significance should be preserved?

☐ Consent registry entry
   • Register consent status
   • Link to finding aid
   • Set review schedule
```

### 2. Donor Agreements

Enhanced donor agreement language:

```markdown
## AI Training and Computational Use

The donor [agrees/does not agree] to the following uses
of the donated materials:

### AI Training Use
☐ Materials may be used for AI/ML model training
☐ Materials may NOT be used for AI/ML training
☐ Conditional: [specify conditions]

### Non-Interpretive Systems
☐ Materials may be processed by NI systems (search, classification)
☐ Materials may NOT be processed by NI systems
☐ Conditional: [specify conditions]

### Emotional Lineage
The donor attests that these materials:
☐ Contain trauma narratives: [describe]
☐ Have cultural significance: [describe]
☐ Require handling protocols: [describe]

### Consent Review
Consent status should be reviewed:
☐ Every [X] years
☐ Upon specific triggers: [describe]
☐ Never (perpetual consent/prohibition)

This consent is registered at:
[consent registry URL]
```

### 3. Access Policies

AI-era access policy additions:

```markdown
## AI and Computational Access Policy

### Research Access
Researchers may access materials for:
- Human reading/viewing: [standard terms]
- Computational analysis: [enhanced terms]
- AI training: [restricted terms]

### Bulk Download
Bulk download for computational purposes requires:
- Ethical review approval
- Palimpsest compliance verification
- Usage tracking agreement
- No downstream AI training (unless explicitly permitted)

### API Access
Programmatic access restrictions:
- Rate limiting per consent category
- Automatic filtering of AI-prohibited materials
- Consent status included in metadata responses

### Third-Party Platforms
Materials shared with third parties must:
- Preserve Palimpsest metadata
- Propagate consent restrictions
- Enable consent verification
- Report suspected violations
```

---

## Integration with Archival Systems

### ArchivesSpace

```ruby
# ArchivesSpace plugin: palimpsest_consent
# Add to plugins/ directory

class PalimpsestConsent < Plugin
  def self.add_fields(record)
    record.add_field(:palimpsest_ai_consent, :enum,
      values: ['permitted', 'prohibited', 'conditional', 'unknown'])
    record.add_field(:palimpsest_consent_registry_url, :string)
    record.add_field(:palimpsest_emotional_context, :text)
    record.add_field(:palimpsest_trauma_markers, :multi_enum,
      values: ['personal', 'collective', 'intergenerational', 'vicarious'])
  end
end
```

### Archivematica

```python
# Archivematica MCP client script for Palimpsest

def extract_palimpsest_metadata(sip_path):
    """Extract Palimpsest metadata from SIP for METS"""
    metadata = {
        'palimpsest_version': '0.4',
        'ai_consent': extract_consent_status(sip_path),
        'emotional_lineage': extract_emotional_context(sip_path),
        'consent_registry': lookup_consent_registry(sip_path)
    }
    return metadata

def add_palimpsest_to_mets(mets, metadata):
    """Add Palimpsest metadata to METS document"""
    # Add rights extension
    rights = mets.find('.//rights:rightsStatement')
    palimpsest = ET.SubElement(rights, '{https://palimpsestlicense.org}consent')
    palimpsest.set('version', metadata['palimpsest_version'])
    palimpsest.set('ai-training', metadata['ai_consent'])
    return mets
```

### AtoM (Access to Memory)

```php
// AtoM plugin for Palimpsest metadata

class PalimpsestPlugin extends sfPlugin
{
    public function initialize()
    {
        // Add Palimpsest fields to information object
        QubitInformationObject::addField('palimpsest_consent');
        QubitInformationObject::addField('palimpsest_emotional_context');
        QubitInformationObject::addField('palimpsest_trauma_markers');

        // Hook into EAD export
        $this->dispatcher->connect(
            'informationobject.ead_export',
            array($this, 'addPalimpsestToEad')
        );
    }
}
```

---

## Handling Specific Material Types

### Oral Histories

```
ORAL HISTORY PALIMPSEST CONSIDERATIONS
──────────────────────────────────────

Interview consent form additions:
├── AI transcription consent
├── AI training use consent
├── Voice cloning prohibition
├── Emotional lineage statement
└── Community review rights

Metadata requirements:
├── Narrator consent (per use type)
├── Community consent (if applicable)
├── Trauma markers (if disclosed)
├── Cultural context
└── Review schedule

Special handling:
├── Voice data is biometric
├── Stories may have multiple owners
├── Consent may need narrator + community
└── Future use prediction impossible
```

### Photographs

```
PHOTOGRAPH COLLECTION CONSIDERATIONS
────────────────────────────────────

Subject consent layers:
├── Photographer consent
├── Subject consent (if identifiable)
├── Community consent (if collective)
├── Descendant consent (if historical)

AI-specific risks:
├── Facial recognition training
├── Style transfer source
├── Deepfake generation
├── Surveillance datasets

Palimpsest metadata:
├── Creator consent status
├── Subject consent status (if known)
├── AI training prohibition (recommended default)
├── Specific use authorizations
```

### Government Records

```
GOVERNMENT RECORDS CONSIDERATIONS
─────────────────────────────────

Legal frameworks interact:
├── FOI/FOIA requirements
├── Privacy legislation
├── Records retention schedules
├── Crown/government copyright

Palimpsest role:
├── Supplement, not replace legal requirements
├── Mark AI training status where discretion exists
├── Document emotional significance of policy records
├── Track consent where individuals involved
```

---

## Common Questions

### Q: How does this interact with copyright?

Palimpsest is a license that operates within copyright law. It doesn't replace copyright—it specifies how copyrighted works may be used, particularly regarding AI.

### Q: What about works already in our collection?

For existing collections, conduct a consent audit:
1. Review deed of gift/donor agreements
2. Contact donors where possible
3. Apply conservative defaults (no AI training) where consent unclear
4. Document decisions in finding aids

### Q: How do we handle unknown provenance?

Mark as "consent unknown" and apply conservative restrictions. Document the provenance gap and any remediation attempts.

### Q: What about public domain materials?

Public domain materials aren't subject to copyright licensing, but:
- Emotional lineage metadata can still be preserved
- Cultural context documentation is valuable
- Institutions can apply ethical use guidelines
- Metadata itself may be copyrightable

### Q: How do we verify consent status?

Use the consent registry API:
```bash
curl https://consent.palimpsestlicense.org/api/verify/{work-id}
```

---

## Resources

### Standards Bodies
- Society of American Archivists: [saa.org](https://www2.archivists.org/)
- International Council on Archives: [ica.org](https://www.ica.org/)
- Archives and Records Association (UK): [archives.org.uk](https://www.archives.org.uk/)

### Technical Documentation
- EAD3 Schema: [loc.gov/ead](https://www.loc.gov/ead/)
- ISAD(G): [ica.org/standards](https://www.ica.org/en/isadg-general-international-standard-archival-description-second-edition)
- PREMIS: [loc.gov/premis](https://www.loc.gov/standards/premis/)

### Professional Development
- SAA AI/ML Working Group (forming)
- Digital Preservation Coalition AI guidance
- Palimpsest Archivists Working Group (forming)

---

## Contribution

Archivists can contribute by:

1. **Standards mapping:** Help refine EAD/EAC extensions
2. **Use cases:** Document real accessioning scenarios
3. **Policy templates:** Share institutional policies
4. **Tool integration:** Help build ArchivesSpace/Archivematica plugins
5. **Vocabulary:** Contribute to controlled vocabularies

Contact: archives@palimpsestlicense.org (planned)

---

## Document Review Log

| Date | Reviewer | Status | Notes |
|------|----------|--------|-------|
| 2025-12-08 | AI | Current | Initial creation |

**Next Review Due:** 2026-03-08

