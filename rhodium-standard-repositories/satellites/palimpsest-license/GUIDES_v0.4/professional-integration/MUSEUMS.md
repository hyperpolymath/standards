# Palimpsest License: Guide for Museum Professionals

**Audience:** Curators, registrars, collection managers, museum educators
**Version:** 0.4

---

## The Museum AI Challenge

Museums hold cultural memory. In the AI era, this creates new responsibilities:

- **Collection digitization** creates training data pools
- **Open access initiatives** conflict with AI training concerns
- **AI-generated interpretations** of collections
- **Deepfakes and reproductions** of museum objects
- **Cultural heritage** extraction by AI systems

---

## Integration with Museum Standards

### LIDO (Lightweight Information Describing Objects)

```xml
<lido:lido xmlns:lido="http://www.lido-schema.org"
           xmlns:palimpsest="https://palimpsestlicense.org/lido">

  <lido:administrativeMetadata>
    <lido:rightsWorkWrap>
      <lido:rightsWorkSet>
        <lido:rightsType>
          <lido:term>Palimpsest License v0.4</lido:term>
        </lido:rightsType>

        <!-- Palimpsest extension -->
        <palimpsest:consent>
          <palimpsest:aiTraining>prohibited</palimpsest:aiTraining>
          <palimpsest:niSystems>restricted</palimpsest:niSystems>
          <palimpsest:commercialAI>prohibited</palimpsest:commercialAI>
          <palimpsest:researchAI>conditional</palimpsest:researchAI>
        </palimpsest:consent>

        <palimpsest:emotionalLineage>
          <palimpsest:culturalContext>
            Sacred object from [community]. Reproduction restrictions
            apply per community protocols.
          </palimpsest:culturalContext>
          <palimpsest:traumaMarkers>
            <palimpsest:marker>colonial-acquisition</palimpsest:marker>
          </palimpsest:traumaMarkers>
        </palimpsest:emotionalLineage>
      </lido:rightsWorkSet>
    </lido:rightsWorkWrap>
  </lido:administrativeMetadata>
</lido:lido>
```

### SPECTRUM (Museum Procedures)

Integration with SPECTRUM procedures:

| SPECTRUM Procedure | Palimpsest Integration |
|--------------------|----------------------|
| Object Entry | Capture AI consent terms from depositor |
| Acquisition | Document AI rights in acquisition agreement |
| Location & Movement | Track digital copies and AI exposure |
| Cataloguing | Include Palimpsest metadata |
| Rights Management | AI-specific rights categories |
| Use of Collections | AI use as distinct use type |
| Reproduction | AI reproduction restrictions |
| Deaccession | AI rights implications |

### CDWA (Categories for the Description of Works of Art)

```
CDWA → Palimpsest Mapping
───────────────────────

Rights/Reproduction
├── Rights Type → Palimpsest license type
├── Rights Date → Consent date
├── Rights Holder → Consent authority
├── Credit Line → Attribution requirements
└── (NEW) AI Rights → Training consent, NI consent

Cultural Heritage Status
├── Cultural Context → Emotional lineage context
├── Associated Concepts → Trauma markers
└── (NEW) Community Consent → Required for AI use
```

---

## Collection Management Integration

### CollectiveAccess

```php
// CollectiveAccess Palimpsest plugin

class PalimpsestPlugin extends BasePlugin {

    public function getPluginInfo() {
        return array(
            'name' => 'Palimpsest License',
            'description' => 'AI consent management for collections',
            'version' => '0.4'
        );
    }

    public function registerElements() {
        return array(
            'palimpsest_ai_consent' => array(
                'type' => 'list',
                'options' => array(
                    'permitted' => 'AI Training Permitted',
                    'prohibited' => 'AI Training Prohibited',
                    'conditional' => 'Conditional',
                    'community' => 'Community Consent Required',
                    'unknown' => 'Unknown'
                )
            ),
            'palimpsest_emotional_context' => array(
                'type' => 'text',
                'label' => 'Emotional/Cultural Context'
            ),
            'palimpsest_consent_registry' => array(
                'type' => 'url',
                'label' => 'Consent Registry Link'
            )
        );
    }
}
```

### TMS (The Museum System)

Custom field additions:
```
Object Record Extensions:
├── AI Training Rights (dropdown)
├── Palimpsest License Version
├── Consent Registry URL
├── Cultural Sensitivity Flag
├── Community Consent Authority
└── Emotional Lineage Notes
```

### Axiell EMu

```
EMu Module Extensions
─────────────────────

Rights Module:
├── Rights Type → Add "Palimpsest" option
├── Rights Sub-type → AI Training, NI Systems, etc.
├── Rights Period → Consent validity period
├── Rights Notes → Emotional lineage statement

Object Module:
├── Add "AI Consent Status" field
├── Add "Cultural Protocol" link
├── Add "Consent Registry" reference
```

---

## Policy Templates

### Collection AI Policy

```markdown
# [Museum Name] AI and Collections Policy

## Purpose
This policy governs the use of our collection in AI/ML contexts.

## Principles
1. Creator/community consent is paramount
2. Cultural context must be preserved
3. Commercial AI use requires explicit permission
4. Research AI use may be conditionally permitted
5. Open access ≠ open for AI training

## Categories

### Category A: AI Use Prohibited
- Objects with explicit donor restrictions
- Culturally sensitive materials
- Works with unresolved provenance
- Living artist works without consent
- Trauma-related materials

### Category B: Conditional AI Use
- Research use only with ethics approval
- Non-commercial use only
- Attribution required in any outputs
- Results must be shared with museum

### Category C: AI Use Permitted
- Public domain with clear provenance
- Explicit AI permissions from rights holders
- Museum-created content (internal policy applies)

## Implementation
All objects must have AI consent status recorded in [CMS].
Status reviewed annually or upon rights holder request.

## Metadata
All digital surrogates include Palimpsest metadata indicating:
- AI training consent status
- NI system consent status
- Cultural context markers
- Consent verification link
```

### Donor Agreement Additions

```markdown
## AI and Machine Learning Use

We ask donors to specify their preferences regarding AI:

### AI Training Use
The donated materials [may / may not / conditionally may]
be used to train artificial intelligence or machine learning
systems.

Conditions (if applicable):
□ Non-commercial only
□ Research only
□ Attribution required
□ Community review required
□ Other: _______________

### Non-Interpretive Systems
The donated materials [may / may not] be processed by
non-interpretive AI systems (search, classification, etc.).

### Commercial AI Products
The donated materials [may / may not] contribute to
commercial AI products or services.

### Review
This consent should be reviewed:
□ Never (permanent)
□ Every [X] years
□ If technology changes significantly
□ At donor's request

### Registry
Consent status will be registered at:
[Palimpsest consent registry URL]

Signature: _______________ Date: _______________
```

---

## IIIF Integration

For digital collections using IIIF:

### Manifest Extension

```json
{
  "@context": [
    "https://iiif.io/api/presentation/3/context.json",
    "https://palimpsestlicense.org/iiif/context.json"
  ],
  "id": "https://museum.org/iiif/object123/manifest",
  "type": "Manifest",
  "label": { "en": ["Portrait of..." ] },

  "rights": "https://palimpsestlicense.org/v0.4",

  "metadata": [
    {
      "label": { "en": ["AI Training Status"] },
      "value": { "en": ["Prohibited - Artist has not consented"] }
    },
    {
      "label": { "en": ["Consent Verification"] },
      "value": {
        "en": ["<a href='https://consent.palimpsestlicense.org/verify/xyz'>Verify</a>"]
      }
    }
  ],

  "requiredStatement": {
    "label": { "en": ["License"] },
    "value": {
      "en": ["Palimpsest License v0.4. AI training use prohibited."]
    }
  },

  "palimpsest:consent": {
    "aiTraining": "prohibited",
    "niSystems": "permitted",
    "culturalContext": "Contemporary work by living artist.",
    "consentRegistry": "https://consent.palimpsestlicense.org/verify/xyz"
  }
}
```

---

## Specific Considerations

### Repatriation and AI

```
REPATRIATION-AI INTERSECTION
────────────────────────────

Objects under repatriation consideration:
├── AI training should be prohibited
├── Digital surrogates may need removal from AI datasets
├── Community consent takes precedence
├── Documentation should reflect sensitivity

Post-repatriation:
├── Museum may retain documentation rights
├── AI rights return to originating community
├── Digital copies must honor new consent status
├── Metadata updated to reflect status change
```

### Living Artists

```
LIVING ARTIST CONSIDERATIONS
────────────────────────────

Acquisition:
├── Explicitly discuss AI rights in acquisition
├── Artist consent for AI use must be specific
├── Review mechanism for changing preferences
├── Artist death → estate becomes consent authority

Loans:
├── AI terms in loan agreements
├── No AI use without specific permission
├── Digital documentation consent separate

Commissions:
├── AI rights part of commission contract
├── Artist retains AI consent authority
├── Museum use vs. third-party AI use distinction
```

### Indigenous Collections

```
INDIGENOUS COLLECTIONS
──────────────────────

Protocols:
├── Community consent is mandatory
├── Sacred/secret materials flagged
├── AI use often inappropriate regardless of copyright
├── Consult CARE Principles, OCAP, local protocols

Implementation:
├── Default: AI use prohibited
├── Change requires community consultation
├── Document consultation process
├── Link to community contact for inquiries
```

---

## Open Access and AI

Reconciling open access with AI concerns:

```
OPEN ACCESS ≠ OPEN AI TRAINING
──────────────────────────────

Open Access can mean:
├── Free viewing ✓
├── Educational use ✓
├── Research use ✓
├── Personal use ✓
├── AI training use ← SEPARATE QUESTION

Palimpsest enables:
├── Open access for human use
├── Restricted access for AI use
├── Different conditions for different AI types
├── Clear, machine-readable distinctions
```

### robots.txt and AI Crawlers

```
# robots.txt additions for AI crawlers

# Block AI training crawlers from collection images
User-agent: GPTBot
User-agent: CCBot
User-agent: anthropic-ai
User-agent: Google-Extended
Disallow: /collection/images/
Disallow: /iiif/

# Allow regular search indexing
User-agent: Googlebot
Allow: /

# But refer to Palimpsest for specific object permissions
# See /palimpsest-consent.json for per-object AI terms
```

---

## Resources

### Standards Bodies
- ICOM: [icom.museum](https://icom.museum)
- Collections Trust: [collectionstrust.org.uk](https://collectionstrust.org.uk)
- AAM: [aam-us.org](https://www.aam-us.org)

### Technical Standards
- LIDO: [lido-schema.org](http://lido-schema.org)
- IIIF: [iiif.io](https://iiif.io)
- SPECTRUM: [collectionstrust.org.uk/spectrum](https://collectionstrust.org.uk/spectrum/)

### Ethics Guidance
- ICOM Code of Ethics
- NAGPRA (US Indigenous materials)
- CARE Principles for Indigenous Data Governance

---

## Document Review Log

| Date | Reviewer | Status | Notes |
|------|----------|--------|-------|
| 2025-12-08 | AI | Current | Initial creation |

**Next Review Due:** 2026-03-08

