# Palimpsest License: Guide for Translators & Localization Communities

**Audience:** Translators, interpreters, localization professionals, minority language communities
**Version:** 0.4

---

## Why Translation Is Critical for Palimpsest

Translation is not just changing words—it's transmitting meaning across cultures. For Palimpsest, translation matters because:

1. **Consent must cross languages:** If someone can't read the license, they can't consent
2. **AI threatens minority languages:** Small language datasets → erasure or distortion
3. **Cultural nuance in licensing:** Consent means different things in different cultures
4. **Amplifying silenced voices:** Translation can break isolation
5. **Detecting propagation errors:** Translators see how meaning mutates

---

## The AI-Translation Threat

### What AI Is Doing to Translation

```
AI TRANSLATION EXTRACTION
─────────────────────────

How AI translation works:
├── Trained on parallel texts (translations)
├── Human translation → training data
├── Quality depends on human translator work
├── No attribution or compensation

What gets extracted:
├── Literary translations
├── Subtitle/dubbing work
├── Documentation translations
├── Legal translations
├── Community translations
├── Minority language resources

Consequences:
├── Translators' skill extracted without consent
├── AI "translates" based on stolen work
├── Minority languages get worse AI (smaller datasets)
├── Cultural nuance lost in statistical averaging
├── Translation profession undermined
```

### Minority Language Concerns

```
MINORITY LANGUAGE AI RISKS
──────────────────────────

The paradox:
├── Less data → worse AI performance
├── Community creates data → scraped for AI
├── AI produces bad translations → reinforces errors
├── Cultural damage from AI mistranslation

Specific risks:
├── Sacred texts translated incorrectly
├── Cultural concepts flattened
├── Oral traditions transcribed and distorted
├── Regional variations erased
├── Code-switching mishandled
└── Dialects merged into "standard"

Who's affected:
├── Indigenous languages (globally)
├── Regional languages (Scots, Occitan, Catalan)
├── Diaspora languages
├── Sign languages (visual → text conversion)
├── Endangered languages
└── Community languages (immigrant communities)
```

---

## Palimpsest for Translations

### Licensing Translated Work

```json
{
  "@context": "https://palimpsestlicense.org/translation",
  "@type": "TranslatedWork",

  "sourceWork": {
    "id": "original-work-id",
    "language": "nl",
    "license": "Palimpsest-0.4"
  },

  "translation": {
    "language": "sco",
    "translatorConsent": "explicit",
    "translatorAttribution": "required"
  },

  "culturalAdaptation": {
    "extent": "localized",
    "notes": "Adapted legal terminology for Scottish context"
  },

  "aiTrainingConsent": {
    "sourceLanguage": "prohibited",
    "targetLanguage": "prohibited",
    "parallelCorpus": "prohibited",
    "reason": "Translator has not consented to AI extraction of translation work"
  },

  "communityConsent": {
    "required": true,
    "authority": "Scots Language Centre",
    "status": "granted"
  }
}
```

### Rights Chain in Translation

```
TRANSLATION RIGHTS CHAIN
────────────────────────

Original work:
├── Creator consents to translation? [Y/N]
├── Palimpsest terms for translation?
└── AI consent for original?

Translation:
├── Translator's creative contribution
├── Translator's separate consent
├── Translation-specific AI terms
└── Target culture considerations

Derivative chain:
├── What consent carries forward?
├── Most restrictive applies
├── Translator cannot expand beyond original consent
├── Community consent may add restrictions
```

---

## Translation of Palimpsest Itself

### Priority Languages

The license itself needs translation. Priority:

| Language | Status | Notes |
|----------|--------|-------|
| Dutch (NL) | Complete | Primary legal |
| English (EN) | Complete | Global access |
| Scots (SCO) | Planned | Scottish jurisdiction |
| French (FR) | Needed | EU, Francophone |
| German (DE) | Needed | EU, precision |
| Spanish (ES) | Needed | Latin America |
| Arabic (AR) | Needed | MENA region |
| Mandarin (ZH) | Needed | Major language |
| Hindi (HI) | Needed | India |
| Swahili (SW) | Needed | East Africa |
| Portuguese (PT) | Needed | Brazil, Lusophone |

### Translation Guidelines

```
PALIMPSEST TRANSLATION PRINCIPLES
─────────────────────────────────

1. MEANING OVER WORDS
   └── Convey the concept, not just the text
   └── Legal precision matters
   └── Cultural equivalence where possible

2. BILINGUAL PARITY
   └── Both versions must be legally equivalent
   └── Neither is "primary" (except for jurisdiction)
   └── Cross-reference between versions

3. COMMUNITY REVIEW
   └── Native speakers must review
   └── Legal experts in target culture
   └── Community feedback welcome

4. CULTURAL ADAPTATION
   └── Examples appropriate to culture
   └── Legal concepts localized
   └── Emotional lineage concept explained culturally

5. ACCESSIBILITY
   └── Plain language versions available
   └── Not just legal audience
   └── Multiple reading levels
```

### How to Contribute Translations

**Needed:**
- Qualified translators
- Legal reviewers in target jurisdiction
- Community feedback
- Plain language versions

**Process:**
1. Contact translation@palimpsestlicense.org (planned)
2. Receive source documents and style guide
3. Produce draft translation
4. Community review
5. Legal review
6. Publication with attribution

**Compensation:**
- Volunteer contributions acknowledged
- Funded translations where grants available
- Translators retain rights to their translation
- Attribution always required

---

## Organizations and Networks

### Translation Communities to Engage

| Organization | Focus | Why Partner |
|-------------|-------|-------------|
| Translators Without Borders | Crisis response, minority languages | Humanitarian reach |
| PEN International | Literary translation | Writer networks |
| AIIC | Conference interpreters | Professional standards |
| ATA | American translators | US network |
| ITI | UK translators | UK legal context |
| FIT | International federation | Global coordination |
| Indigenous language centers | Specific language communities | Community authority |
| Sign language associations | Deaf communities | Accessibility |
| Wikimedia language projects | Volunteer translation | Open community |

### Minority Language Organizations

| Community | Language/Region | Why Engage |
|-----------|----------------|------------|
| Endangered Languages Project | Global endangered | Preservation focus |
| First Voices | Indigenous North America | Community-controlled |
| Living Tongues | Endangered worldwide | Documentation |
| SOAS Language Documentation | Academic + community | Research bridge |
| Mercator Network | European minority | EU policy |
| AIATSIS | Australian Indigenous | First Nations |
| Te Taura Whiri | Māori | Revitalization |
| Gaelic agencies | Scottish, Irish | Celtic languages |
| Basque academies | Euskara | Stateless language |

---

## Detecting Propagation Errors

Translators are uniquely positioned to see how meaning mutates:

```
PROPAGATION ERROR DETECTION
───────────────────────────

What translators notice:
├── Concepts that don't translate cleanly
├── Cultural assumptions embedded in source
├── Terms that shift meaning across languages
├── Legal concepts with no equivalent
├── Emotional concepts that need explanation

Useful feedback:
├── "This concept doesn't exist in [language]"
├── "This assumes [cultural norm] not present here"
├── "This legal term means something different"
├── "This is offensive/inappropriate in [culture]"
├── "This metaphor doesn't work"

How to report:
├── GitHub issues tagged "translation"
├── Email: translation@palimpsestlicense.org
├── Working group participation
└── Annual translation review
```

---

## Sign Language Considerations

Deaf communities have specific needs:

```
SIGN LANGUAGE INTEGRATION
─────────────────────────

Challenges:
├── Written text ≠ accessible to all Deaf people
├── Sign language videos needed
├── Regional sign variations
├── AI sign language generation is poor
├── Deaf culture has distinct consent norms

Palimpsest commitments:
├── Key documents in sign language video
├── Deaf community consultation
├── Sign language interpreter consent framework
├── Visual consent mechanisms
├── Deaf culture representation in governance

AI-specific concerns:
├── Sign language video in training data
├── AI-generated signs (often wrong)
├── Deaf creator consent
├── Interpretation consent
```

---

## AI Translation and Consent

### The Consent Gap

```
AI TRANSLATION CONSENT GAP
──────────────────────────

Scenario 1: Original is Palimpsest-licensed
├── Original says "no AI training"
├── Someone AI-translates it
├── AI translation = derivative work
├── AI also trained on parallel corpus
├── Multiple consent violations

Scenario 2: Translation is Palimpsest-licensed
├── Original is public domain
├── Translator creates translation
├── Translator licenses under Palimpsest
├── AI scrapes translation
├── Translator's consent violated

Scenario 3: Community language
├── Community controls language decisions
├── Individual cannot consent for community
├── AI trained on community language
├── Community consent not sought
├── Collective violation
```

### Guidance for Translators

```
TRANSLATOR GUIDANCE
───────────────────

Protecting your work:
├── License translations under Palimpsest
├── Assert AI training prohibition (if desired)
├── Document your translation choices
├── Register in consent registry
├── Include translator attribution

When translating others' work:
├── Check source license for AI terms
├── Cannot expand beyond source consent
├── Can add translator-specific restrictions
├── Document consent chain
├── Preserve emotional lineage

When working in minority languages:
├── Consult community
├── Understand community protocols
├── Don't assume individual consent sufficient
├── Consider AI training implications for language
├── Support language sovereignty
```

---

## Cross-Cultural Consent Concepts

### How Consent Varies

```
CONSENT ACROSS CULTURES
───────────────────────

Western individual consent:
├── Individual autonomy paramount
├── Written/explicit preferred
├── One-time transaction model
├── Legal enforceability focus

Collectivist consent:
├── Family/community consultation
├── Elder/authority involvement
├── Relationship-based
├── Process over document

Indigenous consent:
├── Community sovereignty
├── Land/ancestor connection
├── Intergenerational responsibility
├── Reciprocity expected

Oral culture consent:
├── Witnessed verbal agreement
├── Story/narrative transmission
├── Reputation-based enforcement
├── Ongoing relationship

Implications for Palimpsest:
├── Multiple consent modalities needed
├── Can't assume Western model universal
├── Community mechanisms must be respected
├── Translators help bridge these
```

---

## Practical Implementation

### Adding Palimpsest to Translation Workflow

```
TRANSLATION WORKFLOW + PALIMPSEST
─────────────────────────────────

Pre-translation:
☐ Check source work license
☐ Identify AI consent status
☐ Note emotional lineage context
☐ Consult target community if needed

Translation:
☐ Preserve meaning of consent terms
☐ Adapt cultural examples
☐ Note untranslatable concepts
☐ Document translation choices

Post-translation:
☐ Add Palimpsest metadata to translation
☐ Register translation consent status
☐ Include translator attribution
☐ Submit for community review

Delivery:
☐ Ensure metadata travels with work
☐ Provide consent chain documentation
☐ Flag propagation concerns
```

### Metadata for Translated Works

```xml
<palimpsest:translationMetadata
  xmlns:palimpsest="https://palimpsestlicense.org">

  <palimpsest:sourceWork>
    <palimpsest:id>source-123</palimpsest:id>
    <palimpsest:language>nl</palimpsest:language>
    <palimpsest:aiConsent>prohibited</palimpsest:aiConsent>
  </palimpsest:sourceWork>

  <palimpsest:translation>
    <palimpsest:id>translation-456</palimpsest:id>
    <palimpsest:language>sco</palimpsest:language>
    <palimpsest:translator>Jane Smith</palimpsest:translator>
    <palimpsest:translatorConsent>
      <palimpsest:aiTraining>prohibited</palimpsest:aiTraining>
      <palimpsest:parallelCorpus>prohibited</palimpsest:parallelCorpus>
      <palimpsest:attribution>required</palimpsest:attribution>
    </palimpsest:translatorConsent>
  </palimpsest:translation>

  <palimpsest:culturalNotes>
    <palimpsest:note>
      Legal terminology adapted for Scottish legal context.
    </palimpsest:note>
    <palimpsest:note>
      "Emotional lineage" rendered as "heritage of feeling" per
      community feedback.
    </palimpsest:note>
  </palimpsest:culturalNotes>

</palimpsest:translationMetadata>
```

---

## Get Involved

### As a Translator

- **Translate Palimpsest:** Contact translation@palimpsestlicense.org
- **Review translations:** Provide feedback on existing translations
- **Report issues:** Flag cultural/translation problems
- **Protect your work:** Use Palimpsest for your translations

### As a Language Community

- **Consult:** Tell us what your community needs
- **Localize:** Adapt Palimpsest for your culture
- **Govern:** Join governance discussions
- **Amplify:** Share with your networks

### As an Organization

- **Partner:** Collaborate on translation efforts
- **Fund:** Support minority language translations
- **Advocate:** Include Palimpsest in professional standards

---

## Resources

### Organizations
- FIT (International Federation of Translators): [fit-ift.org](https://www.fit-ift.org)
- Translators Without Borders: [translatorswithoutborders.org](https://translatorswithoutborders.org)
- PEN International Translation: [pen-international.org](https://www.pen-international.org)

### Standards
- ISO 17100 (Translation Services)
- ISO 639 (Language Codes)
- Unicode CLDR (Locale Data)

### Palimpsest Translation Resources
- Style guide: [to be developed]
- Terminology glossary: [to be developed]
- Community contacts: [forming]

---

## Document Review Log

| Date | Reviewer | Status | Notes |
|------|----------|--------|-------|
| 2025-12-08 | AI | Current | Initial creation |

**Next Review Due:** 2026-03-08

