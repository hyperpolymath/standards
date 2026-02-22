# PKP Integration Research

**Status:** FIRST PASS / RESEARCH
**Purpose:** Integration with Public Knowledge Project (OJS, OMP, OPS)

---

## Background

The Public Knowledge Project (PKP) maintains the most widely-used open-source scholarly publishing software:

- **OJS (Open Journal Systems):** ~25,000+ journals worldwide
- **OMP (Open Monograph Press):** Book/monograph publishing
- **OPS (Open Preprint Systems):** Preprint servers

These platforms are critical infrastructure for academic publishing and would be ideal targets for Palimpsest License integration.

---

## Integration Points

### 1. Submission Workflow

```
AUTHOR SUBMISSION FLOW
──────────────────────

┌─────────────────────────────────────────────────────────────┐
│                    OJS SUBMISSION FORM                       │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Title: _______________                                      │
│  Abstract: _______________                                   │
│                                                              │
│  ┌─────────────────────────────────────────────────────────┐│
│  │ LICENSE SELECTION                                       ││
│  │ ────────────────────────────────────────────────────── ││
│  │ ○ Creative Commons CC-BY 4.0                           ││
│  │ ○ Creative Commons CC-BY-NC 4.0                        ││
│  │ ○ Creative Commons CC-BY-ND 4.0                        ││
│  │ ● Palimpsest License v0.4 ← NEW                        ││
│  │   │                                                     ││
│  │   └─► [Configure Palimpsest Options]                   ││
│  │       ├── AI Training Consent: [Yes/No/Conditional]    ││
│  │       ├── Emotional Lineage Tags: [Add...]             ││
│  │       ├── Cultural Context: [Select...]                ││
│  │       └── Trauma Markers: [Optional flags]             ││
│  └─────────────────────────────────────────────────────────┘│
│                                                              │
│  [Submit] [Save Draft]                                       │
└─────────────────────────────────────────────────────────────┘
```

### 2. Metadata Schema Extension

OJS uses Dublin Core + JATS. We need to extend with:

```xml
<!-- Dublin Core extension for Palimpsest -->
<dcterms:license>
  <palimpsest:version>0.4</palimpsest:version>
  <palimpsest:consent>
    <palimpsest:ai-training>conditional</palimpsest:ai-training>
    <palimpsest:ni-systems>explicit-only</palimpsest:ni-systems>
  </palimpsest:consent>
  <palimpsest:lineage>
    <palimpsest:emotional-context>protest-literature</palimpsest:emotional-context>
    <palimpsest:cultural-origin>diaspora-narrative</palimpsest:cultural-origin>
  </palimpsest:lineage>
</dcterms:license>
```

### 3. JATS XML Extension

For full-text articles:

```xml
<article-meta>
  <permissions>
    <license license-type="palimpsest" xlink:href="https://palimpsestlicense.org/v0.4">
      <license-p>Licensed under Palimpsest License v0.4</license-p>
      <ali:license_ref>https://palimpsestlicense.org/v0.4</ali:license_ref>
      <custom-meta-group>
        <custom-meta>
          <meta-name>palimpsest:ai-training-consent</meta-name>
          <meta-value>conditional:academic-only</meta-value>
        </custom-meta>
        <custom-meta>
          <meta-name>palimpsest:emotional-lineage</meta-name>
          <meta-value>trauma-informed-research</meta-value>
        </custom-meta>
      </custom-meta-group>
    </license>
  </permissions>
</article-meta>
```

---

## Plugin Architecture

### OJS 3.x Plugin Structure

```
palimpsest-ojs-plugin/
├── PalimpsestPlugin.php          # Main plugin class
├── version.xml                   # Plugin metadata
├── index.php                     # Entry point
├── locale/
│   ├── en_US/
│   │   └── locale.po             # English strings
│   └── nl_NL/
│       └── locale.po             # Dutch strings
├── templates/
│   ├── submission/
│   │   └── licenseForm.tpl       # License selection form
│   └── settings/
│       └── index.tpl             # Plugin settings
├── classes/
│   ├── form/
│   │   └── PalimpsestForm.php    # Form handling
│   └── metadata/
│       └── PalimpsestMetadata.php # Metadata schema
├── js/
│   └── PalimpsestHandler.js      # Frontend interactions
└── styles/
    └── palimpsest.css            # Plugin styles
```

### Core Plugin Class

```php
<?php
/**
 * @file PalimpsestPlugin.php
 * @class PalimpsestPlugin
 * @brief Palimpsest License integration for OJS
 */

import('lib.pkp.classes.plugins.GenericPlugin');

class PalimpsestPlugin extends GenericPlugin {

    public function register($category, $path, $mainContextId = null) {
        if (parent::register($category, $path, $mainContextId)) {
            if ($this->getEnabled($mainContextId)) {
                // Hook into submission workflow
                HookRegistry::register(
                    'Templates::Submission::SubmissionMetadataForm::AdditionalMetadata',
                    [$this, 'addLicenseSelector']
                );

                // Hook into article display
                HookRegistry::register(
                    'Templates::Article::Details',
                    [$this, 'displayLicenseBadge']
                );

                // Hook into OAI export
                HookRegistry::register(
                    'OAIMetadataFormat_DC::getMetadata',
                    [$this, 'addDCMetadata']
                );
            }
            return true;
        }
        return false;
    }

    public function addLicenseSelector($hookName, $args) {
        $templateMgr = $args[1];
        $output =& $args[2];

        // Render Palimpsest license options
        $output .= $templateMgr->fetch(
            $this->getTemplateResource('submission/licenseForm.tpl')
        );

        return false;
    }

    public function getConsentOptions() {
        return [
            'yes' => __('plugins.generic.palimpsest.consent.yes'),
            'no' => __('plugins.generic.palimpsest.consent.no'),
            'conditional' => __('plugins.generic.palimpsest.consent.conditional'),
            'academic-only' => __('plugins.generic.palimpsest.consent.academicOnly'),
        ];
    }
}
```

---

## OMP (Open Monograph Press) Considerations

Books/monographs have additional complexity:

### Chapter-Level Licensing

```
BOOK STRUCTURE
──────────────

Book: "Voices of the Displaced"
├── Front Matter (CC-BY)
├── Chapter 1: Introduction (Palimpsest - academic consent)
├── Chapter 2: Oral Histories (Palimpsest - no AI training)
│   └── Contains: Trauma narratives, requires consent preservation
├── Chapter 3: Analysis (Palimpsest - conditional)
└── Back Matter (CC-BY)
```

Each chapter may need independent license terms.

### Anthology/Collection Model

```php
class PalimpsestChapterMetadata extends ChapterMetadataPlugin {

    public function getChapterLicenseForm($chapter) {
        return [
            'license_type' => 'palimpsest',
            'consent' => [
                'ai_training' => $chapter->getData('palimpsest_ai_consent'),
                'ni_systems' => $chapter->getData('palimpsest_ni_consent'),
            ],
            'lineage' => [
                'emotional' => $chapter->getData('palimpsest_emotional_tags'),
                'cultural' => $chapter->getData('palimpsest_cultural_context'),
            ],
            'contributors' => $this->getContributorConsents($chapter),
        ];
    }
}
```

---

## OAI-PMH Export

Journals expose metadata via OAI-PMH. We need:

### Custom Metadata Format

```xml
<OAI-PMH>
  <ListRecords>
    <record>
      <metadata>
        <oai_dc:dc>
          <dc:rights>Palimpsest License v0.4</dc:rights>
          <dc:rights.uri>https://palimpsestlicense.org/v0.4</dc:rights.uri>
        </oai_dc:dc>

        <!-- Extended Palimpsest metadata -->
        <palimpsest:metadata>
          <palimpsest:consent-state>conditional</palimpsest:consent-state>
          <palimpsest:lineage-preserved>true</palimpsest:lineage-preserved>
          <palimpsest:verifiable-at>
            https://consent.palimpsestlicense.org/verify/abc123
          </palimpsest:verifiable-at>
        </palimpsest:metadata>
      </metadata>
    </record>
  </ListRecords>
</OAI-PMH>
```

---

## Crossref DOI Registration

Academic articles need DOIs. Crossref schema extension:

```xml
<doi_batch>
  <body>
    <journal_article>
      <ai:program name="AccessIndicators">
        <ai:license_ref applies_to="vor" start_date="2025-01-01">
          https://palimpsestlicense.org/v0.4
        </ai:license_ref>
        <ai:license_ref applies_to="tdm">
          https://palimpsestlicense.org/v0.4/ai-conditional
        </ai:license_ref>
      </ai:program>

      <!-- Custom extension for consent -->
      <palimpsest:training_consent>
        <palimpsest:status>conditional</palimpsest:status>
        <palimpsest:registry>https://consent.palimpsestlicense.org</palimpsest:registry>
      </palimpsest:training_consent>
    </journal_article>
  </body>
</doi_batch>
```

---

## Implementation Phases

### Phase 1: Basic OJS Plugin

- License selection in submission form
- Badge display on article page
- Basic metadata storage

### Phase 2: Full Metadata Integration

- JATS XML extension
- OAI-PMH export
- Dublin Core mapping

### Phase 3: OMP Support

- Chapter-level licensing
- Anthology/collection handling
- Contributor consent management

### Phase 4: Consent Registry Integration

- Real-time consent verification
- Consent state updates
- Usage tracking

### Phase 5: Advanced Features

- AI training opt-out enforcement
- Emotional lineage visualization
- Cultural context preservation

---

## Questions Emerged

1. **How does PKP community governance work?**
   - Plugin review process?
   - Core integration path?
   - Who are the key stakeholders?

2. **Schema extensions:**
   - Can we get JATS extension approved?
   - Dublin Core namespace registration?
   - Crossref TDM license type?

3. **Institutional adoption:**
   - University library buy-in needed?
   - Journal editor training required?
   - Publisher concerns?

4. **Technical constraints:**
   - PHP 7.4+ compatibility?
   - OJS 3.3 vs 3.4 differences?
   - Database migration handling?

5. **Legal/policy:**
   - Do journals need to update policies?
   - Author agreement modifications?
   - Publisher liability concerns?

---

## Resources

- PKP Plugin Gallery: https://github.com/pkp/pkp-plugin-gallery
- OJS Developer Documentation: https://docs.pkp.sfu.ca/dev/
- PKP Technical Forum: https://forum.pkp.sfu.ca/
- JATS Standard: https://jats.nlm.nih.gov/
- Crossref Schema: https://www.crossref.org/documentation/schema-library/

---

## Potential Partners

| Organization | Interest | Contact Path |
|-------------|----------|--------------|
| PKP | Core integration | https://pkp.sfu.ca/contact/ |
| DOAJ | Directory listing | DOAJ application |
| SPARC | Open access advocacy | SPARC membership |
| OASPA | Publisher standards | OASPA working groups |
| Coalition S | Plan S alignment | cOAlition S secretariat |

