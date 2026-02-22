# Technical Vignette: NPM Package with Palimpsest Metadata

## Scenario: JavaScript Package Using Palimpsest for Cultural Data

**Package**: `@cultural-data/proverbs` (African and Asian proverbs database)
**Maintainer**: Collective of cultural researchers
**Platform**: NPM (Node Package Manager)
**Challenge**: Embedding Palimpsest metadata in JavaScript ecosystem
**Outcome**: Accessible cultural data with strong protection

---

## The Package

`@cultural-data/proverbs` provides programmatic access to thousands of proverbs from African and Asian cultures, with context, translations, and cultural explanations.

**Use cases:**
- Educational apps teaching cultural wisdom
- Language learning platforms
- Creative writing tools
- Cultural research databases

**Licensing concern**: Proverbs carry cultural significance. The package needed to be:
- Easy to install and use (standard NPM workflow)
- Protected from exploitation (AI training, commercial misuse)
- Attributed properly (credit communities, not just package maintainers)

### Technical Implementation

**package.json:**
```json
{
  "name": "@cultural-data/proverbs",
  "version": "2.1.0",
  "description": "Culturally-contextualized proverb database from African and Asian traditions",
  "license": "Palimpsest-0.4",
  "author": "Cultural Data Collective <contact@culturaldata.org>",
  "repository": {
    "type": "git",
    "url": "https://github.com/cultural-data/proverbs"
  },
  "funding": {
    "type": "opencollective",
    "url": "https://opencollective.com/cultural-data"
  },
  "palimpsest": {
    "version": "0.4",
    "licenseUrl": "https://palimpsest.license/v0.4",
    "culturalSources": [
      {
        "culture": "Yoruba (Nigeria)",
        "contributors": "Yoruba Cultural Heritage Foundation",
        "consent": "2023-06-12"
      },
      {
        "culture": "Akan (Ghana)",
        "contributors": "Ashanti Elders Council",
        "consent": "2023-08-20"
      }
    ],
    "permissions": {
      "nonInterpretive": true,
      "interpretiveAI": false,
      "commercial": "contact-required"
    },
    "contactForPermissions": "permissions@culturaldata.org",
    "prohibitedUses": [
      "AI training without consent",
      "Stripping cultural context",
      "Commercial use without revenue sharing"
    ]
  },
  "keywords": ["proverbs", "cultural-data", "wisdom", "palimpsest", "african-culture", "asian-culture"],
  "main": "dist/index.js",
  "types": "dist/index.d.ts",
  "scripts": {
    "test": "jest",
    "build": "tsc",
    "validate-licenses": "node scripts/validate-palimpsest.js"
  },
  "devDependencies": {
    "typescript": "^5.0.0",
    "jest": "^29.0.0"
  }
}
```

**Data structure (TypeScript):**
```typescript
// src/types.ts
export interface Proverb {
  id: string;
  text: string;
  language: string;
  culture: string;
  translation?: string;
  culturalContext: string;
  usage: string;
  communitySource: string;
  emotionalWeight: 'light' | 'moderate' | 'deep';
  metadata: {
    collector: string;
    dateRecorded: string;
    licenseTerms: 'Palimpsest-0.4';
    attribution: string[];
  };
}

// Example proverb
const exampleProverb: Proverb = {
  id: "yoruba-001",
  text: "Ilé l'a bí mọ́, ilé l'a yọ lẹ́nu",
  language: "Yoruba",
  culture: "Yoruba (Nigeria)",
  translation: "Home is where we first learn, home is where we come of age",
  culturalContext: "This proverb emphasizes the foundational role of family and community in personal development. In Yoruba culture, home (ilé) represents not just a physical space but the network of relationships and teachings that shape a person.",
  usage: "Often used to remind children of their cultural roots when they venture into the world, or to emphasize the importance of community education over individual achievement.",
  communitySource: "Yoruba Cultural Heritage Foundation",
  emotionalWeight: "deep",
  metadata: {
    collector: "Dr. Adebayo Ogunlesi",
    dateRecorded: "2023-06-15",
    licenseTerms: "Palimpsest-0.4",
    attribution: [
      "Yoruba Cultural Heritage Foundation",
      "Cultural Data Collective",
      "Dr. Adebayo Ogunlesi (researcher)"
    ]
  }
};
```

**API with built-in attribution:**
```typescript
// src/index.ts
import proverbs from './data/proverbs.json';
import { Proverb } from './types';

export class ProverbDatabase {
  /**
   * Get proverb by ID
   * Note: By using this package, you agree to Palimpsest v0.4 license terms.
   * See: https://palimpsest.license/v0.4
   */
  getProverb(id: string): Proverb | undefined {
    return proverbs.find(p => p.id === id);
  }

  /**
   * Search proverbs by culture
   * IMPORTANT: Proverbs include cultural context that must be preserved.
   * Do not strip 'culturalContext' or 'metadata' fields.
   */
  searchByCulture(culture: string): Proverb[] {
    return proverbs.filter(p => p.culture === culture);
  }

  /**
   * Get attribution for use in your app
   * REQUIRED: Display this attribution when showing proverbs
   */
  getAttribution(proverbId: string): string[] {
    const proverb = this.getProverb(proverbId);
    return proverb?.metadata.attribution || [];
  }

  /**
   * Check if a use case requires special permission
   * @param useCase - Description of how you'll use the proverbs
   * @returns Contact info if permission needed, null if use is permitted
   */
  checkPermissions(useCase: {
    purpose: 'education' | 'commercial' | 'ai-training' | 'research';
    scale: 'small' | 'medium' | 'large';
  }): string | null {
    if (useCase.purpose === 'ai-training') {
      return 'AI training requires explicit consent. Contact: permissions@culturaldata.org';
    }
    if (useCase.purpose === 'commercial' && useCase.scale !== 'small') {
      return 'Commercial use at scale requires revenue-sharing. Contact: permissions@culturaldata.org';
    }
    return null; // Permitted use
  }
}

// Convenience exports
export { Proverb } from './types';
export default new ProverbDatabase();
```

**README.md:**
```markdown
# @cultural-data/proverbs

African and Asian proverbs with full cultural context, licensed under Palimpsest v0.4.

## Installation

```bash
npm install @cultural-data/proverbs
```

## Usage

```javascript
const proverbs = require('@cultural-data/proverbs');

// Get a specific proverb
const proverb = proverbs.getProverb('yoruba-001');
console.log(proverb.text);          // Original language
console.log(proverb.translation);    // English translation
console.log(proverb.culturalContext);// Why it matters

// REQUIRED: Display attribution
const attribution = proverbs.getAttribution('yoruba-001');
console.log('Credits:', attribution.join(', '));

// Search by culture
const yorubaProverbs = proverbs.searchByCulture('Yoruba (Nigeria)');
```

## License: Palimpsest v0.4

This package is licensed under **Palimpsest v0.4**, NOT MIT or Apache.

### What You CAN Do:
✅ Use in educational apps
✅ Use in non-profit projects
✅ Research and academic study
✅ Display proverbs with proper attribution

### What Requires Permission:
📧 Commercial applications (revenue-sharing required)
📧 AI training or language models
📧 Large-scale distribution (>100,000 users)

### What You CANNOT Do:
❌ Strip cultural context from proverbs
❌ Use without attribution
❌ Train AI models without consent
❌ Present as "your" wisdom or generic quotes

**Contact for permissions**: permissions@culturaldata.org

## Attribution Requirements

When displaying proverbs, you MUST include:
1. The proverb text and translation
2. Cultural source (e.g., "Yoruba proverb from Nigeria")
3. Package attribution: "from @cultural-data/proverbs"
4. Community source (from `metadata.attribution`)

Example:
```
"Home is where we first learn, home is where we come of age"
— Yoruba proverb (Nigeria)
Cultural context provided by Yoruba Cultural Heritage Foundation
via @cultural-data/proverbs
```

## Why Palimpsest?

Proverbs aren't just data—they're cultural wisdom passed down through generations. The Palimpsest license ensures:
- Communities who shared wisdom are credited
- Cultural context isn't stripped for "clean data"
- AI companies can't train on this without consent
- Commercial users contribute back to source communities

## Revenue Sharing

Commercial licenses contribute 20% of revenue to the cultural communities who provided proverbs. This supports language preservation, elder documentation, and cultural education programs.

## Community Advisory Board

Licensing decisions are made in consultation with representatives from source communities:
- Yoruba Cultural Heritage Foundation (Nigeria)
- Ashanti Elders Council (Ghana)
- Confucian Heritage Association (China)
- Tamil Literary Society (India)

## FAQ

**Q: Can I use this in my React app?**
A: Yes, if it's non-commercial or small-scale. For commercial apps, contact us for licensing.

**Q: Can I train a language model on these proverbs?**
A: Not without explicit consent. Email permissions@culturaldata.org.

**Q: What if I don't display the cultural context?**
A: That violates the license. Cultural context is part of the data, not optional.

**Q: Is this compatible with my project's MIT license?**
A: You can use Palimpsest-licensed data in an MIT project, but the proverb data remains under Palimpsest. Your code can be MIT; the data stays protected.

```

### Automated Compliance

**Pre-publish checks:**
```javascript
// scripts/validate-palimpsest.js
const fs = require('fs');
const packageJson = require('../package.json');

console.log('Validating Palimpsest license compliance...');

// Check license field
if (packageJson.license !== 'Palimpsest-0.4') {
  console.error('ERROR: license field must be "Palimpsest-0.4"');
  process.exit(1);
}

// Check palimpsest metadata
if (!packageJson.palimpsest) {
  console.error('ERROR: Missing palimpsest metadata in package.json');
  process.exit(1);
}

// Check cultural sources
const sources = packageJson.palimpsest.culturalSources;
if (!sources || sources.length === 0) {
  console.error('ERROR: Must document cultural sources');
  process.exit(1);
}

// Validate all proverb entries have required fields
const proverbs = require('../data/proverbs.json');
proverbs.forEach((proverb, index) => {
  if (!proverb.culturalContext) {
    console.error(`ERROR: Proverb ${index} missing cultural context`);
    process.exit(1);
  }
  if (!proverb.metadata.attribution || proverb.metadata.attribution.length === 0) {
    console.error(`ERROR: Proverb ${index} missing attribution`);
    process.exit(1);
  }
  if (proverb.metadata.licenseTerms !== 'Palimpsest-0.4') {
    console.error(`ERROR: Proverb ${index} has incorrect license`);
    process.exit(1);
  }
});

console.log('✅ Palimpsest license compliance validated');
```

**Add to package.json scripts:**
```json
{
  "scripts": {
    "prepublishOnly": "npm run validate-licenses"
  }
}
```

---

## Results

**Year 1:**
- 50,000 npm downloads
- Used in 200+ educational apps
- 12 commercial license requests (10 approved with revenue sharing)
- £30,000 distributed to source communities
- Zero licensing violations reported

**Developer Feedback:**
- "Finally, cultural data with actual context!"
- "Palimpsest license was new to me, but the README made it clear."
- "Appreciate that commercial use is possible with fair compensation."
- "The TypeScript types including cultural context fields helped me build better UX."

**Community Impact:**
- Yoruba Cultural Heritage Foundation expanded their documentation program
- Ashanti Elders Council created youth cultural education initiative
- Package became case study in ethical open source data

---

## Best Practices

### For Package Maintainers

**Do:**
- Document Palimpsest clearly in package.json and README
- Include cultural metadata in data structures (not separate files)
- Provide automated validation scripts
- Make attribution easy (helper methods like `getAttribution()`)
- Respond quickly to permission requests

**Don't:**
- Assume developers know Palimpsest (explain it)
- Hide license terms (make them prominent)
- Strip cultural context for "cleaner" data
- Forget to update community revenue share reports

### For Package Users

**Do:**
- Read the license before using
- Display attribution as required
- Contact maintainers for commercial/AI use
- Preserve cultural context in your UI
- Consider donating/sponsoring the package

**Don't:**
- Assume "npm install" means "free for any use"
- Strip metadata fields
- Bypass permission requirements
- Complain about "restrictive" licenses (if you want unrestricted data, use different sources)

---

## Discussion Questions

1. Should NPM add native support for Palimpsest license detection and display?

2. Is cultural data fundamentally different from code, or should all open source use permissive licenses?

3. Should package managers warn users when installing Palimpsest-licensed packages? Or is it users' responsibility to read licenses?

4. What if someone forks the package and relicenses it under MIT? How do you prevent that?

5. Should community-contributed cultural data always involve revenue sharing? Or is attribution enough?

---

**License Note**: This vignette is licensed under CC BY-SA 4.0. `@cultural-data/proverbs` is fictional, created to illustrate NPM package integration with Palimpsest. The technical patterns are based on real package management practices.
