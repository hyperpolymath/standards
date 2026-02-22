# Technical Vignette: GitHub Repository Integration

## Scenario: Implementing Palimpsest License in Open-Source Repository

**Project**: *CulturalNLP* (natural language processing library for underrepresented languages)
**Maintainer**: Dr. Kwame Asante
**Platform**: GitHub
**Technical Challenge**: Integrating Palimpsest with standard dev workflows
**Outcome**: Successful dual-licensing, automated compliance, community growth

---

## The Project

Dr. Kwame Asante created *CulturalNLP*: an NLP library for processing Akan, Yoruba, Swahili, and other African languages. Unlike commercial NLP tools that treated African languages as afterthoughts, *CulturalNLP* centered linguistic and cultural authenticity.

The project included:
- **Code** (Python library): Functional NLP tools
- **Training data**: Curated text corpora from African literature, oral histories
- **Documentation**: Culturally-informed guides explaining linguistic concepts

**Licensing challenge**: The code could be MIT-licensed (standard open source), but the training data and documentation carried cultural weight. African languages and texts weren't "raw data"—they represented communities, traditions, and intellectual heritage.

### The Dual-Licensing Solution

**Repository structure:**
```
cultural-nlp/
├── LICENSE                  # MIT License (for code)
├── LICENSE-PALIMPSEST       # Palimpsest v0.4 (for data & docs)
├── src/                     # Code (MIT)
│   ├── akan_parser.py
│   └── yoruba_tokenizer.py
├── data/                    # Training data (Palimpsest)
│   ├── akan_corpus/
│   └── yoruba_corpus/
├── docs/                    # Documentation (Palimpsest)
│   ├── akan_guide.md
│   └── cultural_context.md
├── .palimpsest.json         # Machine-readable metadata
├── ATTRIBUTIONS.md          # Cultural attributions
└── README.md                # Explains dual licensing
```

**README.md:**
```markdown
# CulturalNLP: NLP Tools for African Languages

## Dual Licensing

This project uses **dual licensing** to protect both technical and cultural aspects:

### MIT License (Code)
The source code in `/src` is licensed under the MIT License. You can use, modify, and distribute the code freely with attribution.

### Palimpsest v0.4 (Data & Documentation)
Training data (`/data`) and documentation (`/docs`) are licensed under Palimpsest v0.4 because they contain:
- Curated African language texts with cultural significance
- Community-sourced oral histories
- Culturally-informed linguistic explanations

**What this means:**
- **You CAN**: Use data for research, education, language preservation
- **You NEED PERMISSION for**: AI training, commercial products, large-scale distribution
- **You CANNOT**: Strip cultural context, use for surveillance, train models without consent

Read the full license: [LICENSE-PALIMPSEST](./LICENSE-PALIMPSEST)

## Why Dual Licensing?

African languages aren't just "data"—they're living cultural heritage. While we want the technical code to be maximally accessible (MIT), we protect the cultural and linguistic content from exploitation (Palimpsest).

### Quick Start

```bash
# Install (code is MIT)
pip install cultural-nlp

# Use with proper attribution (data is Palimpsest)
import cultural_nlp
akan = cultural_nlp.AkanParser()  # Works fine

# If you want to use training data in your own research:
# 1. Read LICENSE-PALIMPSEST
# 2. Include cultural attributions (see ATTRIBUTIONS.md)
# 3. For AI training: contact maintainers@cultural-nlp.org
```

## Attribution

See [ATTRIBUTIONS.md](./ATTRIBUTIONS.md) for full cultural credits.

TL;DR: Akan corpus sourced from Ghana Association of Writers; Yoruba texts from University of Ibadan Digital Archive; oral histories from community elders (anonymized).
```

### Technical Implementation

**1. Machine-Readable Metadata (`.palimpsest.json`):**

```json
{
  "@context": "https://palimpsest.license/schema/v0.4",
  "type": "Dataset",
  "name": "CulturalNLP Training Data & Documentation",
  "author": {
    "name": "Dr. Kwame Asante",
    "email": "kwame@cultural-nlp.org",
    "orcid": "0000-0002-1234-5678"
  },
  "license": "https://palimpsest.license/v0.4",
  "dateCreated": "2024-03-15",
  "culturalSources": [
    {
      "language": "Akan",
      "community": "Ghana Association of Writers",
      "consent": "Written permission obtained 2024-01-10"
    },
    {
      "language": "Yoruba",
      "community": "University of Ibadan Digital Archive",
      "consent": "Institutional agreement 2023-11-05"
    }
  ],
  "emotionalLineage": {
    "purpose": "Language preservation and cultural sustainability for African languages underrepresented in NLP",
    "culturalContext": "African languages carry oral traditions, proverbs, and cultural knowledge that must be preserved authentically",
    "intent": "Enable African language NLP without extraction or exploitation"
  },
  "permissions": {
    "nonInterpretiveAI": true,
    "interpretiveAI": false,
    "commercialUse": false,
    "requiresAttribution": true
  },
  "prohibitedUses": [
    "Training large language models without explicit consent",
    "Commercial products without revenue-sharing agreement",
    "Surveillance or border control applications",
    "Stripping cultural context from linguistic data"
  ],
  "contactForPermissions": "permissions@cultural-nlp.org"
}
```

**2. Automated License Checking (GitHub Actions):**

```yaml
# .github/workflows/license-check.yml
name: License Compliance Check

on: [pull_request]

jobs:
  check-licenses:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Check for license headers
        run: |
          # Check code files have MIT headers
          find src/ -name "*.py" -exec grep -L "SPDX-License-Identifier: MIT" {} \;

          # Check data files have Palimpsest headers
          find data/ -name "*.txt" -exec grep -L "License: Palimpsest-0.4" {} \;

      - name: Validate Palimpsest metadata
        run: |
          # Ensure .palimpsest.json is valid
          python scripts/validate_palimpsest_metadata.py

      - name: Check attributions
        run: |
          # Ensure ATTRIBUTIONS.md is updated
          python scripts/check_attributions.py
```

**3. Contribution Guidelines:**

```markdown
## Contributing to CulturalNLP

### Code Contributions (MIT)
Code contributions are licensed under MIT. By submitting a PR, you agree to license your code under MIT.

### Data Contributions (Palimpsest)
If contributing training data:
1. **Verify you have rights**: Ensure you have permission from language communities
2. **Document cultural context**: Fill out data source template
3. **Obtain community consent**: For significant corpora, show proof of community approval
4. **License under Palimpsest**: Your contribution will be added under Palimpsest v0.4

Template: `data/SOURCE_TEMPLATE.md`

### Documentation Contributions (Palimpsest)
Documentation explaining cultural linguistic concepts is under Palimpsest to preserve cultural context.

### Questions?
Email: contribute@cultural-nlp.org
```

**4. Issue Templates:**

```markdown
## Request to Use Data for AI Training

**Organization**: [Your organization]
**Intended Use**: [Describe AI system and purpose]
**Languages**: [Which language corpora?]
**Attribution Plan**: [How will you credit sources?]
**Revenue Model**: [Commercial? Non-profit? Academic?]
**Community Benefit**: [How does this benefit African language communities?]

**Our Review Process**:
We'll consult with the language communities who provided data. Approval requires:
- Non-exploitative use
- Proper cultural attribution
- Benefit to source communities (revenue share, access, etc.)

Expected timeline: 14-30 days
```

---

## Results

**Year 1:**
- 2,500 stars on GitHub
- 45 contributors (developers)
- 12 community organizations contributed linguistic data
- Zero licensing violations reported

**Licensing Outcomes:**
- 8 research groups requested AI training permission (6 approved with conditions, 2 denied)
- 3 commercial companies requested licenses (negotiated revenue-sharing agreements)
- 200+ academic citations properly attributed data sources

**Community Impact:**
- Ghana Association of Writers received £15,000 in revenue share from commercial licenses
- University of Ibadan expanded digital archive using project revenue
- African linguists recognized as co-creators, not data sources

**Technical Lessons Learned:**

1. **Dual licensing is viable**: MIT for code + Palimpsest for data/docs works smoothly
2. **Automation helps compliance**: GitHub Actions caught licensing errors before merge
3. **Clear documentation reduces confusion**: Explicit README prevented most licensing questions
4. **Community trust grew**: Language communities contributed more data because Palimpsest protected it
5. **Commercial interest didn't disappear**: Companies were willing to negotiate licenses for valuable data

---

## Technical Best Practices

### For Repository Maintainers

**Setup:**
1. Create separate `LICENSE` and `LICENSE-PALIMPSEST` files
2. Add `.palimpsest.json` for machine-readable metadata
3. Structure repo to clearly separate MIT (code) and Palimpsest (data/docs) content
4. Add license headers to all files
5. Create `ATTRIBUTIONS.md` for cultural credits

**Automation:**
1. GitHub Actions to validate license compliance in PRs
2. Scripts to check metadata validity
3. Pre-commit hooks preventing unlicensed file commits
4. Automated reminders for contributors about dual licensing

**Documentation:**
1. Prominent dual-licensing explanation in README
2. Contributing guidelines explaining which license applies when
3. Issue templates for permission requests
4. FAQ covering common licensing questions

**Community:**
1. Regular updates to language communities whose data you use
2. Revenue-sharing transparency (publish how much and to whom)
3. Community advisory board for major licensing decisions
4. Annual reports on project impact

### For Users

**Respecting Dual Licensing:**
1. **Using code (MIT)**: Fork, modify, distribute freely with attribution
2. **Using data/docs (Palimpsest)**: Check LICENSE-PALIMPSEST, follow restrictions
3. **AI training**: Always request permission, explain your use case
4. **Commercial use**: Contact maintainers for licensing agreement

**Attribution:**
1. Credit Dr. Kwame Asante and CulturalNLP project
2. Credit language communities (see ATTRIBUTIONS.md)
3. Include cultural context when using linguistic data
4. Link back to repository

---

## Discussion Questions

1. Should all culturally-significant open-source projects use dual licensing? Or is it too complex?

2. What if contributors don't understand Palimpsest and accidentally violate it? Whose responsibility?

3. Should GitHub add native support for dual-licensing (better UI, automatic detection)?

4. How granular should licensing be? File-by-file? Directory-level? Entire repo?

5. Does Palimpsest contradict open-source ethos? Or extend it to cover cultural protection?

---

**License Note**: This vignette is licensed under CC BY-SA 4.0. *CulturalNLP* and Dr. Kwame Asante are fictional, created to illustrate GitHub integration of Palimpsest. The technical patterns are based on real dual-licensing practices.
