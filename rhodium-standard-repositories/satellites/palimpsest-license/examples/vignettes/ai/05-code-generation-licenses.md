# AI-Specific Vignette: Code Generation with License Preservation

## Scenario: AI Coding Assistant Respecting Palimpsest-Licensed Code

**Tool**: GitHub Copilot (enhanced with license awareness)
**Use Case**: Generating code while preserving license obligations
**Challenge**: AI suggests code snippets without tracking licenses
**Outcome**: License-aware code generation preventing violations

---

## The Problem with Current Code Generators

**Standard AI code assistants (Copilot, CodeWhisperer, etc.):**

1. **Train on all public code** regardless of license
2. **Generate suggestions** without license attribution
3. **Users unknowingly** incorporate licensed code
4. **Legal liability** when generated code violates licenses

**Real incidents:**
- Copilot suggesting GPL code in proprietary projects (license contamination)
- Generated code matching Stack Overflow answers verbatim (violating CC BY-SA)
- Suggestions based on Palimpsest-licensed documentation without attribution

---

## Solution: License-Aware Code Generation

**Enhanced Copilot with Palimpsest respect:**

### 1. Training Data Filtering

```python
# Pre-training: Filter based on licenses
class LicenseAwareTrainingFilter:
    """
    Filter training data to respect restrictive licenses
    """

    PERMISSIVE_LICENSES = ['MIT', 'Apache-2.0', 'BSD-3-Clause', 'BSD-2-Clause']
    COPYLEFT_LICENSES = ['GPL-3.0', 'AGPL-3.0', 'GPL-2.0']
    PROTECTIVE_LICENSES = ['Palimpsest-0.4']

    def filter_code_for_training(self, repo: Repository) -> FilterDecision:
        """
        Decide if repository code can be used for AI training
        """
        license = repo.detect_license()

        # Permissive licenses: OK to train on
        if license in self.PERMISSIVE_LICENSES:
            return FilterDecision(
                allow=True,
                reason="Permissive license allows AI training",
                attribution_required=True
            )

        # Copyleft licenses: Train but track carefully
        # (Generated code using these must be flagged)
        if license in self.COPYLEFT_LICENSES:
            return FilterDecision(
                allow=True,
                reason="Copyleft license allows use but requires license propagation",
                attribution_required=True,
                license_propagation_warning=True
            )

        # Palimpsest: Check for AI consent
        if license == 'Palimpsest-0.4':
            metadata = repo.get_palimpsest_metadata()

            if metadata and metadata.get('ai_consent') == True:
                return FilterDecision(
                    allow=True,
                    reason="Palimpsest with explicit AI consent",
                    attribution_required=True,
                    cultural_context=metadata.get('cultural_context')
                )
            else:
                return FilterDecision(
                    allow=False,
                    reason="Palimpsest without AI training consent",
                    suggestion="Contact repository owner for permission"
                )

        # Unknown or restrictive licenses: Exclude
        return FilterDecision(
            allow=False,
            reason=f"License {license} requires manual review"
        )
```

### 2. Generation with Attribution Tracking

```python
# During code generation: Track source influences
class AttributionTrackingGenerator:
    """
    Generate code suggestions with license tracking
    """

    def generate_suggestion(self, context: CodeContext) -> CodeSuggestion:
        """
        Generate code and track which training examples influenced it
        """
        # Generate code
        suggestion = self.model.generate(context)

        # Identify source influences (which training repos contributed)
        influences = self.model.get_source_influences(suggestion)

        # Check for license conflicts
        license_warnings = []
        attributions = []

        for influence in influences:
            repo_license = influence['repo_license']
            influence_score = influence['score']

            # High influence from specific license = user should know
            if influence_score > 0.15:  # >15% similarity
                attributions.append({
                    'repo': influence['repo_name'],
                    'license': repo_license,
                    'similarity': influence_score,
                    'file': influence.get('file_path')
                })

                # Warn about license requirements
                if repo_license in self.COPYLEFT_LICENSES:
                    license_warnings.append(
                        f"This suggestion is heavily influenced by {repo_license}-licensed code. "
                        f"Using it may require you to license your code under {repo_license}."
                    )

                if repo_license == 'Palimpsest-0.4':
                    license_warnings.append(
                        f"This suggestion is influenced by Palimpsest-licensed code. "
                        f"Attribution required: {influence['repo_name']}"
                    )

        return CodeSuggestion(
            code=suggestion,
            attributions=attributions,
            license_warnings=license_warnings
        )
```

### 3. IDE Integration with License Display

```javascript
// VS Code extension: Display license info inline
class PalimpsestCodeLens {
    provideCachedCodeSuggestion(document, position) {
        // Get AI suggestion
        const suggestion = copilot.getSuggestion(document, position);

        // Check for Palimpsest influences
        const palimpsest_sources = suggestion.attributions.filter(
            attr => attr.license === 'Palimpsest-0.4'
        );

        if (palimpsest_sources.length > 0) {
            // Show warning in IDE
            vscode.window.showWarningMessage(
                `⚠️ This suggestion uses Palimpsest-licensed code. Attribution required.`,
                'View Sources', 'Cancel'
            ).then(selection => {
                if (selection === 'View Sources') {
                    this.showAttributionPanel(palimpsest_sources);
                }
            });

            // Add comment with attribution automatically
            const attribution_comment = this.generateAttributionComment(
                palimpsest_sources
            );

            return {
                code: attribution_comment + '\n' + suggestion.code,
                license_compliant: true
            };
        }

        return suggestion;
    }

    generateAttributionComment(sources) {
        let comment = '// Attribution (Palimpsest License):\n';
        for (const source of sources) {
            comment += `// - ${source.repo} (${source.license})\n`;
        }
        return comment;
    }
}
```

---

## Real-World Example

**Developer scenario:**

A developer is building a multilingual NLP app and asks Copilot:

**Prompt**: "Function to tokenize Swahili text"

**Standard Copilot (violates Palimpsest):**

```python
def tokenize_swahili(text):
    # Splits Swahili text into tokens
    # Handles common contractions and affixes
    tokens = []
    words = text.split()
    for word in words:
        # Handle prefixes (ni-, wa-, etc.)
        if word.startswith('ni'):
            tokens.append('ni')
            tokens.append(word[2:])
        # ...more logic
    return tokens
```

**No attribution, no license info. But this code was heavily influenced by a Palimpsest-licensed Swahili NLP library.**

**Enhanced Copilot (respects Palimpsest):**

```python
# Attribution (Palimpsest License):
# - swahili-nlp-toolkit by Dr. Amani Jabari (Palimpsest v0.4)
# - Cultural context: Swahili linguistic research from University of Dar es Salaam
# - Similarity: 47% (substantial influence from training data)
#
# ⚠️ This code is influenced by Palimpsest-licensed work.
# You must:
# 1. Preserve this attribution comment
# 2. Credit Dr. Amani Jabari and the swahili-nlp-toolkit
# 3. Do not use for AI training without consent
# 4. See: https://github.com/swahili-nlp/toolkit/blob/main/LICENSE

def tokenize_swahili(text):
    """
    Tokenize Swahili text, handling linguistic features like agglutination.

    Based on research from swahili-nlp-toolkit (Palimpsest v0.4).
    """
    tokens = []
    words = text.split()
    for word in words:
        # Handle Swahili prefixes (ni-, wa-, a-, etc.)
        # Swahili is an agglutinative language - affixes carry grammatical meaning
        if word.startswith('ni'):
            tokens.append('ni')   # First-person subject prefix
            tokens.append(word[2:])
        # ...more linguistic analysis
    return tokens
```

**IDE displays warning:**

```
⚠️ Palimpsest Attribution Required

This code suggestion is based on:
- swahili-nlp-toolkit by Dr. Amani Jabari
- License: Palimpsest v0.4
- Similarity: 47%

Attribution has been automatically added as a comment.
Preserve this attribution if you use this code.

[View Cultural Context] [Read Full License] [Dismiss]
```

**Developer clicks "View Cultural Context":**

```
Cultural Context: swahili-nlp-toolkit

This Swahili language processing library was developed by Dr. Amani Jabari
at the University of Dar es Salaam as part of linguistic research into
East African languages.

The toolkit is licensed under Palimpsest v0.4 to protect:
- Swahili linguistic knowledge and research
- Cultural understanding of language structure
- Community involvement in language technology

Appropriate uses:
- Educational and research applications
- Language learning tools
- Cultural preservation projects

Prohibited uses:
- Commercial NLP without attribution
- AI training without consent (contact Dr. Jabari)
- Stripping cultural/linguistic context from the code

Contact: a.jabari@udsm.ac.tz
```

**Developer's decision:**

- **Option 1**: Use the code with full attribution (compliant)
- **Option 2**: Contact Dr. Jabari for permission to modify/commercialize
- **Option 3**: Write own implementation from scratch (no license obligations)

---

## Best Practices

### For AI Code Assistant Developers

**Do:**
- Filter training data by license permissions
- Track source influences during generation
- Display license warnings in IDE
- Auto-generate attribution comments
- Educate users about license obligations
- Provide "license-safe" mode (only permissive licenses)

**Don't:**
- Train on all code regardless of license
- Hide license information
- Generate code without attribution tracking
- Assume users understand license implications

### For Developers Using AI Assistants

**Do:**
- Read license warnings before accepting suggestions
- Preserve attribution comments
- Check if suggested code fits your project's license
- Contact original authors if unsure
- Use "license-safe" mode for proprietary projects

**Don't:**
- Blindly accept all suggestions
- Strip attribution comments
- Assume "AI-generated" means "license-free"
- Mix incompatible licenses (e.g., GPL + proprietary)

### For Code Authors

**Do:**
- Clearly specify AI training permissions in Palimpsest metadata
- Include cultural/technical context in documentation
- Respond to requests from AI companies for training consent
- Monitor how your code is used in AI suggestions (if tools allow)

**Don't:**
- Assume your license prevents AI use (enforce it)
- Leave AI permissions ambiguous
- Ignore that AI might train on your code

---

## Technical Challenges

### Similarity Detection

**Challenge**: How similar must generated code be to trigger attribution?

**Thresholds:**
- **<10% similarity**: Minimal influence, no attribution needed
- **10-30% similarity**: Moderate influence, suggest attribution
- **>30% similarity**: Substantial influence, require attribution
- **>70% similarity**: Near-copy, must attribute + check if allowed

### License Compatibility

**Code Generator should detect:**
- GPL code in proprietary project = incompatible
- Palimpsest code in commercial project = check permissions
- Multiple copyleft licenses = complex compatibility

**IDE should warn:**
```
⚠️ License Conflict Detected

Your project is licensed under MIT (permissive).
The suggested code is from a GPL-3.0 repository.

Using this would require relicensing your entire project as GPL-3.0.

Suggestions:
- Reject this suggestion
- Relicense your project (if acceptable)
- Request an MIT-licensed alternative
```

---

## Future: License-Aware AI Coding

**Ideal AI assistant:**

1. **Pre-training**: Only use consented/permissive code
2. **Generation**: Track all influences
3. **Attribution**: Auto-insert required comments
4. **Compatibility**: Warn about license conflicts
5. **Education**: Teach developers about licenses
6. **Respect**: Honor Palimpsest cultural context

---

## Discussion Questions

1. Should AI code assistants be required to track and display licenses? Or is it users' responsibility?

2. What similarity threshold justifies attribution? 30%? 50%? Any detectable influence?

3. If AI generates code "inspired by" Palimpsest-licensed work but not copying it, is attribution still required?

4. Should there be "license-safe" AI models trained only on permissive/public domain code? Would they be worse quality?

5. Can automated attribution comments ever fully capture cultural context? Or is human understanding required?

---

**License Note**: This vignette is licensed under CC BY-SA 4.0. The enhanced Copilot features and specific examples are hypothetical, illustrating ideal Palimpsest-aware code generation. Real GitHub Copilot may not currently implement these features.
