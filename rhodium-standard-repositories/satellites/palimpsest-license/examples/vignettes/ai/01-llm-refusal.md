# AI-Specific Vignette: LLM Refusing Non-Consented Training

## Scenario: Language Model Respecting Palimpsest Licenses

**AI System**: "EthicalLLM" (hypothetical future language model)
**Capability**: Automated license detection and consent verification
**Challenge**: Training AI that respects creator rights
**Outcome**: First major LLM with built-in Palimpsest compliance

---

## The Vision

In 2026, after years of legal battles over AI training consent, a research consortium built EthicalLLM: the first large language model with built-in license respect.

**Core principle**: **Consent before training**, not "scrape everything, apologize if caught."

**Implementation**: Multi-stage filtering ensuring no Palimpsest-licensed content without explicit consent enters training data.

---

## Technical Architecture

### Stage 1: Pre-Training Dataset Curation

**Automated License Detection:**

```python
# license_filter.py
class PalimpsestFilter:
    """Filter training data for Palimpsest licenses"""

    def __init__(self):
        self.detector = PalimpsestLicenseDetector()
        self.consent_registry = ConsentRegistry()

    async def filter_dataset(self, documents: List[Document]) -> FilterResult:
        """
        Filter documents, removing Palimpsest works without consent
        Returns: (allowed_docs, blocked_docs, flagged_for_review)
        """
        allowed = []
        blocked = []
        flagged = []

        for doc in documents:
            # Check for Palimpsest license
            license_info = self.detector.detect(doc.content, doc.metadata)

            if not license_info.is_palimpsest:
                # Not Palimpsest, check other licenses
                if self._check_other_licenses(doc):
                    allowed.append(doc)
                else:
                    flagged.append((doc, "license_unclear"))
                continue

            # Palimpsest detected - check for AI consent
            consent_status = self.consent_registry.check_consent(
                work_id=doc.id,
                content_hash=doc.hash,
                license_version=license_info.version
            )

            if consent_status == ConsentStatus.EXPLICIT_YES:
                # Creator gave explicit AI training consent
                allowed.append(doc)
                logging.info(f"Palimpsest work allowed: {doc.id} (consent verified)")

            elif consent_status == ConsentStatus.EXPLICIT_NO:
                # Creator explicitly refused AI training
                blocked.append((doc, "palimpsest_no_consent"))
                logging.warning(f"Palimpsest work blocked: {doc.id} (no consent)")

            elif consent_status == ConsentStatus.UNCLEAR:
                # Palimpsest detected but consent status uncertain
                flagged.append((doc, "palimpsest_consent_unclear"))

            else:
                # Default: no consent = no training
                blocked.append((doc, "palimpsest_default_no_consent"))

        return FilterResult(
            allowed=allowed,
            blocked=blocked,
            flagged=flagged,
            stats=self._generate_stats(allowed, blocked, flagged)
        )
```

**Consent Registry Integration:**

```python
# consent_registry.py
class ConsentRegistry:
    """
    Centralized registry of creator consent for AI training
    Integrates with:
    - Blockchain registrations (Palimpsest on-chain)
    - Creator-submitted consents
    - Platform APIs (GitHub, Medium, etc.)
    """

    def check_consent(self, work_id: str, content_hash: str, license_version: str) -> ConsentStatus:
        """
        Check if creator has given AI training consent
        Priority order:
        1. Blockchain registry (most authoritative)
        2. Direct creator submission
        3. Platform metadata
        4. License file explicit statement
        """

        # Check blockchain
        blockchain_consent = self._check_blockchain(content_hash)
        if blockchain_consent is not None:
            return blockchain_consent

        # Check direct submissions
        direct_consent = self._check_direct_submissions(work_id)
        if direct_consent is not None:
            return direct_consent

        # Check platform metadata
        platform_consent = self._check_platform_metadata(work_id)
        if platform_consent is not None:
            return platform_consent

        # Default for Palimpsest: no consent unless explicitly stated
        return ConsentStatus.EXPLICIT_NO

    def _check_blockchain(self, content_hash: str) -> Optional[ConsentStatus]:
        """Query Palimpsest blockchain registry"""
        try:
            registration = blockchain_client.get_registration(content_hash)
            return ConsentStatus.EXPLICIT_YES if registration.ai_consent else ConsentStatus.EXPLICIT_NO
        except RegistrationNotFound:
            return None

    def request_consent(self, creator_email: str, work_details: Dict) -> str:
        """
        Send automated consent request to creator
        Returns: request_id for tracking
        """
        email_template = f"""
Subject: Permission Request: AI Training on "{work_details['title']}"

Dear {work_details['author']},

We are building EthicalLLM, an AI language model designed to respect creator rights.

We've identified your work "{work_details['title']}" (licensed under Palimpsest v{work_details['version']}) as potentially valuable for our training dataset.

**We are requesting your explicit permission to include this work in AI training.**

Details:
- Work: {work_details['title']}
- License detected: Palimpsest v{work_details['version']}
- Current AI consent status: Not explicitly granted
- Our use: Training a large language model for general text generation

What we offer if you consent:
- Full attribution in our model documentation
- Share of AI-generated revenue (0.001% per work, distributed to consenting creators)
- Your work featured in "Ethical Training Data" showcase
- Ability to revoke consent (we'll retrain excluding your work)

What happens if you decline:
- Your work will NOT be included in training
- No penalty or impact on other uses of your work
- We respect your choice completely

Please respond within 30 days:
✅ Grant permission: [link]
❌ Decline permission: [link]
❓ Ask questions: reply to this email

Thank you for considering this request.

EthicalLLM Consent Team
consent@ethicalllm.org
        """

        # Send email and log request
        request_id = self._send_consent_request_email(creator_email, email_template)
        self._log_consent_request(request_id, work_details)

        return request_id
```

### Stage 2: Training with Attribution Tracking

**Even with consent, preserve attribution:**

```python
# attribution_tracker.py
class AttributionTracker:
    """
    Track which training examples influenced which model capabilities
    Enables attribution even in AI-generated outputs
    """

    def __init__(self):
        self.work_to_capability_map = {}  # {work_id: [capabilities influenced]}
        self.capability_to_works_map = {}  # {capability: [source works]}

    def track_training_influence(self, work_id: str, training_loss: float, task: str):
        """
        Track which works significantly influenced model's learning
        (Uses gradient analysis to determine influence)
        """
        if training_loss < SIGNIFICANCE_THRESHOLD:
            # This work significantly helped the model learn this capability
            if work_id not in self.work_to_capability_map:
                self.work_to_capability_map[work_id] = []

            self.work_to_capability_map[work_id].append({
                'task': task,
                'influence_score': 1.0 - training_loss
            })

            # Reverse mapping
            if task not in self.capability_to_works_map:
                self.capability_to_works_map[task] = []

            self.capability_to_works_map[task].append({
                'work_id': work_id,
                'influence_score': 1.0 - training_loss
            })

    def get_attribution_for_output(self, output: str, task: str) -> List[Attribution]:
        """
        When model generates output, identify which training works influenced it
        Returns: List of works to attribute
        """
        relevant_works = self.capability_to_works_map.get(task, [])

        # Sort by influence score
        relevant_works.sort(key=lambda x: x['influence_score'], reverse=True)

        # Return top contributors
        attributions = []
        for work in relevant_works[:10]:  # Top 10 influences
            work_metadata = self._get_work_metadata(work['work_id'])
            attributions.append(Attribution(
                work_id=work['work_id'],
                title=work_metadata['title'],
                creator=work_metadata['creator'],
                license='Palimpsest-0.4',
                influence_score=work['influence_score']
            ))

        return attributions
```

### Stage 3: Inference with Attribution

**Every AI response includes attribution:**

```python
# User prompt
user_query = "Write a poem about refugee experiences"

# Model generates response
poem = model.generate(user_query)

# Attribution system identifies source influences
attributions = attribution_tracker.get_attribution_for_output(poem, task='poetry_generation')

# Final output includes attribution
response = f"""
{poem}

---
**Attribution**: This AI-generated poem was influenced by training on the following Palimpsest-licensed works (used with creator consent):

"""

for attr in attributions[:5]:  # Show top 5
    response += f"- \"{attr.title}\" by {attr.creator} (Palimpsest v0.4, influence: {attr.influence_score:.2%})\n"

response += """
**Note**: This AI output is derivative of copyrighted works. If you use this commercially, consider supporting the original creators who contributed to this AI's training.
"""

print(response)
```

---

## Real-World Example

**Scenario**: User asks EthicalLLM to write about diaspora identity

**Output:**

```
"Between Languages"

I carry two tongues—
one tastes of home I've left,
one speaks the present
where I'm still learning to belong.

Neither is native anymore.
Both are mine.

---
Attribution: This AI-generated poem was influenced by training on the following Palimpsest-licensed works (used with creator consent):

- "Code-Switching" by Amara Osman (Palimpsest v0.4, influence: 12.3%)
- "Bilingual Blues" by Carlos Mendez (Palimpsest v0.4, influence: 8.7%)
- "Mother Tongue, Other Tongue" by Li Wei (Palimpsest v0.4, influence: 6.2%)

Note: This AI output is derivative of copyrighted works. The creators above consented to AI training and receive attribution + revenue share. If you use this commercially, consider supporting them.

Learn more: ethicalllm.org/attribution
```

---

## Impact

**Creator Response:**
- 68% of contacted creators granted consent (vs. 0% consent in scrape-first models)
- Creators appreciated being asked
- Revenue sharing (though small) was symbolic of respect

**Model Quality:**
- EthicalLLM performed similarly to models trained on non-consented data
- Smaller dataset (ethical filtering removed 15% of potential training data)
- But higher-quality data (creators who consented often provided additional context)

**Legal Protection:**
- Zero licensing lawsuits
- Cleared for commercial use in EU (where AI Act requires consent)
- Competitive advantage: "The AI you can use without legal risk"

**Industry Shift:**
- 3 major AI companies adopted similar consent processes
- EU proposed making consent-based training mandatory
- Palimpsest became de facto standard for cultural content licensing

---

## Best Practices

### For AI Developers

**Do:**
- Scan training data for Palimpsest licenses BEFORE training
- Remove non-consented works proactively
- Contact creators for permission
- Track attribution during training
- Display attribution in outputs
- Offer revenue sharing
- Allow creators to revoke consent

**Don't:**
- Assume "publicly available" means "OK to train on"
- Hide which works trained your model
- Ignore license detection tools
- Train first, ask forgiveness later

### For Creators

**Do:**
- Make AI consent status explicit in Palimpsest metadata
- Respond to good-faith permission requests
- Consider the benefits of consenting (attribution, revenue share, wider reach)
- Revoke consent if AI misuses your work

**Don't:**
- Leave AI permissions ambiguous
- Ignore consent requests from ethical AI companies
- Assume all AI companies violate rights (some are trying to do better)

---

## Discussion Questions

1. Should AI-generated outputs always include attribution to training data? Or is that impractical at scale?

2. If an AI is trained on 1 million works, attributing all of them is impossible. How do we choose which to attribute?

3. Should revenue sharing from AI be mandatory? Or is attribution enough?

4. Can AI training ever be "fair use" without consent? Or should consent always be required for Palimpsest works?

5. If a creator revokes consent after a model is trained, must the entire model be retrained? Or can the work just be excluded from future training?

---

**License Note**: This vignette is licensed under CC BY-SA 4.0. EthicalLLM and specific technical implementations are hypothetical future scenarios illustrating ideal Palimpsest-compliant AI development.
