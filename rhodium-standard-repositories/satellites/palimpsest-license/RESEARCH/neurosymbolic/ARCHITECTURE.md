# Neurosymbolic SLM Architecture

**Status:** FIRST PASS / RESEARCH
**Purpose:** Trustworthy AI for legal/consent reasoning

---

## The Problem with Current AI

Most language models:
- Hallucinate confidently
- Can't cite sources reliably
- Treat all tokens as equal (legal terms = casual speech)
- Have no concept of "I don't know"
- Cannot be constrained by formal knowledge

For legal/consent work, this is dangerous.

---

## Proposed Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    USER QUERY                           │
│         "Can I use this image for AI training?"         │
└──────────────────────┬──────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────┐
│              NEURAL LAYER (Small LM)                    │
│  ────────────────────────────────────────────────────── │
│  • Parse natural language query                         │
│  • Extract entities: work, intended use, context        │
│  • Generate candidate responses                         │
│  • CANNOT emit anything not validated below             │
└──────────────────────┬──────────────────────────────────┘
                       │ Candidates
                       ▼
┌─────────────────────────────────────────────────────────┐
│            SYMBOLIC LAYER (OCanren + Ontology)          │
│  ────────────────────────────────────────────────────── │
│  • Check candidates against formal knowledge            │
│  • Run license compatibility queries                    │
│  • Verify consent states                                │
│  • Return: VALID / INVALID / UNKNOWN                    │
└──────────────────────┬──────────────────────────────────┘
                       │ Validated
                       ▼
┌─────────────────────────────────────────────────────────┐
│              RESPONSE GENERATOR                         │
│  ────────────────────────────────────────────────────── │
│  • Only emit validated statements                       │
│  • Cite sources (OSCOLA format)                         │
│  • Explicitly mark uncertainty                          │
│  • Include apophatic markers where relevant             │
└──────────────────────┴──────────────────────────────────┘
```

---

## Key Constraints

### 1. Neural Layer is Muzzled

The neural component CANNOT:
- Make claims about license terms
- Assert consent status
- Generate legal advice
- Claim certainty it doesn't have

It CAN:
- Parse natural language
- Identify entities and intents
- Generate candidate phrasings
- Ask clarifying questions

### 2. Symbolic Layer is Authoritative

The symbolic component IS:
- The source of truth for license facts
- The arbiter of logical consistency
- The generator of formal queries
- The validator of all claims

### 3. Uncertainty is Explicit

Three response modes:
- **KNOWN TRUE**: "This use IS permitted by the license (citing clause X)"
- **KNOWN FALSE**: "This use IS NOT permitted (citing clause Y)"
- **UNKNOWN**: "I cannot determine this. Here's why, and what would resolve it."

No confident bullshitting.

---

## Training Approach

### Neural Layer
- Fine-tune small model (e.g., Phi-3, Mistral 7B) on:
  - Legal document parsing
  - Entity extraction from license texts
  - Question parsing for licensing queries
- NOT trained to generate legal opinions

### Symbolic Layer
- Hand-crafted rules from license texts
- OCanren relations for reasoning
- Ontology from LKIF + SPDX + Palimpsest

### Integration
- Constrained decoding: neural can only emit what symbolic validates
- Retrieval augmentation: neural retrieves, symbolic verifies
- Chain-of-thought with symbolic checkpoints

---

## Questions Emerged

1. **How small can the neural layer be and still parse effectively?**
   - Can we use sub-1B parameter models?
   - Is this a classification task more than generation?

2. **What's the latency budget?**
   - Symbolic reasoning can be slow
   - Is real-time interaction possible?

3. **How do we handle edge cases?**
   - Novel license combinations
   - Ambiguous natural language
   - Conflicting information

4. **What training data exists for legal NLU?**
   - Legal document corpora
   - License text datasets
   - Consent interaction logs

5. **Can this architecture handle the "emotional lineage" dimension?**
   - Symbolic for what CAN be formalized
   - But emotional significance resists formalization
   - Hybrid approach: symbolic for facts, neural for interpretation?

6. **How do we evaluate trustworthiness?**
   - Legal expert review
   - Adversarial testing
   - Real-world usage monitoring

---

## Apophatic Features

The system should explicitly mark:

```
APOPHATIC MARKERS
├── "This question involves emotional significance that
│    I cannot formally evaluate"
├── "The cultural context of this work is beyond my
│    knowledge base"
├── "Legal interpretation in your jurisdiction may
│    differ—consult a lawyer"
└── "This metadata is not the work itself"
```

These aren't failure modes—they're features.

---

## Implementation Phases

### Phase 1: Symbolic Foundation
- OCanren knowledge base
- License fact database
- Query interface (CLI)

### Phase 2: Neural Parser
- Entity extraction model
- Intent classification
- Question parsing

### Phase 3: Integration
- Constrained generation pipeline
- Validation loop
- Response formatting

### Phase 4: Apophatic Layer
- Uncertainty quantification
- Domain boundary detection
- Explicit limitation markers

---

## Research Dependencies

- **OCanren design** (see OCANREN_DESIGN.md)
- **Palimpsest ontology** (to be designed)
- **Training data curation** (legal corpora)
- **Small LM selection** (Phi-3? Mistral? Custom?)

---

## Open Questions

- Is this architecture actually novel or just careful engineering?
- What does "trustworthy" mean formally?
- How do we prevent gaming/adversarial attacks?
- Can this scale to real-world legal complexity?
- What happens when symbolic and neural disagree?
