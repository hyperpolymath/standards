# Frequently Asked Questions (FAQ)

Comprehensive answers to questions about HTTP 430 (Consent Required) and AIBDP (AI Boundary Declaration Protocol).

## Table of Contents

- [General Concepts](#general-concepts)
- [HTTP 430 Status Code](#http-430-status-code)
- [AIBDP Manifest](#aibdp-manifest)
- [Implementation](#implementation)
- [Legal and Ethical](#legal-and-ethical)
- [Technical Details](#technical-details)
- [Adoption and Deployment](#adoption-and-deployment)
- [Troubleshooting](#troubleshooting)

---

## General Concepts

### What is consent-aware HTTP infrastructure?

Consent-aware HTTP infrastructure is a set of web protocols that enable content publishers to declare boundaries for AI usage and enforce those boundaries through standard HTTP mechanisms. It consists of:

1. **HTTP 430 (Consent Required)** - A new HTTP status code for rejecting requests when AI consent requirements are not met
2. **AIBDP** - A manifest format for declaring AI usage boundaries at `/.well-known/aibdp.json`

Together, these provide a standardized way to say "no" to unauthorized AI use while maintaining web openness for legitimate access.

### Why is this needed? Doesn't robots.txt already do this?

robots.txt was designed for web crawlers in the 1990s and has significant limitations for modern AI:

- **robots.txt** controls crawling and indexing
- **AIBDP** controls AI-specific uses like training, generation, fine-tuning, and commercial exploitation

robots.txt can't distinguish between "index my content for search" and "train your LLM on my content." AIBDP fills this gap with granular policy declarations.

### Is this anti-AI?

**No.** This is **pro-boundary**. The protocols enable:

- **Allowed**: "Yes, you may use my content for search indexing"
- **Conditional**: "Yes, but only with attribution and for non-commercial purposes"
- **Refused**: "No, training on my creative work is not permitted"
- **Encouraged**: "Yes, I actively support AI fine-tuning for accessibility tools"

Many implementers will allow or encourage AI use. The point is **procedural clarity** and **consent**, not blanket prohibition.

### What's the philosophical foundation?

This project draws on:

- **bell hooks**: "Boundary is where meaning begins" - boundary-setting as care and dignity
- **Virginia Woolf**: Architectural refusal as a form of self-determination
- **Journalism ethics**: The right to decline co-option without justification
- **IndieWeb principles**: Self-sovereignty and federated control

"Without refusal, permission is meaningless."

### Who created this?

Jonathan D.A. Jewell, NEC PRC Representative and NUJ Ethics Council member, with input from journalism ethics communities, IndieWeb participants, and federated web advocates.

---

## HTTP 430 Status Code

### What is HTTP 430?

HTTP 430 (Consent Required) is a proposed HTTP status code that servers use to reject requests when AI-specific consent requirements declared in an AIBDP manifest are not satisfied.

Example response:

```http
HTTP/1.1 430 Consent Required
Link: <https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"
Retry-After: 86400

{
  "error": "AI usage boundaries not satisfied",
  "manifest": "https://example.org/.well-known/aibdp.json"
}
```

### How is 430 different from 403 (Forbidden)?

| Code | Meaning | Use Case |
|------|---------|----------|
| **403** | Forbidden | General authorization failure |
| **428** | Precondition Required | Missing required headers |
| **451** | Legal | Content blocked for legal reasons (court orders) |
| **430** | Consent Required | AI consent boundaries violated |

430 provides **procedural clarity**: "Your AI agent violated declared boundaries. See manifest for details."

### Is HTTP 430 an official standard?

Not yet. It's defined in `draft-jewell-http-430-consent-required-00`, an Internet-Draft intended for IETF submission. It follows RFC processes for new HTTP status codes.

### Can I use HTTP 430 today?

**Yes.** While not yet standardized, HTTP allows custom status codes in the 4xx range. Many implementers are using 430 now to:

1. Signal intent to AI system operators
2. Generate public discussion
3. Demonstrate demand for standardization
4. Establish prior art for IETF submission

### What if AI systems ignore HTTP 430?

HTTP 430 is **declarative**, not **enforcement**. It provides:

- **Procedural clarity**: Clear signal of boundary violation
- **Good-faith compliance**: Responsible AI operators respect it
- **Legal foundation**: Documented evidence of non-consent
- **Reputational pressure**: Public violations harm AI system credibility

Physical enforcement requires additional measures (authentication, rate limiting, legal action).

---

## AIBDP Manifest

### What is an AIBDP manifest?

An AIBDP (AI Boundary Declaration Protocol) manifest is a JSON file hosted at `/.well-known/aibdp.json` that declares what AI uses are allowed, refused, conditional, or encouraged for a website's content.

### What does a minimal manifest look like?

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:policy@example.org",
  "policies": {
    "training": { "status": "refused" },
    "indexing": { "status": "allowed" }
  }
}
```

### What policy types can I declare?

- **training**: Use for AI model training
- **indexing**: Building searchable indices
- **summarization**: Generating summaries
- **question_answering**: RAG systems answering queries
- **generation**: Synthetic content creation
- **fine_tuning**: Specialized model adaptation
- **embedding**: Creating vector representations
- **commercial_training**: Training commercial AI products

You can also define custom policies.

### What are the policy status values?

- **allowed**: Usage permitted without conditions
- **refused**: Usage explicitly prohibited
- **conditional**: Usage permitted if conditions are met
- **encouraged**: Usage actively welcomed

### Can I have different policies for different parts of my site?

**Yes!** Use the `scope` field:

```json
{
  "policies": {
    "training": {
      "status": "conditional",
      "conditions": ["Attribution required"],
      "scope": ["/blog/**", "/articles/**"],
      "exceptions": [
        {
          "path": "/public/**",
          "status": "allowed"
        }
      ]
    }
  }
}
```

### How do I specify conditions?

Use the `conditions` array:

```json
{
  "training": {
    "status": "conditional",
    "conditions": [
      "Attribution required to 'Example Org'",
      "Non-commercial use only",
      "Preserve context and citations"
    ]
  }
}
```

AI systems can check these via consent headers (e.g., `AI-Consent-Conditions: attribution,non-commercial`).

### Where should I host the manifest?

**Required location**: `/.well-known/aibdp.json`

This follows RFC 8615 (Well-Known URIs) convention used by security.txt, robots.txt equivalents, and other metadata files.

### Can I use a CDN or different domain?

**Yes**, use `canonical_uri`:

```json
{
  "canonical_uri": "https://origin.example.org/.well-known/aibdp.json",
  ...
}
```

AI systems should fetch the canonical version for authoritative policy.

### How do I validate my manifest?

Use the JSON Schema:

```bash
# Node.js (ajv-cli)
npm install -g ajv-cli
ajv validate -s schemas/aibdp-schema-v0.2.json -d .well-known/aibdp.json

# Python (jsonschema)
python -c "
import json
import jsonschema
with open('schemas/aibdp-schema-v0.2.json') as f:
    schema = json.load(f)
with open('.well-known/aibdp.json') as f:
    manifest = json.load(f)
jsonschema.validate(manifest, schema)
print('Valid!')
"
```

Or use the reference implementation validators (Node.js, Python).

### Should I sign my manifest cryptographically?

**Recommended for high-value content**. AIBDP supports COSE signatures (RFC 9052):

```json
{
  "signature": {
    "algorithm": "ES256",
    "public_key_uri": "https://example.org/.well-known/aibdp-key.jwk",
    "value": "base64-encoded-signature"
  }
}
```

This prevents intermediary tampering (CDNs, proxies) and enables audit trails.

---

## Implementation

### How do I add AIBDP to my website?

**4 steps:**

1. **Create manifest** at `.well-known/aibdp.json`
2. **Configure server** to serve it with `Content-Type: application/aibdp+json`
3. **Add enforcement** (optional): Block AI bots with HTTP 430
4. **Test** with `curl` and AI user-agents

See [server-configurations.md](server-configurations.md) for nginx, Apache, Caddy, Cloudflare, etc.

### Do I need to write code?

**No.** Server configuration (nginx, Apache) is often sufficient:

```nginx
# nginx example
location = /.well-known/aibdp.json {
    add_header Content-Type application/aibdp+json;
}

# Block AI training bots
map $http_user_agent $is_ai_bot {
    ~*GPTBot 1;
}

if ($is_ai_bot) {
    return 430;
}
```

For advanced enforcement, use middleware (Node.js, Python available).

### What if I use WordPress/Ghost/Jekyll?

- **WordPress**: Install a plugin or add to `.htaccess`
- **Ghost**: Add to nginx config or use middleware
- **Jekyll/Hugo**: Include manifest in static files, configure via Netlify/Vercel

See [server-configurations.md](server-configurations.md) for platform-specific guides.

### How do I detect AI bots?

Check `User-Agent` header:

- GPTBot (OpenAI)
- ChatGPT-User
- Claude-Web (Anthropic)
- Google-Extended
- CCBot (Common Crawl)
- PerplexityBot
- Diffbot

Reference implementations include comprehensive detection patterns.

### Can AI systems lie about their User-Agent?

**Yes.** This is a limitation of all HTTP-based detection (including robots.txt).

Mitigation strategies:
- Request patterns (high-volume sequential access)
- Require AI systems to send consent headers
- Cryptographic client authentication
- Legal/reputational consequences for deception

AIBDP establishes **procedural clarity**, not foolproof enforcement.

### What about AI systems that don't identify themselves?

AIBDP is a **good-faith protocol** for responsible AI operators. Non-compliant systems face:

- **Reputational damage**: Public disclosure of violations
- **Legal consequences**: AIBDP provides documented non-consent
- **Community pressure**: Ethical AI community standards

Perfect technical enforcement is impossible, but procedural clarity matters.

---

## Legal and Ethical

### Is AIBDP legally binding?

AIBDP itself is a **declaration protocol**, not a law. Legal effect depends on jurisdiction and applicable law:

- **Copyright**: Manifests document non-consent for training (relevant to fair use analysis)
- **Contract**: Manifests may form part of terms of service
- **Common law**: Establishes lack of implied license
- **Reputation**: Violations documented for public disclosure

Consult a lawyer for specific legal advice.

### Does this work with GDPR/privacy laws?

**Complementary.** AIBDP focuses on **content usage boundaries**, while GDPR focuses on **personal data protection**.

- GDPR: Protects individuals' personal data (PII)
- AIBDP: Protects content from unauthorized AI use (training, generation)

Both can apply simultaneously (e.g., healthcare website).

### What about copyright and fair use?

AIBDP **documents** your position on AI use, which may be relevant to fair use analysis:

**Fair use factors** (US):
1. Purpose and character (commercial?)
2. Nature of copyrighted work (creative?)
3. Amount used (whole corpus?)
4. Market effect (substitute for original?)

AIBDP manifest provides evidence that:
- You did NOT grant implied license
- You declared boundaries explicitly
- Commercial AI training may harm your market

Courts will decide, but documentation helps.

### Can I charge for AI training access?

**Yes.** Use `conditional` status:

```json
{
  "commercial_training": {
    "status": "conditional",
    "conditions": [
      "Licensing agreement required",
      "Contact legal@example.org for terms"
    ]
  }
}
```

AIBDP supports **licensing models**, not just prohibition.

### Is this compatible with Creative Commons licenses?

**Yes.** AIBDP and CC licenses work together:

```json
{
  "policies": {
    "training": {
      "status": "allowed",
      "conditions": [
        "Respect CC BY-SA 4.0 license terms",
        "Preserve attribution",
        "Share-alike applies to derivatives"
      ]
    }
  },
  "metadata": {
    "license": "CC BY-SA 4.0"
  }
}
```

AIBDP provides **AI-specific** guidance within your broader licensing framework.

---

## Technical Details

### What's the relationship to robots.txt?

**Complementary:**

| Feature | robots.txt | AIBDP |
|---------|-----------|-------|
| **Purpose** | Crawling control | AI usage boundaries |
| **Granularity** | Path-based | Purpose-based (training, generation, etc.) |
| **Format** | Text | JSON |
| **AI-specific** | No | Yes |
| **Conditional** | No | Yes (status: conditional) |

**Use both:**
- robots.txt: Control indexing
- AIBDP: Control AI training/generation

### How does path scoping work?

Glob-style patterns:

- `/articles/**` - All files under /articles/
- `*.pdf` - All PDF files
- `/docs/public/**` - Subdirectory matching
- `all` - Entire site

Example:

```json
{
  "training": {
    "status": "refused",
    "scope": ["/blog/**", "/articles/**"]
  }
}
```

### What Content-Type should I use?

```http
Content-Type: application/aibdp+json
```

This is the IANA-registered media type for AIBDP manifests (pending registration).

### Should I cache the manifest?

**Yes.** Set appropriate headers:

```http
Cache-Control: public, max-age=3600
```

AI systems should cache for the duration specified in `expires` field or 1 hour default.

### Can I have multiple manifests for different subdomains?

**Yes.** Each domain/subdomain can have its own manifest:

- `https://example.org/.well-known/aibdp.json`
- `https://blog.example.org/.well-known/aibdp.json`

Or use `canonical_uri` to share a single policy.

### How do I handle CDN caching?

**Option 1**: Set `canonical_uri` to origin:

```json
{
  "canonical_uri": "https://origin.example.org/.well-known/aibdp.json"
}
```

**Option 2**: Sign manifest cryptographically (COSE) to ensure integrity through CDN.

### What about Cloudflare/AWS CloudFront?

See [server-configurations.md](server-configurations.md) for:

- Cloudflare Workers enforcement
- AWS Lambda@Edge implementation
- CloudFront distribution configuration

---

## Adoption and Deployment

### Should I adopt this now or wait for standardization?

**Adopt now** if:
- You want to declare AI boundaries immediately
- You're comfortable with emerging standards
- You want to influence standardization process

**Wait** if:
- You need IETF-approved standards only
- Your organization has strict policy requirements
- You prefer mature, widely-adopted protocols

Many sites are adopting now to establish boundaries and demonstrate demand.

### Who else is using this?

(As of July 2025, this is a new protocol. Early adopters will be listed at [https://consent-aware-http.org/adopters](https://consent-aware-http.org/adopters))

**Target adopters:**
- Personal blogs and creative portfolios
- News organizations (journalism ethics alignment)
- Academic repositories
- IndieWeb participants
- Federated platforms (Mastodon, etc.)

### How do I announce my adoption?

1. Add manifest to your site
2. Publish a blog post explaining your AI policy
3. Tag social media with #AIBDP #ConsentAwareHTTP
4. Consider adding a badge (see `assets/badges/`)
5. Submit your site to adopters list

### What if I change my mind about my policies?

**Update your manifest.** AI systems should re-fetch when:

- `expires` field timestamp passes
- They receive HTTP 430 with updated `Link` header
- Regular periodic checks (default: daily)

Versioning is supported via `aibdp_version` field.

### Can I use this with a paywalled site?

**Yes.** AIBDP works with authenticated content:

```json
{
  "policies": {
    "training": {
      "status": "refused",
      "rationale": "Subscription content - training undermines our business model"
    },
    "indexing": {
      "status": "conditional",
      "conditions": ["Respect paywall", "Index headlines and summaries only"]
    }
  }
}
```

### How do I migrate from robots.txt-only?

**Keep both:**

1. Keep existing robots.txt for crawler control
2. Add AIBDP manifest for AI-specific boundaries
3. Ensure policies are consistent
4. Update over time as you refine boundaries

---

## Troubleshooting

### My manifest isn't being fetched. What's wrong?

**Check:**

1. **Location**: Must be exactly `/.well-known/aibdp.json`
2. **Permissions**: File must be readable by web server (644 or 755)
3. **Directory exists**: `.well-known/` directory must exist
4. **Server config**: Not blocked by nginx/Apache rules
5. **HTTPS**: Some AI systems require HTTPS

**Test:**
```bash
curl https://your-site.com/.well-known/aibdp.json
```

### AI bots are still accessing my site despite HTTP 430

**Remember:** AIBDP is **declarative**, not **enforcement**. Options:

1. **Contact AI operator**: Report violation via manifest `contact` field
2. **Public disclosure**: Document violation publicly
3. **Legal action**: If applicable in your jurisdiction
4. **Stronger blocking**: IP blocking, rate limiting, authentication

### My manifest validator says it's invalid

**Common issues:**

- **JSON syntax errors**: Use `jq .` to validate JSON
- **Missing required fields**: `aibdp_version`, `contact`, `policies` are required
- **Invalid status values**: Must be `allowed`, `refused`, `conditional`, or `encouraged`
- **Malformed URIs**: `contact`, `canonical_uri`, `policy_uri` must be valid URIs

Run through JSON Schema validator for detailed errors.

### How do I know if AI systems are respecting my boundaries?

**Monitoring:**

1. **Server logs**: Track User-Agent patterns and HTTP 430 responses
2. **Analytics**: Monitor bot traffic and blocked requests
3. **Community reports**: Other sites may report violations
4. **Public disclosure**: Some AI operators publish training data sources

Perfect monitoring is impossible, but violations can be documented.

### What if I accidentally set too-restrictive policies?

**Update manifest immediately:**

1. Edit `.well-known/aibdp.json`
2. Update `expires` field to force re-fetch
3. Announce change if relevant
4. AI systems should pick up new policy on next check

### Can I test my setup locally?

**Yes:**

```bash
# Test manifest accessibility
curl http://localhost:3000/.well-known/aibdp.json

# Test AI bot blocking
curl http://localhost:3000/articles/ -H "User-Agent: GPTBot/1.0"

# Should return HTTP 430

# Test normal access
curl http://localhost:3000/ -H "User-Agent: Mozilla/5.0"

# Should return HTTP 200
```

See reference implementations (Node.js, Python) for local testing.

---

## Getting Help

### Where can I ask questions?

- **GitHub Discussions**: https://github.com/Hyperpolymath/consent-aware-http/discussions
- **Issues**: https://github.com/Hyperpolymath/consent-aware-http/issues
- **Email**: jonathan@metadatastician.art

### Where can I find more information?

- **Technical guide**: [technical.md](technical.md)
- **Server configs**: [server-configurations.md](server-configurations.md)
- **Examples**: [manifest-scenarios/](../examples/manifest-scenarios/)
- **Ethics**: [ethics.md](ethics.md)
- **IETF Drafts**:
  - [draft-jewell-http-430-consent-required-00](../draft-jewell-http-430-consent-required-00.xml)
  - [draft-jewell-aibdp-00](../drafts/draft-jewell-aibdp-00.xml)

### How can I contribute?

See [CONTRIBUTING.md](../.github/CONTRIBUTING.md).

Contributions welcome:
- Documentation improvements
- Implementation examples
- Adoption stories
- Bug reports
- Feature proposals

---

_"Boundary is where meaning begins." - bell hooks_

**Last updated**: 2025-07-20
**AIBDP version**: 0.2
**HTTP 430 draft**: 00
