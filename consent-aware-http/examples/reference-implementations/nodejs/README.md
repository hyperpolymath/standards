# AIBDP + HTTP 430 Middleware for Express.js

Reference implementation of the AI Boundary Declaration Protocol (AIBDP) with HTTP 430 (Consent Required) enforcement for Node.js/Express applications.

## Features

- **AIBDP Manifest Parsing**: Load and cache `/.well-known/aibdp.json`
- **AI System Detection**: Identify AI user-agents (GPTBot, Claude-Web, etc.)
- **Policy Enforcement**: Block or allow based on declared boundaries
- **HTTP 430 Responses**: Standards-compliant consent violation responses
- **Path Scoping**: Glob-pattern matching for granular control
- **Conditional Policies**: Check for consent headers and conditions
- **Automatic Caching**: Manifest caching with configurable TTL

## Installation

```bash
npm install
```

## Quick Start

### Basic Usage

```javascript
import express from 'express';
import { aibdpMiddleware, serveManifest } from './index.js';

const app = express();

// Serve AIBDP manifest
app.use(serveManifest('.well-known/aibdp.json'));

// Apply AIBDP enforcement
app.use(aibdpMiddleware({
  manifestPath: '.well-known/aibdp.json'
}));

// Your routes
app.get('/', (req, res) => {
  res.send('Hello, consent-aware world!');
});

app.listen(3000);
```

### Run Example Server

```bash
node example-server.js
```

Then test with:

```bash
# Normal browser access (allowed)
curl http://localhost:3000/

# AI bot access (may be blocked based on manifest)
curl http://localhost:3000/article.html -H "User-Agent: GPTBot/1.0"

# View AIBDP manifest
curl http://localhost:3000/.well-known/aibdp.json
```

## API Reference

### `aibdpMiddleware(options)`

Creates Express middleware for AIBDP enforcement.

**Options:**

- `manifestPath` (string): Path to AIBDP manifest file. Default: `.well-known/aibdp.json`
- `enforceForAll` (boolean): Enforce for all requests, not just AI bots. Default: `false`
- `onViolation` (function): Callback when violation detected. Signature: `(req, policy, purpose) => {}`

**Returns:** Express middleware function

**Example:**

```javascript
app.use(aibdpMiddleware({
  manifestPath: './my-aibdp.json',
  enforceForAll: false,
  onViolation: (req, policy, purpose) => {
    console.log(`Blocked ${purpose} from ${req.ip}`);
  }
}));
```

### `serveManifest(manifestPath)`

Creates middleware to serve AIBDP manifest at `/.well-known/aibdp.json`.

**Arguments:**

- `manifestPath` (string): Path to manifest file

**Returns:** Express middleware function

**Example:**

```javascript
app.use(serveManifest('./aibdp.json'));
```

### Utility Functions

#### `isAIUserAgent(userAgent)`

Check if User-Agent header indicates an AI system.

```javascript
import { isAIUserAgent } from './index.js';

if (isAIUserAgent('GPTBot/1.0')) {
  console.log('AI system detected');
}
```

#### `extractAIPurpose(headers)`

Extract AI purpose from request headers (training, indexing, etc.).

```javascript
import { extractAIPurpose } from './index.js';

const purpose = extractAIPurpose({
  'user-agent': 'GPTBot/1.0',
  'ai-purpose': 'training'
});
console.log(purpose); // 'training'
```

#### `pathMatches(requestPath, pattern)`

Check if request path matches glob pattern from manifest.

```javascript
import { pathMatches } from './index.js';

pathMatches('/docs/guide.html', '/docs/**'); // true
pathMatches('/article.pdf', '*.pdf'); // true
pathMatches('/blog/post.html', '/docs/**'); // false
```

## Manifest Format

Example `.well-known/aibdp.json`:

```json
{
  "aibdp_version": "0.2",
  "contact": "mailto:policy@example.org",
  "policies": {
    "training": {
      "status": "conditional",
      "conditions": ["Attribution required", "Non-commercial use only"],
      "scope": ["/articles/**"]
    },
    "indexing": {
      "status": "allowed",
      "scope": "all"
    },
    "generation": {
      "status": "refused",
      "rationale": "Content should not be synthetically replicated"
    }
  }
}
```

### Policy Status Values

- `allowed`: Usage permitted without conditions
- `refused`: Usage explicitly prohibited
- `conditional`: Usage permitted if conditions met
- `encouraged`: Usage actively encouraged

## HTTP 430 Response Format

When a policy is violated, the middleware responds with HTTP 430:

```http
HTTP/1.1 430 Consent Required
Content-Type: application/json
Link: <https://example.org/.well-known/aibdp.json>; rel="blocked-by-consent"
Retry-After: 86400

{
  "error": "AI usage boundaries declared in AIBDP manifest not satisfied",
  "manifest": "https://example.org/.well-known/aibdp.json",
  "violated_policy": "training",
  "policy_status": "refused",
  "required_conditions": [],
  "rationale": "Content should not be used for training",
  "contact": "mailto:policy@example.org"
}
```

## AI Consent Headers

AI systems can indicate compliance by sending:

```http
GET /article.html HTTP/1.1
Host: example.org
User-Agent: ResearchBot/1.0
AI-Purpose: indexing
AI-Consent-Reviewed: https://example.org/.well-known/aibdp.json
AI-Consent-Conditions: attribution,non-commercial
```

The middleware checks for these headers when enforcing conditional policies.

## Detected AI User-Agents

The middleware detects the following AI systems:

- GPTBot (OpenAI)
- ChatGPT-User
- Claude-Web (Anthropic)
- Google-Extended
- CCBot (Common Crawl)
- Bingbot, Googlebot (when used for AI training)
- PerplexityBot
- Diffbot
- And more...

## Testing

Run tests:

```bash
npm test
```

## Deployment Considerations

### Production Checklist

- ✅ Create AIBDP manifest at `/.well-known/aibdp.json`
- ✅ Set appropriate `expires` field in manifest (30-90 days recommended)
- ✅ Provide contact information for policy questions
- ✅ Monitor logs for violations
- ✅ Set up manifest validation in CI/CD
- ✅ Consider HTTPS + signatures for high-value content
- ✅ Document rationale in human-readable policy page

### Performance

- Manifest is cached in memory (default: 1 hour)
- Minimal latency impact for non-AI requests
- Failed manifest loads fail open (don't break site)
- Pattern matching is optimized with regex compilation

### Security

- Manifest served over HTTPS prevents tampering
- JSON parsing errors fail gracefully
- No sensitive information in manifest
- Signature verification supported (future)

## Standards Compliance

This implementation follows:

- [draft-jewell-aibdp-00](https://github.com/Hyperpolymath/consent-aware-http/blob/main/drafts/draft-jewell-aibdp-00.xml) - AIBDP specification
- [draft-jewell-http-430-consent-required-00](https://github.com/Hyperpolymath/consent-aware-http/blob/main/draft-jewell-http-430-consent-required-00.xml) - HTTP 430 status code
- [RFC 8615](https://www.rfc-editor.org/info/rfc8615) - Well-Known URIs
- [RFC 8259](https://www.rfc-editor.org/info/rfc8259) - JSON format

## License

MIT License - see LICENSE file for details

## Contributing

See [CONTRIBUTING.md](../../../.github/CONTRIBUTING.md) in the main repository.

## Support

- **Issues**: https://github.com/Hyperpolymath/consent-aware-http/issues
- **Discussions**: https://github.com/Hyperpolymath/consent-aware-http/discussions
- **Email**: jonathan@metadatastician.art

## Related Projects

- [AIBDP Specification](https://github.com/Hyperpolymath/consent-aware-http)
- [Python Implementation](../python/)
- [Rust Implementation](../rust/)

---

_"Without refusal, permission is meaningless."_
