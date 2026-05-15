# AIBDP + HTTP 430 Middleware for Deno

Reference implementation of the AI Boundary Declaration Protocol (AIBDP) with HTTP 430 (Consent Required) enforcement for Deno servers. This is a port of the former Python/Flask reference implementation — Python is banned across the Hyperpolymath estate; Deno is the standard runtime.

## Features

- **AIBDP Manifest Parsing**: Load and cache `.well-known/aibdp.json`
- **AI System Detection**: Identify AI user-agents (GPTBot, Claude-Web, etc.)
- **Policy Enforcement**: Block or allow based on declared boundaries
- **HTTP 430 Responses**: Standards-compliant consent violation responses
- **Path Scoping**: Glob-pattern matching for granular control
- **Conditional Policies**: Check for consent headers and conditions
- **Automatic Caching**: Manifest caching with configurable TTL
- **ES modules**: Importable, framework-free (`Deno.serve`)
- **Wrapper Support**: `aibdpRequired()` for route-specific protection

## Installation

No installation step — Deno fetches dependencies on first run. Requires [Deno](https://deno.land/) 1.40+.

## Quick Start

### Basic Usage

```javascript
import { AIBDPMiddleware, serveManifest } from "./aibdp_middleware.js";

const middleware = new AIBDPMiddleware({
  manifestPath: ".well-known/aibdp.json",
});

const manifest = serveManifest();

const handler = middleware.wrap((req) => {
  const { pathname } = new URL(req.url);
  if (pathname === "/.well-known/aibdp.json") return manifest();
  return new Response("Hello, consent-aware world!");
});

Deno.serve({ port: 5000 }, handler);
```

### Run Example Server

```bash
deno run --allow-read --allow-net example_server.js
```

Then test with:

```bash
# Normal browser access (allowed)
curl http://localhost:5000/

# AI bot access (may be blocked based on manifest)
curl http://localhost:5000/article -H "User-Agent: GPTBot/1.0"

# View AIBDP manifest
curl http://localhost:5000/.well-known/aibdp.json
```

## API Reference

### `AIBDPMiddleware`

Middleware class for AIBDP enforcement.

**Constructor:**

```javascript
new AIBDPMiddleware({
  manifestPath = ".well-known/aibdp.json",
  enforceForAll = false,
  onViolation = null,
})
```

- `manifestPath` (string): Path to AIBDP manifest file
- `enforceForAll` (boolean): Enforce for all requests, not just AI bots
- `onViolation` (function): Callback `(req, policy, purpose) => void` when a violation is detected

`middleware.wrap(handler)` returns a `Deno.serve` handler that returns an HTTP 430 `Response` on violation, otherwise delegates to `handler`.

### `serveManifest(manifestPath)`

Returns a handler that serves the AIBDP manifest (`application/aibdp+json`, cached, CORS-open), or a 404 JSON response if the manifest is missing.

### `aibdpRequired(handler, { manifestPath, purpose })`

Wrap a single route handler with AIBDP enforcement for a given `purpose` (e.g. `"training"`). Returns HTTP 430 if the purpose is refused or conditional requirements are not met.

### Utility Functions

- `isAiUserAgent(userAgent)` → boolean
- `extractAiPurpose(headers)` → string (`headers` may be a `Headers` instance or a plain object)
- `pathMatches(requestPath, pattern)` → boolean (glob: `**` = any, `*` = non-slash, `?` = one char)
- `getApplicablePolicy(manifest, purpose, requestPath)` → policy object or `null`
- `checkPolicyConditions(policy, headers)` → `[satisfied, missing[]]`
- `create430Response(manifest, policy, purpose, extra?)` → `Response`

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
    "indexing": { "status": "allowed", "scope": "all" },
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
GET /article HTTP/1.1
Host: example.org
User-Agent: ResearchBot/1.0
AI-Purpose: indexing
AI-Consent-Reviewed: https://example.org/.well-known/aibdp.json
AI-Consent-Conditions: attribution,non-commercial
```

The middleware checks for these headers when enforcing conditional policies.

## Deployment Considerations

- ✅ Create AIBDP manifest at `.well-known/aibdp.json`
- ✅ Set appropriate `expires` field in manifest (30–90 days recommended)
- ✅ Provide contact information for policy questions
- ✅ Monitor logs for violations
- ✅ Enable HTTPS for manifest integrity
- ✅ Manifest is cached in memory (default: 1 hour)
- ✅ Failed manifest loads fail open (don't break the site)

## Standards Compliance

- [draft-jewell-aibdp-00](https://github.com/Hyperpolymath/consent-aware-http/blob/main/drafts/draft-jewell-aibdp-00.xml) — AIBDP specification
- [draft-jewell-http-430-consent-required-00](https://github.com/Hyperpolymath/consent-aware-http/blob/main/draft-jewell-http-430-consent-required-00.xml) — HTTP 430 status code
- [RFC 8615](https://www.rfc-editor.org/info/rfc8615) — Well-Known URIs
- [RFC 8259](https://www.rfc-editor.org/info/rfc8259) — JSON format

## License

MIT License — see LICENSE file for details.

## Related Projects

- [AIBDP Specification](https://github.com/Hyperpolymath/consent-aware-http)
- [Node.js Implementation](../nodejs/)

---

_"Without refusal, permission is meaningless."_
