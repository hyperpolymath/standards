Developer Explainer — Implementing Consent-Aware HTTP
This document helps developers adopt HTTP 430 and AIBDP manifests with clarity and minimal overhead.

⚙️ HTTP 430: Consent Required
Add logic to your server that returns the following response when AI-specific consent is not granted:

Status Line & Headers

HTTP/1.1 430 Consent Required

Content-Type: text/plain

Body

Consent manifest missing or denied. See /.well-known/aibdp.json
Trigger this response under the following conditions:

No AIBDP manifest is present

Manifest explicitly refuses your AI use case

Request is from an agent declared as “AI” and boundary is violated

🧭 AIBDP Manifest
Place a JSON file at /.well-known/aibdp.json with the following structure:

json
{
  "aibdp_version": "0.1",
  "contact": "mailto:editor@sinople.pub",
  "indexing": "allowed",
  "training": "refused",
  "regeneration": "refused"
}
You may include additional fields such as "summary" or "policy_url" for context.

🛠 Recommended Request Headers
Agents or infrastructures initiating AI-related requests should include:

User-Agent: AI-Boundary-Agent/1.0

X-AI-Intent: training or regeneration or indexing

If the declared intent conflicts with the origin’s manifest, return HTTP 430.

🧪 Tooling Ideas
Developers may wish to build or extend tools that support:

Manifest validators (syntax and semantics)

Consent-aware extensions for robots.txt

Middleware for auto-430 response handling

Analytics dashboards for refusal types and triggers

Caching layers for manifest lookups

COSE/JWS signature verification of manifests (advanced)

Consent isn't an afterthought. It’s architecture. Implement it with clarity. Refuse with dignity.