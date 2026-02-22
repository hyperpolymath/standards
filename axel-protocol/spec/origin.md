<!--
SPDX-License-Identifier: PMPL-1.0-or-later
SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
-->

# AXEL-O: Origin/Edge Enforcement Profile

**Version**: 1.1.0-draft
**Author**: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
**License**: PMPL-1.0-or-later
**Status**: Normative

---

## 1. Introduction

AXEL-O defines the origin/CDN edge enforcement profile for the AXEL Protocol.
This is the **primary enforcement model** for AXEL.

The enforcement point is the origin server or CDN edge, operating over
standard HTTPS on port 443. AXEL-O does not require new ports, custom
protocol extensions, or network-layer interception.

### 1.1 Rationale

Network-layer enforcement is unreliable due to VPNs, encrypted DNS, ECH,
and alternate network paths. Origin enforcement provides:

- **Universal applicability**: works on the public internet without ISP cooperation.
- **Precise targeting**: only the classified content is gated, not entire IP ranges.
- **Privacy preservation**: no DPI or SNI inspection required.
- **Immediate deployability**: uses existing HTTPS infrastructure.

---

## 2. Policy Link Relation

### 2.1 Link Header

Origins MUST include a `Link` header pointing to the AXEL policy on gating
responses. Origins SHOULD include it on all responses for discoverability.

```http
Link: <https://explicit.example.com/.well-known/axel-policy>; rel="axel-policy"
```

The `rel` value `axel-policy` identifies the linked resource as an AXEL
Policy Object.

### 2.2 HTML Link Element (Optional)

For HTML responses, origins MAY additionally include:

```html
<link rel="axel-policy" href="https://explicit.example.com/.well-known/axel-policy">
```

This is supplementary; the `Link` HTTP header is the normative discovery
mechanism for AXEL-O.

---

## 3. Browser Navigation Gating

When a browser navigates to AXEL-protected content and no valid proof of
eligibility is present, the origin SHOULD respond with:

### 3.1 Redirect to Gate

```http
HTTP/1.1 303 See Other
Location: https://explicit.example.com/verify?return_to=%2Fcontent%2Fpage
Cache-Control: no-store
Link: <https://explicit.example.com/.well-known/axel-policy>; rel="axel-policy"
```

**Requirements**:

- **303 See Other**: the redirect MUST use 303 to indicate the resource exists
  but the client must complete a verification step first.
- **Cache-Control: no-store**: the gating response MUST NOT be cached,
  ensuring subsequent requests are re-evaluated.
- **`return_to` parameter**: RECOMMENDED to enable redirect-back after
  verification. The parameter name is a publisher choice.

### 3.2 Gate Page

The gate page (`/verify` in the example) is publisher-controlled. It:

1. Explains that the content requires age verification.
2. Provides links to supported verifiers (from the policy's `verifiers` array).
3. Accepts proof of eligibility and issues a session credential.
4. Redirects back to the original content URL.

AXEL v1 does NOT standardize the gate page UX, the proof format, or the
session credential. These are verifier/publisher business.

---

## 4. API Gating

When an API client requests AXEL-protected content without valid proof,
the origin SHOULD respond with:

### 4.1 Problem Response (RFC 9457)

```http
HTTP/1.1 403 Forbidden
Content-Type: application/problem+json
Link: <https://explicit.example.com/.well-known/axel-policy>; rel="axel-policy"
```

```json
{
  "type": "https://axel-protocol.org/problems/proof-required",
  "title": "Age verification required",
  "status": 403,
  "detail": "This content requires proof of age eligibility. See the AXEL policy for verification options.",
  "instance": "/api/v1/content/12345",
  "axel_policy": "https://explicit.example.com/.well-known/axel-policy"
}
```

**Requirements**:

- **403 Forbidden**: the response MUST use 403 to indicate the request is
  understood but denied without proof.
- **application/problem+json**: the response MUST use RFC 9457 problem details.
- **`type` URI**: SHOULD use a registered AXEL problem type.
- **`axel_policy` field**: MUST include a pointer to the AXEL policy URL.

### 4.2 After Proof

Once the client provides valid proof, the origin serves the requested content
normally (200 OK). The proof mechanism and its transport are out of scope
for AXEL v1.

---

## 5. Response Headers

### 5.1 Summary

| Header | When | Purpose |
|--------|------|---------|
| `Link: <...>; rel="axel-policy"` | All responses (SHOULD), gating responses (MUST) | Policy discovery |
| `Cache-Control: no-store` | Gating responses (MUST) | Prevent caching of gate redirects |
| `Vary: *` | Gating responses (RECOMMENDED) | Signal that response varies by proof state |

### 5.2 No Custom Headers

AXEL-O intentionally avoids defining custom HTTP headers. Discovery uses
the standard `Link` header with a registered relation type. This maximizes
compatibility with existing HTTP infrastructure (proxies, CDNs, caches).

---

## 6. Proof Formats (Out of Scope for v1)

AXEL v1 deliberately does NOT mandate a specific proof format. Possible
future extensions include:

- JWT bearer tokens
- Zero-knowledge proof presentations
- Privacy Pass tokens
- OAuth 2.0 token exchange

The proof format is a matter between the verifier and the publisher/origin.
AXEL v1 standardizes only the policy discovery and gating signals, not the
proof exchange.

---

## 7. CDN Integration

CDN edges implementing AXEL-O:

1. Fetch and cache the AXEL policy for configured domains.
2. On incoming requests, check for valid proof (mechanism is CDN-specific).
3. If no proof: return the gating response (303 for browsers, 403 for APIs).
4. If valid proof: proxy to origin normally.
5. Include the `Link` header on all proxied responses.

CDNs MAY implement proof verification at the edge or delegate to the origin.

---

## 8. Security Considerations

### 8.1 Open Redirectors

The `return_to` parameter in the gate redirect MUST be validated by the gate
page to prevent open redirect vulnerabilities. The gate page SHOULD only
redirect to URLs within the same origin.

### 8.2 Cache Poisoning

Gating responses MUST use `Cache-Control: no-store` to prevent CDN or browser
caches from serving gate redirects to verified users.

### 8.3 Bypass via Direct IP

If the origin's IP address is known, an attacker could bypass CDN-level
AXEL-O enforcement by connecting directly. Publishers SHOULD restrict
origin access to CDN source IPs (standard origin hardening).

---

## 9. Examples

### 9.1 Full Browser Flow

```
1. Browser → GET https://explicit.example.com/content/page
2. Origin  → 303 See Other
              Location: https://explicit.example.com/verify?return_to=/content/page
              Cache-Control: no-store
              Link: <https://explicit.example.com/.well-known/axel-policy>; rel="axel-policy"

3. Browser → GET https://explicit.example.com/verify?return_to=/content/page
4. Origin  → 200 OK (gate page with verifier options)

5. Browser → [completes verification with chosen verifier]
6. Verifier → [issues proof to browser/origin]

7. Browser → GET https://explicit.example.com/content/page
              [with proof cookie/header]
8. Origin  → 200 OK (content served)
              Link: <https://explicit.example.com/.well-known/axel-policy>; rel="axel-policy"
```

### 9.2 Full API Flow

```
1. Client → GET https://explicit.example.com/api/v1/content/12345
2. Origin → 403 Forbidden
             Content-Type: application/problem+json
             Link: <https://explicit.example.com/.well-known/axel-policy>; rel="axel-policy"
             {"type":"https://axel-protocol.org/problems/proof-required",...}

3. Client → [fetches policy, discovers verifiers, obtains proof]

4. Client → GET https://explicit.example.com/api/v1/content/12345
             Authorization: Bearer <proof-token>
5. Origin → 200 OK (content served)
```

---

*This document is part of the AXEL Protocol specification set.*
*See also: [Core Discovery](core.md) | [Policy Object](policy.md) | [Managed Network (AXEL-N)](network.md) | [Transport Security](transport.md)*
