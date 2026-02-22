<!--
SPDX-License-Identifier: PMPL-1.0-or-later
SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
-->

# AXEL-O: Origin/Edge HTTP Enforcement Signaling

**Internet-Draft**: draft-jewell-axel-origin-00
**Intended Status**: Standards Track
**Author**: J. Jewell, Open University
**Date**: 2026-02

---

## Abstract

This document defines AXEL-O, the origin/edge enforcement profile for the AXEL
Protocol. AXEL-O specifies HTTP signaling mechanisms for content gating at
origin servers and CDN edges. It uses the standard `Link` header for policy
discovery, HTTP 303 redirects for browser navigation gating, and RFC 9457
problem details for API gating. AXEL-O operates entirely over HTTPS on port 443
and requires no new ports, headers, or protocol extensions.

## Status of This Memo

This Internet-Draft is submitted for discussion purposes. Distribution of
this memo is unlimited.

---

## 1. Introduction

Network-layer enforcement of content policies is unreliable due to VPNs,
encrypted DNS, Encrypted Client Hello (ECH), and alternate routing. AXEL-O
provides a deployable enforcement model at the origin server or CDN edge using
existing HTTP semantics.

### 1.1. Terminology

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "NOT RECOMMENDED", "MAY", and "OPTIONAL" in this
document are to be interpreted as described in BCP 14 [RFC2119] [RFC8174] when,
and only when, they appear in ALL CAPITALS.

---

## 2. Policy Link Relation

### 2.1. Link Header

Origins MUST include a Link header on gating responses and SHOULD include it on
all responses:

```
Link: <https://example.com/.well-known/axel-policy>; rel="axel-policy"
```

The `rel` value "axel-policy" identifies the target as an AXEL Policy Object.

---

## 3. Browser Navigation Gating

When proof of eligibility is absent, the origin SHOULD respond:

```
HTTP/1.1 303 See Other
Location: <gate-url>?return_to=<original-path>
Cache-Control: no-store
Link: <https://example.com/.well-known/axel-policy>; rel="axel-policy"
```

303 See Other indicates the resource exists but requires a verification step.

Cache-Control: no-store prevents caching of the gating redirect.

The gate URL and proof exchange mechanism are publisher-defined. AXEL v1 does
not standardize the proof format.

---

## 4. API Gating

When an API client lacks proof, the origin SHOULD respond:

```
HTTP/1.1 403 Forbidden
Content-Type: application/problem+json
Link: <https://example.com/.well-known/axel-policy>; rel="axel-policy"
```

The response body MUST conform to RFC 9457 (Problem Details for HTTP APIs):

```json
{
  "type": "https://axel-protocol.org/problems/proof-required",
  "title": "Age verification required",
  "status": 403,
  "detail": "This content requires proof of age eligibility.",
  "axel_policy": "https://example.com/.well-known/axel-policy"
}
```

---

## 5. Proof Formats

AXEL v1 does NOT standardize proof formats. The proof mechanism is a matter
between the verifier and the publisher. Future extensions may define:
- JWT bearer tokens
- Privacy Pass tokens
- Zero-knowledge proof presentations

---

## 6. Security Considerations

### 6.1. Open Redirectors

The `return_to` parameter in gate redirects MUST be validated to prevent open
redirect attacks. Gate pages SHOULD restrict redirects to the same origin.

### 6.2. Cache Poisoning

Gating responses MUST use `Cache-Control: no-store` to prevent caches from
serving gate redirects to verified users.

### 6.3. Direct-to-Origin Bypass

If origin IPs are known, attackers may bypass CDN-level AXEL-O enforcement.
Publishers SHOULD restrict origin access to CDN source IPs.

---

## 7. Privacy Considerations

- AXEL-O does not require DPI or SNI inspection.
- No stable cross-site identifiers are introduced.
- Proof mechanisms (future) SHOULD be domain-scoped and short-lived.

---

## 8. IANA Considerations

### 8.1. Link Relation Type

Relation Name: `axel-policy`
Description: Links to an AXEL Protocol policy document
Reference: This document

### 8.2. Well-Known URI

This document relies on the Well-Known URI `axel-policy` registered in
[draft-jewell-axel-core-00].

---

## 9. References

### 9.1. Normative References

- [RFC2119] Bradner, S., "Key words for use in RFCs to Indicate Requirement
  Levels", BCP 14, RFC 2119, March 1997.
- [RFC8174] Leiba, B., "Ambiguity of Uppercase vs Lowercase in RFC 2119 Key
  Words", BCP 14, RFC 8174, May 2017.
- [RFC9457] Nottingham, M., Wilde, E., Dalal, S., "Problem Details for HTTP
  APIs", RFC 9457, July 2023.

### 9.2. Informative References

- [draft-jewell-axel-core-00] Jewell, J., "AXEL Core: DNS Discovery and Policy
  Retrieval", 2026.
- [draft-jewell-axel-policy-00] Jewell, J., "AXEL Policy Object Format", 2026.

---

## Authors' Addresses

Jonathan D.A. Jewell
The Open University
Email: jonathan.jewell@open.ac.uk
