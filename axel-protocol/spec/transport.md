<!--
SPDX-License-Identifier: PMPL-1.0-or-later
SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
-->

# AXEL Transport Security

**Version**: 1.1.0-draft
**Author**: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
**License**: PMPL-1.0-or-later
**Status**: Normative

---

## 1. TLS Requirements

### 1.1 Policy Endpoints

AXEL policy endpoints (`/.well-known/axel-policy`) MUST use TLS 1.3
(RFC 8446) or later.

TLS 1.2 and earlier MUST NOT be accepted for policy retrieval. This
ensures modern cipher suites and forward secrecy for all policy exchanges.

### 1.2 Content Endpoints

Content endpoints protected by AXEL-O SHOULD use TLS 1.3. TLS 1.2 is
acceptable for content delivery but not for policy endpoints.

### 1.3 HSTS

Publishers SHOULD deploy HTTP Strict Transport Security (HSTS) on domains
publishing AXEL policies, with `includeSubDomains` and a long `max-age`.

---

## 2. Encrypted Client Hello (ECH)

### 2.1 Recommendation

ECH (Encrypted Client Hello, draft-ietf-tls-esni) SHOULD be supported
by publishers to prevent passive observers from learning which domain a
client is connecting to.

### 2.2 Non-Requirement

ECH MUST NOT be required by AXEL. ECH deployment depends on CDN and
browser support that is not yet universal. AXEL enforcement MUST NOT
depend on SNI visibility; therefore ECH does not affect AXEL-O enforcement.

### 2.3 Interaction with AXEL-N

When ECH is deployed, AXEL-N gateways cannot determine the target hostname
from the TLS handshake. This reinforces that AXEL-N MUST use destination
IP prefix matching (not SNI inspection) for enforcement.

---

## 3. AXEL-DANE Profile (Optional)

### 3.1 Overview

Enforcers MAY validate TLSA records (RFC 6698, DANE) for the AXEL policy
endpoint to provide additional assurance that the TLS certificate is
legitimate.

### 3.2 Applicability

- AXEL-DANE is an OPTIONAL profile for enforcers with DANE support.
- Browsers are NOT required to implement DANE validation.
- Server-side enforcers (CDN edges, gateways) SHOULD implement DANE
  when DNSSEC is available for the policy domain.

### 3.3 TLSA Record

Publishers MAY publish a TLSA record for the policy endpoint:

```dns
_443._tcp.example.com. IN TLSA 3 1 1 <sha256-hash-of-leaf-cert-spki>
```

Enforcers supporting AXEL-DANE:

1. MUST validate DNSSEC for the TLSA record.
2. MUST match the TLSA record against the TLS certificate presented
   by the policy endpoint.
3. If TLSA validation fails, the enforcer SHOULD log the failure and
   MAY refuse to use the fetched policy.

---

## 4. Privacy Considerations

### 4.1 DNS Privacy

Encrypted DNS (DoH [RFC 8484], DoT [RFC 7858], DoQ [RFC 9250]) protects
DNS queries against on-path observers. AXEL encourages but does not require
encrypted DNS for end users.

Enforcers performing DNS-based discovery will observe the AXEL TXT records
they query. This is inherent to their enforcement role and does not
constitute a privacy violation.

### 4.2 Accurate Privacy Claims

AXEL specifications MUST NOT claim that encrypted DNS prevents ISP logging
of enforcement actions. Accurate statement:

> Encrypted DNS protects against on-path observers who are not the resolver
> operator. Enforcing operators still observe what they must to enforce.
> AXEL aims for no DPI, minimal data retention, and no stable cross-site
> identifiers.

### 4.3 No Cross-Site Tracking

AXEL proof mechanisms (future extensions) MUST NOT introduce stable
cross-site identifiers. Proof tokens SHOULD be:

- Domain-scoped (not reusable across origins)
- Short-lived (minutes, not days)
- Unlinkable (different proofs for different domains cannot be correlated)

---

## 5. IANA Considerations

### 5.1 Well-Known URI

AXEL requests registration of the Well-Known URI `axel-policy`:

| Field | Value |
|-------|-------|
| URI suffix | `axel-policy` |
| Change controller | ASPEC / AXEL Protocol Authors |
| Reference | This specification |
| Status | Permanent |

### 5.2 Link Relation Type

AXEL requests registration of the link relation type `axel-policy`:

| Field | Value |
|-------|-------|
| Relation Name | `axel-policy` |
| Description | Links to an AXEL Protocol policy document |
| Reference | This specification |

### 5.3 No New Ports

AXEL v1 does NOT request new port assignments. All AXEL communication
occurs over HTTPS on port 443.

### 5.4 No Flow Label Reservation

AXEL v1 does NOT request IPv6 flow label reservations. Flow labels are
not used for AXEL signaling.

---

*This document is part of the AXEL Protocol specification set.*
*See also: [Core Discovery](core.md) | [Policy Object](policy.md) | [Origin Enforcement (AXEL-O)](origin.md) | [Managed Network (AXEL-N)](network.md)*
