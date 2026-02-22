<!--
SPDX-License-Identifier: PMPL-1.0-or-later
SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
-->

# AXEL Core: DNS Discovery and Policy Retrieval

**Internet-Draft**: draft-jewell-axel-core-00
**Intended Status**: Standards Track
**Author**: J. Jewell, Open University
**Date**: 2026-02

---

## Abstract

This document defines the AXEL (Access for eXplicit Enforcement & Labeling)
Core protocol for discovering and retrieving content classification policies
via DNS and HTTPS. Publishers declare content classification by publishing a
DNS TXT record at `_axel.<domain>` and serving a JSON policy document at
`https://<domain>/.well-known/axel-policy`. The protocol uses MTA-STS-style
policy ID pinning to detect updates and defines caching and failure semantics
for enforcers.

## Status of This Memo

This Internet-Draft is submitted for discussion purposes. Distribution of
this memo is unlimited.

---

## 1. Introduction

Existing content labeling systems (ICRA, PICS, RTA) rely on voluntary
self-labeling without technical enforcement. AXEL provides a machine-readable
content classification system with defined discovery, retrieval, and caching
semantics that enable enforcers (origin servers, CDN edges, managed network
gateways) to act on publisher declarations.

AXEL cannot guarantee enforcement at the network layer due to encrypted
transports, VPNs, and alternate routing. The primary enforcement model is
origin/CDN edge enforcement over standard HTTPS (see
[draft-jewell-axel-origin-00]).

### 1.1. Terminology

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "NOT RECOMMENDED", "MAY", and "OPTIONAL" in this
document are to be interpreted as described in BCP 14 [RFC2119] [RFC8174] when,
and only when, they appear in ALL CAPITALS.

**Publisher**: An entity that publishes content and declares its AXEL policy.

**Enforcer**: An entity that discovers and acts on AXEL policy.

**Policy**: A JSON document describing content classification and enforcement
metadata.

---

## 2. DNS Discovery

### 2.1. TXT Record

Publishers MUST publish a DNS TXT record at `_axel.<domain>`. The record
payload uses semicolon-delimited key-value pairs:

```
v=AXEL1; id=<policy-id>
```

`v` (REQUIRED): Protocol version. MUST be "AXEL1".

`id` (REQUIRED): Opaque policy identifier. When `id` changes, enforcers
MUST re-fetch the policy document.

### 2.2. DNSSEC

Enforcers operating in enforce posture MUST validate DNSSEC for the
`_axel.<domain>` TXT record.

---

## 3. Policy Retrieval

The policy URL is `https://<domain>/.well-known/axel-policy`.

The response MUST be `application/json` conforming to
[draft-jewell-axel-policy-00].

The endpoint MUST use TLS 1.3 [RFC8446] or later.

---

## 4. Caching

The policy object includes `cache.max_age_seconds` and
`cache.stale_if_error_seconds` fields.

When DNS `id` changes, the enforcer MUST re-fetch regardless of cache state.

When the policy endpoint is unreachable, enforcers MAY use the last-known-good
policy for up to `stale_if_error_seconds`.

---

## 5. Security Considerations

- DNSSEC prevents DNS record spoofing.
- TLS 1.3 prevents policy tampering in transit.
- Policy ID pinning detects stale or tampered policies.
- Enforcers must not trust unsigned DNS records for enforcement decisions.

---

## 6. Privacy Considerations

- Encrypted DNS (DoH, DoT, DoQ) protects queries from on-path observers.
- Enforcing operators necessarily observe the domains they enforce.
- AXEL aims for no DPI, minimal data retention, no stable cross-site
  identifiers.

---

## 7. IANA Considerations

### 7.1. Well-Known URI Registration

URI suffix: `axel-policy`
Change controller: ASPEC / AXEL Protocol Authors
Reference: This document
Status: Permanent

---

## 8. References

### 8.1. Normative References

- [RFC2119] Bradner, S., "Key words for use in RFCs to Indicate Requirement
  Levels", BCP 14, RFC 2119, March 1997.
- [RFC8174] Leiba, B., "Ambiguity of Uppercase vs Lowercase in RFC 2119 Key
  Words", BCP 14, RFC 8174, May 2017.
- [RFC8446] Rescorla, E., "The Transport Layer Security (TLS) Protocol Version
  1.3", RFC 8446, August 2018.

### 8.2. Informative References

- [RFC8461] Margolis, D., et al., "SMTP MTA Strict Transport Security
  (MTA-STS)", RFC 8461, September 2018.
- [RFC8484] Hoffman, P., McManus, P., "DNS Queries over HTTPS (DoH)", RFC 8484,
  October 2018.
- [RFC9250] Huitema, C., et al., "DNS over Dedicated QUIC Connections", RFC
  9250, May 2022.

---

## Authors' Addresses

Jonathan D.A. Jewell
The Open University
Email: jonathan.jewell@open.ac.uk
