<!--
SPDX-License-Identifier: PMPL-1.0-or-later
SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
-->

# AXEL Policy Object Format

**Internet-Draft**: draft-jewell-axel-policy-00
**Intended Status**: Standards Track
**Author**: J. Jewell, Open University
**Date**: 2026-02

---

## Abstract

This document defines the AXEL Policy Object, a JSON format for declaring
content classification, enforcement profiles, isolation levels, and caching
directives. The policy object is served at
`https://<domain>/.well-known/axel-policy` and is the authoritative source of
AXEL metadata for a domain.

## Status of This Memo

This Internet-Draft is submitted for discussion purposes. Distribution of
this memo is unlimited.

---

## 1. Introduction

The AXEL Policy Object provides a machine-readable content classification for
a domain. It declares what type of content the domain hosts, what age
restrictions apply, how enforcement is expected to work, and how long the
policy may be cached.

### 1.1. Terminology

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "NOT RECOMMENDED", "MAY", and "OPTIONAL" in this
document are to be interpreted as described in BCP 14 [RFC2119] [RFC8174] when,
and only when, they appear in ALL CAPITALS.

---

## 2. Media Type

The policy endpoint MUST return Content-Type: `application/json`.

---

## 3. Policy Structure

### 3.1. Required Fields

**version** (string, REQUIRED): MUST be "AXEL1".

**id** (string, REQUIRED): Policy identifier matching the DNS TXT record id.

**scope** (object, REQUIRED): Contains `hostnames` (array of strings), the
domains this policy covers.

**content** (object, REQUIRED): Contains `category` (string enum: "adult",
"mature", "gambling", "alcohol", "tobacco", "cannabis", "custom") and
`min_age` (integer, minimum age for access).

**enforcement** (object, REQUIRED): Contains `profiles` (array, MUST include
"AXEL-O") and `proof_required` (boolean). MAY contain `browser_flow` and
`api_flow` objects.

**cache** (object, REQUIRED): Contains `max_age_seconds` (positive integer) and
`stale_if_error_seconds` (non-negative integer).

### 3.2. Optional Fields

**isolation** (object): Contains `level` ("L0", "L1", or "L1-AUDITED"),
`prefixes` (object with `ipv6` and `ipv4` arrays), and `cdn_pool` (string).

**verifiers** (array of objects): Verification service discovery metadata.
Each object contains `name`, `url`, and `methods`.

**auditing** (object): Auditor attestation metadata. Contains `statement_url`
and `policy_hash_sha256` (hex-encoded SHA-256 of the canonical policy).

**extensions** (object): Vendor/future extensions.

### 3.3. Isolation Levels

**L0** (Label-only): Origin enforcement expected. Network enforcement may not be
precise because restricted content may share hostnames with unrestricted content.

**L1** (Network-enforceable): Restricted content on dedicated hostnames/prefixes
or a CDN explicit-only pool. Network enforcement is safe.

**L1-AUDITED** (Audited): L1 with independent auditor confirmation.

---

## 4. Validation

Implementations MUST reject policies where:
- `version` is not "AXEL1"
- `id` is missing or empty
- `scope.hostnames` is missing or empty
- `content.category` is not a recognized value
- `enforcement.profiles` does not contain "AXEL-O"
- `cache` is missing required sub-fields

---

## 5. Auditing

Auditor attestation confirms publisher legitimacy and policy integrity.
It binds: policy hash, domain, classification, min_age, and isolation level.

Auditing is NOT user age verification. It is about publisher/domain trust.

---

## 6. Security Considerations

- Policies MUST be served over TLS 1.3.
- `id` changes signal policy updates, preventing stale enforcement.
- `policy_hash_sha256` enables integrity verification independent of TLS.

---

## 7. Privacy Considerations

- The policy document itself is public metadata, not user data.
- Proof mechanisms (future) MUST NOT introduce cross-site identifiers.

---

## 8. IANA Considerations

This document relies on the Well-Known URI registered in
[draft-jewell-axel-core-00].

---

## 9. References

### 9.1. Normative References

- [RFC2119] Bradner, S., "Key words for use in RFCs to Indicate Requirement
  Levels", BCP 14, RFC 2119, March 1997.
- [RFC8174] Leiba, B., "Ambiguity of Uppercase vs Lowercase in RFC 2119 Key
  Words", BCP 14, RFC 8174, May 2017.

### 9.2. Informative References

- [draft-jewell-axel-core-00] Jewell, J., "AXEL Core: DNS Discovery and Policy
  Retrieval", 2026.

---

## Authors' Addresses

Jonathan D.A. Jewell
The Open University
Email: jonathan.jewell@open.ac.uk
