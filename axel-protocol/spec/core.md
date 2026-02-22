<!--
SPDX-License-Identifier: PMPL-1.0-or-later
SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
-->

# AXEL Core: Discovery & Policy Retrieval

**AXEL** = **A**ccess for e**X**plicit **E**nforcement & **L**abeling

**Version**: 1.1.0-draft
**Author**: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
**License**: PMPL-1.0-or-later
**Status**: Normative

---

## 1. Introduction

AXEL Core defines the mandatory discovery and policy retrieval mechanisms for
the AXEL Protocol. All conforming implementations MUST support AXEL Core.

AXEL addresses the failures of voluntary content labeling systems (ICRA, PICS)
by providing technically enforceable content classification. The primary
enforcement model is **origin/CDN edge enforcement over standard HTTPS**.

AXEL cannot guarantee universal enforcement purely at the network layer.
VPNs, alternate networks, encrypted DNS, ECH, and other privacy technologies
make network-level interception unreliable. Therefore AXEL's primary
enforcement point is the origin server or CDN edge, not intermediate
network devices.

### 1.1 Terminology

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "NOT RECOMMENDED", "MAY", and "OPTIONAL" in this
document are to be interpreted as described in BCP 14 [RFC 2119] [RFC 8174]
when, and only when, they appear in ALL CAPITALS, as shown here.

- **Publisher**: An entity that publishes content and declares its AXEL policy.
- **Enforcer**: An entity that acts on AXEL policy (origin, CDN, or network gateway).
- **Policy**: A machine-readable JSON document declaring content classification and enforcement metadata.
- **Verifier**: A service that provides proof of eligibility (e.g., age verification).
- **Posture**: An enforcer's operational mode (`testing` or `enforce`).

### 1.2 Protocol Profiles

AXEL defines the following profiles:

| Profile | Status | Description |
|---------|--------|-------------|
| **AXEL Core** | Mandatory | DNS discovery + HTTPS policy retrieval + caching + security |
| **AXEL-O** | Mandatory | Origin/Edge Enforcement: HTTP signaling at origin or CDN |
| **AXEL-N** | Optional | Managed Network Profile: gateway/ISP enforcement for L1+ content |

### 1.3 Isolation Levels

AXEL policy MUST declare an isolation level to prevent collateral damage
when network-level enforcement is applied:

| Level | Name | Description |
|-------|------|-------------|
| **L0** | Label-only | Origin enforcement expected. Network enforcement not necessarily precise. Restricted content MAY share hostnames with unrestricted content. |
| **L1** | Network-enforceable | Restricted content on dedicated hostnames and/or dedicated serving prefixes or a CDN "explicit-only pool." AXEL-N enforcement is safe. |
| **L1-AUDITED** | Audited network-enforceable | L1 with an independent auditor/MVA statement confirming the isolation claim. |

Dedicated prefixes/hostnames are NOT required for AXEL overall; they are
required only for L1/L1-AUDITED content that opts into AXEL-N enforcement.

---

## 2. DNS Discovery

### 2.1 TXT Record Format

Publishers MUST publish a DNS TXT record at:

```
_axel.<domain>
```

The TXT record payload uses a semicolon-delimited key-value format:

```
v=AXEL1; id=<policy-id>
```

| Key | Required | Description | Example |
|-----|----------|-------------|---------|
| `v` | REQUIRED | Protocol version. MUST be `AXEL1`. | `AXEL1` |
| `id` | REQUIRED | Policy identifier. An opaque string that changes when policy content changes (MTA-STS-style pinning). | `20260212T1200Z` |

**Example**:

```dns
_axel.example.com. 3600 IN TXT "v=AXEL1; id=20260212T1200Z"
```

### 2.2 Parsing Rules

Parsers MUST:

1. Parse only the TXT record payload (RDATA), not the full RR line.
2. Treat `v=AXEL1` as REQUIRED. If missing or not `AXEL1`, the record MUST
   be rejected (fail closed).
3. Treat `id` as REQUIRED. If missing, the record MUST be rejected.
4. Ignore unknown keys (forward compatibility).
5. Treat the entire record as invalid if required keys are missing or malformed.

Parsers MUST NOT:

1. Default `v` to `AXEL1` if absent.
2. Accept empty or whitespace-only `id` values.
3. Parse full RR lines (owner name + TTL + class + type + RDATA) as if they
   were payload.

### 2.3 DNSSEC Requirements

Enforcers operating in `enforce` posture MUST require DNSSEC validation
for the `_axel.<domain>` TXT record before making enforcement decisions.

Enforcers in `testing` posture SHOULD require DNSSEC validation but MAY
log and continue if validation fails.

Publishers SHOULD sign their zones with DNSSEC.

---

## 3. Policy Retrieval

### 3.1 Policy URL

The canonical policy URL for a domain is:

```
https://<domain>/.well-known/axel-policy
```

Enforcers MUST fetch the policy from this URL using HTTPS (TLS 1.3; see
[Transport Security](transport.md)).

The response MUST be `application/json` with a valid AXEL Policy Object
(see [Policy Object Specification](policy.md)).

### 3.2 Policy ID Pinning

Policy ID pinning follows the MTA-STS model [RFC 8461]:

1. On first fetch, the enforcer records the `id` from the DNS TXT record
   and the corresponding policy document.
2. On subsequent DNS lookups, if the `id` has changed, the enforcer MUST
   re-fetch the policy from the well-known URL.
3. If the `id` has not changed and the cached policy has not expired, the
   enforcer SHOULD use the cached policy.

### 3.3 Caching

The policy object includes caching directives:

```json
{
  "cache": {
    "max_age_seconds": 86400,
    "stale_if_error_seconds": 604800
  }
}
```

| Field | Description |
|-------|-------------|
| `max_age_seconds` | Maximum time (in seconds) to cache the policy before re-fetching. |
| `stale_if_error_seconds` | Time (in seconds) a stale policy may be used if the policy endpoint is unreachable. |

### 3.4 Failure Behavior

When policy retrieval fails:

| Scenario | Testing Posture | Enforce Posture |
|----------|-----------------|-----------------|
| DNS record absent | No enforcement | No enforcement |
| DNS record present, policy fetch fails | Log, use last-known-good | Use last-known-good within `stale_if_error_seconds`; if expired, enforcer choice (fail-open or fail-closed) |
| DNS record present, policy invalid JSON | Log, ignore | Treat as no policy (fail-open) or reject (fail-closed); enforcer choice |
| DNS `id` changed, new policy fetch fails | Log, keep old policy | Keep old policy within `stale_if_error_seconds` |

**Last-known-good**: Enforcers SHOULD cache the most recent valid policy and
use it as a fallback when the policy endpoint is temporarily unavailable.
The `stale_if_error_seconds` field governs how long a stale policy remains
usable.

**Fail-open vs. fail-closed**: The posture decision (whether to fail-open
or fail-closed when no valid policy is available) is an enforcer/operator
choice, not a protocol requirement. AXEL does not mandate either behavior.

---

## 4. Security Considerations

### 4.1 DNS Spoofing

**Threat**: Attacker publishes fraudulent `_axel` TXT records.
**Mitigation**: DNSSEC validation is REQUIRED for `enforce` posture.

### 4.2 Policy Tampering

**Threat**: Attacker modifies policy in transit.
**Mitigation**: TLS 1.3 is REQUIRED for policy endpoint; HSTS RECOMMENDED.

### 4.3 Downgrade Attacks

**Threat**: Attacker removes `_axel` TXT record or prevents policy fetch.
**Mitigation**: Last-known-good caching with `stale_if_error_seconds`.
Publishers MAY submit domains to AXEL preload lists (future extension).

### 4.4 Privacy

Encrypted DNS (DoH, DoT, DoQ) protects DNS queries against on-path observers.
However, enforcing operators still observe what they must to enforce.
AXEL aims for:

- No deep packet inspection (DPI)
- Minimal data retention
- No stable cross-site identifiers

AXEL does NOT claim that encrypted DNS alone prevents ISP logging of
enforcement actions. Operators performing enforcement necessarily observe
the domains and policies they enforce.

---

## 5. References

- [RFC 2119] Key words for use in RFCs to Indicate Requirement Levels
- [RFC 8174] Ambiguity of Uppercase vs Lowercase in RFC 2119 Key Words
- [RFC 8461] SMTP MTA Strict Transport Security (MTA-STS)
- [RFC 6698] DNS-Based Authentication of Named Entities (DANE)
- [RFC 9250] DNS over Dedicated QUIC Connections (DoQ)
- [RFC 8484] DNS Queries over HTTPS (DoH)

---

*This document is part of the AXEL Protocol specification set.*
*See also: [Policy Object](policy.md) | [Origin Enforcement (AXEL-O)](origin.md) | [Managed Network (AXEL-N)](network.md) | [Transport Security](transport.md)*
