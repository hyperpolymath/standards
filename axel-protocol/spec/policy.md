<!--
SPDX-License-Identifier: PMPL-1.0-or-later
SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
-->

# AXEL Policy Object Specification

**Version**: 1.1.0-draft
**Author**: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
**License**: PMPL-1.0-or-later
**Status**: Normative

---

## 1. Overview

The AXEL Policy Object is a JSON document served at
`https://<domain>/.well-known/axel-policy`. It declares the content
classification, enforcement profile, and caching behavior for a domain.

The policy object is the **authoritative** source of AXEL metadata for a
domain. DNS TXT records provide discovery; the policy object provides the
complete classification and enforcement configuration.

---

## 2. Content Type

The policy endpoint MUST return:

```
Content-Type: application/json
```

The response body MUST be a single JSON object conforming to this specification.

---

## 3. Schema

### 3.1 Required Fields

| Field | Type | Description |
|-------|------|-------------|
| `version` | string | MUST be `"AXEL1"`. |
| `id` | string | Policy identifier. MUST match the `id` in the DNS TXT record. Changes when policy content changes. |
| `scope` | object | Declares which hostnames this policy covers. |
| `scope.hostnames` | array of strings | List of hostnames. MUST include at least the serving domain. |
| `content` | object | Content classification. |
| `content.category` | string | Content category enum (see Section 3.3). |
| `content.min_age` | integer | Minimum age for access (e.g., 18). |
| `enforcement` | object | Enforcement configuration. |
| `enforcement.profiles` | array of strings | List of supported profiles (e.g., `["AXEL-O"]`). |
| `enforcement.proof_required` | boolean | Whether proof of eligibility is required for access. |
| `cache` | object | Caching directives. |
| `cache.max_age_seconds` | integer | Maximum cache lifetime in seconds. |
| `cache.stale_if_error_seconds` | integer | Stale-if-error window in seconds. |

### 3.2 Optional Fields

| Field | Type | Description |
|-------|------|-------------|
| `isolation` | object | Isolation level declaration. |
| `isolation.level` | string | One of `"L0"`, `"L1"`, `"L1-AUDITED"`. Default: `"L0"`. |
| `isolation.prefixes` | object | Dedicated IP prefixes for L1 content. |
| `isolation.prefixes.ipv6` | array of strings | IPv6 CIDR prefixes. |
| `isolation.prefixes.ipv4` | array of strings | IPv4 CIDR prefixes (legacy). |
| `isolation.cdn_pool` | string | CDN pool identifier for explicit-only hosting. |
| `verifiers` | array of objects | Discovery metadata for verification services. |
| `verifiers[].name` | string | Human-readable verifier name. |
| `verifiers[].url` | string | HTTPS URL of the verifier service. |
| `verifiers[].methods` | array of strings | Supported verification methods. |
| `auditing` | object | Auditor attestation metadata. |
| `auditing.statement_url` | string | HTTPS URL of the auditor's statement. |
| `auditing.policy_hash_sha256` | string | SHA-256 hash of this policy document (hex). |
| `enforcement.browser_flow` | object | Browser gating configuration (see AXEL-O spec). |
| `enforcement.browser_flow.gate_url` | string | URL to redirect browsers for proof. |
| `enforcement.api_flow` | object | API gating configuration (see AXEL-O spec). |
| `enforcement.api_flow.problem_type` | string | RFC 9457 problem type URI. |
| `extensions` | object | Vendor/future extensions. Unknown fields MUST be placed here. |

### 3.3 Content Categories

| Category | Description |
|----------|-------------|
| `"adult"` | Sexually explicit content (18+) |
| `"mature"` | Mature content (may include violence, language) |
| `"gambling"` | Gambling/wagering content |
| `"alcohol"` | Alcohol-related content |
| `"tobacco"` | Tobacco-related content |
| `"cannabis"` | Cannabis-related content |
| `"custom"` | Custom category (described in `extensions`) |

### 3.4 Extensibility

Unknown top-level fields MUST be ignored by parsers. Publishers SHOULD
place non-standard fields under the `extensions` key. This ensures forward
compatibility as the protocol evolves.

---

## 4. Example Policy

### 4.1 Minimal Policy (L0, Origin Enforcement)

```json
{
  "version": "AXEL1",
  "id": "20260212T1200Z",
  "scope": {
    "hostnames": ["explicit.example.com"]
  },
  "content": {
    "category": "adult",
    "min_age": 18
  },
  "enforcement": {
    "profiles": ["AXEL-O"],
    "proof_required": true,
    "browser_flow": {
      "gate_url": "https://explicit.example.com/verify"
    },
    "api_flow": {
      "problem_type": "https://axel-protocol.org/problems/proof-required"
    }
  },
  "cache": {
    "max_age_seconds": 86400,
    "stale_if_error_seconds": 604800
  }
}
```

### 4.2 Full Policy (L1-AUDITED, Origin + Network Enforcement)

```json
{
  "version": "AXEL1",
  "id": "20260212T1430Z",
  "scope": {
    "hostnames": ["explicit.example.com", "cdn-explicit.example.com"]
  },
  "content": {
    "category": "adult",
    "min_age": 18
  },
  "enforcement": {
    "profiles": ["AXEL-O", "AXEL-N"],
    "proof_required": true,
    "browser_flow": {
      "gate_url": "https://explicit.example.com/verify"
    },
    "api_flow": {
      "problem_type": "https://axel-protocol.org/problems/proof-required"
    }
  },
  "isolation": {
    "level": "L1-AUDITED",
    "prefixes": {
      "ipv6": ["2001:db8:abcd::/48"],
      "ipv4": ["198.51.100.0/24"]
    },
    "cdn_pool": "explicit-only-pool-us-east"
  },
  "verifiers": [
    {
      "name": "ExampleVerify",
      "url": "https://verify.example.com",
      "methods": ["zkp", "gov_id"]
    }
  ],
  "auditing": {
    "statement_url": "https://auditor.example.org/statements/example-com-2026.json",
    "policy_hash_sha256": "a1b2c3d4e5f6..."
  },
  "cache": {
    "max_age_seconds": 86400,
    "stale_if_error_seconds": 604800
  },
  "extensions": {}
}
```

---

## 5. Validation Rules

Validators MUST enforce:

1. `version` MUST equal `"AXEL1"`.
2. `id` MUST be a non-empty string.
3. `scope.hostnames` MUST be a non-empty array of valid hostnames.
4. `content.category` MUST be one of the defined categories.
5. `content.min_age` MUST be a non-negative integer.
6. `enforcement.profiles` MUST be a non-empty array containing at least `"AXEL-O"`.
7. `enforcement.proof_required` MUST be a boolean.
8. `cache.max_age_seconds` MUST be a positive integer.
9. `cache.stale_if_error_seconds` MUST be a non-negative integer.
10. If `isolation.level` is `"L1"` or `"L1-AUDITED"`, then `isolation.prefixes`
    or `isolation.cdn_pool` SHOULD be present.
11. If `isolation.level` is `"L1-AUDITED"`, then `auditing` SHOULD be present.

---

## 6. Auditing (MVA-Style Attestation)

Auditor/MVA-style attestation is for:

- **Organization/domain legitimacy**: confirming the publisher is who they claim.
- **Policy integrity**: binding policy hash to domain, classification, min_age,
  and isolation level.

Auditing is **NOT** age verification of users. User eligibility proofs are
handled by verifiers, not auditors.

The `auditing.policy_hash_sha256` field, when present, MUST be the hex-encoded
SHA-256 hash of the canonical policy JSON (minified, sorted keys, no trailing
whitespace).

---

## 7. A2ML Authoring Format (Informative)

Publishers MAY author AXEL policies in A2ML format and compile to the
canonical JSON policy. A2ML is an authoring convenience and is NOT required
for implementers. All AXEL implementations MUST parse the JSON policy format.
A2ML support is OPTIONAL and informative only.

---

## 8. Security Considerations

See [AXEL Core: Security Considerations](core.md#4-security-considerations).

Additionally:

- Policy documents MUST be served over TLS 1.3.
- Publishers SHOULD implement HSTS on the policy endpoint.
- The `id` field prevents stale policy use after updates.
- The `auditing.policy_hash_sha256` field enables integrity verification
  independent of TLS.

---

*This document is part of the AXEL Protocol specification set.*
*See also: [Core Discovery](core.md) | [Origin Enforcement (AXEL-O)](origin.md) | [Managed Network (AXEL-N)](network.md) | [Transport Security](transport.md)*
