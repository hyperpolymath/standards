<!--
SPDX-License-Identifier: PMPL-1.0-or-later
SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
-->

# AXEL Protocol Specification

**AXEL** = **A**ccess e**X**plicit **E**nforcement & **L**abeling

**Version**: 1.0.0-draft
**Author**: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
**License**: PMPL-1.0-or-later

---

## 1. Introduction

The AXEL Protocol is an IPv6-based protocol for isolating age-restricted content using DNS-based enforcement mechanisms, authorized IP prefix lists, and privacy-preserving attestation. It addresses the failures of voluntary content labeling systems (ICRA, PICS) by providing technical enforcement at the network layer.

### 1.1 Terminology

- **AXEL-STS**: AXEL Protocol Strict Transport Security
- **APL**: Address Prefix List (DNS record type)
- **DoQ**: DNS over QUIC
- **OHTTP**: Oblivious HTTP
- **ZKP**: Zero-Knowledge Proof
- **Attestation Token**: Short-lived credential proving age verification

---

## 2. AXEL-STS (Strict Transport Security)

### 2.1 DNS TXT Record Format

```dns
_axel._sts.<domain> IN TXT "v=AXEL1; mode=<testing|enforce>; ipv6-only=<0|1>; attestation=<URL>"
```

| Field | Required | Type | Description | Example |
|-------|----------|------|-------------|---------|
| `v` | Yes | String | Protocol version (fixed: `AXEL1`) | `AXEL1` |
| `mode` | Yes | Enum | `testing`: Log violations; `enforce`: Block non-AXEL traffic | `enforce` |
| `ipv6-only` | Yes | Boolean | `1`: Require IPv6; `0`: Allow IPv4 (deprecated) | `1` |
| `attestation` | Yes | URL | HTTPS endpoint for attestation tokens | `https://example.com/.well-known/axel/attestation` |

### 2.2 Behavior

- **Testing Mode**: Log all AXEL policy violations but allow traffic
- **Enforce Mode**: Block traffic that violates AXEL-STS policy
- **IPv6-Only**: When set to `1`, reject all IPv4 requests with `HTTP 426 Upgrade Required`

### 2.3 Example

```dns
_axel._sts.example.com. IN TXT "v=AXEL1; mode=enforce; ipv6-only=1; attestation=https://example.com/.well-known/axel/attestation"
```

---

## 3. APL (Authorized Prefix List)

### 3.1 DNS APL Record Format

```dns
axel._apl.<domain> IN APL <family>:<prefix> [<family>:<prefix> ...]
```

| Family | Description | Example Prefix |
|--------|-------------|----------------|
| 1 | IPv6 | `2001:db8::/32` |
| 2 | IPv4 (deprecated) | `192.0.2.0/24` |

### 3.2 Purpose

APL records define which IP address ranges are authorized to serve AXEL content for a given domain. This prevents spoofing and enables precise network-level filtering.

### 3.3 Validation

- APL records MUST be DNSSEC-signed
- APL prefixes SHOULD be validated against RPKI (Resource Public Key Infrastructure)
- Clients/proxies MUST drop packets from IPs not in the APL

### 3.4 Example

```dns
axel._apl.example.com. IN APL 1:2001:db8::/32 1:2606:4700::/32
```

---

## 4. Attestation Tokens

### 4.1 Token Format (JWT Recommended)

```json
{
  "iss": "https://example.com",
  "sub": "user-identifier",
  "aud": "https://axel.example.com",
  "exp": 1735689600,
  "nbf": 1735688700,
  "iat": 1735688700,
  "axel": {
    "age_verified": true,
    "method": "zkp",
    "min_age": 18
  }
}
```

| Claim | Required | Description |
|-------|----------|-------------|
| `iss` | Yes | Issuer (attestation service URL) |
| `sub` | Yes | Subject (pseudonymous user ID) |
| `aud` | Yes | Audience (AXEL service URL) |
| `exp` | Yes | Expiration time (max 15 minutes from `iat`) |
| `nbf` | No | Not before time |
| `iat` | Yes | Issued at time |
| `axel.age_verified` | Yes | Boolean indicating age verification |
| `axel.method` | Yes | Verification method (`zkp`, `gov_id`, `credit_card`) |
| `axel.min_age` | No | Minimum age verified (default: 18) |

### 4.2 Token Binding

Tokens MUST be bound to the client's IPv6 address to prevent sharing:

- Include client IPv6 in JWT `sub` claim (hashed)
- Validate via TLS client certificate SAN (Subject Alternative Name)

### 4.3 Token Lifetime

- **Maximum**: 15 minutes
- **Recommended**: 5 minutes
- **Refresh**: Clients should refresh tokens before expiration

### 4.4 Privacy-Preserving Methods

**Zero-Knowledge Proofs (ZKPs)**:
- Prove age > 18 without revealing exact birthdate
- Use zk-SNARKs or similar cryptographic techniques
- Example: EU Digital Identity Wallet pilot

**Government ID Verification**:
- NFC-based document reading (e.g., passport chips)
- Verify government-issued digital signatures
- Return minimal attestation (boolean: age >= 18)

---

## 5. IPv6 Requirements

### 5.1 Flow Labels

AXEL traffic SHOULD use flow label `0x000201FF` (decimal 131071) for QoS tagging:

```
IPv6 Header:
  Flow Label: 0x000201FF (131071)
```

### 5.2 Extension Headers

AXEL MAY use IPv6 Hop-by-Hop or Destination Options headers to carry:
- Attestation hints (token expiry, method used)
- Fallback indicators (IPv4 client via 464XLAT)

### 5.3 Mandatory Features

- **IPsec**: All AXEL traffic SHOULD use IPsec for transport security
- **SLAAC**: Stateless Address Autoconfiguration for client addressing
- **SEND**: SEcure Neighbor Discovery to prevent spoofing

---

## 6. .well-known Endpoints

### 6.1 /axel/config

**Method**: GET
**Response**: `application/json`

Returns AXEL configuration for the domain:

```json
{
  "version": "AXEL1",
  "modes": ["testing", "enforce"],
  "ipv6_only": true,
  "attestation_methods": ["zkp", "gov_id"],
  "attestation_url": "https://example.com/.well-known/axel/verify"
}
```

### 6.2 /axel/verify

**Method**: POST
**Request Body**: `application/json`

```json
{
  "method": "zkp",
  "proof": "<base64-encoded-zkp>"
}
```

**Response**: `application/jwt`

Returns a short-lived JWT attestation token.

### 6.3 /axel/revoke

**Method**: POST
**Request Body**: `application/json`

```json
{
  "token": "<jwt-to-revoke>"
}
```

Revokes a previously issued token (adds to local revocation list).

---

## 7. Fallback for IPv4

### 7.1 464XLAT

AXEL services MAY support IPv4 clients via 464XLAT (IPv4-to-IPv6 translation):

- Add `AXEL-Fallback: 1` HTTP header for IPv4 requests
- Apply rate limiting (e.g., 10 req/min for IPv4, unlimited for IPv6)
- Add 500ms latency to incentivize IPv6 adoption

### 7.2 Deprecation Timeline

- **Year 1**: IPv4 allowed with warnings
- **Year 2**: IPv4 rate-limited (10x slower than IPv6)
- **Year 3**: IPv4 blocked entirely

---

## 8. Security Considerations

### 8.1 APL Spoofing

**Threat**: Attacker claims to be in authorized IP range
**Mitigation**:
- DNSSEC-sign all APL records
- Validate against RPKI ROAs (Route Origin Authorizations)
- Use BGP monitoring (e.g., RIPE RIS) to detect hijacks

### 8.2 Token Theft

**Threat**: Attacker steals JWT token
**Mitigation**:
- Bind tokens to IPv6 address + TLS client cert
- Short expiry (max 15 minutes)
- Token revocation endpoint

### 8.3 Downgrade Attacks

**Threat**: Attacker strips AXEL-STS headers
**Mitigation**:
- Sign AXEL-STS records with DNSSEC
- Use HSTS-style preload list for critical domains
- Monitor for policy violations in logs

### 8.4 Privacy Leaks

**Threat**: ISP logs AXEL traffic metadata
**Mitigation**:
- Require DoQ for all DNS queries
- Use OHTTP to hide destination from ISP
- Minimize token claims (no PII)

---

## 9. Deployment

### 9.1 DNS Configuration

1. Add AXEL-STS record:
   ```dns
   _axel._sts.example.com. IN TXT "v=AXEL1; mode=testing; ipv6-only=1; attestation=https://example.com/.well-known/axel/attestation"
   ```

2. Add APL record:
   ```dns
   axel._apl.example.com. IN APL 1:2001:db8::/32
   ```

3. Sign zone with DNSSEC:
   ```bash
   ldns-signzone example.com Kexample.com.+013+12345
   ```

### 9.2 Server Configuration

1. Deploy attestation service at `/.well-known/axel/verify`
2. Configure firewall to allow only APL IPs
3. Enable DoQ resolver (e.g., Cloudflare 1.1.1.1, Quad9)

### 9.3 Client Configuration

1. Use DoQ-capable resolver
2. Fetch attestation token before accessing AXEL content
3. Include token in `Authorization: Bearer <token>` header

---

## 10. IANA Considerations

### 10.1 Port Assignment

Request assignment of **port 459** for AXEL (TCP/UDP).

### 10.2 IPv6 Flow Label

Request reservation of flow label **0x000201FF** (131071) for AXEL traffic.

### 10.3 Well-Known URI

Register `/.well-known/axel/` prefix with IANA Well-Known URIs registry.

---

## 11. References

- **RFC 8461**: SMTP MTA Strict Transport Security (MTA-STS)
- **RFC 6698**: DNS-Based Authentication of Named Entities (DANE)
- **RFC 9250**: DNS over Dedicated QUIC Connections (DoQ)
- **RFC 6437**: IPv6 Flow Label Specification
- **RFC 7858**: Specification for DNS over Transport Layer Security (DoT)

---

## Appendix A: Example Implementations

See `examples/` directory for:
- ReScript validator (`axelSts.res`)
- WASM proxy integration
- firewalld rules
- Terraform deployment

---

## Appendix B: Changelog

- **2025-01-30**: Initial draft (v1.0.0-draft)
