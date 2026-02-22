<!--
SPDX-License-Identifier: PMPL-1.0-or-later
SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
-->

# AXEL-N: Managed Network Profile

**Version**: 1.1.0-draft
**Author**: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
**License**: PMPL-1.0-or-later
**Status**: Optional (Normative when implemented)

---

## 1. Introduction

AXEL-N defines an **optional** managed network enforcement profile. It is
intended for use by network gateways, ISPs, and managed environments
(schools, libraries, corporate networks) that wish to enforce AXEL policy
at the network layer.

### 1.1 Applicability

AXEL-N is applicable ONLY when:

- The content's isolation level is **L1** or **L1-AUDITED**.
- The restricted content is served from **dedicated hostnames** and/or
  **dedicated IP prefixes** (or a CDN "explicit-only pool").

AXEL-N MUST NOT be applied to L0 content, as L0 content may share
hostnames and IP addresses with unrestricted content, making network-level
blocking imprecise and causing collateral damage.

### 1.2 Limitations

AXEL-N **cannot guarantee universal enforcement**. Users can circumvent
network-level controls via:

- VPNs and tunnels
- Tor and onion routing
- Alternate DNS resolvers (DoH, DoT, DoQ to external resolvers)
- Encrypted Client Hello (ECH) preventing SNI inspection

AXEL-N is a **defense-in-depth** measure, not the primary enforcement
mechanism. AXEL-O (origin/edge enforcement) remains the primary guarantee.

---

## 2. Enforcement Model

### 2.1 Destination-Based Controls

AXEL-N enforcement operates on **destination IP prefixes and hostnames**
declared in the policy's `isolation.prefixes` and `scope.hostnames` fields.

Gateways MUST NOT:

- Inspect SNI (Server Name Indication) for enforcement decisions.
- Perform deep packet inspection (DPI) on TLS-encrypted traffic.
- Terminate or intercept TLS connections.

Gateways SHOULD:

- Use destination IP prefix matching from the policy's `isolation.prefixes`.
- Use DNS response filtering for `scope.hostnames` (when the gateway
  operates a local resolver).
- Serve an informative block page via captive portal (for HTTP) or
  TCP RST (for HTTPS) when blocking.

### 2.2 DNS Response Filtering

Managed network gateways that operate local DNS resolvers MAY:

1. Fetch AXEL policies for domains in their enforcement list.
2. If the policy declares `isolation.level` >= `L1` and the gateway's
   enforcement policy requires blocking:
   - Return NXDOMAIN or a redirect to a block page for queries to
     `scope.hostnames`.

This approach is effective only when clients use the managed resolver.
Clients using external DoH/DoT/DoQ resolvers bypass this control.

### 2.3 IP Prefix Blocking

For L1/L1-AUDITED content with declared `isolation.prefixes`:

1. The gateway fetches the AXEL policy and extracts `isolation.prefixes`.
2. The gateway installs firewall rules blocking or redirecting traffic
   to those prefixes.
3. Rules MUST be scoped to the declared prefixes only.

---

## 3. Gateway Requirements

### 3.1 Policy Fetching

Gateways implementing AXEL-N:

1. MUST fetch policies over HTTPS with TLS 1.3.
2. MUST validate DNSSEC for `_axel.<domain>` TXT records.
3. MUST respect `cache.max_age_seconds` and `cache.stale_if_error_seconds`.
4. MUST check that `isolation.level` is `L1` or `L1-AUDITED` before
   applying network enforcement.
5. SHOULD prefer `L1-AUDITED` content for enforcement (higher confidence).

### 3.2 Transparency

Gateways SHOULD:

- Log enforcement actions (domain, action, timestamp) for accountability.
- Provide users with a mechanism to report false positives.
- Publish their enforcement policy (which AXEL categories are enforced).

### 3.3 Non-Interference

Gateways MUST NOT:

- Block traffic to domains that do not publish AXEL policies.
- Apply AXEL-N enforcement to L0 content.
- Use AXEL infrastructure for surveillance or content inspection
  beyond the declared AXEL categories.

---

## 4. Firewall Integration

### 4.1 Origin Hardening (Not AXEL Enforcement)

CDN-to-origin source allowlists are **not** AXEL enforcement. They are
standard origin hardening to prevent direct-to-origin access bypassing
the CDN. These rules are at the publisher's discretion and outside the
scope of AXEL-N.

### 4.2 Gateway Enforcement (AXEL-N)

Gateway firewall rules for AXEL-N enforcement:

```
# Example: nftables rules for AXEL-N gateway enforcement
# Block traffic to L1 AXEL content prefixes

table inet axel_gateway {
  set axel_blocked_v6 {
    type ipv6_addr
    flags interval
    # Populated from AXEL policies (isolation.prefixes.ipv6)
    elements = { 2001:db8:abcd::/48 }
  }

  set axel_blocked_v4 {
    type ipv4_addr
    flags interval
    # Populated from AXEL policies (isolation.prefixes.ipv4)
    elements = { 198.51.100.0/24 }
  }

  chain forward {
    type filter hook forward priority filter; policy accept;
    ip6 daddr @axel_blocked_v6 reject with icmpv6 admin-prohibited
    ip daddr @axel_blocked_v4 reject with icmp admin-prohibited
  }
}
```

**Note**: The ICMP reject message will NOT be visible to end users in
browsers. For meaningful user communication, use captive portal / DNS
redirect approaches that can serve an explanatory page.

---

## 5. Security Considerations

### 5.1 Collateral Damage

Network-level blocking is inherently imprecise. Even with L1 isolation,
shared CDN IP ranges may serve both restricted and unrestricted content.
AXEL-N enforcers MUST only block prefixes explicitly declared in the
policy's `isolation.prefixes`.

### 5.2 Policy Authenticity

Gateways MUST validate DNSSEC and fetch policies over TLS 1.3 to prevent
enforcement based on forged policies.

### 5.3 Circumvention

AXEL-N cannot prevent determined circumvention. This is by design: AXEL
prioritizes origin enforcement (AXEL-O) over network enforcement (AXEL-N).
Managed networks provide an additional layer, not a guarantee.

---

*This document is part of the AXEL Protocol specification set.*
*See also: [Core Discovery](core.md) | [Policy Object](policy.md) | [Origin Enforcement (AXEL-O)](origin.md) | [Transport Security](transport.md)*
