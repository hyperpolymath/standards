---
title: AXEL Protocol | Access eXplicit Enforcement & Labeling
description: DNS-based labeling for sexually explicit content with privacy-preserving enforcement
status: Draft Specification v1.0.0
---

# AXEL Protocol

**Access eXplicit Enforcement & Labeling**

---

## What is AXEL?

AXEL is a DNS-based protocol for labeling sexually explicit content to enable
precise network filtering while preserving user privacy. It addresses failures
of voluntary labeling systems (ICRA, PICS) by providing technical enforcement
at the network layer.

### Key Features

**Privacy-First:** DoQ + OHTTP prevent ISP logging; zero-knowledge proofs for age verification

**Jurisdiction-Aware:** Content labeled universally; enforcement applied locally per jurisdiction

**Opt-In:** Voluntary adoption by platforms; no censorship or surveillance backdoors

**IPv6-Optimized:** Leverages IPv6 QoS for bandwidth optimization and flow labeling

## How It Works

**Content providers** label their content via DNS records
(similar to MTA-STS for email). **Networks** enforce policies
based on jurisdiction (18+ in UK, 21+ in Singapore, blanket block in restrictive
countries). **Users** attest compliance using short-lived tokens
that don't leak identity.

Example DNS record:

```
_axel._sts.example.com. IN TXT "v=AXEL1; mode=enforce; ipv6-only=1; attestation=https://example.com/.well-known/axel/attestation"
```

## Quick Links

- [Specification & Code (GitHub)](https://github.com/hyperpolymath/axel-protocol)
- [Read the Documentation](https://github.com/hyperpolymath/axel-protocol/blob/main/README.adoc)
- [Governance (ASPEC)](https://github.com/hyperpolymath/axel-protocol/blob/main/GOVERNANCE.adoc)
- [Contact Us](mailto:info@axel-protocol.org)

## Governance

AXEL is maintained by **ASPEC** (AXEL Standards and Protocol
Enforcement Consortium), a multi-stakeholder organization ensuring neutral
governance. We are currently recruiting founding members from CDNs, privacy
organizations, academic institutions, and platform providers.

**Interested in joining?** Contact: [membership@axel-protocol.org](mailto:membership@axel-protocol.org)

---

_AXEL Protocol is licensed under [PMPL-1.0-or-later](https://github.com/hyperpolymath/palimpsest-license)
(Palimpsest-MPL)_

© 2025 AXEL Protocol Contributors |
[License](https://github.com/hyperpolymath/axel-protocol/blob/main/LICENSE) |
[Security](mailto:security@axel-protocol.org)
