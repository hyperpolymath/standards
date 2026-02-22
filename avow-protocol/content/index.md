---
title: STAMP Protocol - Verifiable Consent and Compliance
description: A protocol design for verifiable consent, working unsubscribe links, and enforceable rate limits. This site demonstrates the concepts.
date: 2026-01-30
---

# STAMP Protocol

**Secure Typed Announcement Messaging Protocol**

Protocol design that aims to make consent, unsubscribe, and rate-limit compliance verifiable.

[Try Live Demo on Telegram →](https://t.me/stamp_demo_bot)
[Learn How It Works](#how-it-works)
[Read the protocol draft →](docs/PROTOCOL.md)

## The Problem

### 📧 Email Spam

Unsubscribe links often don't work. No proof consent was given.

**$200M+ market**

### 🤖 Social Media Bots

Fake profiles, astroturfing, election interference. Current solutions don't scale.

**$1.2B+ market**

### 💸 Platform Costs

Companies spend $100M+/year fighting spam and bots. Still losing.

**Bots remain a persistent problem**

## The STAMP Solution {#how-it-works}

Use **dependent types** (Idris2) to make message properties verifiable at compile-time.

### ✓ Proven Consent

Cryptographically provable consent chains. Can't fake timestamps or skip verification.

```
proof : confirmation > initial_request
```

### ✓ Working Unsubscribe

Unsubscribe links validated before sending to avoid broken links.

```
proof : response.code = OK ∧ response.time < 200ms
```

### ✓ Rate Limiting

Protocol-level rate limits based on account age. Cannot be bypassed.

```
proof : messages_today < daily_limit
```

## How It Works

### ❌ Without Dependent Types

```rust
struct UnsubscribeLink {
    url: String,
    tested: bool,  // Can lie
}
```

No guarantees. Developers can lie about testing.

### ✅ With STAMP (Idris2)

```idris
record UnsubscribeLink where
    url : URL
    tested_at : Timestamp
    response : HTTPResponse
    {auto proof : response.code = OK}
    {auto proof : response.time < 200ms}
```

Designed to be verifiable. The protocol aims to reject invalid links before send.

## Try It Now

### Telegram Bot Demo

See STAMP in action with our live Telegram bot (demo flow):

1. Open Telegram and search for **@stamp_demo_bot**
2. Send `/start` to subscribe
3. See consent flow steps in action
4. Send `/verify` to see full verification
5. Send `/unsubscribe` to test the demo flow

[Open @stamp_demo_bot →](https://t.me/stamp_demo_bot)

### Demo Notes

The browser demo uses build-time proof data for URL checks and simulates the consent proof flow. Full proofs are part of the protocol draft.

## Use Cases

### Email Marketing

Design for compliance with CAN-SPAM, GDPR. Verified unsubscribe links.

### Dating Apps

Reduce fake profiles with verified identity chains.

### Social Media

Combat astroturfing and election interference with verified accounts.

### Business Messaging

RCS, SMS marketing with verifiable compliance.

## Impact

- **TBD** Bot Reduction
- **TBD** Platform Savings
- **TBD** Compliance Proof

## Ready to Prove Compliance?

Contact us to integrate STAMP into your platform.

- [GitHub](https://github.com/hyperpolymath/libstamp)
- [Demo Bot](https://t.me/stamp_demo_bot)
- [Contact](mailto:jonathan.jewell@open.ac.uk)

---

© 2026 STAMP Protocol. Licensed under AGPL-3.0.
