# AVOW Protocol (Draft)

AVOW (Authenticated Verifiable Open Web) Protocol is a protocol design for verifiable consent and compliant messaging.

This repository is the **demo site** and reference implementation for key concepts. The protocol definition is a living draft and is intentionally short until the core invariants are finalized.

## 1. Overview

AVOW aims to make consent, unsubscribe, and rate‑limit compliance **verifiable** instead of implied. It focuses on evidence that a platform enforced policy, without exposing private subscriber data.

### Goals

- Make consent state **verifiable**, not just asserted.
- Ensure unsubscribe links are **valid and testable** before send.
- Enforce rate limits at the protocol boundary.
- Provide auditability without exposing private subscriber data.

### Non‑Goals

- Replacing full CRM/marketing platforms.
- Defining a transport‑layer messaging protocol.
- Handling subscriber identity verification (out of scope for v1).

## 2. Actors and Roles

- **Sender**: The entity initiating a message.
- **Subscriber**: The recipient whose consent is tracked.
- **Platform**: The system enforcing consent, unsubscribe, and rate‑limit rules.
- **Auditor**: A party verifying evidence of compliance.

## 3. Consent Model

### Consent Event Types (Draft)

- `request`
- `confirm`
- `revoke`
- `expire`

### Consent Chain Invariants

- Confirmation must occur **after** request.
- Revocation must occur **after** confirmation.
- Expiry must occur **after** confirmation.

## 4. Proof Artifacts

### Proof Envelope (Draft)

- `event_id`
- `event_type`
- `timestamp`
- `subject`
- `policy_version`
- `signature`

### Signature Requirements

- Proofs should be signed by the platform.
- Signatures must be verifiable without private subscriber data.

## 5. Unsubscribe Requirements

- URLs must parse cleanly.
- HTTPS is required for unsubscribe links.
- A failed unsubscribe request must be recorded as an event.

## 6. Rate Limits

- Rate limits are policy‑defined per sender class.
- Enforcement should produce an auditable decision record.

## 7. Interoperability

- Canonical JSON schemas for events and proofs (planned).
- Versioned policy documents.
- Clear downgrade paths for older clients.

## 8. Security Considerations

- Tamper‑evident proof storage.
- Replay protection for consent events.
- Privacy constraints on stored evidence.

## 9. Test Vectors (Planned)

- Valid/invalid consent chains.
- Valid/invalid unsubscribe links.
- Rate‑limit edge cases.

## 10. Implementation Notes

- Build‑time validation for links.
- Runtime enforcement hooks for consent and rate limits.
- Suggested library APIs for proof verification.

## What Is Verifiable Today

- URL validation is backed by the `proven` library and can be enforced at build time.
- The demo site uses build‑time proof data for URL checks and a fixed consent proof example.

## What Is Still In Draft

- Cryptographic proof format and signature flow.
- Canonical consent event schema.
- Protocol interoperability requirements.
- Threat model and formal verification plan for non‑URL components.
