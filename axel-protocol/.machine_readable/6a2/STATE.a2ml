;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm — AXEL Protocol current project state

(state
  (metadata
    (project "axel-protocol")
    (version "1.1.0-draft")
    (updated "2026-02-12")
    (author "Jonathan D.A. Jewell"))

  (project-context
    (description "DNS-based content classification protocol with origin/CDN edge enforcement over HTTPS")
    (domain "axel-protocol.org")
    (governance "ASPEC — AXEL Standards and Protocol Enforcement Consortium"))

  (current-position
    (phase "specification-complete")
    (completion-percentage 80)
    (milestone "v1.1.0-draft specification set complete"))

  (completed
    (spec-core "AXEL Core: DNS discovery + policy retrieval + caching + DNSSEC")
    (spec-policy "AXEL Policy Object: JSON format + schema + validation")
    (spec-origin "AXEL-O: Origin/edge HTTP enforcement signaling")
    (spec-network "AXEL-N: Optional managed network enforcement profile")
    (spec-transport "Transport security: TLS 1.3, ECH, DANE")
    (json-schema "schemas/axel-policy.schema.json")
    (policy-examples "public/.well-known/axel-policy + example-adult.json")
    (rescript-parser "src/AxelSts.res — strict DNS TXT parser, fail-closed")
    (validation-tooling "deno task validate:policy + test suite (32 tests)")
    (ietf-drafts "ietf/draft-axel-{core,policy,origin}-00.md")
    (firewall-examples "Origin hardening + gateway enforcement, separated")
    (governance "CHARTER.adoc + GOVERNANCE.adoc"))

  (blockers-and-issues
    (none-critical))

  (critical-next-actions
    (iana-registration "Register .well-known/axel-policy URI and axel-policy link relation")
    (ietf-submission "Submit Internet-Drafts to IETF")
    (reference-verifier "Build proof-of-concept verifier implementation")
    (cdn-guides "Write Cloudflare Workers + Fastly Compute@Edge integration guides")
    (early-adopters "Recruit 2-3 publisher pilots")))
