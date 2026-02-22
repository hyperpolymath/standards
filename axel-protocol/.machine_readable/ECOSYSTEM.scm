;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm — AXEL Protocol ecosystem position

(ecosystem
  (version "1.0")
  (name "axel-protocol")
  (type "protocol-specification")
  (purpose "DNS-based content classification with origin/CDN edge enforcement")

  (position-in-ecosystem
    (domain "content-classification")
    (layer "application")
    (enforcement "origin-first"))

  (related-projects
    (proven
      (relationship "dependency")
      (description "Formally verified domain validation (Idris2)")
      (url "https://github.com/hyperpolymath/proven"))

    (mta-sts
      (relationship "inspiration")
      (description "AXEL Core uses MTA-STS-style policy ID pinning (RFC 8461)")
      (url "https://datatracker.ietf.org/doc/html/rfc8461"))

    (rfc9457
      (relationship "standard-dependency")
      (description "AXEL-O API gating uses RFC 9457 Problem Details")
      (url "https://datatracker.ietf.org/doc/html/rfc9457"))

    (hypatia
      (relationship "potential-consumer")
      (description "Neurosymbolic CI/CD intelligence — may scan AXEL policies")
      (url "https://github.com/hyperpolymath/hypatia"))

    (panic-attacker
      (relationship "potential-consumer")
      (description "Security scanning — may audit AXEL policy correctness")
      (url "https://github.com/hyperpolymath/panic-attacker"))))
