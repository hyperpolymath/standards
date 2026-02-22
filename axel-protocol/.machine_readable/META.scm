;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm — AXEL Protocol meta-level information

(meta
  (metadata
    (project "axel-protocol")
    (media-type "application/vnd.meta+scm")
    (version "1.0"))

  (architecture-decisions
    (adr-001
      (title "Origin/edge enforcement as primary model")
      (status "accepted")
      (context "Network-layer enforcement is unreliable due to VPNs, encrypted DNS, ECH")
      (decision "AXEL-O (origin/CDN edge) is the mandatory enforcement profile; AXEL-N (network) is optional")
      (consequences "Works on public internet without ISP cooperation; cannot guarantee universal enforcement"))

    (adr-002
      (title "No new ports or IPv6 extension headers")
      (status "accepted")
      (context "Port 459 and flow label reservation are not deployable on the real internet")
      (decision "All AXEL communication over HTTPS port 443; no custom ports, flow labels, or hop-by-hop headers")
      (consequences "Immediately deployable with existing infrastructure"))

    (adr-003
      (title "JSON policy as authoritative source")
      (status "accepted")
      (context "DNS records are limited in size and expressiveness")
      (decision "DNS TXT provides discovery (version + id); JSON at .well-known/axel-policy is authoritative")
      (consequences "Rich policy metadata; DNS record is minimal"))

    (adr-004
      (title "Isolation levels for collateral damage prevention")
      (status "accepted")
      (context "Network-level blocking of shared hostnames causes collateral damage")
      (decision "Three levels: L0 (label-only), L1 (network-enforceable), L1-AUDITED")
      (consequences "AXEL-N only applies to L1+; L0 content is safe from network overblocking"))

    (adr-005
      (title "No standardized proof format in v1")
      (status "accepted")
      (context "JWT, ZKP, Privacy Pass are all viable but premature to standardize")
      (decision "v1 standardizes discovery and gating signals only; proof format is verifier/publisher business")
      (consequences "Faster adoption; proof format as future extension")))

  (development-practices
    (language "ReScript for validators, Deno for tooling")
    (testing "deno task test — 32 tests")
    (validation "deno task validate:policy — JSON Schema validation")
    (ci "GitHub Actions: quality, codeql, scorecard")))
