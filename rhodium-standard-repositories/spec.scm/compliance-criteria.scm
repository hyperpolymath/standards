;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath
;;
;; RSR Compliance Criteria - Machine Readable
;; 11 categories, 50+ criteria for automated validation
;;
;; VERSION: 1.0.0 (FROZEN 2025-12-27)
;; These criteria are immutable in v1.x. Changes require v2.0.

(define-module (rsr compliance-criteria)
  #:use-module (rsr version)
  #:export (categories
            get-category
            criteria-count
            validate-criterion
            spec-version))

;; All 11 compliance categories
(define categories
  '((foundational-infrastructure
     (id . 1)
     (name . "Foundational Infrastructure")
     (weight . 15)
     (criteria
       ((id . "1.1.1") (name . "nix-flake") (description . "flake.nix and flake.lock present"))
       ((id . "1.1.2") (name . "nickel-configs") (description . "Infrastructure-as-code in Nickel"))
       ((id . "1.1.3") (name . "justfile") (description . "Task runner with 15+ recipes"))
       ((id . "1.1.4") (name . "podman") (description . "Container config uses Podman"))
       ((id . "1.1.5") (name . "chainguard") (description . "Base images use Chainguard Wolfi"))
       ((id . "1.2.1") (name . "gitlab") (description . "Repository on GitLab"))
       ((id . "1.2.2") (name . "git-hooks") (description . "Pre-commit/pre-push hooks"))
       ((id . "1.2.3") (name . "rvc") (description . "Robot Vacuum Cleaner configured"))
       ((id . "1.2.4") (name . "saltrover") (description . "Offline repo management"))))

    (documentation-standards
     (id . 2)
     (name . "Documentation Standards")
     (weight . 10)
     (criteria
       ((id . "2.1.1") (name . "readme") (description . "README.md or README.adoc"))
       ((id . "2.1.2") (name . "license") (description . "LICENSE.txt (plain text)"))
       ((id . "2.1.3") (name . "security") (description . "SECURITY.md"))
       ((id . "2.1.4") (name . "coc") (description . "CODE_OF_CONDUCT.md/.adoc"))
       ((id . "2.1.5") (name . "contributing") (description . "CONTRIBUTING.md/.adoc"))
       ((id . "2.1.6") (name . "funding") (description . "FUNDING.yml"))
       ((id . "2.1.7") (name . "governance") (description . "GOVERNANCE.adoc"))
       ((id . "2.1.8") (name . "maintainers") (description . "MAINTAINERS.md"))
       ((id . "2.1.9") (name . "gitignore") (description . ".gitignore"))
       ((id . "2.1.10") (name . "gitattributes") (description . ".gitattributes"))
       ((id . "2.2.1") (name . "security-txt") (description . ".well-known/security.txt"))
       ((id . "2.2.2") (name . "ai-txt") (description . ".well-known/ai.txt"))
       ((id . "2.2.3") (name . "consent-txt") (description . ".well-known/consent-required.txt"))
       ((id . "2.2.4") (name . "provenance") (description . ".well-known/provenance.json"))
       ((id . "2.2.5") (name . "humans-txt") (description . ".well-known/humans.txt"))
       ((id . "2.3.1") (name . "meta-scm") (description . "META.scm present"))
       ((id . "2.3.2") (name . "ecosystem-scm") (description . "ECOSYSTEM.scm present"))
       ((id . "2.3.3") (name . "state-scm") (description . "STATE.scm present"))))

    (security-architecture
     (id . 3)
     (name . "Security Architecture")
     (weight . 15)
     (criteria
       ((id . "3.1.1") (name . "type-safe-lang") (description . "Uses type-safe languages"))
       ((id . "3.1.2") (name . "memory-safe") (description . "Memory-safe by default"))
       ((id . "3.2.1") (name . "crdt-state") (description . "CRDT for distributed state"))
       ((id . "3.2.2") (name . "offline-first") (description . "Works without network"))
       ((id . "3.3.1") (name . "deno-perms") (description . "Explicit Deno permissions"))
       ((id . "3.3.2") (name . "rootless") (description . "Podman rootless containers"))
       ((id . "3.4.1") (name . "spdx-headers") (description . "SPDX headers on all files"))
       ((id . "3.4.2") (name . "sha-pinned") (description . "SHA-pinned dependencies"))
       ((id . "3.4.3") (name . "sbom") (description . "SBOM generation"))))

    (architecture-principles
     (id . 4)
     (name . "Architecture Principles")
     (weight . 10)
     (criteria
       ((id . "4.1.1") (name . "distributed-first") (description . "Distributed-first design"))
       ((id . "4.1.2") (name . "reversibility") (description . "Operations can be undone"))
       ((id . "4.1.3") (name . "reflexivity") (description . "Self-reasoning systems"))
       ((id . "4.1.4") (name . "interop") (description . "iSOS interoperability"))))

    (web-standards
     (id . 5)
     (name . "Web Standards & Protocols")
     (weight . 10)
     (criteria
       ((id . "5.1.1") (name . "well-known-dir") (description . ".well-known/ directory"))
       ((id . "5.2.1") (name . "dnssec") (description . "DNSSEC validation"))
       ((id . "5.2.2") (name . "tls13") (description . "TLS 1.3 only"))
       ((id . "5.3.1") (name . "csp") (description . "Content-Security-Policy"))
       ((id . "5.3.2") (name . "hsts") (description . "HSTS enabled"))))

    (semantic-web
     (id . 6)
     (name . "Semantic Web & IndieWeb")
     (weight . 5)
     (criteria
       ((id . "6.1.1") (name . "schema-org") (description . "Schema.org markup"))
       ((id . "6.1.2") (name . "microformats") (description . "Microformats (h-card, etc.)"))
       ((id . "6.2.1") (name . "webmention") (description . "Webmention support"))
       ((id . "6.2.2") (name . "posse") (description . "POSSE syndication"))))

    (foss-licensing
     (id . 7)
     (name . "FOSS & Licensing")
     (weight . 10)
     (criteria
       ((id . "7.1.1") (name . "license-txt") (description . "LICENSE.txt (SPDX)"))
       ((id . "7.1.2") (name . "spdx-audit") (description . "SPDX audit passing"))
       ((id . "7.2.1") (name . "palimpsest") (description . "Palimpsest-compatible"))
       ((id . "7.2.2") (name . "dco") (description . "DCO or CLA"))))

    (cognitive-ergonomics
     (id . 8)
     (name . "Cognitive Ergonomics")
     (weight . 5)
     (criteria
       ((id . "8.1.1") (name . "consistent-structure") (description . "Consistent directory structure"))
       ((id . "8.1.2") (name . "progressive-disclosure") (description . "Simple to complex"))
       ((id . "8.2.1") (name . "wcag-aa") (description . "WCAG 2.1 AA"))
       ((id . "8.2.2") (name . "semantic-html") (description . "Semantic HTML"))
       ((id . "8.3.1") (name . "i18n") (description . "Internationalization ready"))))

    (lifecycle-management
     (id . 9)
     (name . "Lifecycle Management")
     (weight . 5)
     (criteria
       ((id . "9.1.1") (name . "vendoring") (description . "Critical deps vendored"))
       ((id . "9.1.2") (name . "pinned-versions") (description . "Pinned versions"))
       ((id . "9.2.1") (name . "semver") (description . "Semantic versioning"))
       ((id . "9.2.2") (name . "deprecation") (description . "Deprecation warnings"))
       ((id . "9.3.1") (name . "sunset-policy") (description . "Sunset policy documented"))))

    (community-governance
     (id . 10)
     (name . "Community & Governance")
     (weight . 10)
     (criteria
       ((id . "10.1.1") (name . "tpcf") (description . "TPCF contribution model"))
       ((id . "10.1.2") (name . "coc-enforcement") (description . "CoC enforcement"))
       ((id . "10.2.1") (name . "governance-model") (description . "Governance documented"))
       ((id . "10.2.2") (name . "succession") (description . "Succession planning"))))

    (accountability
     (id . 11)
     (name . "Mutually Assured Accountability")
     (weight . 5)
     (criteria
       ((id . "11.1.1") (name . "maa-framework") (description . "MAA principles"))
       ((id . "11.1.2") (name . "audit-trails") (description . "Immutable audit logs"))
       ((id . "11.1.3") (name . "provenance-chains") (description . "Provenance tracking"))))))

;; Get a category by ID or name
(define (get-category id-or-name)
  (find (lambda (cat)
          (or (eq? (car cat) id-or-name)
              (equal? (assoc-ref (cdr cat) 'id) id-or-name)))
        categories))

;; Count total criteria
(define (criteria-count)
  (apply + (map (lambda (cat)
                  (length (assoc-ref (cdr cat) 'criteria)))
                categories)))

;; Validate a single criterion (stub - implement in actual validator)
(define (validate-criterion criterion-id repo-path)
  ;; Returns: 'pass | 'fail | 'partial | 'na
  'pass)  ; Stub

;; Return spec version for this module
(define (spec-version)
  (version-string))
