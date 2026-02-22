;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath
;;
;; RSR Specification Version - Machine Readable
;; This file defines the canonical version of the RSR specification.

(define-module (rsr version)
  #:export (rsr-version
            version-major
            version-minor
            version-patch
            version-status
            freeze-date
            version-string
            is-frozen?))

;; RSR Specification Version
(define rsr-version
  '((major . 1)
    (minor . 0)
    (patch . 0)
    (status . frozen)
    (freeze-date . "2025-12-27")))

;; Version component accessors
(define (version-major)
  (assoc-ref rsr-version 'major))

(define (version-minor)
  (assoc-ref rsr-version 'minor))

(define (version-patch)
  (assoc-ref rsr-version 'patch))

(define (version-status)
  (assoc-ref rsr-version 'status))

(define (freeze-date)
  (assoc-ref rsr-version 'freeze-date))

;; Format version as string
(define (version-string)
  (format #f "~a.~a.~a"
          (version-major)
          (version-minor)
          (version-patch)))

;; Check if current version is frozen
(define (is-frozen?)
  (eq? (version-status) 'frozen))

;; Version compatibility check
;; A repo compliant with version X.Y.Z is compatible with any X.Y.*
(define (version-compatible? major minor)
  (and (= major (version-major))
       (<= minor (version-minor))))

;; What "Standard" means in RSR v1.0
;;
;; A Rhodium Standard Repository is defined by:
;;
;; 1. CORE PRINCIPLES (immutable)
;;    - Emotional Safety: Reversibility, no shame, safe experimentation
;;    - Offline-First: Intermittent connectivity never blocks work
;;    - Post-JavaScript Liberation: Escape npm/node ecosystem fragility
;;    - Formal Verification: Correctness as solidarity
;;    - Community Over Ego: Architecture enforces collaboration
;;    - Language Polyglotism: Diverse languages resist monoculture
;;
;; 2. COMPLIANCE TIERS (locked thresholds)
;;    - Bronze: 75-89% compliance
;;    - Silver: 90-99% compliance
;;    - Gold: 100% compliance
;;    - Rhodium: 100% + exemplary practices
;;
;; 3. LANGUAGE POLICY (locked lists)
;;    - Allowed: ReScript, Rust, Deno, Gleam, OCaml, Ada, Julia, Guile, Nickel, Bash
;;    - Banned: TypeScript, Node/npm, Go, Python (except SaltStack), Java/Kotlin, Swift
;;
;; 4. REQUIRED FILES (by tier)
;;    - Bronze: README, LICENSE.txt, SECURITY.md, .gitignore, .gitattributes
;;    - Silver: +GOVERNANCE.adoc, MAINTAINERS.md, FUNDING.yml, .well-known/security.txt
;;    - Gold: +STATE.scm, META.scm, ECOSYSTEM.scm, .well-known/{ai,humans,provenance}
;;    - Rhodium: +Formal verification, community recognition
;;
;; 5. COMPLIANCE CATEGORIES (11 weighted)
;;    1. Foundational Infrastructure (15%)
;;    2. Documentation Standards (10%)
;;    3. Security Architecture (15%)
;;    4. Architecture Principles (10%)
;;    5. Web Standards & Protocols (10%)
;;    6. Semantic Web & IndieWeb (5%)
;;    7. FOSS & Licensing (10%)
;;    8. Cognitive Ergonomics (5%)
;;    9. Lifecycle Management (5%)
;;   10. Community & Governance (10%)
;;   11. Mutually Assured Accountability (5%)
;;
;; This definition is FROZEN as of 2025-12-27.
;; Changes require RSR v2.0.
