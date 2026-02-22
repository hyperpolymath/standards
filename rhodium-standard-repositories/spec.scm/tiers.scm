;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath
;;
;; RSR Tier Definitions - Machine Readable
;; For tooling integration with RSR compliance checkers
;;
;; VERSION: 1.0.0 (FROZEN 2025-12-27)
;; Tier thresholds and requirements are immutable in v1.x.

(define-module (rsr tiers)
  #:use-module (rsr version)
  #:export (tiers
            tier-requirements
            get-tier-for-score
            tier-color
            spec-version))

;; Tier definitions with score ranges and display colors
(define tiers
  '((bronze   . ((min-score . 75)
                 (max-score . 89)
                 (color . "#cd7f32")
                 (label . "Bronze")))
    (silver   . ((min-score . 90)
                 (max-score . 99)
                 (color . "#c0c0c0")
                 (label . "Silver")))
    (gold     . ((min-score . 100)
                 (max-score . 100)
                 (color . "#ffd700")
                 (label . "Gold")))
    (rhodium  . ((min-score . 100)
                 (max-score . 100)
                 (exemplary . #t)
                 (color . "#e5e4e2")
                 (label . "Rhodium")))))

;; Requirements per tier (each tier includes previous)
(define tier-requirements
  '((bronze
     (required-files
       "README.md" "README.adoc"  ; one of
       "LICENSE.txt"
       "SECURITY.md"
       ".gitignore"
       ".gitattributes")
     (ci-cd . #t)
     (spdx-headers . #t))

    (silver
     (includes . bronze)
     (required-files
       "CODE_OF_CONDUCT.md" "CODE_OF_CONDUCT.adoc"
       "CONTRIBUTING.md" "CONTRIBUTING.adoc"
       "GOVERNANCE.adoc"
       "MAINTAINERS.md"
       "FUNDING.yml")
     (well-known-files
       "security.txt")
     (scorecard-min . 7.0)
     (sha-pinned-actions . #t))

    (gold
     (includes . silver)
     (required-files
       "STATE.scm"
       "META.scm"
       "ECOSYSTEM.scm")
     (well-known-files
       "ai.txt"
       "humans.txt"
       "provenance.json")
     (scorecard-min . 9.0)
     (package-manager . (guix nix)))  ; one of

    (rhodium
     (includes . gold)
     (exemplary-practices . #t)
     (formal-verification . #t)
     (community-recognition . #t))))

;; Get tier for a given compliance score
(define (get-tier-for-score score exemplary?)
  (cond
    ((and (>= score 100) exemplary?) 'rhodium)
    ((>= score 100) 'gold)
    ((>= score 90) 'silver)
    ((>= score 75) 'bronze)
    (else #f)))

;; Get display color for tier
(define (tier-color tier)
  (let ((tier-data (assoc-ref tiers tier)))
    (if tier-data
        (assoc-ref tier-data 'color)
        "#808080")))  ; gray for unknown

;; Return spec version for this module
(define (spec-version)
  (version-string))
