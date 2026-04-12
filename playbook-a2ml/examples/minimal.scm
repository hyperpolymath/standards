;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Example Author
;;
;; DEPRECATED: This file uses the legacy Guile Scheme (.scm) format.
;; See minimal.a2ml for the current A2ML format.

;;; PLAYBOOK.scm — Minimal Example
;;; minimal-project

(define-module (minimal-project playbook)
  #:export (derivation-source
            procedures))

;; Every PLAYBOOK must declare its derivation
(define derivation-source
  '((type . "derived")
    (meta-rules . (adr-001))
    (timestamp . "2026-01-03T00:00:00Z")))

;; Minimal procedure definition
(define procedures
  '((build
     (description . "Build the project")
     (steps
       ((step 1) (action . "deno task build")))
     (on-failure . "abort"))))
