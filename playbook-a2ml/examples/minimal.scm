;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Example Author

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
