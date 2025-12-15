;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; STATE.scm — standards

(define-module (standards state)
  #:export (metadata project-context current-position critical-next-actions))

(define metadata
  '((version . "1.0.0")
    (schema-version . "1.0")
    (created . "2025-12-15")
    (updated . "2025-12-15")
    (project . "standards")
    (repo . "hyperpolymath/standards")))

(define project-context
  '((name . "standards")
    (tagline . "Hyperpolymath ecosystem standards and templates")
    (tech-stack . ("Documentation" "Templates"))))

(define current-position
  '((phase . "stable")
    (overall-completion . 100)
    (components
     ((name . "Community files") (status . "complete"))
     ((name . "License") (status . "complete"))
     ((name . "Security policy") (status . "complete")))
    (working-features
     ("CODE_OF_CONDUCT.md"
      "CONTRIBUTING.md"
      "SECURITY.md"
      "LICENSE.txt"))))

(define critical-next-actions
  '((immediate . ())
    (this-week . ())
    (this-month . ())))
