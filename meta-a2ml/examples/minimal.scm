;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Example Author

;;; META.scm — Minimal Example
;;; minimal-project

(define-module (minimal-project meta)
  #:export (architecture-decisions))

;;; Architecture Decisions Record (ADR)

(define architecture-decisions
  '((adr-001
     (title . "Initial Project Setup")
     (status . "accepted")
     (date . "2025-01-01")
     (context . "Need to establish project foundation")
     (decision . "Use standard project structure")
     (consequences . ("Familiar layout for contributors"
                      "Works with standard tooling")))))
