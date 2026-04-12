;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Example Author
;;
;; DEPRECATED: This file uses the legacy Guile Scheme (.scm) format.
;; See minimal.a2ml for the current A2ML format.

;;; AGENTIC.scm — Minimal Example
;;; minimal-project

(define-module (minimal-project agentic)
  #:export (gating-policies))

;; Minimal gating policy
(define gating-policies
  '((default
     (mode . "strict")
     (require-explicit-intent . #t))))
