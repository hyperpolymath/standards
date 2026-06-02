;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2026 Example Author

;;; ANCHOR.scm — Minimal Example
;;; minimal-project
;;; Purpose: Stock-taking
;;; Date: 2026-01-03
;;; Authority: Project Owner

(define-module (minimal-project anchor)
  #:export (authority purpose directives))

;; Minimal authority declaration
(define authority
  '((superintendent
     (name . "Project Owner")
     (role . "owner"))
    (issued
     (date . "2026-01-03"))))

;; Minimal purpose
(define purpose
  '((type . "stock-taking")
    (summary . "Quarterly alignment review")))

;; Minimal directives
(define directives
  '((immediate
     ((action . "review-state")
      (description . "Verify STATE.scm is accurate")))))
