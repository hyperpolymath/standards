;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Example Author

;;; NEUROSYM.scm — Minimal Example
;;; minimal-project

(define-module (minimal-project neurosym)
  #:export (operation-definitions))

;; Minimal operation definitions
(define operation-definitions
  '((file-read
     (forward-semantics . "Return file contents")
     (inverse . #f)
     (claim-type . "verified")
     (entropy-contribution . 1))))
