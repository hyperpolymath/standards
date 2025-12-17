;; SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; STATE.scm — standards

(define-module (standards state)
  #:export (metadata project-context current-position critical-next-actions roadmap))

(define metadata
  '((version . "1.1.0")
    (schema-version . "1.0")
    (created . "2025-12-15")
    (updated . "2025-12-17")
    (project . "standards")
    (repo . "hyperpolymath/standards")))

(define project-context
  '((name . "standards")
    (tagline . "Hyperpolymath ecosystem standards and templates")
    (tech-stack . ("Documentation" "Templates" "GitHub Actions"))))

(define current-position
  '((phase . "stable")
    (overall-completion . 90)
    (components
     ((name . "Community files") (status . "complete"))
     ((name . "License") (status . "complete"))
     ((name . "Security policy") (status . "complete"))
     ((name . "SCM metadata") (status . "complete"))
     ((name . "CI/CD workflows") (status . "complete"))
     ((name . "README documentation") (status . "pending")))
    (working-features
     ("CODE_OF_CONDUCT.md"
      "CONTRIBUTING.md"
      "SECURITY.md"
      "LICENSE.txt"
      "META.scm"
      "ECOSYSTEM.scm"
      "STATE.scm"
      ".github/workflows/codeql.yml"
      ".github/dependabot.yml"))))

(define critical-next-actions
  '((immediate
     ("Add README.adoc content with project overview"))
    (this-week
     ("Add CHANGELOG.md for version tracking"
      "Add issue templates for bug reports and features"))
    (this-month
     ("Create template validation CI workflow"
      "Add security acknowledgments file"))))

(define roadmap
  '((phase-1
     (name . "Foundation Complete")
     (status . "done")
     (items
      ("Community standards files"
       "Dual MIT/AGPL licensing"
       "Security policy"
       "SCM metadata files"
       "SHA-pinned GitHub Actions")))

    (phase-2
     (name . "Documentation Enhancement")
     (status . "in-progress")
     (items
      ("README.adoc with full project documentation"
       "CHANGELOG.md for release tracking"
       "Template usage guide")))

    (phase-3
     (name . "Automation")
     (status . "planned")
     (items
      ("Template validation workflow"
       "Automatic version bumping"
       "Release automation")))

    (phase-4
     (name . "Ecosystem Integration")
     (status . "planned")
     (items
      ("Cross-repository template synchronization"
       "RSR compliance checker integration"
       "Automated security scanning reports")))))
