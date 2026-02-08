;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; STATE.scm - Project state for meta-scm
;; Media-Type: application/state+scheme

(state
  (metadata
    (version "1.0.0")
    (schema-version "1.0")
    (created "2025-01-01")
    (updated "2026-01-03")
    (project "meta-scm")
    (repo "github.com/hyperpolymath/meta-scm"))

  (project-context
    (name "meta-scm")
    (tagline "Machine-readable Engineering and Technical Architecture files")
    (tech-stack ("Guile Scheme" "AsciiDoc" "ABNF" "JSON Schema")))

  (current-position
    (phase "community-specification")
    (overall-completion 75)
    (components
      ((name "META-FORMAT-SPEC") (status "complete") (completion 100))
      ((name "STATE companion") (status "complete") (completion 100))
      ((name "ECOSYSTEM companion") (status "complete") (completion 100))
      ((name "ABNF grammar") (status "complete") (completion 100))
      ((name "JSON Schema") (status "complete") (completion 100))
      ((name "Examples") (status "complete") (completion 100))
      ((name "Tooling") (status "planned") (completion 0))
      ((name "IANA registration") (status "planned") (completion 0)))
    (working-features
      ("RFC-style specification"
       "ABNF grammar definition"
       "JSON Schema validation"
       "Minimal and comprehensive examples"
       "Standards track roadmap")))

  (route-to-mvp
    (milestones
      ((id "m1")
       (name "Community Specification")
       (status "in-progress")
       (items
         (("Spec complete" . done)
          ("Examples complete" . done)
          ("Reference tooling" . todo))))
      ((id "m2")
       (name "IANA Registration")
       (status "planned")
       (items
         (("Submit media type" . todo)
          ("Coordinate with siblings" . todo))))
      ((id "m3")
       (name "IETF RFC")
       (status "planned")
       (items
         (("Draft I-D" . todo)
          ("WG adoption" . todo))))))

  (blockers-and-issues
    (critical)
    (high)
    (medium
      ((id "i1")
       (description "Need reference parser implementations")
       (category "tooling")))
    (low))

  (critical-next-actions
    (immediate
      ("Update ecosystem relationships"))
    (this-week
      ("Coordinate with sibling -scm repos"))
    (this-month
      ("Begin reference tooling development")))

  (session-history
    ((date "2026-01-03")
     (accomplishments
       ("Updated README with full SCM Format Family table"
        "Added execution pipeline documentation"
        "Updated relationships to sibling repos")))))
