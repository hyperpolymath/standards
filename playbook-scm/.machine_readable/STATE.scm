;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Project state for playbook-scm
;; Media-Type: application/vnd.state+scm

(state
  (metadata
    (version "0.1.0")
    (schema-version "1.0")
    (created "2025-12-20")
    (updated "2026-01-03")
    (project "playbook-scm")
    (repo "github.com/hyperpolymath/playbook-scm"))

  (project-context
    (name "PLAYBOOK.scm Specification")
    (tagline "Executable plans derived from META")
    (tech-stack ("Guile Scheme" "AsciiDoc" "ABNF")))

  (current-position
    (phase "initial-development")
    (overall-completion 40)
    (components
      ((spec "in-progress" 60)
       (examples "complete" 100)
       (abnf-grammar "pending" 0)
       (json-schema "pending" 0)))
    (working-features
      ("Core specification document"
       "Minimal and comprehensive examples"
       "README documentation")))

  (route-to-mvp
    (milestones
      ((milestone "M1")
       (name "Specification Complete")
       (items
         (("Complete ABNF grammar" "pending")
          ("Add JSON Schema" "pending")
          ("Add VERSION-POLICY.adoc" "pending"))))
      ((milestone "M2")
       (name "Reference Parser")
       (items
         (("Deno parser implementation" "not-started")
          ("Validation tooling" "not-started"))))))

  (blockers-and-issues
    (critical)
    (high)
    (medium ("Need ABNF grammar for formal syntax"))
    (low))

  (critical-next-actions
    (immediate ("Complete spec/ directory structure"))
    (this-week ("Add ABNF grammar" "Add JSON Schema"))
    (this-month ("Reference parser implementation")))

  (session-history
    ((date "2026-01-03")
     (accomplishments
       ("Created spec/PLAYBOOK-FORMAT-SPEC.adoc"
        "Created examples/minimal.scm"
        "Created examples/comprehensive.scm"
        "Updated README.adoc")))))
