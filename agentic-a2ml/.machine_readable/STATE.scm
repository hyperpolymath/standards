;; SPDX-License-Identifier: MPL-2.0-or-later
;; STATE.scm - Project state for agentic-scm
;; Media-Type: application/vnd.state+scm

(state
  (metadata
    (version "0.2.0")
    (schema-version "1.0")
    (created "2025-12-20")
    (updated "2026-01-09")
    (project "agentic-scm")
    (repo "github.com/hyperpolymath/agentic-scm"))

  (project-context
    (name "AGENTIC.scm Specification")
    (tagline "Operational gating for AI agents")
    (tech-stack ("Guile Scheme" "AsciiDoc" "ABNF" "JSON Schema" "Ada")))

  (current-position
    (phase "specification-complete")
    (overall-completion 85)
    (components
      ((spec "complete" 100)
       (examples "complete" 100)
       (abnf-grammar "complete" 100)
       (json-schema "complete" 100)
       (version-policy "complete" 100)
       (ada-tooling "complete" 100)
       (meta-decisions "complete" 100)
       (neurosym-integration "complete" 100)
       (reference-implementation "pending" 0)))
    (working-features
      ("Core specification document"
       "Minimal and comprehensive examples"
       "README documentation"
       "ABNF grammar (RFC 5234 compliant)"
       "JSON Schema (draft-2020-12)"
       "VERSION-POLICY.adoc"
       "Ada RSR adapter implementation"
       "Architecture Decision Records"
       "Neurosymbolic integration config"
       "Gating policies section"
       "Entropy budgets section"
       "Risk thresholds section"
       "Override paths section"
       "Decision recording section")))

  (route-to-mvp
    (milestones
      ((milestone "M1")
       (name "Specification Complete")
       (status "complete")
       (items
         (("Complete ABNF grammar" "complete")
          ("Add JSON Schema" "complete")
          ("Add VERSION-POLICY.adoc" "complete"))))
      ((milestone "M2")
       (name "Reference Implementation")
       (status "in-progress")
       (items
         (("Deno gating evaluator" "not-started")
          ("Validation tooling" "not-started"))))))

  (blockers-and-issues
    (critical)
    (high)
    (medium)
    (low))

  (critical-next-actions
    (immediate ("Begin reference implementation design"))
    (this-week ("Implement basic Deno parser for AGENTIC.scm"))
    (this-month ("Complete validation tooling")))

  (session-history
    ((date "2026-01-03")
     (accomplishments
       ("Created spec/AGENTIC-FORMAT-SPEC.adoc"
        "Created examples/minimal.scm"
        "Created examples/comprehensive.scm"
        "Updated README.adoc with SCM Family table")))
    ((date "2026-01-09")
     (accomplishments
       ("Created spec/agentic.abnf - ABNF grammar"
        "Created spec/agentic.schema.json - JSON Schema"
        "Created spec/VERSION-POLICY.adoc"
        "Completed rsr-adapter.ads and rsr-adapter.adb"
        "Filled META.scm architecture-decisions with 5 ADRs"
        "Completed NEUROSYM.scm integration section"
        "Updated STATE.scm to reflect completion")))))
