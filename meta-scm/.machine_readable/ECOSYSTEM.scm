;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm - Ecosystem position for meta-scm
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "meta-scm")
  (type "specification")
  (purpose "Define META.scm, STATE.scm, and ECOSYSTEM.scm formats for software project metadata")

  (position-in-ecosystem
    (category "SCM Format Family")
    (subcategory "Primary Tier")
    (unique-value
      ("Defines 3 of 7 SCM formats"
       "Architecture Decision Records in machine-readable format"
       "Project state persistence for AI assistants"
       "Ecosystem relationship mapping")))

  (related-projects
    ((name "standards")
     (url "https://github.com/hyperpolymath/standards")
     (relationship "hub")
     (description "Organization standards hub - registers all satellites"))

    ((name "playbook-scm")
     (url "https://github.com/hyperpolymath/playbook-scm")
     (relationship "sibling-execution")
     (description "PLAYBOOK.scm specification - executable plans derived from META"))

    ((name "agentic-scm")
     (url "https://github.com/hyperpolymath/agentic-scm")
     (relationship "sibling-execution")
     (description "AGENTIC.scm specification - AI agent operational gating"))

    ((name "neurosym-scm")
     (url "https://github.com/hyperpolymath/neurosym-scm")
     (relationship "sibling-execution")
     (description "NEUROSYM.scm specification - symbolic semantics and proofs"))

    ((name "anchor.scm")
     (url "https://github.com/hyperpolymath/anchor.scm")
     (relationship "sibling-intervention")
     (description "ANCHOR.scm specification - project recalibration interventions")))

  (what-this-is
    ("Primary specification repo for META.scm format"
     "Defines companion formats STATE.scm and ECOSYSTEM.scm"
     "Provides ABNF grammar and JSON Schema"
     "Documents ADR structure and development practices format"
     "Parent specification for execution-tier formats"))

  (what-this-is-not
    ("Not a tooling implementation"
     "Not a validator or parser"
     "Not the standards hub (that is hyperpolymath/standards)")))
