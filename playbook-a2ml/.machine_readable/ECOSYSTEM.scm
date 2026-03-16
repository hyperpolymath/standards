;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for playbook-scm
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "playbook-scm")
  (type "specification")
  (purpose "Define executable plan format for .scm Format Family")

  (position-in-ecosystem
    (category "standards")
    (subcategory "scm-format-family")
    (tier "execution")
    (unique-value
      ("Derived authority from META"
       "Gated execution through AGENTIC"
       "Machine-readable runbooks")))

  (related-projects
    ((name "standards")
     (relationship "parent")
     (url "https://github.com/hyperpolymath/standards"))
    ((name "meta-scm")
     (relationship "constitutional-authority")
     (url "https://github.com/hyperpolymath/meta-scm"))
    ((name "agentic-scm")
     (relationship "sibling")
     (url "https://github.com/hyperpolymath/agentic-scm"))
    ((name "neurosym-scm")
     (relationship "sibling")
     (url "https://github.com/hyperpolymath/neurosym-scm"))
    ((name "anchor-scm")
     (relationship "intervention-layer")
     (url "https://github.com/hyperpolymath/anchor-scm")))

  (what-this-is
    ("Executable plan specification"
     "Operational runbook format"
     "Machine-readable deployment procedures"
     "Failure handling and rollback definitions"))

  (what-this-is-not
    ("Constitutional authority (that's META)"
     "Operational gating (that's AGENTIC)"
     "Symbolic semantics (that's NEUROSYM)"
     "Implementation code")))
