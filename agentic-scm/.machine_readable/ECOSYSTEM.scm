;; SPDX-License-Identifier: MPL-2.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for agentic-scm
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "agentic-scm")
  (type "specification")
  (purpose "Define operational gating format for AI agent safety")

  (position-in-ecosystem
    (category "standards")
    (subcategory "scm-format-family")
    (tier "execution")
    (unique-value
      ("Entropy budget tracking"
       "Risk threshold classification"
       "Explicit intent verification"
       "Decision audit trails")))

  (related-projects
    ((name "standards")
     (relationship "parent")
     (url "https://github.com/hyperpolymath/standards"))
    ((name "meta-scm")
     (relationship "constitutional-authority")
     (url "https://github.com/hyperpolymath/meta-scm"))
    ((name "playbook-scm")
     (relationship "sibling")
     (url "https://github.com/hyperpolymath/playbook-scm"))
    ((name "neurosym-scm")
     (relationship "sibling")
     (url "https://github.com/hyperpolymath/neurosym-scm"))
    ((name "anchor-scm")
     (relationship "intervention-layer")
     (url "https://github.com/hyperpolymath/anchor-scm")))

  (what-this-is
    ("Operational gating specification"
     "AI agent safety controls"
     "Entropy budget tracking system"
     "Risk classification framework"
     "Confirmation requirement definitions"
     "Decision audit trail format"))

  (what-this-is-not
    ("Constitutional authority (that's META)"
     "Executable plans (that's PLAYBOOK)"
     "Symbolic semantics (that's NEUROSYM)"
     "Implementation code")))
