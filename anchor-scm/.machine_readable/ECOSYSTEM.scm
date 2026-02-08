;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for anchor.scm
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "anchor.scm")
  (type "specification")
  (purpose "Define project recalibration and realignment format")

  (position-in-ecosystem
    (category "standards")
    (subcategory "scm-format-family")
    (tier "intervention")
    (unique-value
      ("Superintendent authority declarations"
       "Scope arrest directives"
       "SCM file review guidance"
       "Verification criteria")))

  (related-projects
    ((name "standards")
     (relationship "parent")
     (url "https://github.com/hyperpolymath/standards"))
    ((name "meta-scm")
     (relationship "target-of-intervention")
     (url "https://github.com/hyperpolymath/meta-scm"))
    ((name "playbook-scm")
     (relationship "sibling")
     (url "https://github.com/hyperpolymath/playbook-scm"))
    ((name "agentic-scm")
     (relationship "sibling")
     (url "https://github.com/hyperpolymath/agentic-scm"))
    ((name "neurosym-scm")
     (relationship "sibling")
     (url "https://github.com/hyperpolymath/neurosym-scm")))

  (what-this-is
    ("Intervention layer specification"
     "Project recalibration format"
     "Scope arrest directives"
     "AI agent realignment tool"
     "Superintendent authority declarations"))

  (what-this-is-not
    ("Constitutional authority (that's META)"
     "Operational gating (that's AGENTIC)"
     "Executable plans (that's PLAYBOOK)"
     "Symbolic semantics (that's NEUROSYM)"
     "Implementation code")))
