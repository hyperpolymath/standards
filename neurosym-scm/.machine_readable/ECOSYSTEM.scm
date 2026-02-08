;; SPDX-License-Identifier: MPL-2.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for neurosym-scm
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "neurosym-scm")
  (type "specification")
  (purpose "Define symbolic semantics and proof obligations")

  (position-in-ecosystem
    (category "standards")
    (subcategory "scm-format-family")
    (tier "execution")
    (unique-value
      ("Symbolic semantics definitions"
       "Composition rule algebra"
       "Proof obligation interfaces"
       "Type system specifications")))

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
    ((name "agentic-scm")
     (relationship "sibling")
     (url "https://github.com/hyperpolymath/agentic-scm"))
    ((name "anchor-scm")
     (relationship "intervention-layer")
     (url "https://github.com/hyperpolymath/anchor-scm")))

  (what-this-is
    ("Symbolic semantics specification"
     "Operation meaning definitions"
     "Composition rule algebra"
     "Proof obligation interfaces"
     "Type system for operations"))

  (what-this-is-not
    ("Constitutional authority (that's META)"
     "Operational gating (that's AGENTIC)"
     "Executable plans (that's PLAYBOOK)"
     "Implementation code")))
