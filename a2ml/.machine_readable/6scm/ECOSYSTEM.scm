;; SPDX-License-Identifier: AGPL-3.0-or-later
;; ECOSYSTEM.scm - Ecosystem relationships for a2ml
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0.0")
  (name "a2ml")
  (type "library")  ;; or: application, tool, specification, template
  (purpose "Hyperpolymath ecosystem component")

  (position-in-ecosystem
    "Part of the hyperpolymath ecosystem of 500+ repositories "
    "following Rhodium Standard Repository (RSR) conventions.")

  (related-projects
    (inspiration "Djot" "Lightweight markup inspiration")
    (inspiration "AsciiDoc" "Structured document format")
    (sibling-standard "rsr-template-repo" "Repository standards")
    (potential-consumer "stateful-artefacts-for-gitforges" "Event-chain workflows")
    (potential-consumer "ddraig-ssg" "Static site generation")
    (dependency "hypatia" "Security scanning")
    (consumer "gitbot-fleet" "Quality enforcement"))

  (what-this-is
    "A2ML (Attested Markup Language) is a typed markup format with "
    "formal verification. It combines lightweight authoring (Djot-like surface) "
    "with strong guarantees (Idris2 dependent types and proof obligations). "
    "Supports progressive strictness: lax → checked → attested modes. "
    "Reference implementation includes parser with decidable proofs, "
    "compiling to JavaScript for web/CLI use. "
    "Designed for documents requiring structural invariants: "
    "unique IDs, resolved references, required sections, opaque payload fidelity.")

  (what-this-is-not
    "A2ML is not a general-purpose Markdown replacement. "
    "It is not designed for casual note-taking or simple formatting. "
    "It does not compete with reStructuredText for Python documentation. "
    "It is not a runtime execution environment - documents are static data. "
    "It is not tied to a single rendering format (HTML/PDF/LaTeX agnostic)."))
