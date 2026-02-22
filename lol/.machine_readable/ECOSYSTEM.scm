;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for lol
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "1000Langs")
  (type "application")
  (purpose "Super-parallel corpus crawler and analyzer for 1500+ languages from Bible translations")

  (position-in-ecosystem
    (category "nlp-tools")
    (subcategory "corpus-building")
    (unique-value
      ("Largest open parallel corpus crawler targeting 1500+ languages")
      ("4-language polyglot: ReScript + Elixir + Julia + Deno")
      ("Integrated quality verification via VeriSimDB pipeline")
      ("Statistical phylogenetic analysis of language distances")))

  (related-projects
    (verisimdb-data
      (type "integration-target")
      (relationship "exports-to")
      (description "Git-backed corpus quality scan storage, receives lol scan JSON"))
    (hypatia
      (type "downstream-consumer")
      (relationship "triggered-by")
      (description "Neurosymbolic CI/CD reads verisimdb-data, dispatches to gitbot-fleet"))
    (panic-attacker
      (type "sibling-tool")
      (relationship "parallel-scanner")
      (description "Code vulnerability scanner, lol adapts same scan format for corpus quality"))
    (cladistics-jl
      (type "inspiration")
      (relationship "code-reuse")
      (description "Julia phylogenetic clustering patterns reused in analysis/src/phylogenetic.jl"))
    (zeroprob-jl
      (type "inspiration")
      (relationship "concept-reuse")
      (description "Zero-probability estimation concepts used in analysis/src/zero_frequency.jl"))
    (polyglot-i18n
      (type "related-project")
      (relationship "complementary")
      (description "Language family tree visualization, shares Phylogenetics.jl patterns"))
    (axiology-jl
      (type "inspiration")
      (relationship "pattern-reuse")
      (description "Multi-criteria quality scoring framework adapted for corpus assessment"))
    (gitbot-fleet
      (type "downstream-consumer")
      (relationship "receives-from-hypatia")
      (description "Bot fleet acts on quality findings from VeriSimDB pipeline")))

  (what-this-is
    ("A multilingual parallel corpus builder from Bible translation sources")
    ("An orchestrated crawler system using BEAM for massive concurrency")
    ("A statistical NLP analysis pipeline with Julia computation")
    ("A VeriSimDB-integrated quality verification system"))

  (what-this-is-not
    ("Not a Bible study tool or religious application")
    ("Not a machine translation system")
    ("Not a web scraping framework")))
