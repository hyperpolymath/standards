;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Project state for lol
;; Media-Type: application/vnd.state+scm

(state
  (metadata
    (version "0.1.0")
    (schema-version "1.0")
    (created "2026-01-03")
    (updated "2026-02-08")
    (project "lol")
    (repo "github.com/hyperpolymath/lol"))

  (project-context
    (name "1000Langs")
    (tagline "Super-parallel corpus crawler for 1500+ languages")
    (tech-stack
      ("ReScript" "Primary application code, type-safe crawlers")
      ("Deno" "Runtime, replaces Node.js")
      ("Elixir" "BEAM orchestrator for horizontal scaling")
      ("Julia" "Statistical analysis, phylogenetic clustering")))

  (current-position
    (phase "active-development")
    (overall-completion 30)
    (components
      (verisimdb "complete" "VeriSimDB corpus quality verification types, analyzer, export")
      (deno-runtime "complete" "Switched from Node.js to Deno with proper deno.json")
      (http-module "complete" "Deno fetch bindings with rate limiting and retry")
      (crawler-biblecloud "complete" "API-based crawler with JSON parsing")
      (crawler-biblecom "complete" "HTML scraping crawler with CSS selectors")
      (crawler-pngscriptures "complete" "ZIP/USFM parser for PNG translations")
      (crawler-ebible "complete" "Bulk download crawler for ebible.org")
      (crawler-findbible "complete" "API+HTML hybrid crawler for find.bible")
      (crawler-digitalbible "complete" "Full Digital Bible Platform API wrapper")
      (cli "complete" "CLI dispatch for crawl, verify, list-sources commands")
      (elixir-orchestrator "complete" "BEAM-based parallel crawl orchestration")
      (julia-analysis "complete" "Statistical analysis, distances, phylogenetics, quality"))
    (working-features
      ("CLI interface with Deno runtime")
      ("6 crawler implementations with rate limiting")
      ("VeriSimDB quality analysis pipeline")
      ("Elixir DynamicSupervisor for parallel crawling")
      ("Julia JSON-over-stdio bridge for Elixir")
      ("Frequency analysis and Zipf coefficient")
      ("Language distance matrices (JSD, cosine, Jaccard)")
      ("Phylogenetic tree construction (UPGMA, NJ, Ward)")
      ("Zero-frequency smoothing (Good-Turing, Kneser-Ney)")
      ("Corpus quality scoring and weak point detection")))

  (route-to-mvp
    (milestones
      (m1 "Integration testing" "End-to-end test: crawl -> analyze -> verify" "next")
      (m2 "API keys" "Configure and test real API credentials" "planned")
      (m3 "First 100 languages" "Crawl 100+ languages from bible.cloud + ebible" "planned")
      (m4 "Distance matrix" "Compute pairwise distances for 100+ languages" "planned")
      (m5 "VeriSimDB pipeline" "Full automated verify -> ingest -> hypatia flow" "planned")))

  (blockers-and-issues
    (critical)
    (high
      ("API keys needed for bible.cloud and Digital Bible Platform"))
    (medium
      ("HTML parsing for BibleCom may need linkedom or DOMParser polyfill")
      ("Julia Package UUIDs in Project.toml need verification"))
    (low
      ("OpenCyc integration module still stub")))

  (critical-next-actions
    (immediate
      ("Run deno install to verify ReScript builds with Deno")
      ("Configure API keys for bible.cloud"))
    (this-week
      ("Integration test: crawl eng from bible.cloud end-to-end")
      ("Run Julia tests to verify analysis modules"))
    (this-month
      ("Crawl first 100 languages")
      ("Generate first language distance matrix")
      ("Connect to verisimdb-data pipeline")))

  (session-history
    ("2026-02-08: Major polyglot integration - VeriSimDB, Deno, Elixir, Julia")))
