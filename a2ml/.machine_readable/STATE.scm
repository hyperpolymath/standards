;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Project state for a2ml

(state
  (metadata
    (version "1.0.0")  ; Spec frozen, infrastructure complete
    (schema-version "1.0")
    (created "2026-01-26")
    (updated "2026-01-30")
    (project "a2ml")
    (repo "hyperpolymath/a2ml"))

  (project-context
    (name "A2ML")
    (tagline "Attested Markup Language with Formal Verification")
    (tech-stack ("idris2" "rescript" "deno"))
    (status "pre-release")  ; Ready for v1.0, pending arXiv/IANA submission)

  (current-position
    (phase "pre-release")  ; Infrastructure complete, documentation pending
    (overall-completion 80)  ; Core + infrastructure done, docs/submission pending
    (working-features
      ("v1.0.0 specification frozen (SPEC-v1.0.adoc)"
       "Idris2 parser with decidable proofs (UniqueIds, RefsResolve, HasAbstract)"
       "Proof obligations validated at compile time"
       "Compiles to JavaScript (45KB) via codegen node"
       "ReScript v12 compatibility layer (Compat.res)"
       "ReScript parser and renderer (Module 0)"
       "CLI tools (validate, render, ast, dump)"
       "8 test vectors - ALL PASSING"
       "Git tags: v0.1.0, v0.5.0, v0.6.0"
       "Production CI/CD: Idris2 tests, ReScript tests, fuzzing"
       "Security: Hypatia scan, CodeQL, TruffleHog, fuzzing"
       "OpenSSF Scorecard monitored"
       ".well-known/ directory (security.txt, ai.txt, humans.txt)"
       "Multi-forge mirroring (GitLab, Bitbucket, Codeberg, etc.)"
       "RSR compliant workflows (SHA-pinned, SPDX headers, permissions)"))
    (needs-work
      ("ReScript bindings to Idris2 JS output"
       "Integrate Idris2 parser into web demo"
       "WASM compilation (deferred to v1.1)"
       "Web component for client-side rendering"
       "Performance benchmarks vs other parsers")))

  (route-to-v1
    (milestone-1
      (name "arXiv Submission")
      (status "in-progress")
      (items
        ("Write academic paper (cs.PL category)"
         "Document formal verification approach"
         "Include proof obligation examples"
         "Benchmark against Markdown/AsciiDoc/Djot"
         "Submit to arXiv")))
    (milestone-2
      (name "IANA Media Type Registration")
      (status "not-started")
      (items
        ("Complete RFC 6838 registration template"
         "text/a2ml media type specification"
         "Security considerations documented"
         "Interoperability section with parser implementations"
         "Contact information and references"
         "Submit to IANA")))
    (milestone-3
      (name "Documentation Website")
      (status "not-started")
      (items
        ("Live parser demo (Idris2 JS + ReScript GUI)"
         "Interactive proof obligation explorer"
         "Grammar reference with examples"
         "Migration guide from Markdown/AsciiDoc"
         "API documentation for parser implementations"
         "Deploy to GitHub Pages or custom domain")))
    (milestone-4
      (name "Package Distribution")
      (status "not-started")
      (items
        ("Deno package (primary distribution)"
         "npm package (compatibility)"
         "Standalone CLI binaries (Idris2 codegen)"
         "Docker image for CI/CD integration"
         "Installation instructions"))))

  (blockers-and-issues
    (critical
      ())
    (high-priority
      ("Academic paper not written (arXiv submission blocked)"
       "IANA registration document incomplete"))
    (medium-priority
      ("Documentation website not started"
       "No performance benchmarks"))
    (low-priority
      ("Web component not implemented"
       "WASM deferred to v1.1")))

  (critical-next-actions
    (immediate
      ("Start arXiv paper outline (cs.PL category)"
       "Draft IANA registration template sections"))
    (this-week
      ("Complete arXiv paper first draft"
       "Complete IANA registration document"
       "Run performance benchmarks (Markdown/AsciiDoc/Djot comparison)"))
    (this-month
      ("Submit arXiv paper"
       "Submit IANA registration"
       "Build documentation website with live demo"
       "Package for Deno distribution")))

  (session-history
    (session-2026-01-30
      (accomplishments
        ("CI/CD infrastructure complete (Idris2 tests, ReScript tests, fuzzing)"
         ".well-known directory created (security.txt, ai.txt, humans.txt)"
         "Removed irrelevant Jekyll workflows"
         "All workflows SHA-pinned and RSR compliant"
         "STATE.scm updated to v1.0.0 pre-release status")))))
