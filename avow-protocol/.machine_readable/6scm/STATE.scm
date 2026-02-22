;; SPDX-License-Identifier: AGPL-3.0-or-later
;; STATE.scm - Project state tracking for stamp-protocol

(define state
  (metadata
    (version "1.0.0")
    (schema-version "1.0")
    (created "2026-01-30")
    (updated "2026-01-30")
    (project "stamp-protocol")
    (repo "https://github.com/hyperpolymath/stamp-protocol"))

  (project-context
    (name "STAMP Protocol Website")
    (tagline "Interactive demonstration of STAMP consent protocol")
    (tech-stack
      (languages "ReScript" "JavaScript" "HTML" "CSS")
      (runtime "Deno")
      (libraries "proven" "rescript-tea" "cadre-tea-router")
      (verified "ProvenSafeUrl for unsubscribe link validation")))

  (current-position
    (phase "MVP Development")
    (overall-completion 65)
    (components
      (component "ReScript compilation" "complete" 100)
      (component "Deno integration" "complete" 100)
      (component "proven URL validation" "complete" 100)
      (component "Security hardening" "complete" 100
        "HTTPS, CSP, .well-known/, DNS CAA/SPF/DMARC")
      (component "StampApp.res logic" "in-progress" 70
        "Model, update, and render functions implemented")
      (component "DOM integration" "pending" 20
        "TEA mounting and event handlers needed")
      (component "Visual design" "pending" 30
        "Basic styles present, interactive UI needed")
      (component "API integration" "pending" 0
        "Backend consent API not yet connected"))
    (working-features
      "ReScript builds successfully"
      "Proven URL validation functional"
      "Basic TEA architecture in place"
      "Security headers configured"))

  (route-to-mvp
    (milestone "Complete TEA Integration" "in-progress"
      (items
        "Mount StampApp to DOM via Tea.App.standardProgram"
        "Wire up NextStep button event handlers"
        "Implement URL input field with validation"
        "Add visual feedback for validation state"))
    (milestone "Visual Polish" "pending"
      (items
        "Design consent flow diagram"
        "Add step indicators (1/4, 2/4, etc.)"
        "Style validation feedback (✓/✗)"
        "Mobile-responsive layout"))
    (milestone "Public Launch" "pending"
      (items
        "Deploy to Cloudflare Pages"
        "Configure stamp-protocol.org domain"
        "Add analytics (Cloudflare Web Analytics)"
        "SEO optimization (meta tags, sitemap)")))

  (blockers-and-issues
    (critical)  ; None
    (high
      (issue "DOM mounting not yet implemented"
        "StampApp.res has model/update/render but no DOM integration"))
    (medium
      (issue "Visual design incomplete"
        "Needs consent flow diagram and step indicators"))
    (low
      (issue "No backend API yet"
        "Currently client-side only demo")))

  (critical-next-actions
    (immediate
      "Wire StampApp to DOM via Tea.App.standardProgram"
      "Test interactive URL validation in browser")
    (this-week
      "Complete visual consent flow UI"
      "Deploy to Cloudflare Pages staging")
    (this-month
      "Launch public site at stamp-protocol.org"
      "Add API integration for real consent tracking"))

  (session-history
    (snapshot "2026-01-30" "Security hardening + proven integration"
      (accomplishments
        "Applied comprehensive security (HTTPS, CSP, .well-known/)"
        "Integrated proven library for formally verified URL validation"
        "Created StampApp.res with TEA architecture"
        "Configured Deno for ReScript compilation"
        "Updated README with current architecture"))))

;; Helper functions
(define (get-completion-percentage)
  65)

(define (get-blockers level)
  (case level
    ((critical) '())
    ((high) '("DOM mounting not yet implemented"))
    ((medium) '("Visual design incomplete"))
    ((low) '("No backend API yet"))))

(define (get-milestone name)
  (case name
    (("Complete TEA Integration") '((status . "in-progress") (items . 4)))
    (("Visual Polish") '((status . "pending") (items . 4)))
    (("Public Launch") '((status . "pending") (items . 4)))))
