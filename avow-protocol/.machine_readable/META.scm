;; SPDX-License-Identifier: AGPL-3.0-or-later
;; META.scm - Meta-level information for stamp-protocol

(define meta
  (architecture-decisions
    (adr "adr-001"
      (status "accepted")
      (date "2026-01-30")
      (context
        "Need to choose between full-stack framework (Next.js, Nuxt) vs. minimal"
        "client-side ReScript application for STAMP demo site.")
      (decision
        "Use minimal ReScript TEA application without heavy framework."
        "Rationale:"
        "- STAMP demo is inherently client-side (no backend needed yet)"
        "- ReScript provides type safety with small, predictable output"
        "- TEA architecture makes state predictable and testable"
        "- Deno provides a clean, permissioned runtime"
        "- Minimal bundle size for fast loading")
      (consequences
        "Positive: Fast, type-safe, minimal dependencies, easy to understand"
        "Negative: Manual DOM integration needed (no framework magic)"
        "Mitigation: Tea.App provides standard mounting pattern"))

    (adr "adr-002"
      (status "accepted")
      (date "2026-01-30")
      (context
        "Unsubscribe links are security-critical - malformed URLs could lead to XSS"
        "or bypass consent mechanisms.")
      (decision
        "Use ProvenSafeUrl from proven library for all URL validation."
        "This provides mathematical proofs that URLs are well-formed.")
      (consequences
        "Positive: Impossible to have URL parsing bugs at runtime"
        "Positive: Formally verified security properties"
        "Negative: Adds proven library dependency (acceptable trade-off)"
        "Negative: Learning curve for dependent types (mitigated by bindings)"))

    (adr "adr-003"
      (status "accepted")
      (date "2026-01-30")
      (context
        "Need to enforce security best practices for web deployment.")
      (decision
        "Implement comprehensive security hardening:"
        "- HTTPS-only (no HTTP fallback)"
        "- Content Security Policy headers"
        "- .well-known/security.txt (RFC 9116)"
        "- DNS CAA records for certificate issuance"
        "- SPF/DMARC for email security (when STAMP uses email)")
      (consequences
        "Positive: Industry-standard security posture"
        "Positive: Demonstrates STAMP Protocol takes security seriously"
        "Negative: Slightly more complex deployment (acceptable)")))

  (development-practices
    (code-style
      "Follow ReScript conventions:"
      "- PascalCase for types and modules"
      "- camelCase for values and functions"
      "- Explicit type annotations for public functions"
      "- Pattern matching over if/else chains"
      ""
      "File naming:"
      "- *App.res for TEA applications"
      "- Proven*.res for proven library bindings"
      "- Tea.res for TEA runtime")

    (security
      "All URL handling must use ProvenSafeUrl (never raw string parsing)"
      "All user input must be validated before processing"
      "HTTPS-only in production (proven validates isHttps)"
      "CSP headers prevent inline scripts"
      "Regular security audits via Hypatia CI/CD")

    (testing
      "Unit tests for StampApp state transitions (pending)"
      "Integration tests for DOM mounting (pending)"
      "Property-based tests for URL validation (provided by proven)")

    (versioning
      "Semantic versioning (MAJOR.MINOR.PATCH)"
      "Major: Breaking changes to public API"
      "Minor: New features (backward compatible)"
      "Patch: Bug fixes only")

    (documentation
      "README.md - User-facing setup and usage"
      ".machine_readable/STATE.scm - Current project state and roadmap"
      ".machine_readable/ECOSYSTEM.scm - Relationships to other projects"
      ".machine_readable/META.scm - This file - architecture and philosophy"
      "Code comments - Why, not what (code explains what)")

    (branching
      "main - production-ready code"
      "feature/* - new features"
      "fix/* - bug fixes"
      "PR required for main (branch protection enabled)"))

  (design-rationale
    (why-rescript
      "Type safety without TypeScript ceremony"
      "Excellent JavaScript interop for proven integration"
      "Fast compilation, small bundles"
      "Strong pattern matching for state machines")

    (why-deno
      "Permissioned runtime model for safer tooling"
      "Simple task runner and dependencies"
      "Modern URL imports instead of node_modules")

    (why-proven
      "STAMP Protocol claims mathematical consent proofs"
      "Must walk the walk - use formal verification ourselves"
      "ProvenSafeUrl demonstrates feasibility of verified web apps"
      "Educational value - show developers how formal methods work")

    (why-tea
      "Elm Architecture is proven pattern for web UIs"
      "Predictable state updates (model -> msg -> update -> model)"
      "Testable without mocking (pure functions)"
      "Scales from toy demos to production apps")

    (why-static-site
      "No backend needed for demonstration"
      "Fast CDN delivery (Cloudflare Pages)"
      "Minimal attack surface (no server-side code)"
      "Easy to replicate and study")))
