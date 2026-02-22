;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Position in ecosystem for 0-ai-gatekeeper-protocol
;; Last Updated: 2026-02-07

(ecosystem
  (version "1.0.0")
  (name "0-ai-gatekeeper-protocol")
  (type "specification-and-tooling")
  (purpose "Universal nucleation point ensuring AI agents read repository manifests before operations")
  (media-type "application/vnd.ecosystem+scm")

  (position-in-ecosystem
    (role "Foundation/Infrastructure")
    (scope "Universal - applies to ALL hyperpolymath repos and beyond")
    (maturity "stable-foundation")
    (criticality "high")
    (description
      "Serves as the nucleation point for AI agent interaction with repositories. Prevents context loss, duplicate files, and invariant violations across sessions and platforms."))

  (related-projects
    (sibling
      '((name "mcp-repo-guardian")
        (relationship "enforcement-component")
        (repo "hyperpolymath/mcp-repo-guardian")
        (description "MCP server providing hard enforcement for Claude and MCP-compatible agents")
        (dependency-type "sibling-implementation"))

      '((name "repo-guardian-fs")
        (relationship "enforcement-component")
        (repo "hyperpolymath/repo-guardian-fs")
        (status "planned")
        (description "FUSE wrapper providing universal enforcement for ANY AI agent/tool")
        (dependency-type "sibling-implementation"))

      '((name "rsr-template-repo")
        (relationship "integration-target")
        (repo "hyperpolymath/rsr-template-repo")
        (description "Repository template that will include AI.a2ml manifest by default")
        (dependency-type "downstream-consumer"))))

    (foundation
      '((name "Model Context Protocol (MCP)")
        (relationship "underlying-technology")
        (provider "Anthropic")
        (description "Protocol enabling communication between AI agents and external tools")
        (dependency-type "foundation-technology"))

      '((name "FUSE (Filesystem in Userspace)")
        (relationship "underlying-technology")
        (provider "Linux kernel")
        (description "Framework for implementing filesystem in userspace, used by repo-guardian-fs")
        (dependency-type "foundation-technology"))))

    (consumers
      '((name "nextgen-languages")
        (relationship "early-adopter")
        (repo "hyperpolymath/nextgen-languages")
        (description "First production repo to adopt AI.a2ml manifest with canonical locations")
        (adoption-date "2026-02-07"))

      '((name "git-seo")
        (relationship "planned-adopter")
        (repo "hyperpolymath/git-seo")
        (description "SEO tool repo planned to adopt gatekeeper protocol")
        (status "pending"))

      '((name "ochrance")
        (relationship "planned-adopter")
        (repo "hyperpolymath/ochrance")
        (description "Project foundations laid, will adopt protocol")
        (status "pending"))

      '((name "all-hyperpolymath-repos")
        (relationship "universal-adoption-target")
        (count "567+ repositories")
        (description "All hyperpolymath repos will eventually have AI.a2ml manifests")
        (status "planned"))))

    (inspiration
      '((name "Docker containerization")
        (relationship "conceptual-inspiration")
        (description "Similar concept: declare environment requirements explicitly")
        (similarity "Manifest declares expectations, enforcer ensures compliance"))

      '((name "Kubernetes admission controllers")
        (relationship "architectural-inspiration")
        (description "Intercept operations, validate against policy, allow/deny")
        (similarity "Gatekeeper intercepts file access, validates against manifest, blocks until acknowledged"))

      '((name "Pre-commit hooks")
        (relationship "pattern-inspiration")
        (description "Enforce standards before allowing operations")
        (similarity "Enforce manifest reading before allowing repo operations"))))

    (integration-points
      '((name "gitbot-fleet")
        (relationship "bot-ecosystem")
        (repos "rhodibot, echidnabot, sustainabot, glambot, seambot, finishbot")
        (description "Bot fleet will natively respect AI.a2ml manifests")
        (status "planned-integration"))

      '((name "robot-repo-automaton")
        (relationship "automation-framework")
        (repo "hyperpolymath/robot-repo-automaton")
        (description "Automated fix bot will validate manifest compliance")
        (status "planned-integration"))

      '((name "CI/CD workflows")
        (relationship "validation-layer")
        (description "GitHub Actions validate manifest existence and invariant compliance")
        (status "planned"))

      '((name "AI platforms")
        (relationship "target-platforms")
        (platforms "Claude, Gemini, OpenAI, Mistral, GitHub Copilot, Cursor, etc.")
        (description "Protocol designed to work across all AI platforms")
        (status "universal-target")))))

  (dependencies
    (runtime
      '((name "@modelcontextprotocol/sdk")
        (version "^0.5.0")
        (purpose "MCP server implementation")
        (repo "mcp-repo-guardian")))

    (development
      '((name "TypeScript")
        (version "^5.6.0")
        (purpose "Type-safe implementation of MCP server"))

      '((name "Node.js")
        (version ">=22.0.0")
        (purpose "Runtime for MCP server"))

      '((name "gh CLI")
        (purpose "GitHub operations, repo creation, starring"))))

    (documentation
      '((name "AsciiDoc")
        (purpose "Technical documentation format"))

      '((name "Markdown")
        (purpose "Quick start guides and rationale"))))

  (influence
    (influences-upstream
      '("Could influence MCP protocol evolution"
        "Could inspire native manifest support in Claude"
        "Could inspire similar systems in other AI platforms"
        "Could become industry standard for AI-repository interaction"))

    (influences-downstream
      '("All hyperpolymath repos will adopt AI.a2ml"
        "rsr-template-repo will include manifest by default"
        "Bot fleet will read manifests first"
        "CI/CD will validate manifest compliance"
        "Community repos may adopt format"))

    (influences-lateral
      '("Other organizations may adopt protocol"
        "AI platform vendors may integrate natively"
        "Standardization bodies may formalize spec"
        "Research papers may reference approach")))

  (community
    (target-audience
      '("Repository maintainers tired of AI mistakes"
        "AI agent developers wanting better context"
        "Platform vendors (Anthropic, Google, OpenAI)"
        "Bot developers building automation tools"
        "Users working across multiple AI platforms"))

    (adoption-strategy
      '("Demonstrate value with nextgen-languages"
        "Document rationale clearly"
        "Provide easy-to-use enforcement tools"
        "Make integration frictionless"
        "Show metrics (reduced errors, time saved)"
        "Build community momentum"))

    (contribution-areas
      '("Platform-specific integrations (Gemini, OpenAI, etc.)"
        "IDE plugins and extensions"
        "Validation tooling"
        "Migration guides"
        "Example manifests"
        "Documentation improvements"
        "Testing and feedback")))

  (metrics
    (success-indicators
      '("Adoption: 100+ repos with manifests by 2026-Q2"
        "Quality: <5% manifest violation rate"
        "Satisfaction: >90% user satisfaction"
        "Platform: Native support in 2+ AI platforms by 2026-Q4"
        "Community: 10+ external adopters by 2026-Q3"
        "Impact: Zero data corruption incidents"))

    (tracking
      '("GitHub stars/forks"
        "Issue/PR activity"
        "Adoption in hyperpolymath repos"
        "External adoption (other organizations)"
        "Platform integration progress"
        "Community feedback"))))
