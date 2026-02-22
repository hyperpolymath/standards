;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Architecture decisions and governance for 0-ai-gatekeeper-protocol
;; Last Updated: 2026-02-07

(meta
  (version "1.0.0")
  (last-updated "2026-02-07T03:00:00Z")
  (media-type "application/meta+scheme")

  (architecture-decisions
    (adr-001
      (title "Use Plain Text Manifest Format")
      (status "accepted")
      (date "2026-02-07")
      (context
        "Need universal format readable by ANY AI agent (Claude, Gemini, OpenAI, Mistral, etc.)")
      (decision
        "Use plain text .a2ml format with markdown-style headings. No JSON/YAML to avoid parsing complexity.")
      (consequences
        "✅ Universal readability"
        "✅ Human-readable without tools"
        "✅ Easy to extend"
        "❌ No built-in schema validation"))

    (adr-002
      (title "Attestation via SHA-256 Hash")
      (status "accepted")
      (date "2026-02-07")
      (context
        "Need proof that AI actually read manifest, not just claimed to")
      (decision
        "Require SHA-256 hash of manifest content as attestation. Agent must compute hash to prove reading.")
      (consequences
        "✅ Cryptographic proof of reading"
        "✅ Detects stale/modified manifests"
        "✅ Prevents lazy skimming"
        "❌ Requires hash computation capability"))

    (adr-003
      (title "MCP as Primary Enforcement Mechanism")
      (status "accepted")
      (date "2026-02-07")
      (context
        "Need hard enforcement that blocks file access until acknowledgment")
      (decision
        "Build MCP server (mcp-repo-guardian) as primary enforcement. Works immediately with Claude.")
      (consequences
        "✅ Hard enforcement for Claude users"
        "✅ Session-based access control"
        "✅ Proven technology (MCP)"
        "❌ Claude-specific initially"
        "→ Mitigate with FUSE wrapper for universality"))

    (adr-004
      (title "0-Prefix for Alphabetical Sorting")
      (status "accepted")
      (date "2026-02-07")
      (context
        "Manifest must be first file AI agents see when browsing repo")
      (decision
        "Use 0-AI-MANIFEST.a2ml or !AI.a2ml to sort before README.md alphabetically")
      (consequences
        "✅ Maximum visibility"
        "✅ Hard to miss"
        "✅ Clear priority"
        "❌ Unconventional naming"))

    (adr-005
      (title "Canonical Location Enforcement")
      (status "accepted")
      (date "2026-02-07")
      (context
        "Root cause of user frustration: duplicate SCM files in wrong locations")
      (decision
        "Manifest MUST declare canonical locations. MCP/FUSE enforce these. CI/CD validates.")
      (consequences
        "✅ Prevents duplicate file errors"
        "✅ Single source of truth"
        "✅ Mechanically enforced"
        "✅ Reduces user frustration"))

    (adr-006
      (title "Session-Based Access Control")
      (status "accepted")
      (date "2026-02-07")
      (context
        "Need to track which agents have acknowledged manifest")
      (decision
        "MCP server creates session after acknowledgment. All operations require valid session.")
      (consequences
        "✅ Stateful tracking"
        "✅ Session timeout for security"
        "✅ Per-agent isolation"
        "❌ Requires session management"))

    (adr-007
      (title "FUSE Wrapper for Universal Enforcement")
      (status "accepted")
      (date "2026-02-07")
      (context
        "MCP only works with compatible platforms. Need universal solution.")
      (decision
        "Build repo-guardian-fs FUSE wrapper. Mount repos through virtual FS that enforces protocol.")
      (consequences
        "✅ Works with ANY AI agent/tool"
        "✅ OS-level enforcement"
        "✅ Universal coverage"
        "❌ More complex implementation"
        "❌ Requires FUSE privileges")))

  (development-practices
    (licensing
      (primary "PMPL-1.0-or-later")
      (rationale "Hyperpolymath standard license")
      (fallback "MPL-2.0 where required by platform"))

    (versioning
      (scheme "semantic-versioning")
      (current "1.0.0")
      (stability "stable"))

    (documentation
      (style "RFC-like for specifications")
      (format "AsciiDoc for technical docs, Markdown for guides")
      (requirement "All features must be documented before release"))

    (testing
      (unit-tests "Required for all code")
      (integration-tests "Required for MCP server and FUSE wrapper")
      (validation "Manifest validator for format checking"))

    (git-workflow
      (branching "main branch for stable releases")
      (commits "Conventional Commits format")
      (sign-off "Co-Authored-By: Claude Sonnet 4.5"))

    (code-review
      (required "All PRs require review")
      (automation "CI/CD must pass")
      (standards "Follow hyperpolymath RSR")))

  (design-rationale
    (problem-statement
      "AI agents lose context between sessions and across platforms, leading to repeated mistakes and user frustration.")

    (solution-approach
      "Universal manifest system with mechanical enforcement ensures agents read repository structure first.")

    (key-principles
      '("Platform-agnostic - works with any AI"
        "Mechanically enforced - not just documentation"
        "Attestation-based - cryptographic proof of reading"
        "Defense-in-depth - MCP + FUSE + CI/CD"
        "Fail-safe - blocks by default, allows on proof"))

    (non-goals
      '("Not a replacement for good documentation"
        "Not a security mechanism (public repos)"
        "Not a DRM system"
        "Not AI-specific magic - works because of structure"))

    (success-criteria
      '("Reduced duplicate file creation"
        "Reduced user frustration"
        "Adoption across platforms"
        "Community contributions"
        "Standardization discussions")))

  (governance
    (maintainer "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>")
    (organization "hyperpolymath")
    (license-governance "PMPL-1.0-or-later with MPL-2.0 fallback")
    (contribution-model "Open source, PR-based")
    (decision-making "Benevolent dictator (maintainer) with community input")
    (roadmap-visibility "Public via ROADMAP.adoc")
    (communication-channels
      '("GitHub Issues"
        "GitHub Discussions"
        "Documentation"))))
