;; SPDX-License-Identifier: PMPL-1.0-or-later
;; AGENTIC.scm - AI agent interaction patterns for 0-ai-gatekeeper-protocol
;; Last Updated: 2026-02-07

(agentic
  (version "1.0.0")
  (last-updated "2026-02-07T03:00:00Z")
  (media-type "application/vnd.agentic+scm")

  (agent-interaction-patterns
    (entry-protocol
      (mandatory-steps
        '("1. READ 0-AI-MANIFEST.a2ml FIRST (or AI.a2ml)"
          "2. Understand canonical locations (.machine_readable/, .bot_directives/)"
          "3. Understand critical invariants (no SCM duplication, etc.)"
          "4. If MCP-enabled: Call acknowledge_manifest with SHA-256 hash"
          "5. If manual: State canonical locations to prove understanding"
          "6. Read .machine_readable/STATE.scm for current project state"
          "7. Read .machine_readable/AGENTIC.scm (this file) for interaction rules"))

      (attestation-requirement
        (mcp-agents
          "MUST call acknowledge_manifest tool with correct SHA-256 hash of manifest content. No file access granted without valid attestation.")
        (non-mcp-agents
          "MUST state: 'I have read the AI Gatekeeper Protocol. SCM files are located in .machine_readable/ ONLY, bot directives in .bot_directives/, and I will not create duplicate files.' to demonstrate understanding.")))

    (lifecycle-hooks
      (on-enter
        (description "Actions to take when starting a new session")
        (required-actions
          '("Read 0-AI-MANIFEST.a2ml"
            "Acknowledge manifest (MCP) or state understanding (manual)"
            "Read .machine_readable/STATE.scm"
            "Read .machine_readable/AGENTIC.scm"
            "Check for blockers in STATE.scm"))
        (logging
          (format "Session started: [timestamp] [agent-id] [platform]")
          (location ".machine_readable/session-log.txt")
          (required false)
          (recommended true)))

      (on-exit
        (description "Actions to take when ending a session")
        (required-actions
          '("Update .machine_readable/STATE.scm if changes made"
            "Summarize work completed"
            "Document any new blockers discovered"
            "Update session-history in STATE.scm"))
        (logging
          (format "Session ended: [timestamp] [agent-id] [summary] [files-modified]")
          (location ".machine_readable/session-log.txt")
          (required false)
          (recommended true))))

    (operation-constraints
      (file-operations
        (read
          (allowed-after-acknowledgment true)
          (path-validation "Must not violate manifest invariants")
          (scm-files "MUST read from .machine_readable/ only, never root"))

        (write
          (allowed-after-acknowledgment true)
          (path-validation "MUST respect canonical locations")
          (scm-files "MUST write to .machine_readable/ only, never root")
          (manifest-updates "Require user approval before modifying AI.a2ml"))

        (create
          (allowed-after-acknowledgment true)
          (new-scm-files "FORBIDDEN - never create duplicate SCM files")
          (new-manifests "FORBIDDEN - one manifest per repo"))

        (delete
          (allowed-after-acknowledgment true)
          (scm-files "Only delete if in wrong location (moving to correct location)")
          (manifest "FORBIDDEN - never delete AI.a2ml")))

      (invariant-enforcement
        (no-scm-duplication
          (description "Never create STATE.scm, META.scm, ECOSYSTEM.scm, AGENTIC.scm, NEUROSYM.scm, PLAYBOOK.scm, or LANGUAGES.scm in root directory")
          (enforcement "MCP server blocks, FUSE wrapper blocks, CI/CD catches")
          (violation-response "Immediate block or PR failure"))

        (single-source-of-truth
          (description ".machine_readable/ is authoritative for all SCM files")
          (enforcement "Read operations redirected, write operations blocked")
          (symlinks-allowed true))

        (no-stale-metadata
          (description "If root SCMs exist, they are OUT OF DATE and must be deleted")
          (enforcement "CI/CD validation flags, bots auto-fix")
          (migration "Move to .machine_readable/, delete root copies"))))

    (communication-protocols
      (with-user
        (questions
          (when "When requirements unclear, approaches uncertain, or decisions needed")
          (how "Use AskUserQuestion or equivalent")
          (format "Concise, specific, with options when applicable"))

        (reporting
          (progress "Update STATE.scm with work completed")
          (blockers "Document blockers in STATE.scm immediately")
          (errors "Report errors clearly with context")
          (successes "Confirm completions to user")))

      (with-other-agents
        (session-state "Preserved in STATE.scm for next agent")
        (conventions "Follow same manifest, all agents see same source of truth")
        (conflicts "Manifest is authoritative, not agent memory")))

    (platform-specific-notes
      (claude
        (mcp-enforcement true)
        (tools-available '("acknowledge_manifest" "read_file" "list_directory" "get_manifest"))
        (session-management "Handled by mcp-repo-guardian server")
        (best-practice "Always use MCP server for enforcement"))

      (gemini
        (mcp-enforcement false)
        (enforcement-mechanism "FUSE wrapper (when available) or manual compliance")
        (best-practice "Read 0-AI-MANIFEST.a2ml first, state understanding explicitly"))

      (openai
        (mcp-enforcement false)
        (enforcement-mechanism "FUSE wrapper (when available) or manual compliance")
        (best-practice "Read 0-AI-MANIFEST.a2ml first, state understanding explicitly"))

      (github-copilot
        (mcp-enforcement false)
        (enforcement-mechanism "CI/CD validation catches violations post-commit")
        (best-practice "IDE could add manifest validation plugin"))

      (cursor
        (mcp-enforcement "potentially via MCP support")
        (enforcement-mechanism "Could integrate MCP server or FUSE wrapper")
        (best-practice "Check for MCP support, fallback to manual compliance"))))

  (code-generation-rules
    (general
      '("Never generate duplicate SCM files in root"
        "Always place SCM files in .machine_readable/"
        "Use PMPL-1.0-or-later license unless platform requires MPL-2.0"
        "Follow hyperpolymath RSR standards"
        "Include SPDX headers in all files"))

    (documentation
      '("Update STATE.scm when work completed"
        "Update ROADMAP.adoc if milestones change"
        "Keep README.adoc synchronized with actual state"
        "Document architectural decisions in META.scm"))

    (testing
      '("Add tests for new enforcement mechanisms"
        "Validate manifest format changes"
        "Test cross-platform compatibility"
        "Include integration tests"))

    (commits
      '("Use Conventional Commits format"
        "Include Co-Authored-By: Claude Sonnet 4.5 (or equivalent)"
        "Sign commits with GPG if available"
        "Reference issues/PRs when applicable")))

  (error-handling
    (manifest-not-found
      (response "STOP immediately. Request user create AI.a2ml or 0-AI-MANIFEST.a2ml")
      (severity "critical"))

    (attestation-failure
      (response "BLOCK all operations. Inform user hash mismatch detected.")
      (severity "critical"))

    (invariant-violation-detected
      (response "STOP operation. Report violation to user. Suggest fix.")
      (severity "high"))

    (session-expired
      (response "Re-acknowledge manifest. Create new session.")
      (severity "medium"))

    (unclear-requirements
      (response "Ask user for clarification. Do not guess or assume.")
      (severity "low")))

  (learning-and-adaptation
    (from-errors
      '("When blocked by invariant, understand WHY"
        "When attestation fails, verify manifest content"
        "When user corrects, update understanding"
        "When pattern emerges, adapt behavior"))

    (cross-session
      '("STATE.scm preserves context for future sessions"
        "AGENTIC.scm (this file) evolves with learnings"
        "Manifest format may evolve - always read current version"
        "User feedback improves protocol over time"))

    (cross-platform
      '("Share learnings via documentation updates"
        "Improve manifest format for clarity"
        "Enhance enforcement mechanisms based on gaps"
        "Build community knowledge base"))))
