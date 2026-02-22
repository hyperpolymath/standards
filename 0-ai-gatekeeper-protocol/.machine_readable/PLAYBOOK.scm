;; SPDX-License-Identifier: PMPL-1.0-or-later
;; PLAYBOOK.scm - Operational runbook for 0-ai-gatekeeper-protocol
;; Last Updated: 2026-02-07

(playbook
  (version "1.0.0")
  (last-updated "2026-02-07T03:00:00Z")
  (media-type "application/vnd.playbook+scm")

  (operational-procedures
    (daily-operations
      (monitoring
        (what-to-check
          '("GitHub Issues for bug reports"
            "Discussions for questions/feedback"
            "PRs for community contributions"
            "CI/CD status on both repos"
            "Adoption metrics (stars, forks, usage)"))
        (frequency "daily")
        (owner "maintainer"))

      (maintenance
        (what-to-maintain
          '("Update dependencies (npm, TypeScript)"
            "Address security vulnerabilities"
            "Review and merge PRs"
            "Update documentation as needed"))
        (frequency "weekly")
        (owner "maintainer")))

    (release-process
      (version-bump
        (steps
          '("1. Update version in package.json"
            "2. Update version in all SCM files"
            "3. Update CHANGELOG.md with changes"
            "4. Update ROADMAP.adoc status"
            "5. Commit: 'chore: bump version to X.Y.Z'"
            "6. Tag: git tag vX.Y.Z"
            "7. Push: git push && git push --tags")))

      (publishing
        (mcp-repo-guardian
          '("Build: npm run build"
            "Test: npm test"
            "Publish: npm publish"
            "Announce: GitHub Release + discussion"))

        (documentation
          '("Update 0-ai-gatekeeper-protocol README"
            "Publish blog post (if major release)"
            "Update integration guides"
            "Notify community via discussions"))))

    (incident-response
      (manifest-format-bug
        (severity "critical")
        (steps
          '("1. Assess impact (how many repos affected)"
            "2. Create hotfix branch"
            "3. Fix format issue"
            "4. Test with nextgen-languages"
            "5. Emergency release"
            "6. Notify affected users"
            "7. Update documentation"))
        (owner "maintainer")
        (sla "24-hours"))

      (mcp-server-crash
        (severity "high")
        (steps
          '("1. Check error logs"
            "2. Reproduce issue"
            "3. Identify root cause"
            "4. Implement fix"
            "5. Add regression test"
            "6. Release patch version"
            "7. Document in CHANGELOG"))
        (owner "maintainer")
        (sla "48-hours"))

      (documentation-outdated
        (severity "medium")
        (steps
          '("1. Identify outdated sections"
            "2. Update with current information"
            "3. Review for consistency"
            "4. Commit and push"
            "5. Close related issues"))
        (owner "maintainer-or-contributor")
        (sla "1-week"))))

  (runbooks
    (adding-new-repo-to-protocol
      (description "Steps to add AI.a2ml manifest to a new repo")
      (steps
        '("1. Copy examples/0-AI-MANIFEST.a2ml to repo root"
          "2. Customize canonical locations"
          "3. List repository-specific invariants"
          "4. Document repo structure"
          "5. Compute and add manifest hash to README"
          "6. Add CI/CD validation workflow"
          "7. Test with MCP server (if applicable)"
          "8. Commit and push"
          "9. Update adoption metrics"))
      (time-estimate "30-minutes")
      (automation-potential "high"))

    (updating-manifest-format
      (description "Steps to evolve the AI.a2ml format")
      (steps
        '("1. Document proposed changes in GitHub Issue"
          "2. Gather community feedback"
          "3. Update AI-MANIFEST-SPEC.adoc"
          "4. Update examples/0-AI-MANIFEST.a2ml template"
          "5. Update manifest parser in mcp-repo-guardian"
          "6. Add validation tests"
          "7. Update all existing manifests"
          "8. Announce breaking change (if applicable)"
          "9. Release new version"))
      (time-estimate "1-week")
      (requires-approval true))

    (deploying-mcp-server
      (description "Steps for users to deploy mcp-repo-guardian")
      (steps
        '("1. Clone mcp-repo-guardian repo"
          "2. npm install"
          "3. npm run build"
          "4. Add to ~/.claude/config.json"
          "5. Set REPOS_PATH environment variable"
          "6. Restart Claude Code"
          "7. Test with acknowledge_manifest"
          "8. Verify enforcement working"))
      (time-estimate "15-minutes")
      (automation-potential "medium"))

    (investigating-violation
      (description "Steps when manifest invariant is violated")
      (steps
        '("1. Identify which invariant violated"
          "2. Check git blame for offending commit"
          "3. Review commit message and PR"
          "4. Determine if intentional or accidental"
          "5. If accidental: fix and add test"
          "6. If intentional: discuss and update invariant or reject"
          "7. Update documentation if needed"
          "8. Add to common-patterns if recurring"))
      (time-estimate "1-hour")
      (owner "maintainer-or-contributor")))

  (alerts-and-thresholds
    (adoption-rate
      (metric "repos with AI.a2ml manifest")
      (threshold "< 10 after 1 month")
      (severity "medium")
      (action "Increase outreach, simplify onboarding"))

    (violation-rate
      (metric "manifest violations per 100 commits")
      (threshold "> 5%")
      (severity "high")
      (action "Improve enforcement, enhance documentation"))

    (user-complaints
      (metric "issues with tag 'user-frustration'")
      (threshold "> 3 open")
      (severity "high")
      (action "Prioritize UX improvements"))

    (mcp-server-errors
      (metric "error rate in logs")
      (threshold "> 1% of requests")
      (severity "critical")
      (action "Emergency investigation and fix")))

  (escalation-paths
    (level-1-community
      (handles "questions, documentation requests, minor bugs")
      (response-time "1-week")
      (resolver "community-contributors"))

    (level-2-maintainer
      (handles "feature requests, design decisions, major bugs")
      (response-time "48-hours")
      (resolver "maintainer"))

    (level-3-emergency
      (handles "security issues, data loss, critical bugs")
      (response-time "24-hours")
      (resolver "maintainer-immediately")))

  (automation-opportunities
    (high-priority
      '("Auto-generate manifest templates"
        "Auto-validate manifest format in CI/CD"
        "Auto-update manifest hashes"
        "Auto-migrate repos to protocol"
        "Auto-detect common violations"))

    (medium-priority
      '("Auto-generate documentation from SCM files"
        "Auto-publish releases on tag"
        "Auto-notify community on updates"
        "Auto-sync mirrors (GitLab, Bitbucket)"))

    (low-priority
      '("Auto-generate metrics dashboard"
        "Auto-create adoption reports"
        "Auto-suggest manifest improvements")))

  (knowledge-base
    (common-issues
      '((issue "Agent doesn't acknowledge manifest")
        (cause "MCP server not configured or hash incorrect")
        (solution "Check ~/.claude/config.json and verify hash computation"))

      '((issue "Duplicate SCM files still created")
        (cause "Agent not using MCP server or FUSE wrapper")
        (solution "Enable enforcement mechanism or rely on CI/CD validation"))

      '((issue "Manifest format confusing")
        (cause "Documentation unclear or format too complex")
        (solution "Improve examples, add more guidance"))

      '((issue "Platform doesn't support MCP")
        (cause "Gemini, OpenAI, etc. don't have native MCP support yet")
        (solution "Use FUSE wrapper when available, manual compliance until then"))))

    (best-practices
      '("Always test manifest changes with nextgen-languages first"
        "Keep manifest format simple and readable"
        "Document rationale for invariants"
        "Provide clear error messages"
        "Update examples when format evolves"
        "Gather feedback before breaking changes"))

    (lessons-learned
      '("Plain text format more universal than JSON/YAML"
        "Attestation provides trust without complexity"
        "Defense-in-depth needed (MCP + FUSE + CI/CD)"
        "User frustration drove innovation"
        "Cross-platform support critical for adoption"))))
