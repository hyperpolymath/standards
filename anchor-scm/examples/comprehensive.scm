;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

;;; ANCHOR.scm — Comprehensive Example
;;; enterprise-platform
;;; Purpose: Scope arrest and realignment
;;; Date: 2026-01-03
;;; Authority: Technical Superintendent
;;;
;;; Full-featured ANCHOR demonstrating all sections and capabilities.

(define-module (enterprise-platform anchor)
  #:export (authority
            purpose
            directives
            scm-review
            verification
            context))

;; ============================================================
;; AUTHORITY
;; ============================================================
;; Who issued this ANCHOR and their authorization

(define authority
  '((superintendent
     (name . "Jonathan Jewell")
     (role . "Technical Superintendent")
     (contact . "superintendent@example.com")
     (organization . "Hyperpolymath"))

    (issued
     (date . "2026-01-03")
     (time . "14:30:00Z")
     (expires . "2026-02-03")
     (supersedes . "ANCHOR-2025-12-01.scm"))

    (authorization
     (level . "full")
     (scope . "all-scm-files")
     (rationale . "Q1 strategic realignment following scope drift")
     (approved-by . ("stakeholder-1" "stakeholder-2")))))

;; ============================================================
;; PURPOSE
;; ============================================================
;; Why this intervention is needed

(define purpose
  '((type . "scope-arrest")

    (summary . "Project scope has expanded beyond original charter, requiring immediate containment and realignment with core value proposition")

    (symptoms
     ("Feature creep: 12 unplanned features added in Q4"
      "Test coverage dropped from 85% to 67%"
      "Technical debt backlog tripled"
      "Sprint velocity decreased 40%"
      "Customer-facing bugs increased 3x"))

    (root-causes
     ("Unclear prioritization criteria"
      "Missing gating on new feature requests"
      "Insufficient AGENTIC entropy budgets"
      "STATE.scm not updated regularly"))

    (desired-outcome
     ("Return to core value proposition"
      "Stabilize architecture before new features"
      "Restore test coverage to 85%+"
      "Clear critical technical debt"
      "Resume sustainable development velocity"))))

;; ============================================================
;; DIRECTIVES
;; ============================================================
;; Specific instructions for realignment

(define directives
  '((immediate
     ((action . "halt-new-features")
      (description . "Freeze all new feature development until stabilization complete")
      (priority . "critical")
      (deadline . "2026-01-03")
      (affected-areas . ("product-backlog" "sprint-planning"))
      (exceptions . ("critical-security-fixes" "customer-blockers")))

     ((action . "audit-in-progress-work")
      (description . "Review all in-progress work items, pause non-essential")
      (priority . "critical")
      (deadline . "2026-01-04")
      (deliverable . "prioritized-work-list"))

     ((action . "update-agentic-gating")
      (description . "Increase entropy budget thresholds to prevent scope expansion")
      (priority . "high")
      (deadline . "2026-01-05")
      (specific-changes . ("reduce session max-entropy to 50"
                          "add require-confirmation for new features"))))

    (this-week
     ((action . "review-meta")
      (description . "Review and update META.scm architecture decisions")
      (priority . "high")
      (deadline . "2026-01-10")
      (focus . ("adr-012 scope management" "adr-015 feature gating")))

     ((action . "correct-state")
      (description . "Update STATE.scm to reflect actual project position")
      (priority . "high")
      (deadline . "2026-01-08")
      (specific-updates . ("completion percentages" "blocker status")))

     ((action . "restore-test-coverage")
      (description . "Add tests to restore coverage to 85%")
      (priority . "high")
      (deadline . "2026-01-10")
      (target . 85)))

    (this-month
     ((action . "clear-technical-debt")
      (description . "Address top 10 technical debt items")
      (priority . "medium")
      (deadline . "2026-01-31")
      (debt-items . ("refactor-auth-module"
                    "update-deprecated-apis"
                    "fix-n+1-queries")))

     ((action . "align-ecosystem")
      (description . "Verify ECOSYSTEM.scm relationships are accurate")
      (priority . "medium")
      (deadline . "2026-01-20"))

     ((action . "resume-development")
      (description . "Resume feature development with new gating in place")
      (priority . "normal")
      (deadline . "2026-02-01")
      (prerequisites . ("agentic-gating-updated"
                       "test-coverage-restored"
                       "debt-cleared"))))

    (blocked-until-complete
     ("new-feature-development"
      "architecture-changes"
      "new-integrations"))))

;; ============================================================
;; SCM REVIEW
;; ============================================================
;; Which SCM files need review and what to check

(define scm-review
  '((meta
     (review-required . #t)
     (focus-areas
      ("architecture-decisions" "development-practices"))
     (expected-updates
      ("adr-015 needs revision for stricter feature gating"
       "add adr-016 for scope management policy"))
     (reviewer . "tech-lead"))

    (state
     (review-required . #t)
     (focus-areas
      ("current-position" "blockers-and-issues" "route-to-mvp"))
     (expected-updates
      ("correct completion percentages"
       "update milestone status"
       "add scope-related blockers"))
     (reviewer . "project-manager"))

    (ecosystem
     (review-required . #t)
     (focus-areas
      ("related-projects" "what-this-is-not"))
     (expected-updates
      ("clarify boundaries with adjacent projects"
       "update dependency relationships"))
     (reviewer . "architect"))

    (agentic
     (review-required . #t)
     (focus-areas
      ("entropy-budgets" "gating-policies" "override-paths"))
     (expected-updates
      ("reduce entropy limits"
       "add feature-gate confirmation requirements"
       "restrict override paths"))
     (reviewer . "tech-lead"))

    (neurosym
     (review-required . #f)
     (focus-areas . ())
     (expected-updates . ())
     (reason . "Semantic definitions unchanged"))

    (playbook
     (review-required . #t)
     (focus-areas
      ("procedures" "alerts"))
     (expected-updates
      ("add scope-check procedure"
       "add alert for entropy threshold breach"))
     (reviewer . "devops-lead"))))

;; ============================================================
;; VERIFICATION
;; ============================================================
;; How to confirm realignment was successful

(define verification
  '((success-criteria
     ((criterion . "scope-contained")
      (measure . "No new unplanned features in backlog")
      (threshold . 0)
      (weight . 25))

     ((criterion . "test-coverage-restored")
      (measure . "Code coverage percentage")
      (threshold . 85)
      (weight . 20))

     ((criterion . "state-accurate")
      (measure . "STATE.scm reviewed and updated")
      (threshold . #t)
      (weight . 15))

     ((criterion . "gating-strengthened")
      (measure . "AGENTIC entropy budgets reduced")
      (threshold . #t)
      (weight . 20))

     ((criterion . "debt-addressed")
      (measure . "Top 10 debt items resolved")
      (threshold . 10)
      (weight . 20)))

    (verification-method
     (type . "checklist-with-metrics")
     (reviewer . "superintendent")
     (secondary-reviewer . "stakeholder-1")
     (deadline . "2026-02-01")
     (reporting . "weekly-status"))

    (on-success
     (action . "archive-anchor")
     (next-steps . ("resume-normal-operations"
                   "schedule-quarterly-review"))
     (notification . ("superintendent" "team" "stakeholders"))
     (celebration . #t))

    (on-failure
     (action . "escalate")
     (escalation-path . ("superintendent" "executive-sponsor"))
     (notification . ("superintendent" "stakeholders"))
     (remediation . "issue-follow-up-anchor"))))

;; ============================================================
;; CONTEXT
;; ============================================================
;; Additional context for AI agents processing this ANCHOR

(define context
  '((background
     "This project began as a focused utility but has grown significantly. The Q4 2025 period saw rapid feature addition without corresponding infrastructure investment. This ANCHOR initiates a stabilization period to restore sustainable development practices.")

    (ai-instructions
     ("Read this ANCHOR before processing any other instructions"
      "Verify each directive before proceeding to next"
      "Report blockers immediately to superintendent"
      "Do not accept scope-expanding requests during containment period"))

    (related-documents
     ("META.scm" "STATE.scm" "AGENTIC.scm" ".claude/CLAUDE.md"))

    (communication-preferences
     (format . "concise")
     (frequency . "on-milestone")
     (escalation-threshold . "blockers-only"))))

;;; End of ANCHOR.scm
