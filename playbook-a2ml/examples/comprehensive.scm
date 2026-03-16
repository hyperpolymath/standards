;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

;;; PLAYBOOK.scm — Comprehensive Example
;;; enterprise-platform
;;;
;;; Full-featured PLAYBOOK demonstrating all sections and capabilities.

(define-module (enterprise-platform playbook)
  #:export (derivation-source
            procedures
            alerts
            contacts
            rollback-strategies))

;; ============================================================
;; DERIVATION SOURCE
;; ============================================================
;; Documents the authority chain for this PLAYBOOK

(define derivation-source
  '((type . "derived")
    (meta-rules . (adr-001 adr-005 adr-012 adr-015))
    (state-context . "beta-phase")
    (agentic-gate . "entropy-budget-ok")
    (user-intent . "full-deployment-cycle")
    (timestamp . "2026-01-03T12:00:00Z")
    (validated-by . "pre-deploy-checklist")))

;; ============================================================
;; PROCEDURES
;; ============================================================
;; Executable plans with steps, preconditions, postconditions

(define procedures
  '(;; BUILD PROCEDURE
    (build
     (description . "Build all project artifacts")
     (preconditions . ("src/ directory exists"
                       "dependencies installed"
                       "no uncommitted changes"))
     (steps
       ((step 1)
        (name . "clean")
        (action . "deno task clean")
        (timeout . 60)
        (can-fail . #t))
       ((step 2)
        (name . "compile")
        (action . "deno task build")
        (timeout . 300)
        (can-fail . #f))
       ((step 3)
        (name . "test")
        (action . "deno task test")
        (timeout . 600)
        (can-fail . #f))
       ((step 4)
        (name . "lint")
        (action . "deno lint")
        (timeout . 120)
        (can-fail . #f)))
     (postconditions . ("dist/ directory created"
                        "all tests pass"
                        "no lint errors"))
     (on-failure . "abort-and-notify")
     (artifacts . ("dist/" "coverage/")))

    ;; TEST PROCEDURE
    (test
     (description . "Run comprehensive test suite")
     (preconditions . ("build successful"))
     (steps
       ((step 1)
        (name . "unit-tests")
        (action . "deno task test:unit")
        (timeout . 300))
       ((step 2)
        (name . "integration-tests")
        (action . "deno task test:integration")
        (timeout . 600))
       ((step 3)
        (name . "e2e-tests")
        (action . "deno task test:e2e")
        (timeout . 900)
        (requires-agentic-gate . "e2e-allowed")))
     (on-failure . "collect-diagnostics")
     (coverage-minimum . 80))

    ;; DEPLOY PROCEDURE
    (deploy
     (description . "Deploy to production environment")
     (preconditions . ("build successful"
                       "all tests pass"
                       "agentic-gate passed"
                       "no active incidents"))
     (requires-confirmation . #t)
     (confirmation-message . "Deploy to production? This affects live users.")
     (steps
       ((step 1)
        (name . "backup")
        (action . "backup-current-deployment")
        (timeout . 300)
        (can-fail . #f))
       ((step 2)
        (name . "pre-deploy-check")
        (action . "verify-target-environment")
        (timeout . 60))
       ((step 3)
        (name . "deploy")
        (action . "push-to-production")
        (timeout . 600)
        (neurosym-claim . "reversible"))
       ((step 4)
        (name . "verify")
        (action . "smoke-test-production")
        (timeout . 180))
       ((step 5)
        (name . "monitor")
        (action . "watch-metrics --duration=300")
        (timeout . 360)))
     (postconditions . ("deployment healthy"
                        "metrics normal"
                        "no error spikes"))
     (on-failure . "rollback-and-alert")
     (rollback-strategy . "blue-green"))

    ;; ROLLBACK PROCEDURE
    (rollback
     (description . "Rollback to previous deployment")
     (preconditions . ("backup exists"
                       "rollback-authorized"))
     (steps
       ((step 1)
        (name . "halt-traffic")
        (action . "drain-connections")
        (timeout . 60))
       ((step 2)
        (name . "restore")
        (action . "restore-from-backup")
        (timeout . 300))
       ((step 3)
        (name . "verify")
        (action . "smoke-test-rollback")
        (timeout . 180)))
     (on-failure . "escalate-immediately")
     (auto-triggered-by . ("deploy.on-failure")))

    ;; DEBUG PROCEDURE
    (debug
     (description . "Collect diagnostic information")
     (preconditions . ())
     (steps
       ((step 1)
        (name . "collect-logs")
        (action . "gather-logs --since=1h"))
       ((step 2)
        (name . "collect-metrics")
        (action . "export-metrics --format=json"))
       ((step 3)
        (name . "collect-state")
        (action . "dump-state-snapshot")))
     (on-failure . "log-and-continue")
     (output-dir . "diagnostics/"))))

;; ============================================================
;; ALERTS
;; ============================================================
;; Notification configuration for various events

(define alerts
  '((build-failure
     (severity . "medium")
     (channels . ("slack"))
     (message . "Build failed for {{project}}: {{error}}")
     (escalation-delay . 1800))

    (test-failure
     (severity . "medium")
     (channels . ("slack"))
     (message . "Tests failed: {{test-count}} failures")
     (escalation-delay . 3600))

    (deploy-failure
     (severity . "critical")
     (channels . ("slack" "pager" "email"))
     (message . "CRITICAL: Deployment failed - initiating rollback")
     (escalation-delay . 0))

    (rollback-failure
     (severity . "critical")
     (channels . ("slack" "pager" "email" "phone"))
     (message . "CRITICAL: Rollback failed - manual intervention required")
     (escalation-delay . 0)
     (auto-page . #t))

    (deployment-success
     (severity . "info")
     (channels . ("slack"))
     (message . "Deployment successful: {{version}} now live"))))

;; ============================================================
;; CONTACTS
;; ============================================================
;; On-call and escalation contacts

(define contacts
  '((primary-oncall
     (name . "Platform Team")
     (slack . "#platform-oncall")
     (email . "platform-oncall@example.com")
     (hours . "24/7")
     (response-sla . 15))

    (secondary-oncall
     (name . "SRE Team")
     (slack . "#sre-oncall")
     (pager . "sre-pager-group")
     (hours . "24/7")
     (response-sla . 30))

    (engineering-lead
     (name . "Engineering Leadership")
     (email . "eng-leads@example.com")
     (hours . "business-hours")
     (escalation-only . #t))))

;; ============================================================
;; ROLLBACK STRATEGIES
;; ============================================================
;; Named rollback strategies referenced by procedures

(define rollback-strategies
  '((blue-green
     (type . "instant-switch")
     (description . "Switch traffic back to previous deployment")
     (steps . ("switch-load-balancer" "verify" "cleanup-failed")))

    (canary-rollback
     (type . "gradual")
     (description . "Gradually shift traffic away from failed deployment")
     (steps . ("reduce-canary-to-0" "verify" "cleanup-failed")))

    (restore-from-backup
     (type . "restore")
     (description . "Restore from backup snapshot")
     (steps . ("stop-current" "restore-snapshot" "restart" "verify")))))

;;; End of PLAYBOOK.scm
