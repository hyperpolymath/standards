;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 The Rhodium Standard Contributors
;;
;; TESTING-REPORT.scm - Machine-readable testing report for RSR
;; Generated: 2025-12-29T11:45:27+00:00

(testing-report
  (metadata
    (version "1.0.0")
    (schema-version "1.0.0")
    (generated "2025-12-29T11:45:27+00:00")
    (project "rhodium-standard-repositories")
    (tested-by "claude-opus-4-5-20251101"))

  (summary
    (overall-status 'pass)
    (rsr-compliance-level 'silver)
    (rsr-compliance-score 91.14)
    (total-tests 24)
    (tests-passed 24)
    (tests-failed 0)
    (examples-built 4)
    (examples-failed 1)
    (issues-fixed 2))

  (environment
    (os "Fedora Linux")
    (kernel "6.17.12-300.fc43.x86_64")
    (platform "linux")
    (rust-edition "2021")
    (python-version "3.14"))

  (build-results
    (example
      (name "minimal-rust-project")
      (status 'pass)
      (compliance-level 'bronze)
      (dependencies '())
      (notes "No dependencies, minimal example"))

    (example
      (name "standard-library")
      (status 'pass)
      (compliance-level 'silver)
      (dependencies (optional (serde "1.0")))
      (notes "Optional serde serialization support"))

    (example
      (name "rhodium-minimal")
      (status 'pass)
      (compliance-level 'silver-plus)
      (dependencies '())
      (notes "Minimal RSR-compliant example"))

    (example
      (name "enterprise-service")
      (status 'pass)
      (compliance-level 'gold)
      (dependencies
        (tokio "1.35")
        (axum "0.7")
        (serde "1.0")
        (tracing "0.1")
        (tower-http "0.5")
        (chrono "0.4"))
      (notes "Production-ready REST API example"))

    (example
      (name "ai-ml-project")
      (status 'fail)
      (error-type 'dependency-incompatibility)
      (error-detail "PyO3 0.24.2 incompatible with Python 3.14 (max: 3.13)")
      (dependencies
        (tokio "1.35")
        (axum "0.7")
        (ndarray "0.16")
        (pyo3 "0.24.1"))
      (workaround "Set PYO3_USE_ABI3_FORWARD_COMPATIBILITY=1")))

  (satellite-results
    (project
      (name "rsr-certifier/engine")
      (status 'fail)
      (error-count 51)
      (error-type 'compilation)
      (errors
        (missing-dependency "surrealdb")
        (type-inference "arangodb async queries")))

    (project
      (name "rsr-certifier/lsp")
      (status 'blocked)
      (blocked-by "rsr-certifier/engine")))

  (test-results
    (project
      (name "minimal-rust-project")
      (unit-tests
        (passed 2)
        (failed 0)
        (tests
          ("test_add" 'pass)
          ("test_multiply" 'pass)))
      (doc-tests
        (passed 2)
        (failed 0)))

    (project
      (name "standard-library")
      (unit-tests
        (passed 7)
        (failed 0)
        (tests
          ("queue_tests::test_enqueue_dequeue" 'pass)
          ("queue_tests::test_front" 'pass)
          ("queue_tests::test_new_queue" 'pass)
          ("stack_tests::test_clear" 'pass)
          ("stack_tests::test_new_stack" 'pass)
          ("stack_tests::test_peek" 'pass)
          ("stack_tests::test_push_pop" 'pass)))
      (doc-tests
        (passed 7)
        (failed 0)))

    (project
      (name "rhodium-minimal")
      (unit-tests
        (passed 3)
        (failed 0)
        (tests
          ("test_no_network_calls" 'pass)
          ("test_compliance_check" 'pass)
          ("test_main_runs_without_error" 'pass))))

    (project
      (name "enterprise-service")
      (unit-tests
        (passed 6)
        (failed 0)
        (tests
          ("handlers::tests::test_create_user" 'pass)
          ("handlers::tests::test_delete_user" 'pass)
          ("handlers::tests::test_get_nonexistent_user" 'pass)
          ("handlers::tests::test_get_user" 'pass)
          ("handlers::tests::test_list_users" 'pass)
          ("handlers::tests::test_create_user_invalid_email" 'pass)))))

  (lint-results
    (project
      (name "minimal-rust-project")
      (status 'pass)
      (warnings 0))

    (project
      (name "standard-library")
      (status 'pass)
      (warnings 0))

    (project
      (name "rhodium-minimal")
      (status 'pass)
      (warnings 0))

    (project
      (name "enterprise-service")
      (status 'pass)
      (warnings 0)
      (fixed-issues
        (unused-import "Path")
        (unused-import "State")
        (unused-import "StatusCode")
        (unused-import "post")
        (unused-import "IntoResponse")
        (unused-import "AppError")
        (unused-import "User"))))

  (rsr-audit
    (total-checks 79)
    (passed-checks 72)
    (failed-checks 7)
    (score 91.14)
    (compliance-level 'silver)

    (categories
      (foundational-infrastructure
        (passed 11)
        (total 11))
      (documentation-standards
        (passed 14)
        (total 17)
        (failures
          ("README.md present" "Uses README.adoc instead")
          ("CONTRIBUTING.md present" "Uses CONTRIBUTING.adoc instead")))
      (security-architecture
        (passed 6)
        (total 6))
      (architecture-principles
        (passed 4)
        (total 4))
      (web-standards
        (passed 7)
        (total 7))
      (semantic-web
        (passed 3)
        (total 3))
      (foss-licensing
        (passed 5)
        (total 6)
        (failures
          ("SPDX identifier format" "MIT OR AGPL-3.0-or-later vs expected pattern")))
      (cognitive-ergonomics
        (passed 2)
        (total 2))
      (lifecycle-management
        (passed 4)
        (total 4))
      (community-governance
        (passed 5)
        (total 6)
        (failures
          ("CONTRIBUTING.md present" "Uses CONTRIBUTING.adoc instead")))
      (maa
        (passed 3)
        (total 4))
      (bonus
        (passed 7)
        (total 7))))

  (issues-fixed
    (issue
      (id 1)
      (severity 'medium)
      (type 'unused-imports)
      (file "examples/enterprise-service/src/main.rs")
      (description "Removed unused imports causing Clippy warnings")
      (fixed #t))

    (issue
      (id 2)
      (severity 'low)
      (type 'script-bug)
      (file "rsr-audit.sh")
      (line 588)
      (description "Fixed arithmetic syntax error in signed commits check")
      (root-cause "grep -c output not properly sanitized")
      (fixed #t)))

  (known-issues
    (issue
      (id "KI-001")
      (severity 'high)
      (type 'dependency-incompatibility)
      (project "ai-ml-project")
      (description "PyO3 0.24.2 incompatible with Python 3.14")
      (impact "Cannot build on Python 3.14+ systems")
      (workaround "Set PYO3_USE_ABI3_FORWARD_COMPATIBILITY=1")
      (recommended-fix "Update to PyO3 0.27+ when available"))

    (issue
      (id "KI-002")
      (severity 'high)
      (type 'compilation-failure)
      (project "rsr-certifier")
      (description "Engine fails to compile with 51 errors")
      (impact "Cannot build RSR certification engine")
      (root-causes
        ("Missing surrealdb crate")
        ("Type inference failures in ArangoDB queries"))
      (recommended-fix "Add surrealdb dependency and explicit type annotations")))

  (recommendations
    (high-priority
      (item "Fix rsr-certifier dependencies and type annotations")
      (item "Update PyO3 or pin Python version for ai-ml-project"))

    (medium-priority
      (item "Add README.md symlink or update audit to accept .adoc")
      (item "Harmonize LICENSE.txt SPDX format with audit expectations"))

    (low-priority
      (item "Enable GPG commit signing for Gold compliance")
      (item "Add integration tests for enterprise-service"))))

;; Helper functions for querying this report
(define (get-overall-status report)
  (cadr (assoc 'overall-status (cadr (assoc 'summary report)))))

(define (get-compliance-score report)
  (cadr (assoc 'rsr-compliance-score (cadr (assoc 'summary report)))))

(define (get-failed-tests report)
  (cadr (assoc 'tests-failed (cadr (assoc 'summary report)))))

(define (get-issues-fixed-count report)
  (cadr (assoc 'issues-fixed (cadr (assoc 'summary report)))))

;; End of TESTING-REPORT.scm
