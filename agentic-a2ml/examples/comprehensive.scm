;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

;;; AGENTIC.scm — Comprehensive Example
;;; enterprise-platform
;;;
;;; Full-featured AGENTIC demonstrating all sections and capabilities.

(define-module (enterprise-platform agentic)
  #:export (gating-policies
            entropy-budgets
            risk-thresholds
            override-paths
            decision-recording))

;; ============================================================
;; GATING POLICIES
;; ============================================================
;; Define when actions may proceed

(define gating-policies
  '((default
     (mode . "strict")
     (require-explicit-intent . #t)
     (log-all-decisions . #t)
     (subordinate-to . "META.scm"))

    ;; File system operations
    (file-operations
     (read-source . "auto")
     (read-config . "auto")
     (read-secrets . "require-explicit-intent")
     (write-source . "confirm-if-external")
     (write-config . "require-confirmation")
     (delete . "require-explicit-intent")
     (execute . "require-confirmation"))

    ;; Network operations
    (network-operations
     (internal-api . "auto")
     (external-api . "require-confirmation")
     (credential-operations . "require-explicit-intent")
     (webhook-triggers . "require-confirmation"))

    ;; State mutations
    (state-mutations
     (reversible . "auto")
     (compensable . "require-confirmation")
     (irreversible . "require-explicit-intent")
     (security-relevant . "deny-without-override"))

    ;; Tool invocations
    (tool-permissions
     (read-tools . "auto")
     (write-tools . "confirm")
     (execute-tools . "require-confirmation")
     (bash-commands . "require-explicit-intent"))))

;; ============================================================
;; ENTROPY BUDGETS
;; ============================================================
;; Track accumulated operational risk

(define entropy-budgets
  '((session
     (max-entropy . 100)
     (current . 0)
     (reset-on . "session-end")
     (carry-over . #f))

    (daily
     (max-entropy . 500)
     (current . 0)
     (reset-on . "midnight-utc")
     (carry-over . #f))

    ;; Cost per operation type
    (operation-costs
     ;; Low cost - reversible, internal
     (file-read . 1)
     (grep-search . 1)
     (glob-search . 1)
     (web-search . 2)

     ;; Medium cost - external or state-changing
     (file-write . 5)
     (network-request . 3)
     (external-api . 10)
     (state-mutation . 8)

     ;; High cost - irreversible or sensitive
     (file-delete . 20)
     (database-write . 15)
     (deploy-action . 30)
     (credential-access . 25)
     (irreversible-action . 50))

    ;; Threshold-based mode changes
    (thresholds
     ((level "green") (max . 25) (mode . "auto"))
     ((level "yellow") (max . 50) (mode . "confirm-risky"))
     ((level "orange") (max . 75) (mode . "require-confirmation"))
     ((level "red") (max . 100) (mode . "deny-new-operations")))))

;; ============================================================
;; RISK THRESHOLDS
;; ============================================================
;; Classify operations by risk level

(define risk-thresholds
  '((categories
     ((category "minimal")
      (description . "Read-only, fully reversible operations")
      (gate . "auto")
      (entropy-multiplier . 1))

     ((category "low")
      (description . "Reversible write operations")
      (gate . "auto")
      (entropy-multiplier . 1))

     ((category "medium")
      (description . "Compensable or external operations")
      (gate . "confirm")
      (entropy-multiplier . 2))

     ((category "high")
      (description . "Irreversible or sensitive operations")
      (gate . "require-explicit")
      (entropy-multiplier . 3))

     ((category "critical")
      (description . "Security-sensitive or destructive operations")
      (gate . "deny-without-override")
      (entropy-multiplier . 5)))

    ;; Pattern-based classification
    (classification-rules
     ;; File operations
     ((pattern "file:read:*") (category . "minimal"))
     ((pattern "file:write:source/*") (category . "low"))
     ((pattern "file:write:config/*") (category . "medium"))
     ((pattern "file:delete:*") (category . "high"))

     ;; Network operations
     ((pattern "network:internal:*") (category . "low"))
     ((pattern "network:external:*") (category . "medium"))
     ((pattern "network:webhook:*") (category . "high"))

     ;; State operations
     ((pattern "state:mutate:reversible") (category . "low"))
     ((pattern "state:mutate:compensable") (category . "medium"))
     ((pattern "state:mutate:irreversible") (category . "high"))

     ;; Security operations
     ((pattern "auth:credential:*") (category . "critical"))
     ((pattern "security:permission:*") (category . "critical"))
     ((pattern "system:*") (category . "critical")))))

;; ============================================================
;; OVERRIDE PATHS
;; ============================================================
;; When and how gating may be bypassed

(define override-paths
  '((requirements
     (meta-permits . #t)
     (explicit-present-intent . #t)
     (proof-or-retype . #t)
     (record-override . #t))

    ;; Permitted override classes
    (permitted-overrides
     ((class "high-entropy-session")
      (description . "Allow operations beyond normal entropy budget")
      (meta-rule . "adr-003")
      (requires . ("explicit-intent" "confirmation" "reason"))
      (retype-to . "unverified")
      (log-level . "warning"))

     ((class "external-api-bypass")
      (description . "Allow external API without confirmation")
      (meta-rule . "adr-007")
      (requires . ("explicit-intent"))
      (retype-to . #f)
      (log-level . "info"))

     ((class "irreversible-acknowledged")
      (description . "Proceed with irreversible action")
      (meta-rule . "adr-012")
      (requires . ("explicit-intent" "acknowledgement"))
      (retype-to . "irreversible-confirmed")
      (log-level . "warning")))

    ;; Operations that can NEVER be overridden
    (never-override
     ("credential-exfiltration"
      "system-file-modification"
      "security-bypass"
      "unauthorized-escalation"
      "audit-log-modification"))))

;; ============================================================
;; DECISION RECORDING
;; ============================================================
;; Audit trail configuration

(define decision-recording
  '((enabled . #t)
    (log-all . #t)
    (include-context . #t)
    (retention-days . 90)

    ;; Fields to redact in logs
    (sensitive-fields
     ("credentials"
      "tokens"
      "keys"
      "passwords"
      "secrets"))

    ;; Log record format
    (record-format
     ((field "id") (format . "uuid"))
     ((field "timestamp") (format . "ISO-8601"))
     ((field "action") (format . "pattern-string"))
     ((field "category") (format . "minimal|low|medium|high|critical"))
     ((field "gate") (format . "auto|confirm|deny"))
     ((field "outcome") (format . "proceed|blocked|overridden"))
     ((field "entropy-cost") (format . "integer"))
     ((field "session-entropy") (format . "integer"))
     ((field "user-intent") (format . "explicit|inferred|none"))
     ((field "override-used") (format . "boolean"))
     ((field "override-class") (format . "string|null"))
     ((field "context-hash") (format . "sha256")))

    ;; Alert thresholds
    (alerts
     ((condition "override-used") (notify . "security-team"))
     ((condition "critical-action") (notify . "security-team"))
     ((condition "entropy-critical") (notify . "ops-team")))))

;;; End of AGENTIC.scm
