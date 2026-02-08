;; SPDX-License-Identifier: MPL-2.0-or-later
;; NEUROSYM.scm - Neurosymbolic integration config for agentic-scm

(define neurosym-config
  `((version . "1.0.0")

    (symbolic-layer
      ((type . "scheme")
       (reasoning . "deductive")
       (verification . "formal")
       (capabilities
         ("S-expression parsing"
          "Pattern matching for classification rules"
          "Entropy arithmetic"
          "Boolean gating logic"
          "Override path validation"))))

    (neural-layer
      ((embeddings . false)
       (fine-tuning . false)
       (use-cases
         ("Future: Natural language intent classification"
          "Future: Anomaly detection in operation patterns"
          "Future: Risk prediction from context"))))

    (integration
      ((agentic-proof-obligations
         ((name . "explicit-intent-verification")
          (description . "Verify user intent is explicit and present")
          (symbolic-check . "intent-type = explicit AND intent-timestamp = current")
          (neural-assist . "Intent classification from natural language")
          (fallback . "require-confirmation"))

         ((name . "entropy-threshold-check")
          (description . "Verify operation within entropy budget")
          (symbolic-check . "session-entropy + operation-cost <= max-entropy")
          (neural-assist . false)
          (fallback . "deny-with-explanation"))

         ((name . "risk-classification")
          (description . "Classify operation risk level")
          (symbolic-check . "pattern-match against classification-rules")
          (neural-assist . "Semantic similarity for unmatched patterns")
          (fallback . "default-to-high"))

         ((name . "override-validation")
          (description . "Validate override request")
          (symbolic-check . "meta-rule exists AND requirements-met")
          (neural-assist . false)
          (fallback . "deny-override")))

       (proof-outcomes
         ((verified . "Action proceeds with full confidence")
          (unverified . "Action retyped, proceeds with warning")
          (failed . "Action blocked, recorded in audit")
          (timeout . "Action blocked, escalate to user")))

       (meta-subordination
         ((principle . "AGENTIC cannot override META denials")
          (enforcement . "Check META validation before AGENTIC gating")
          (recording . "Both META and AGENTIC decisions logged")))

       (state-integration
         ((on-proceed . "STATE.scm updated with operation record")
          (on-block . "STATE.scm unchanged, block recorded in audit")
          (on-override . "STATE.scm updated with override annotation")))

       (playbook-gating
         ((principle . "PLAYBOOK execution gated by AGENTIC")
          (sequence . "1. META permits -> 2. AGENTIC gates -> 3. PLAYBOOK executes")
          (rollback . "PLAYBOOK steps rolled back on AGENTIC block")))))))
