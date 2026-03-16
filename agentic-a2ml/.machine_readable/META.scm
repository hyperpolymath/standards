;; SPDX-License-Identifier: MPL-2.0-or-later
;; META.scm - Meta-level information for agentic-scm
;; Media-Type: application/meta+scheme

(meta
  (architecture-decisions
    ((adr "ADR-001")
     (title "Use Scheme S-expressions for AGENTIC format")
     (status "accepted")
     (date "2025-12-20")
     (context "Need machine-readable format for operational gating policies")
     (decision "Use Guile Scheme S-expressions for AGENTIC.scm files")
     (rationale
       ("Homoiconic - code and data share same structure"
        "Well-defined parsing semantics"
        "Compatible with STATE.scm and NEUROSYM.scm formats"
        "Supports extensibility via macros"
        "Human-readable and machine-parseable"))
     (consequences
       ("Requires Scheme parser or compatible reader"
        "Learning curve for non-Lisp developers"
        "JSON Schema provided for alternative tooling")))

    ((adr "ADR-002")
     (title "Subordinate AGENTIC to META constitutional authority")
     (status "accepted")
     (date "2025-12-20")
     (context "Need clear hierarchy between permission and execution gating")
     (decision "AGENTIC can only gate actions already permitted by META")
     (rationale
       ("Separation of concerns: permission vs timing"
        "META defines what CAN happen, AGENTIC defines what WILL happen now"
        "Prevents authority escalation through operational policies"
        "Clear audit trail for decision accountability"))
     (consequences
       ("AGENTIC cannot grant new capabilities"
        "Override paths must reference META ADRs"
        "Simpler security model")))

    ((adr "ADR-003")
     (title "Entropy budgets for operational risk management")
     (status "accepted")
     (date "2025-12-20")
     (context "Need mechanism to limit accumulated risk in a session")
     (decision "Implement entropy budget tracking with configurable thresholds")
     (rationale
       ("Prevents runaway operations"
        "Provides natural pause points for high-risk sessions"
        "Quantifiable risk metric for audit"
        "User can override with explicit intent"))
     (consequences
       ("Requires state tracking during session"
        "May block legitimate high-volume operations"
        "Override mechanism needed for batch operations")))

    ((adr "ADR-004")
     (title "Explicit present intent vs inferred intent")
     (status "accepted")
     (date "2026-01-03")
     (context "AI agents may infer user intent from context, memories, or patterns")
     (decision "Distinguish explicit present intent from inferred/historical intent")
     (rationale
       ("User memories are not equivalent to current requests"
        "Prevents action drift from accumulated context"
        "Maintains user agency over each decision"
        "Required for high-risk operations"))
     (consequences
       ("More confirmation prompts for sensitive operations"
        "Clear audit trail of user intent"
        "May slow down workflows that rely on inferred context")))

    ((adr "ADR-005")
     (title "ABNF and JSON Schema for formal specification")
     (status "accepted")
     (date "2026-01-09")
     (context "Need formal grammar for validation and tooling")
     (decision "Provide both ABNF grammar and JSON Schema")
     (rationale
       ("ABNF is RFC standard for protocol specification"
        "JSON Schema enables validation in web tooling"
        "Dual formats support different ecosystems"
        "Formal grammar catches ambiguities early"))
     (consequences
       ("Must keep ABNF and JSON Schema synchronized"
        "Additional maintenance burden"
        "Better interoperability"))))

  (development-practices
    (code-style
      ((scheme "GNU Guile conventions")
       (ada "GNAT style with SPARK subset where applicable")
       (documentation "AsciiDoc with RFC-style sections")))
    (security
      (principle "Defense in depth")
      (requirements
        ("Path traversal validation"
         "No hardcoded secrets"
         "Explicit intent for destructive operations"
         "Audit logging for all gating decisions")))
    (testing
      ((unit "Scheme test suites for parser")
       (integration "Example file validation")
       (conformance "ABNF grammar compliance")))
    (versioning "SemVer")
    (documentation "AsciiDoc")
    (branching "main for stable"))

  (design-rationale
    ((principle "Fail-safe defaults")
     (description "Denial causes no side effects, state remains unchanged"))
    ((principle "Proof-aware typing")
     (description "Treats proof outcomes as truth predicates"))
    ((principle "Auditable decisions")
     (description "All gating decisions recorded for review"))))
