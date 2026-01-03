;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; SATELLITES.scm — Hub-Satellite Registry for hyperpolymath/standards
;;
;; This file registers all satellite repositories that orbit the standards hub.
;; Satellites inherit enforcement policies, language rules, and governance from here.

(satellites
  (version "1.0.0")
  (hub
    (name "standards")
    (url "https://github.com/hyperpolymath/standards")
    (description "Organization-wide standards, policies, and enforcement mechanisms")
    (governs
      (language-policy "CLAUDE.md")
      (enforcement-workflows ".github/workflows/")
      (scm-templates "scm-templates/")
      (pre-commit-hooks "hooks/")))

  ;; ============================================================
  ;; SCM FORMAT FAMILY - Specification Repositories
  ;; ============================================================
  ;; These repos define the 7 machine-readable metadata formats
  ;; used across all hyperpolymath projects.

  (satellite-repos
    ;; PRIMARY SPECIFICATION
    (satellite
      (name "meta-scm")
      (url "https://github.com/hyperpolymath/meta-scm")
      (role "specification")
      (tier "primary")
      (format-defines ("META.scm" "STATE.scm" "ECOSYSTEM.scm"))
      (media-types ("application/meta+scheme"
                    "application/state+scheme"
                    "application/vnd.ecosystem+scm"))
      (description "META format specification - architecture decisions, development practices")
      (relationships
        (parent "standards")
        (children ("playbook-scm" "agentic-scm" "neurosym-scm" "anchor.scm"))))

    ;; EXECUTION LAYER SPECIFICATIONS
    (satellite
      (name "playbook-scm")
      (url "https://github.com/hyperpolymath/playbook-scm")
      (role "specification")
      (tier "execution")
      (format-defines ("PLAYBOOK.scm"))
      (media-types ("application/playbook+scheme"))
      (description "Executable plan specification - derived from META, gated by AGENTIC")
      (relationships
        (parent "meta-scm")
        (siblings ("agentic-scm" "neurosym-scm"))
        (depends-on ("META.scm" "STATE.scm" "AGENTIC.scm"))))

    (satellite
      (name "agentic-scm")
      (url "https://github.com/hyperpolymath/agentic-scm")
      (role "specification")
      (tier "execution")
      (format-defines ("AGENTIC.scm"))
      (media-types ("application/agentic+scheme"))
      (description "AI agent operational gating - entropy budgets, risk thresholds, safety controls")
      (relationships
        (parent "meta-scm")
        (siblings ("playbook-scm" "neurosym-scm"))
        (depends-on ("META.scm"))
        (gates ("PLAYBOOK.scm"))))

    (satellite
      (name "neurosym-scm")
      (url "https://github.com/hyperpolymath/neurosym-scm")
      (role "specification")
      (tier "execution")
      (format-defines ("NEUROSYM.scm"))
      (media-types ("application/neurosym+scheme"))
      (description "Symbolic semantics - composition algebra, proof obligations, verification hooks")
      (relationships
        (parent "meta-scm")
        (siblings ("playbook-scm" "agentic-scm"))
        (provides-semantics-for ("PLAYBOOK.scm" "AGENTIC.scm"))))

    ;; INTERVENTION SPECIFICATION
    (satellite
      (name "anchor.scm")
      (url "https://github.com/hyperpolymath/anchor.scm")
      (role "specification")
      (tier "intervention")
      (format-defines ("ANCHOR.scm"))
      (media-types ("application/anchor+scheme"))
      (description "Project recalibration intervention - scope management, realignment trigger")
      (relationships
        (parent "meta-scm")
        (can-intervene-on ("META.scm" "STATE.scm" "ECOSYSTEM.scm"
                           "PLAYBOOK.scm" "AGENTIC.scm" "NEUROSYM.scm"))
        (special-role "superintendent-drop-document")))

    ;; ============================================================
    ;; RSR (RHODIUM STANDARD REPOSITORIES)
    ;; ============================================================
    (satellite
      (name "rhodium-standard-repositories")
      (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
      (role "specification")
      (tier "infrastructure")
      (description "RSR specification - repository compliance standards")
      (relationships
        (parent "standards")
        (enforces-on "all-repos")))

    (satellite
      (name "git-rsr-certified")
      (url "https://github.com/hyperpolymath/git-rsr-certified")
      (role "tooling")
      (tier "infrastructure")
      (description "Universal RSR compliance engine")
      (relationships
        (implements "rhodium-standard-repositories")))

    ;; ============================================================
    ;; RELATED STANDARDS
    ;; ============================================================
    (satellite
      (name "palimpsest-licence")
      (url "https://github.com/hyperpolymath/palimpsest-licence")
      (role "governance")
      (tier "foundation")
      (description "Philosophical license overlay for ethical AI development")
      (relationships
        (applied-to "all-repos"))))

  ;; ============================================================
  ;; EXECUTION PIPELINE
  ;; ============================================================
  ;; All implementations MUST respect this ordering

  (execution-pipeline
    (order
      ((step 1) (format "META.scm") (action "validation") (authority "constitutional"))
      ((step 2) (format "AGENTIC.scm") (action "gating") (authority "operational-safety"))
      ((step 3) (format "NEUROSYM.scm") (action "semantics") (authority "proof-obligations"))
      ((step 4) (format "PLAYBOOK.scm") (action "derivation") (authority "executable-plan"))
      ((step 5) (action "execution"))
      ((step 6) (format "ECOSYSTEM.scm") (action "integrity-check"))
      ((step 7) (format "STATE.scm") (action "update")))

    (special-intervention
      ((format "ANCHOR.scm")
       (trigger "superintendent-drop")
       (effect "recalibrate-all-scm-files")
       (authority "external-to-pipeline")))))

;;; End of SATELLITES.scm
