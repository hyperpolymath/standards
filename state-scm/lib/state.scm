;;; ==================================================
;;; lib/state.scm — Main STATE Module Entry Point
;;; ==================================================
;;;
;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2025 Jonathan D.A. Jewell
;;;
;;; Main entry point that loads and re-exports all STATE modules.
;;; Use this for convenient access to all STATE functionality.
;;;
;;; Usage:
;;;   (use-modules (state))
;;;   ; Now all STATE functions are available
;;;
;;; ==================================================

(define-module (state)
  ;; Re-export everything from submodules
  #:use-module ((state-core) #:prefix core:)
  #:use-module ((state-kanren) #:prefix kanren:)
  #:use-module ((state-graph) #:prefix graph:)
  #:use-module ((state-history) #:prefix history:)

  #:re-export (
    ;; From state-core
    core:get-section
    core:get-user-name
    core:get-current-focus
    core:get-all-projects
    core:get-project-by-name
    core:get-blocked-projects
    core:get-in-progress-projects
    core:get-critical-next
    core:get-session-info
    core:get-history
    core:messages-remaining?
    core:project-exists?
    core:project-blocked?
    core:project-complete?
    core:query-by-status
    core:query-by-category
    core:query-by-completion-range
    core:query-nearly-complete
    core:query-just-started
    core:query-dependencies
    core:query-dependents
    core:query-next-actions
    core:query-all-next-actions
    core:session-health
    core:should-checkpoint?

    ;; From state-kanren
    kanren:run*
    kanren:run
    kanren:fresh
    kanren:conde
    kanren:==
    kanren:succeed
    kanren:fail
    kanren:conj
    kanren:disj
    kanren:projecto
    kanren:statuso
    kanren:dependso
    kanren:blockso
    kanren:categoryo
    kanren:completiono
    kanren:blocked-projects-o
    kanren:dependency-chain-o
    kanren:critical-path-o
    kanren:minikanren-available?

    ;; From state-graph
    graph:generate-dot
    graph:generate-dot-file
    graph:dot-dependency-graph
    graph:dot-status-graph
    graph:dot-critical-path
    graph:generate-mermaid
    graph:generate-mermaid-file
    graph:mermaid-dependency-graph
    graph:mermaid-status-graph
    graph:mermaid-flowchart
    graph:build-adjacency-list
    graph:topological-sort
    graph:find-critical-path
    graph:calculate-depth

    ;; From state-history
    history:get-project-history
    history:get-latest-snapshot
    history:get-snapshots-in-range
    history:calculate-velocity
    history:calculate-average-velocity
    history:project-velocity
    history:overall-velocity
    history:estimate-completion-date
    history:estimate-days-remaining
    history:estimate-project-completion
    history:estimate-all-completions
    history:create-snapshot
    history:add-snapshot-to-history
    history:prune-old-snapshots
    history:velocity-report
    history:progress-report
    history:burndown-data
  )

  #:export (
    ;; Convenience aliases without prefixes
    get-section
    get-user-name
    get-current-focus
    get-all-projects
    get-project-by-name
    get-blocked-projects
    get-in-progress-projects
    get-critical-next
    session-health
    should-checkpoint?

    ;; Quick access
    run*
    fresh
    conde
    ==

    ;; Graphs
    generate-dot
    generate-mermaid

    ;; History
    project-velocity
    estimate-completion-date
    create-snapshot
    velocity-report
    progress-report

    ;; Meta
    state-version
    state-modules-loaded
  ))

;;; --------------------------------------------------
;;; Convenience Aliases
;;; --------------------------------------------------

(define get-section core:get-section)
(define get-user-name core:get-user-name)
(define get-current-focus core:get-current-focus)
(define get-all-projects core:get-all-projects)
(define get-project-by-name core:get-project-by-name)
(define get-blocked-projects core:get-blocked-projects)
(define get-in-progress-projects core:get-in-progress-projects)
(define get-critical-next core:get-critical-next)
(define session-health core:session-health)
(define should-checkpoint? core:should-checkpoint?)

(define run* kanren:run*)
(define fresh kanren:fresh)
(define conde kanren:conde)
(define == kanren:==)

(define generate-dot graph:generate-dot)
(define generate-mermaid graph:generate-mermaid)

(define project-velocity history:project-velocity)
(define estimate-completion-date history:estimate-completion-date)
(define create-snapshot history:create-snapshot)
(define velocity-report history:velocity-report)
(define progress-report history:progress-report)

;;; --------------------------------------------------
;;; Meta Information
;;; --------------------------------------------------

(define state-version "2.0.0")

(define (state-modules-loaded)
  "List all loaded STATE modules and their status."
  `((state-core . loaded)
    (state-kanren . ,(if (kanren:minikanren-available?)
                         'loaded-with-minikanren
                         'loaded-with-fallback))
    (state-graph . loaded)
    (state-history . loaded)))

;;; ==================================================
;;; END lib/state.scm
;;; ==================================================
