;;; ==================================================
;;; lib/state-core.scm — Core STATE Data Structures
;;; ==================================================
;;;
;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2025 Jonathan D.A. Jewell
;;;
;;; Core accessors and data structure definitions for STATE.
;;; This module has no external dependencies.
;;;
;;; ==================================================

(define-module (state-core)
  #:export (
    ;; Accessors
    get-section
    get-user-name
    get-current-focus
    get-all-projects
    get-project-by-name
    get-blocked-projects
    get-in-progress-projects
    get-critical-next
    get-session-info
    get-history
    messages-remaining?

    ;; Predicates
    project-exists?
    project-blocked?
    project-complete?

    ;; Queries
    query-by-status
    query-by-category
    query-by-completion-range
    query-nearly-complete
    query-just-started
    query-dependencies
    query-dependents
    query-next-actions
    query-all-next-actions

    ;; Session
    session-health
    should-checkpoint?
  ))

;;; --------------------------------------------------
;;; Core Accessors
;;; --------------------------------------------------

(define (get-section state section-name)
  "Get a top-level section from state by name."
  (let ((section (assoc section-name (cdr state))))
    (if section (cdr section) #f)))

(define (get-user-name state)
  "Extract user name from state."
  (let ((user (get-section state 'user)))
    (if user
        (cdr (assoc 'name user))
        #f)))

(define (get-current-focus state)
  "Get the current project focus."
  (let ((focus (get-section state 'focus)))
    (if focus
        (cdr (assoc 'current-project focus))
        #f)))

(define (get-all-projects state)
  "Get the list of all projects."
  (get-section state 'projects))

(define (get-project-by-name state name)
  "Find a project by its name."
  (let ((projects (get-all-projects state)))
    (if projects
        (find (lambda (p)
                (equal? (cdr (assoc 'name p)) name))
              projects)
        #f)))

(define (get-blocked-projects state)
  "Get all projects with status 'blocked'."
  (let ((projects (get-all-projects state)))
    (if projects
        (filter (lambda (p)
                  (equal? (cdr (assoc 'status p)) "blocked"))
                projects)
        '())))

(define (get-in-progress-projects state)
  "Get all projects with status 'in-progress'."
  (let ((projects (get-all-projects state)))
    (if projects
        (filter (lambda (p)
                  (equal? (cdr (assoc 'status p)) "in-progress"))
                projects)
        '())))

(define (get-critical-next state)
  "Get the list of critical next actions."
  (get-section state 'critical-next))

(define (get-session-info state)
  "Get session metadata."
  (get-section state 'session))

(define (get-history state)
  "Get the history section for time tracking."
  (get-section state 'history))

(define (messages-remaining? state)
  "Check how many messages remain in session."
  (let ((session (get-session-info state)))
    (if session
        (cdr (assoc 'messages-remaining session))
        #f)))

;;; --------------------------------------------------
;;; Predicates
;;; --------------------------------------------------

(define (project-exists? state name)
  "Check if a project with given name exists."
  (if (get-project-by-name state name) #t #f))

(define (project-blocked? state name)
  "Check if a project is blocked."
  (let ((project (get-project-by-name state name)))
    (and project
         (equal? (cdr (assoc 'status project)) "blocked"))))

(define (project-complete? state name)
  "Check if a project is complete."
  (let ((project (get-project-by-name state name)))
    (and project
         (equal? (cdr (assoc 'status project)) "complete"))))

;;; --------------------------------------------------
;;; Query Functions
;;; --------------------------------------------------

(define (query-by-status status state)
  "Get all projects with given status."
  (let ((projects (get-all-projects state)))
    (filter (lambda (p)
              (equal? (cdr (assoc 'status p)) status))
            projects)))

(define (query-by-category category state)
  "Get all projects in given category."
  (let ((projects (get-all-projects state)))
    (filter (lambda (p)
              (equal? (cdr (assoc 'category p)) category))
            projects)))

(define (query-by-completion-range min-pct max-pct state)
  "Get projects with completion between min and max percent."
  (let ((projects (get-all-projects state)))
    (filter (lambda (p)
              (let ((completion (cdr (assoc 'completion p))))
                (and (>= completion min-pct)
                     (<= completion max-pct))))
            projects)))

(define (query-nearly-complete state)
  "Get projects that are >= 80% complete."
  (query-by-completion-range 80 100 state))

(define (query-just-started state)
  "Get projects that are <= 20% complete."
  (query-by-completion-range 0 20 state))

(define (query-dependencies project-name state)
  "Get all dependencies of a project."
  (let ((project (get-project-by-name state project-name)))
    (if project
        (cdr (assoc 'dependencies project))
        '())))

(define (query-dependents project-name state)
  "Get all projects that depend on this project."
  (let ((projects (get-all-projects state)))
    (filter (lambda (p)
              (member project-name
                      (cdr (assoc 'dependencies p))))
            projects)))

(define (query-next-actions project-name state)
  "Get next actions for a specific project."
  (let ((project (get-project-by-name state project-name)))
    (if project
        (cdr (assoc 'next project))
        '())))

(define (query-all-next-actions state)
  "Get next actions across all in-progress projects."
  (let ((projects (get-in-progress-projects state)))
    (apply append
           (map (lambda (p)
                  (map (lambda (action)
                         (cons (cdr (assoc 'name p)) action))
                       (cdr (assoc 'next p))))
                projects))))

;;; --------------------------------------------------
;;; Session Analysis
;;; --------------------------------------------------

(define (session-health state)
  "Analyze session health and return status."
  (let* ((session (get-session-info state))
         (remaining (if session
                        (cdr (assoc 'messages-remaining session))
                        100))
         (limit-reached (if session
                            (cdr (assoc 'token-limit-reached session))
                            #f)))
    (cond
      (limit-reached 'critical)
      ((< remaining 10) 'low)
      ((< remaining 30) 'moderate)
      (else 'healthy))))

(define (should-checkpoint? state)
  "Returns #t if a checkpoint should be created now."
  (let ((health (session-health state)))
    (or (eq? health 'critical)
        (eq? health 'low))))

;;; ==================================================
;;; END lib/state-core.scm
;;; ==================================================
