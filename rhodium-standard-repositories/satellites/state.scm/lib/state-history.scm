;;; ==================================================
;;; lib/state-history.scm — History Tracking & Estimation
;;; ==================================================
;;;
;;; SPDX-License-Identifier: MPL-2.0-or-later
;;; Copyright (c) 2025 Jonathan D.A. Jewell
;;;
;;; Tracks project completion over time and estimates
;;; future completion dates based on velocity.
;;;
;;; ==================================================

(define-module (state-history)
  #:use-module (state-core)
  #:use-module (srfi srfi-19)  ; Time/date library
  #:use-module (ice-9 format)
  #:export (
    ;; History accessors
    get-project-history
    get-latest-snapshot
    get-snapshots-in-range

    ;; Velocity calculations
    calculate-velocity
    calculate-average-velocity
    project-velocity
    overall-velocity

    ;; Time estimation
    estimate-completion-date
    estimate-days-remaining
    estimate-project-completion
    estimate-all-completions

    ;; History management
    create-snapshot
    add-snapshot-to-history
    prune-old-snapshots

    ;; Reporting
    velocity-report
    progress-report
    burndown-data
  ))

;;; --------------------------------------------------
;;; History Data Structure
;;; --------------------------------------------------
;;;
;;; History section in STATE.scm:
;;;
;;; (history
;;;   (snapshots
;;;     ((timestamp . "2025-12-01T10:00:00Z")
;;;      (projects
;;;        ((name . "Project1") (completion . 20))
;;;        ((name . "Project2") (completion . 50))))
;;;     ((timestamp . "2025-12-03T10:00:00Z")
;;;      (projects
;;;        ((name . "Project1") (completion . 35))
;;;        ((name . "Project2") (completion . 55))))
;;;     ...))

;;; --------------------------------------------------
;;; History Accessors
;;; --------------------------------------------------

(define (get-project-history project-name state)
  "Get completion history for a specific project.
   Returns ((timestamp . completion) ...) sorted by time."
  (let ((history (get-history state)))
    (if (not history)
        '()
        (let ((snapshots (cdr (assoc 'snapshots history))))
          (filter-map
           (lambda (snapshot)
             (let* ((timestamp (cdr (assoc 'timestamp snapshot)))
                    (projects (cdr (assoc 'projects snapshot)))
                    (proj (find (lambda (p)
                                  (equal? (cdr (assoc 'name p)) project-name))
                                projects)))
               (if proj
                   (cons timestamp (cdr (assoc 'completion proj)))
                   #f)))
           (or snapshots '()))))))

(define (get-latest-snapshot state)
  "Get the most recent snapshot from history."
  (let ((history (get-history state)))
    (if (not history)
        #f
        (let ((snapshots (cdr (assoc 'snapshots history))))
          (if (or (not snapshots) (null? snapshots))
              #f
              (car (reverse snapshots)))))))

(define (get-snapshots-in-range start-date end-date state)
  "Get all snapshots between two dates (ISO 8601 strings)."
  (let ((history (get-history state)))
    (if (not history)
        '()
        (let ((snapshots (cdr (assoc 'snapshots history))))
          (filter
           (lambda (snapshot)
             (let ((ts (cdr (assoc 'timestamp snapshot))))
               (and (string>=? ts start-date)
                    (string<=? ts end-date))))
           (or snapshots '()))))))

;;; --------------------------------------------------
;;; Velocity Calculations
;;; --------------------------------------------------

(define (parse-iso-date str)
  "Parse ISO 8601 date string to SRFI-19 date."
  (string->date str "~Y-~m-~dT~H:~M:~SZ"))

(define (days-between date1-str date2-str)
  "Calculate days between two ISO 8601 date strings."
  (let* ((d1 (parse-iso-date date1-str))
         (d2 (parse-iso-date date2-str))
         (t1 (date->time-utc d1))
         (t2 (date->time-utc d2))
         (diff (time-difference t2 t1))
         (seconds (time-second diff)))
    (/ seconds 86400.0)))  ; 86400 seconds per day

(define (calculate-velocity completion1 completion2 days)
  "Calculate velocity as completion-percentage-per-day."
  (if (zero? days)
      0
      (/ (- completion2 completion1) days)))

(define (calculate-average-velocity history-points)
  "Calculate average velocity from a list of (timestamp . completion) pairs."
  (if (or (null? history-points)
          (null? (cdr history-points)))
      0  ; Need at least 2 points
      (let* ((sorted (sort history-points
                           (lambda (a b) (string<? (car a) (car b)))))
             (first-point (car sorted))
             (last-point (car (reverse sorted)))
             (total-days (days-between (car first-point) (car last-point)))
             (total-progress (- (cdr last-point) (cdr first-point))))
        (if (zero? total-days)
            0
            (/ total-progress total-days)))))

(define (project-velocity project-name state)
  "Calculate the current velocity for a project.
   Returns percentage points per day."
  (let ((history (get-project-history project-name state)))
    (calculate-average-velocity history)))

(define (overall-velocity state)
  "Calculate average velocity across all in-progress projects."
  (let ((projects (get-in-progress-projects state)))
    (if (null? projects)
        0
        (let ((velocities
               (map (lambda (p)
                      (project-velocity (cdr (assoc 'name p)) state))
                    projects)))
          (/ (apply + velocities) (length velocities))))))

;;; --------------------------------------------------
;;; Time Estimation
;;; --------------------------------------------------

(define (estimate-days-remaining current-completion velocity)
  "Estimate days to reach 100% given current completion and velocity."
  (if (<= velocity 0)
      +inf.0  ; Infinite if no progress or negative velocity
      (/ (- 100 current-completion) velocity)))

(define (add-days-to-date date-str days)
  "Add days to an ISO 8601 date string."
  (let* ((date (parse-iso-date date-str))
         (time (date->time-utc date))
         (seconds (* days 86400))
         (new-time (add-duration time (make-time time-duration 0 (inexact->exact (round seconds)))))
         (new-date (time-utc->date new-time)))
    (date->string new-date "~Y-~m-~dT~H:~M:~SZ")))

(define (estimate-completion-date project-name state)
  "Estimate when a project will reach 100% completion.
   Returns an ISO 8601 date string or #f if cannot estimate."
  (let* ((project (get-project-by-name state project-name))
         (velocity (project-velocity project-name state))
         (current (if project (cdr (assoc 'completion project)) 0))
         (now (get-current-timestamp)))

    (cond
      ((>= current 100) now)  ; Already complete
      ((<= velocity 0) #f)    ; No velocity data
      (else
       (let ((days (estimate-days-remaining current velocity)))
         (if (infinite? days)
             #f
             (add-days-to-date now days)))))))

(define (estimate-project-completion project-name state)
  "Get detailed completion estimate for a project.
   Returns alist with velocity, days-remaining, estimated-date."
  (let* ((project (get-project-by-name state project-name))
         (velocity (project-velocity project-name state))
         (current (if project (cdr (assoc 'completion project)) 0))
         (days-remaining (estimate-days-remaining current velocity))
         (estimated-date (estimate-completion-date project-name state)))

    `((project . ,project-name)
      (current-completion . ,current)
      (velocity-per-day . ,velocity)
      (days-remaining . ,(if (infinite? days-remaining) "unknown" days-remaining))
      (estimated-completion . ,(or estimated-date "unknown")))))

(define (estimate-all-completions state)
  "Get completion estimates for all in-progress projects."
  (let ((projects (get-in-progress-projects state)))
    (map (lambda (p)
           (estimate-project-completion (cdr (assoc 'name p)) state))
         projects)))

(define (get-current-timestamp)
  "Get current time as ISO 8601 string."
  (date->string (current-date) "~Y-~m-~dT~H:~M:~SZ"))

;;; --------------------------------------------------
;;; History Management
;;; --------------------------------------------------

(define (create-snapshot state)
  "Create a new history snapshot from current project states."
  (let ((projects (get-all-projects state))
        (now (get-current-timestamp)))
    `((timestamp . ,now)
      (projects
       ,@(map (lambda (p)
                `((name . ,(cdr (assoc 'name p)))
                  (completion . ,(cdr (assoc 'completion p)))))
              projects)))))

(define (add-snapshot-to-history snapshot state)
  "Add a snapshot to the state's history.
   Returns a new state with updated history."
  (let* ((history (or (get-history state) '((snapshots))))
         (snapshots (or (cdr (assoc 'snapshots history)) '()))
         (new-snapshots (append snapshots (list snapshot)))
         (new-history `(history (snapshots ,@new-snapshots))))

    ;; Replace history section in state
    (let ((state-list (cdr state)))
      `(state
        ,@(map (lambda (section)
                 (if (and (pair? section)
                          (eq? (car section) 'history))
                     new-history
                     section))
               state-list)
        ,@(if (assoc 'history state-list)
              '()
              (list new-history))))))

(define (prune-old-snapshots max-age-days state)
  "Remove snapshots older than max-age-days.
   Returns a new state with pruned history."
  (let* ((now (get-current-timestamp))
         (cutoff (add-days-to-date now (- max-age-days)))
         (history (get-history state))
         (snapshots (if history (cdr (assoc 'snapshots history)) '()))
         (pruned (filter
                  (lambda (s)
                    (string>=? (cdr (assoc 'timestamp s)) cutoff))
                  snapshots)))

    (if (not history)
        state
        (let ((new-history `(history (snapshots ,@pruned))))
          `(state
            ,@(map (lambda (section)
                     (if (and (pair? section)
                              (eq? (car section) 'history))
                         new-history
                         section))
                   (cdr state)))))))

;;; --------------------------------------------------
;;; Reporting
;;; --------------------------------------------------

(define (velocity-report state)
  "Generate a velocity report for all projects."
  (let ((projects (get-in-progress-projects state)))
    (display "=== Velocity Report ===\n\n")
    (for-each
     (lambda (p)
       (let* ((name (cdr (assoc 'name p)))
              (vel (project-velocity name state)))
         (format #t "~a: ~,2f%/day\n" name vel)))
     projects)
    (format #t "\nOverall velocity: ~,2f%/day\n" (overall-velocity state))))

(define (progress-report state)
  "Generate a progress report with estimates."
  (let ((estimates (estimate-all-completions state)))
    (display "=== Progress Report ===\n\n")
    (for-each
     (lambda (est)
       (format #t "~a:\n" (cdr (assoc 'project est)))
       (format #t "  Current: ~a%\n" (cdr (assoc 'current-completion est)))
       (format #t "  Velocity: ~,2f%/day\n" (cdr (assoc 'velocity-per-day est)))
       (format #t "  Days remaining: ~a\n" (cdr (assoc 'days-remaining est)))
       (format #t "  Est. completion: ~a\n\n" (cdr (assoc 'estimated-completion est))))
     estimates)))

(define (burndown-data project-name state)
  "Get burndown chart data for a project.
   Returns list of (timestamp remaining-work) for plotting."
  (let ((history (get-project-history project-name state)))
    (map (lambda (point)
           (cons (car point) (- 100 (cdr point))))
         history)))

;;; ==================================================
;;; END lib/state-history.scm
;;; ==================================================
