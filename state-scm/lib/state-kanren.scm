;;; ==================================================
;;; lib/state-kanren.scm — minikanren Integration
;;; ==================================================
;;;
;;; SPDX-License-Identifier: PMPL-1.0-or-later
;;; Copyright (c) 2025 Jonathan D.A. Jewell
;;;
;;; Relational/logic programming for STATE queries.
;;; Attempts to use Guile's minikanren if available,
;;; falls back to simplified implementation otherwise.
;;;
;;; ==================================================

(define-module (state-kanren)
  #:use-module (state-core)
  #:export (
    ;; Core minikanren forms
    run*
    run
    fresh
    conde
    ==
    succeed
    fail
    conj
    disj

    ;; STATE-specific goals
    projecto
    statuso
    dependso
    blockso
    categoryo
    completiono

    ;; Relational queries
    blocked-projects-o
    dependency-chain-o
    critical-path-o

    ;; Utility
    minikanren-available?
  ))

;;; --------------------------------------------------
;;; Try to load real minikanren
;;; --------------------------------------------------

(define minikanren-loaded? #f)

;; Attempt to load Guile's minikanren
(catch #t
  (lambda ()
    (use-modules (minikanren))
    (set! minikanren-loaded? #t))
  (lambda (key . args)
    ;; minikanren not available, use fallback
    (set! minikanren-loaded? #f)))

(define (minikanren-available?)
  "Check if real minikanren is loaded."
  minikanren-loaded?)

;;; --------------------------------------------------
;;; Fallback minikanren Implementation
;;; --------------------------------------------------
;;;
;;; This provides a simplified but functional subset of
;;; minikanren for environments without the full library.

(define (succeed s) (list s))
(define (fail s) '())

(define (== x y)
  "Unification goal: succeeds if x equals y."
  (lambda (s)
    (let ((x-val (walk x s))
          (y-val (walk y s)))
      (cond
        ((equal? x-val y-val) (succeed s))
        ((var? x-val) (succeed (extend-s x-val y-val s)))
        ((var? y-val) (succeed (extend-s y-val x-val s)))
        (else (fail s))))))

(define (disj g1 g2)
  "Disjunction (OR): either g1 or g2 succeeds."
  (lambda (s)
    (append (g1 s) (g2 s))))

(define (conj g1 g2)
  "Conjunction (AND): both g1 and g2 must succeed."
  (lambda (s)
    (apply append (map g2 (g1 s)))))

;;; Logic variable support
(define var-counter 0)

(define (make-var name)
  "Create a fresh logic variable."
  (set! var-counter (+ var-counter 1))
  (cons 'var (cons name var-counter)))

(define (var? x)
  "Check if x is a logic variable."
  (and (pair? x) (eq? (car x) 'var)))

(define (walk v s)
  "Walk a term to its value in substitution s."
  (if (var? v)
      (let ((a (assoc v s)))
        (if a (walk (cdr a) s) v))
      v))

(define (extend-s x v s)
  "Extend substitution s with x -> v."
  (cons (cons x v) s))

;;; Macros for minikanren forms

(define-syntax fresh
  (syntax-rules ()
    "Introduce fresh logic variables."
    ((_ () g ...)
     (conj* g ...))
    ((_ (x rest ...) g ...)
     (let ((x (make-var 'x)))
       (fresh (rest ...) g ...)))))

(define-syntax conde
  (syntax-rules ()
    "Conditional with multiple clauses (disjunction of conjunctions)."
    ((_ (g0 g ...) ...)
     (disj* (conj* g0 g ...) ...))))

(define-syntax run*
  (syntax-rules ()
    "Run a query and return all results."
    ((_ (q) g ...)
     (let ((q (make-var 'q)))
       (map (lambda (s) (reify q s))
            ((conj* g ...) '()))))))

(define-syntax run
  (syntax-rules ()
    "Run a query and return at most n results."
    ((_ n (q) g ...)
     (take n (run* (q) g ...)))))

;;; Helper macros
(define-syntax conj*
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (conj g0 (conj* g ...)))))

(define-syntax disj*
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (disj g0 (disj* g ...)))))

(define (take n lst)
  "Take first n elements of list."
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (reify v s)
  "Reify a term: replace logic variables with their values."
  (let ((v (walk v s)))
    (cond
      ((var? v) v)  ; unbound variable
      ((pair? v) (cons (reify (car v) s) (reify (cdr v) s)))
      (else v))))

;;; --------------------------------------------------
;;; STATE-Specific Goals
;;; --------------------------------------------------

(define (projecto name state)
  "Goal: project with given name exists."
  (lambda (s)
    (let ((name-val (walk name s)))
      (if (var? name-val)
          ;; Enumerate all project names
          (let ((projects (get-all-projects state)))
            (apply append
                   (map (lambda (p)
                          ((== name (cdr (assoc 'name p))) s))
                        projects)))
          ;; Check if specific project exists
          (if (get-project-by-name state name-val)
              (succeed s)
              (fail s))))))

(define (statuso name status state)
  "Goal: project has given status."
  (lambda (s)
    (let ((name-val (walk name s))
          (status-val (walk status s)))
      (cond
        ;; Both bound: verify
        ((and (not (var? name-val)) (not (var? status-val)))
         (let ((project (get-project-by-name state name-val)))
           (if (and project
                    (equal? (cdr (assoc 'status project)) status-val))
               (succeed s)
               (fail s))))
        ;; Name bound, status unbound: look up status
        ((not (var? name-val))
         (let ((project (get-project-by-name state name-val)))
           (if project
               ((== status (cdr (assoc 'status project))) s)
               (fail s))))
        ;; Status bound, name unbound: find all with that status
        ((not (var? status-val))
         (let ((projects (query-by-status status-val state)))
           (apply append
                  (map (lambda (p)
                         ((== name (cdr (assoc 'name p))) s))
                       projects))))
        ;; Both unbound: enumerate all
        (else
         (let ((projects (get-all-projects state)))
           (apply append
                  (map (lambda (p)
                         ((conj (== name (cdr (assoc 'name p)))
                                (== status (cdr (assoc 'status p)))) s))
                       projects))))))))

(define (dependso child parent state)
  "Goal: child project depends on parent project."
  (lambda (s)
    (let ((child-val (walk child s))
          (parent-val (walk parent s)))
      (cond
        ;; Both bound: verify dependency
        ((and (not (var? child-val)) (not (var? parent-val)))
         (let ((deps (query-dependencies child-val state)))
           (if (member parent-val deps)
               (succeed s)
               (fail s))))
        ;; Child bound: enumerate its dependencies
        ((not (var? child-val))
         (let ((deps (query-dependencies child-val state)))
           (apply append
                  (map (lambda (d) ((== parent d) s)) deps))))
        ;; Parent bound: find all dependents
        ((not (var? parent-val))
         (let ((dependents (query-dependents parent-val state)))
           (apply append
                  (map (lambda (p)
                         ((== child (cdr (assoc 'name p))) s))
                       dependents))))
        ;; Both unbound: enumerate all dependency relationships
        (else
         (let ((projects (get-all-projects state)))
           (apply append
                  (map (lambda (p)
                         (let ((name (cdr (assoc 'name p)))
                               (deps (cdr (assoc 'dependencies p))))
                           (apply append
                                  (map (lambda (d)
                                         ((conj (== child name)
                                                (== parent d)) s))
                                       deps))))
                       projects))))))))

(define (blockso name blocker state)
  "Goal: project has a specific blocker."
  (lambda (s)
    (let ((name-val (walk name s)))
      (if (var? name-val)
          ;; Enumerate all project/blocker pairs
          (let ((projects (get-all-projects state)))
            (apply append
                   (map (lambda (p)
                          (let ((pname (cdr (assoc 'name p)))
                                (blockers (cdr (assoc 'blockers p))))
                            (apply append
                                   (map (lambda (b)
                                          ((conj (== name pname)
                                                 (== blocker b)) s))
                                        (if (list? blockers) blockers '())))))
                        projects)))
          ;; Name bound: enumerate blockers
          (let ((project (get-project-by-name state name-val)))
            (if project
                (let ((blockers (cdr (assoc 'blockers project))))
                  (apply append
                         (map (lambda (b) ((== blocker b) s))
                              (if (list? blockers) blockers '()))))
                (fail s)))))))

(define (categoryo name category state)
  "Goal: project is in given category."
  (lambda (s)
    (let ((name-val (walk name s))
          (cat-val (walk category s)))
      (cond
        ((and (not (var? name-val)) (not (var? cat-val)))
         (let ((project (get-project-by-name state name-val)))
           (if (and project
                    (equal? (cdr (assoc 'category project)) cat-val))
               (succeed s)
               (fail s))))
        ((not (var? name-val))
         (let ((project (get-project-by-name state name-val)))
           (if project
               ((== category (cdr (assoc 'category project))) s)
               (fail s))))
        ((not (var? cat-val))
         (let ((projects (query-by-category cat-val state)))
           (apply append
                  (map (lambda (p)
                         ((== name (cdr (assoc 'name p))) s))
                       projects))))
        (else (fail s))))))

(define (completiono name pct state)
  "Goal: project has given completion percentage."
  (lambda (s)
    (let ((name-val (walk name s)))
      (if (var? name-val)
          (fail s)  ; Need project name
          (let ((project (get-project-by-name state name-val)))
            (if project
                ((== pct (cdr (assoc 'completion project))) s)
                (fail s)))))))

;;; --------------------------------------------------
;;; Relational Queries
;;; --------------------------------------------------

(define (blocked-projects-o q state)
  "Goal: q is a blocked project name."
  (statuso q "blocked" state))

(define (dependency-chain-o start end chain state)
  "Goal: chain is a list of projects from start to end via dependencies.
   This is a simplified version - full transitive closure is complex."
  (conde
    ;; Direct dependency
    ((dependso start end state)
     (== chain (list start end)))
    ;; One intermediate
    ((fresh (mid)
       (dependso start mid state)
       (dependso mid end state)
       (== chain (list start mid end))))))

(define (critical-path-o project path state)
  "Goal: path is the critical path for completing project.
   Returns the project and all its transitive dependencies."
  (lambda (s)
    (let ((proj-val (walk project s)))
      (if (var? proj-val)
          (fail s)
          (let ((deps (transitive-deps proj-val state '())))
            ((== path (cons proj-val deps)) s))))))

;;; Helper for critical path
(define (transitive-deps name state visited)
  "Get all transitive dependencies, avoiding cycles."
  (if (member name visited)
      '()
      (let ((deps (query-dependencies name state)))
        (if (null? deps)
            '()
            (append deps
                    (apply append
                           (map (lambda (d)
                                  (transitive-deps d state (cons name visited)))
                                deps)))))))

;;; ==================================================
;;; END lib/state-kanren.scm
;;; ==================================================
