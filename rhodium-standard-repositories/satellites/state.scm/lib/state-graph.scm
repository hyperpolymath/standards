;;; ==================================================
;;; lib/state-graph.scm — Dependency Graph Visualization
;;; ==================================================
;;;
;;; SPDX-License-Identifier: MPL-2.0-or-later
;;; Copyright (c) 2025 Jonathan D.A. Jewell
;;;
;;; Generates GraphViz DOT and Mermaid diagrams from
;;; STATE project dependencies.
;;;
;;; ==================================================

(define-module (state-graph)
  #:use-module (state-core)
  #:use-module (ice-9 format)
  #:export (
    ;; GraphViz output
    generate-dot
    generate-dot-file
    dot-dependency-graph
    dot-status-graph
    dot-critical-path

    ;; Mermaid output
    generate-mermaid
    generate-mermaid-file
    mermaid-dependency-graph
    mermaid-status-graph
    mermaid-flowchart

    ;; Analysis
    build-adjacency-list
    topological-sort
    find-critical-path
    calculate-depth
  ))

;;; --------------------------------------------------
;;; Graph Analysis
;;; --------------------------------------------------

(define (build-adjacency-list state)
  "Build an adjacency list representation of project dependencies.
   Returns ((project-name . (list of dependencies)) ...)"
  (let ((projects (get-all-projects state)))
    (map (lambda (p)
           (cons (cdr (assoc 'name p))
                 (or (cdr (assoc 'dependencies p)) '())))
         projects)))

(define (reverse-adjacency-list adj)
  "Reverse the adjacency list (dependents instead of dependencies)."
  (let ((result '()))
    (for-each
     (lambda (entry)
       (let ((node (car entry))
             (deps (cdr entry)))
         ;; Ensure node exists in result
         (unless (assoc node result)
           (set! result (cons (cons node '()) result)))
         ;; Add reverse edges
         (for-each
          (lambda (dep)
            (let ((existing (assoc dep result)))
              (if existing
                  (set-cdr! existing (cons node (cdr existing)))
                  (set! result (cons (cons dep (list node)) result)))))
          deps)))
     adj)
    result))

(define (topological-sort state)
  "Topologically sort projects by dependencies.
   Returns a list where dependencies come before dependents."
  (let* ((adj (build-adjacency-list state))
         (in-degree (make-hash-table))
         (queue '())
         (result '()))

    ;; Initialize in-degrees
    (for-each
     (lambda (entry)
       (hash-set! in-degree (car entry) 0))
     adj)

    ;; Calculate in-degrees
    (for-each
     (lambda (entry)
       (for-each
        (lambda (dep)
          (hash-set! in-degree dep
                     (+ 1 (or (hash-ref in-degree dep) 0))))
        (cdr entry)))
     adj)

    ;; Find nodes with zero in-degree
    (hash-for-each
     (lambda (node degree)
       (when (zero? degree)
         (set! queue (cons node queue))))
     in-degree)

    ;; Process queue
    (while (not (null? queue))
      (let ((node (car queue)))
        (set! queue (cdr queue))
        (set! result (cons node result))
        ;; Reduce in-degree of neighbors
        (let ((entry (assoc node adj)))
          (when entry
            (for-each
             (lambda (dep)
               (let ((new-degree (- (hash-ref in-degree dep) 1)))
                 (hash-set! in-degree dep new-degree)
                 (when (zero? new-degree)
                   (set! queue (cons dep queue)))))
             (cdr entry))))))

    (reverse result)))

(define (find-critical-path target state)
  "Find the longest path to complete target project.
   Returns (path-list . total-length)."
  (let ((memo (make-hash-table)))

    (define (longest-path node)
      (or (hash-ref memo node)
          (let* ((deps (query-dependencies node state))
                 (result
                  (if (null? deps)
                      (cons (list node) 1)
                      (let ((sub-paths (map longest-path deps)))
                        (let ((best (car (sort sub-paths
                                               (lambda (a b)
                                                 (> (cdr a) (cdr b)))))))
                          (cons (cons node (car best))
                                (+ 1 (cdr best))))))))
            (hash-set! memo node result)
            result)))

    (longest-path target)))

(define (calculate-depth node state visited)
  "Calculate the depth of a node in the dependency graph."
  (if (member node visited)
      0  ; Cycle detected
      (let ((deps (query-dependencies node state)))
        (if (null? deps)
            0
            (+ 1 (apply max
                        (map (lambda (d)
                               (calculate-depth d state (cons node visited)))
                             deps)))))))

;;; --------------------------------------------------
;;; Status Colors
;;; --------------------------------------------------

(define (status->dot-color status)
  "Map project status to GraphViz color."
  (case (string->symbol status)
    ((in-progress) "lightblue")
    ((blocked) "lightcoral")
    ((paused) "lightyellow")
    ((complete) "lightgreen")
    ((abandoned) "lightgray")
    (else "white")))

(define (status->mermaid-class status)
  "Map project status to Mermaid CSS class."
  (case (string->symbol status)
    ((in-progress) "inProgress")
    ((blocked) "blocked")
    ((paused) "paused")
    ((complete) "complete")
    ((abandoned) "abandoned")
    (else "default")))

;;; --------------------------------------------------
;;; GraphViz DOT Generation
;;; --------------------------------------------------

(define (sanitize-id name)
  "Sanitize a name for use as a DOT identifier."
  (string-map
   (lambda (c)
     (if (or (char-alphabetic? c)
             (char-numeric? c)
             (char=? c #\_))
         c
         #\_))
   name))

(define (dot-node name status completion)
  "Generate a DOT node definition."
  (format #f "  ~a [label=\"~a\\n~a%\" style=filled fillcolor=\"~a\"];\n"
          (sanitize-id name)
          name
          completion
          (status->dot-color status)))

(define (dot-edge from to)
  "Generate a DOT edge definition."
  (format #f "  ~a -> ~a;\n"
          (sanitize-id from)
          (sanitize-id to)))

(define (generate-dot state #:optional (title "STATE Dependency Graph"))
  "Generate complete GraphViz DOT source."
  (let ((projects (get-all-projects state))
        (output ""))

    ;; Header
    (set! output
      (string-append output
        (format #f "digraph STATE {\n")
        (format #f "  label=\"~a\";\n" title)
        (format #f "  labelloc=t;\n")
        (format #f "  rankdir=BT;\n")  ; Bottom to top (deps at bottom)
        (format #f "  node [shape=box fontname=\"Helvetica\"];\n")
        (format #f "  edge [color=gray50];\n\n")))

    ;; Nodes
    (for-each
     (lambda (p)
       (set! output
         (string-append output
           (dot-node (cdr (assoc 'name p))
                     (cdr (assoc 'status p))
                     (cdr (assoc 'completion p))))))
     projects)

    (set! output (string-append output "\n"))

    ;; Edges
    (for-each
     (lambda (p)
       (let ((name (cdr (assoc 'name p)))
             (deps (cdr (assoc 'dependencies p))))
         (for-each
          (lambda (dep)
            (set! output
              (string-append output (dot-edge name dep))))
          (or deps '()))))
     projects)

    ;; Footer
    (set! output (string-append output "}\n"))
    output))

(define (dot-dependency-graph state)
  "Generate DOT showing only dependency relationships."
  (generate-dot state "Project Dependencies"))

(define (dot-status-graph state)
  "Generate DOT with status-based subgraphs."
  (let ((output "")
        (projects (get-all-projects state)))

    (set! output
      (string-append output
        "digraph STATE {\n"
        "  label=\"Projects by Status\";\n"
        "  labelloc=t;\n"
        "  rankdir=LR;\n"
        "  node [shape=box fontname=\"Helvetica\"];\n\n"))

    ;; Create subgraphs by status
    (for-each
     (lambda (status)
       (let ((status-projects (query-by-status status state)))
         (when (not (null? status-projects))
           (set! output
             (string-append output
               (format #f "  subgraph cluster_~a {\n" status)
               (format #f "    label=\"~a\";\n" (string-titlecase status))
               (format #f "    style=filled;\n")
               (format #f "    color=\"~a\";\n" (status->dot-color status))))
           (for-each
            (lambda (p)
              (set! output
                (string-append output
                  (format #f "    ~a [label=\"~a\"];\n"
                          (sanitize-id (cdr (assoc 'name p)))
                          (cdr (assoc 'name p))))))
            status-projects)
           (set! output (string-append output "  }\n\n")))))
     '("in-progress" "blocked" "paused" "complete" "abandoned"))

    (set! output (string-append output "}\n"))
    output))

(define (dot-critical-path target state)
  "Generate DOT highlighting the critical path to target."
  (let* ((path-info (find-critical-path target state))
         (path (car path-info))
         (output (generate-dot state (format #f "Critical Path to ~a" target))))

    ;; Add highlighting for critical path nodes
    (set! output
      (string-append
       (substring output 0 (- (string-length output) 2))  ; Remove closing }
       "\n  // Critical path highlighting\n"))

    (for-each
     (lambda (node)
       (set! output
         (string-append output
           (format #f "  ~a [penwidth=3 color=red];\n"
                   (sanitize-id node)))))
     path)

    (set! output (string-append output "}\n"))
    output))

(define (generate-dot-file state filename)
  "Write DOT output to a file."
  (call-with-output-file filename
    (lambda (port)
      (display (generate-dot state) port))))

;;; --------------------------------------------------
;;; Mermaid Generation
;;; --------------------------------------------------

(define (mermaid-node-id name)
  "Generate a valid Mermaid node ID."
  (string-map
   (lambda (c)
     (if (or (char-alphabetic? c)
             (char-numeric? c))
         c
         #\_))
   name))

(define (generate-mermaid state #:optional (title "STATE Dependency Graph"))
  "Generate Mermaid diagram source."
  (let ((projects (get-all-projects state))
        (output ""))

    ;; Header
    (set! output
      (string-append output
        "```mermaid\n"
        "flowchart BT\n"
        (format #f "    subgraph ~a\n" title)))

    ;; Nodes with status classes
    (for-each
     (lambda (p)
       (let ((name (cdr (assoc 'name p)))
             (status (cdr (assoc 'status p)))
             (completion (cdr (assoc 'completion p))))
         (set! output
           (string-append output
             (format #f "    ~a[\"~a<br/>~a%\"]:::~a\n"
                     (mermaid-node-id name)
                     name
                     completion
                     (status->mermaid-class status))))))
     projects)

    (set! output (string-append output "    end\n\n"))

    ;; Edges
    (for-each
     (lambda (p)
       (let ((name (cdr (assoc 'name p)))
             (deps (cdr (assoc 'dependencies p))))
         (for-each
          (lambda (dep)
            (set! output
              (string-append output
                (format #f "    ~a --> ~a\n"
                        (mermaid-node-id name)
                        (mermaid-node-id dep)))))
          (or deps '()))))
     projects)

    ;; Style definitions
    (set! output
      (string-append output
        "\n"
        "    classDef inProgress fill:#add8e6,stroke:#333\n"
        "    classDef blocked fill:#f08080,stroke:#333\n"
        "    classDef paused fill:#ffffe0,stroke:#333\n"
        "    classDef complete fill:#90ee90,stroke:#333\n"
        "    classDef abandoned fill:#d3d3d3,stroke:#333\n"
        "```\n"))

    output))

(define (mermaid-dependency-graph state)
  "Generate Mermaid showing dependencies."
  (generate-mermaid state "Dependencies"))

(define (mermaid-status-graph state)
  "Generate Mermaid with status-based grouping."
  (let ((output "")
        (projects (get-all-projects state)))

    (set! output "```mermaid\nflowchart LR\n")

    ;; Create subgraphs by status
    (for-each
     (lambda (status)
       (let ((status-projects (query-by-status status state)))
         (when (not (null? status-projects))
           (set! output
             (string-append output
               (format #f "    subgraph ~a\n" (string-titlecase status))))
           (for-each
            (lambda (p)
              (set! output
                (string-append output
                  (format #f "        ~a[\"~a\"]:::~a\n"
                          (mermaid-node-id (cdr (assoc 'name p)))
                          (cdr (assoc 'name p))
                          (status->mermaid-class status)))))
            status-projects)
           (set! output (string-append output "    end\n")))))
     '("in-progress" "blocked" "paused" "complete" "abandoned"))

    ;; Style definitions
    (set! output
      (string-append output
        "\n"
        "    classDef inProgress fill:#add8e6\n"
        "    classDef blocked fill:#f08080\n"
        "    classDef paused fill:#ffffe0\n"
        "    classDef complete fill:#90ee90\n"
        "    classDef abandoned fill:#d3d3d3\n"
        "```\n"))

    output))

(define (mermaid-flowchart state target)
  "Generate Mermaid flowchart showing path to target."
  (let* ((path-info (find-critical-path target state))
         (path (car path-info))
         (output ""))

    (set! output
      (string-append output
        "```mermaid\n"
        "flowchart BT\n"
        (format #f "    subgraph \"Critical Path to ~a\"\n" target)))

    ;; Path nodes
    (for-each
     (lambda (node)
       (let ((project (get-project-by-name state node)))
         (set! output
           (string-append output
             (format #f "        ~a[\"~a\"]:::critical\n"
                     (mermaid-node-id node) node)))))
     path)

    (set! output (string-append output "    end\n\n"))

    ;; Path edges
    (let loop ((nodes path))
      (when (and (not (null? nodes))
                 (not (null? (cdr nodes))))
        (set! output
          (string-append output
            (format #f "    ~a --> ~a\n"
                    (mermaid-node-id (car nodes))
                    (mermaid-node-id (cadr nodes)))))
        (loop (cdr nodes))))

    (set! output
      (string-append output
        "\n    classDef critical fill:#ff6b6b,stroke:#c92a2a,stroke-width:2px\n"
        "```\n"))

    output))

(define (generate-mermaid-file state filename)
  "Write Mermaid output to a file."
  (call-with-output-file filename
    (lambda (port)
      (display (generate-mermaid state) port))))

;;; ==================================================
;;; END lib/state-graph.scm
;;; ==================================================
