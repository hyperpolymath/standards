#!/usr/bin/env guile
!#
;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath
;;
;; RSR Compliance Checker - Machine-Readable Validation
;; Usage: guile -L spec.scm rsr-check.scm [repo-path]
;;
;; Validates repositories against RSR v1.0 (FROZEN 2025-12-27)

(add-to-load-path ".")

(use-modules (ice-9 ftw)
             (ice-9 regex)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 match)
             (srfi srfi-1)
             (rsr version)
             (rsr tiers)
             (rsr language-policy)
             (rsr compliance-criteria))

;; =============================================================================
;; Configuration
;; =============================================================================

(define *repo-path* (if (> (length (command-line)) 1)
                        (cadr (command-line))
                        "."))

(define *verbose* #f)
(define *output-format* 'text)  ; 'text | 'json | 'sexp

;; =============================================================================
;; Utility Functions
;; =============================================================================

(define (file-exists? path)
  (access? path R_OK))

(define (dir-exists? path)
  (and (access? path R_OK)
       (eq? 'directory (stat:type (stat path)))))

(define (repo-file path)
  "Construct full path from repo root"
  (string-append *repo-path* "/" path))

(define (file-contains? path pattern)
  "Check if file contains pattern (case-insensitive)"
  (let ((full-path (repo-file path)))
    (and (file-exists? full-path)
         (call-with-input-file full-path
           (lambda (port)
             (let loop ()
               (let ((line (read-line port)))
                 (cond
                   ((eof-object? line) #f)
                   ((string-match pattern line) #t)
                   (else (loop))))))))))

(define (count-files-with-extension dir ext)
  "Count files with given extension in directory"
  (if (not (dir-exists? (repo-file dir)))
      0
      (let ((count 0))
        (ftw (repo-file dir)
             (lambda (filename statinfo flag)
               (when (and (eq? flag 'regular)
                          (string-suffix? ext filename))
                 (set! count (1+ count)))
               #t))
        count)))

(define (count-files-with-spdx dir ext)
  "Count files with SPDX headers"
  (if (not (dir-exists? (repo-file dir)))
      0
      (let ((count 0))
        (ftw (repo-file dir)
             (lambda (filename statinfo flag)
               (when (and (eq? flag 'regular)
                          (string-suffix? ext filename)
                          (file-contains-spdx? filename))
                 (set! count (1+ count)))
               #t))
        count)))

(define (file-contains-spdx? path)
  "Check if file has SPDX header"
  (and (file-exists? path)
       (call-with-input-file path
         (lambda (port)
           (let loop ((lines-checked 0))
             (if (> lines-checked 10)
                 #f  ; Only check first 10 lines
                 (let ((line (read-line port)))
                   (cond
                     ((eof-object? line) #f)
                     ((string-match "SPDX-License-Identifier" line) #t)
                     (else (loop (1+ lines-checked)))))))))))

;; =============================================================================
;; Check Result Tracking
;; =============================================================================

(define *results* '())
(define *total-checks* 0)
(define *passed-checks* 0)

(define (reset-results!)
  (set! *results* '())
  (set! *total-checks* 0)
  (set! *passed-checks* 0))

(define (record-check! category-id criterion-id description passed?)
  "Record a check result"
  (set! *total-checks* (1+ *total-checks*))
  (when passed?
    (set! *passed-checks* (1+ *passed-checks*)))
  (set! *results*
        (cons (list 'check
                    (cons 'category category-id)
                    (cons 'criterion criterion-id)
                    (cons 'description description)
                    (cons 'passed passed?))
              *results*)))

(define (check category-id criterion-id description test-thunk)
  "Run a check and record result"
  (let ((passed? (catch #t
                   test-thunk
                   (lambda _ #f))))
    (record-check! category-id criterion-id description passed?)
    passed?))

;; =============================================================================
;; Category 1: Foundational Infrastructure (15%)
;; =============================================================================

(define (check-category-1)
  "Foundational Infrastructure checks"

  ;; 1.1.1 - Nix flake
  (check 1 "1.1.1" "flake.nix present"
         (lambda () (file-exists? (repo-file "flake.nix"))))

  (check 1 "1.1.1b" "flake.lock present"
         (lambda () (file-exists? (repo-file "flake.lock"))))

  ;; 1.1.3 - Justfile
  (check 1 "1.1.3" "Justfile present"
         (lambda () (file-exists? (repo-file "justfile"))))

  (check 1 "1.1.3b" "Justfile has build recipe"
         (lambda () (file-contains? "justfile" "build")))

  (check 1 "1.1.3c" "Justfile has test recipe"
         (lambda () (file-contains? "justfile" "test")))

  (check 1 "1.1.3d" "Justfile has validate recipe"
         (lambda () (file-contains? "justfile" "validate")))

  ;; 1.2.1 - GitLab CI/CD
  (check 1 "1.2.1" "CI/CD configuration present"
         (lambda () (or (file-exists? (repo-file ".gitlab-ci.yml"))
                        (dir-exists? (repo-file ".github/workflows")))))

  ;; Git configuration
  (check 1 "git.1" ".gitignore present"
         (lambda () (file-exists? (repo-file ".gitignore"))))

  (check 1 "git.2" ".gitattributes present"
         (lambda () (file-exists? (repo-file ".gitattributes")))))

;; =============================================================================
;; Category 2: Documentation Standards (10%)
;; =============================================================================

(define (check-category-2)
  "Documentation Standards checks"

  ;; 2.1.1 - README
  (check 2 "2.1.1" "README present"
         (lambda () (or (file-exists? (repo-file "README.md"))
                        (file-exists? (repo-file "README.adoc")))))

  ;; 2.1.2 - LICENSE.txt
  (check 2 "2.1.2" "LICENSE.txt present (plain text)"
         (lambda () (file-exists? (repo-file "LICENSE.txt"))))

  ;; 2.1.3 - SECURITY.md
  (check 2 "2.1.3" "SECURITY.md present"
         (lambda () (file-exists? (repo-file "SECURITY.md"))))

  ;; 2.1.4 - Code of Conduct
  (check 2 "2.1.4" "CODE_OF_CONDUCT present"
         (lambda () (or (file-exists? (repo-file "CODE_OF_CONDUCT.md"))
                        (file-exists? (repo-file "CODE_OF_CONDUCT.adoc")))))

  ;; 2.1.5 - Contributing
  (check 2 "2.1.5" "CONTRIBUTING present"
         (lambda () (or (file-exists? (repo-file "CONTRIBUTING.md"))
                        (file-exists? (repo-file "CONTRIBUTING.adoc")))))

  ;; 2.1.6 - Funding
  (check 2 "2.1.6" "FUNDING.yml present"
         (lambda () (or (file-exists? (repo-file "FUNDING.yml"))
                        (file-exists? (repo-file ".github/FUNDING.yml")))))

  ;; 2.1.7 - Governance
  (check 2 "2.1.7" "GOVERNANCE.adoc present"
         (lambda () (or (file-exists? (repo-file "GOVERNANCE.adoc"))
                        (file-exists? (repo-file "GOVERNANCE.md")))))

  ;; 2.1.8 - Maintainers
  (check 2 "2.1.8" "MAINTAINERS.md present"
         (lambda () (file-exists? (repo-file "MAINTAINERS.md"))))

  ;; 2.1.9-10 - Git files
  (check 2 "2.1.9" ".gitignore present"
         (lambda () (file-exists? (repo-file ".gitignore"))))

  (check 2 "2.1.10" ".gitattributes present"
         (lambda () (file-exists? (repo-file ".gitattributes"))))

  ;; 2.2.x - .well-known directory
  (check 2 "2.2.0" ".well-known/ directory present"
         (lambda () (dir-exists? (repo-file ".well-known"))))

  (check 2 "2.2.1" ".well-known/security.txt present"
         (lambda () (file-exists? (repo-file ".well-known/security.txt"))))

  (check 2 "2.2.2" ".well-known/ai.txt present"
         (lambda () (file-exists? (repo-file ".well-known/ai.txt"))))

  (check 2 "2.2.5" ".well-known/humans.txt present"
         (lambda () (file-exists? (repo-file ".well-known/humans.txt"))))

  ;; 2.3.x - SCM metadata files (Gold tier)
  (check 2 "2.3.1" "META.scm present"
         (lambda () (file-exists? (repo-file "META.scm"))))

  (check 2 "2.3.2" "ECOSYSTEM.scm present"
         (lambda () (file-exists? (repo-file "ECOSYSTEM.scm"))))

  (check 2 "2.3.3" "STATE.scm present"
         (lambda () (file-exists? (repo-file "STATE.scm")))))

;; =============================================================================
;; Category 3: Security Architecture (15%)
;; =============================================================================

(define (check-category-3)
  "Security Architecture checks"

  ;; 3.4.1 - SPDX headers
  (check 3 "3.4.1" "LICENSE.txt has SPDX identifier"
         (lambda () (file-contains? "LICENSE.txt" "SPDX-License-Identifier")))

  ;; Check for type-safe languages
  (check 3 "3.1.1" "Type-safe language used (Rust/Elixir/Ada/ReScript)"
         (lambda () (or (file-exists? (repo-file "Cargo.toml"))
                        (file-exists? (repo-file "mix.exs"))
                        (file-exists? (repo-file "rescript.json"))
                        (file-exists? (repo-file "alire.toml"))
                        (file-exists? (repo-file "*.cabal")))))

  ;; No node_modules
  (check 3 "3.5.1" "No node_modules/ (post-JavaScript)"
         (lambda () (not (dir-exists? (repo-file "node_modules")))))

  ;; SECURITY.md content
  (check 3 "sec.1" "SECURITY.md has vulnerability reporting"
         (lambda () (file-contains? "SECURITY.md" "[Rr]eport")))

  (check 3 "sec.2" "SECURITY.md has response timeline"
         (lambda () (file-contains? "SECURITY.md" "[0-9]+ hours?"))))

;; =============================================================================
;; Category 4: Architecture Principles (10%)
;; =============================================================================

(define (check-category-4)
  "Architecture Principles checks"

  ;; Git-based reversibility
  (check 4 "4.1.2" "Git repository (reversibility)"
         (lambda () (dir-exists? (repo-file ".git"))))

  ;; Reproducible builds
  (check 4 "4.2.1" "Nix flakes (reproducible builds)"
         (lambda () (file-exists? (repo-file "flake.nix")))))

;; =============================================================================
;; Category 5: Web Standards & Protocols (10%)
;; =============================================================================

(define (check-category-5)
  "Web Standards & Protocols checks"

  ;; .well-known directory
  (check 5 "5.1.1" ".well-known/ directory present"
         (lambda () (dir-exists? (repo-file ".well-known"))))

  ;; security.txt RFC 9116 compliance
  (check 5 "5.1.2" "security.txt has Contact field"
         (lambda () (file-contains? ".well-known/security.txt" "Contact:")))

  (check 5 "5.1.3" "security.txt has Expires field"
         (lambda () (file-contains? ".well-known/security.txt" "Expires:"))))

;; =============================================================================
;; Category 7: FOSS & Licensing (10%)
;; =============================================================================

(define (check-category-7)
  "FOSS & Licensing checks"

  ;; LICENSE.txt format
  (check 7 "7.1.1" "LICENSE.txt (not .md)"
         (lambda () (and (file-exists? (repo-file "LICENSE.txt"))
                         (not (file-exists? (repo-file "LICENSE.md"))))))

  (check 7 "7.1.2" "SPDX identifier in LICENSE"
         (lambda () (file-contains? "LICENSE.txt" "SPDX-License-Identifier")))

  (check 7 "7.2.1" "MIT license included"
         (lambda () (file-contains? "LICENSE.txt" "MIT")))

  (check 7 "7.2.2" "Palimpsest license included"
         (lambda () (file-contains? "LICENSE.txt" "Palimpsest"))))

;; =============================================================================
;; Category 10: Community & Governance (10%)
;; =============================================================================

(define (check-category-10)
  "Community & Governance checks"

  ;; Code of Conduct
  (check 10 "10.1.1" "CODE_OF_CONDUCT present"
         (lambda () (or (file-exists? (repo-file "CODE_OF_CONDUCT.md"))
                        (file-exists? (repo-file "CODE_OF_CONDUCT.adoc")))))

  (check 10 "10.1.2" "CoC addresses harassment"
         (lambda () (or (file-contains? "CODE_OF_CONDUCT.md" "[Hh]arass")
                        (file-contains? "CODE_OF_CONDUCT.adoc" "[Hh]arass"))))

  ;; Governance
  (check 10 "10.2.1" "GOVERNANCE document present"
         (lambda () (or (file-exists? (repo-file "GOVERNANCE.adoc"))
                        (file-exists? (repo-file "GOVERNANCE.md")))))

  ;; TPCF framework
  (check 10 "10.3.1" "TPCF mentioned in CONTRIBUTING"
         (lambda () (or (file-contains? "CONTRIBUTING.md" "TPCF\\|[Pp]erimeter")
                        (file-contains? "CONTRIBUTING.adoc" "TPCF\\|[Pp]erimeter")))))

;; =============================================================================
;; Scoring & Tier Calculation
;; =============================================================================

(define (calculate-score)
  "Calculate compliance percentage"
  (if (= *total-checks* 0)
      0
      (* 100.0 (/ *passed-checks* *total-checks*))))

(define (determine-tier score)
  "Determine compliance tier based on score"
  (get-tier-for-score (inexact->exact (round score)) #f))

;; =============================================================================
;; Output Formatting
;; =============================================================================

(define (output-text-report)
  "Generate text report"
  (let* ((score (calculate-score))
         (tier (determine-tier score)))

    (format #t "~%")
    (format #t "═══════════════════════════════════════════════════════════════~%")
    (format #t "  RSR v~a COMPLIANCE CHECK~%" (version-string))
    (format #t "  Frozen: ~a~%" (freeze-date))
    (format #t "═══════════════════════════════════════════════════════════════~%")
    (format #t "~%")
    (format #t "  Repository: ~a~%" *repo-path*)
    (format #t "  Total Checks: ~a~%" *total-checks*)
    (format #t "  Passed: ~a~%" *passed-checks*)
    (format #t "  Failed: ~a~%" (- *total-checks* *passed-checks*))
    (format #t "  Score: ~,1f%~%" score)
    (format #t "~%")

    (case tier
      ((rhodium)
       (format #t "  COMPLIANCE LEVEL: RHODIUM~%")
       (format #t "  Exemplary RSR compliance!~%"))
      ((gold)
       (format #t "  COMPLIANCE LEVEL: GOLD~%")
       (format #t "  Full RSR v1.0 compliance!~%"))
      ((silver)
       (format #t "  COMPLIANCE LEVEL: SILVER~%")
       (format #t "  Strong RSR compliance.~%"))
      ((bronze)
       (format #t "  COMPLIANCE LEVEL: BRONZE~%")
       (format #t "  Basic RSR compliance.~%"))
      (else
       (format #t "  NON-COMPLIANT~%")
       (format #t "  Does not meet RSR v1.0 minimum (75%).~%")))

    (format #t "~%")
    (format #t "═══════════════════════════════════════════════════════════════~%")

    ;; Show failed checks
    (let ((failed (filter (lambda (r)
                            (and (eq? (car r) 'check)
                                 (not (assoc-ref (cdr r) 'passed))))
                          *results*)))
      (when (> (length failed) 0)
        (format #t "~%  FAILED CHECKS:~%")
        (for-each
          (lambda (f)
            (format #t "    - [~a] ~a~%"
                    (assoc-ref (cdr f) 'criterion)
                    (assoc-ref (cdr f) 'description)))
          failed)))

    (format #t "~%")))

(define (output-json-report)
  "Generate JSON report"
  (let* ((score (calculate-score))
         (tier (determine-tier score)))
    (format #t "{~%")
    (format #t "  \"spec_version\": \"~a\",~%" (version-string))
    (format #t "  \"freeze_date\": \"~a\",~%" (freeze-date))
    (format #t "  \"repository\": \"~a\",~%" *repo-path*)
    (format #t "  \"total_checks\": ~a,~%" *total-checks*)
    (format #t "  \"passed_checks\": ~a,~%" *passed-checks*)
    (format #t "  \"score\": ~,2f,~%" score)
    (format #t "  \"tier\": \"~a\",~%" (or tier "non-compliant"))
    (format #t "  \"compliant\": ~a~%" (if (>= score 75) "true" "false"))
    (format #t "}~%")))

(define (output-sexp-report)
  "Generate S-expression report"
  (let* ((score (calculate-score))
         (tier (determine-tier score)))
    (write `((spec-version . ,(version-string))
             (freeze-date . ,(freeze-date))
             (repository . ,*repo-path*)
             (total-checks . ,*total-checks*)
             (passed-checks . ,*passed-checks*)
             (score . ,score)
             (tier . ,tier)
             (compliant . ,(>= score 75))
             (results . ,*results*)))
    (newline)))

;; =============================================================================
;; Main Entry Point
;; =============================================================================

(define (run-all-checks)
  "Run all compliance checks"
  (reset-results!)

  (check-category-1)   ; Foundational Infrastructure
  (check-category-2)   ; Documentation Standards
  (check-category-3)   ; Security Architecture
  (check-category-4)   ; Architecture Principles
  (check-category-5)   ; Web Standards
  (check-category-7)   ; FOSS & Licensing
  (check-category-10)) ; Community & Governance

(define (main)
  (run-all-checks)

  (case *output-format*
    ((json) (output-json-report))
    ((sexp) (output-sexp-report))
    (else (output-text-report)))

  ;; Exit with appropriate code
  (let ((score (calculate-score)))
    (cond
      ((>= score 100) (exit 0))  ; Gold/Rhodium
      ((>= score 90) (exit 1))   ; Silver
      ((>= score 75) (exit 2))   ; Bronze
      (else (exit 3)))))         ; Non-compliant

;; Run main
(main)
