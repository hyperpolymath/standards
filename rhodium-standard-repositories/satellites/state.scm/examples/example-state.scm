;;; ==================================================
;;; example-state.scm — Sample STATE.scm File
;;; ==================================================
;;;
;;; SPDX-License-Identifier: MPL-2.0-or-later
;;; Copyright (c) 2025 Jonathan D.A. Jewell
;;;
;;; This is an example showing a realistic state checkpoint
;;; with multiple projects, dependencies, history, and session context.
;;;
;;; ==================================================

(define state
  '((metadata
      (format-version . "2.0")
      (schema-version . "2025-12-06")
      (created-at . "2025-12-01T10:00:00Z")
      (last-updated . "2025-12-06T15:30:00Z")
      (generator . "Claude/STATE-system"))

    (user
      (name . "Jonathan D.A. Jewell")
      (roles . ("PhD researcher" "Associate Lecturer" "NUJ leadership"))
      (preferences
        (languages-preferred . ("Rust" "Elixir" "Haskell" "Ada" "OCaml"))
        (languages-avoid . ("Python" "Go"))
        (tools-preferred . ("GitLab" "Podman" "Guix" "Nickel" "Just"))
        (values . ("FOSS" "reproducibility" "formal-verification" "cooperative-economics"))))

    (session
      (conversation-id . "2025-12-06-PROJECT-PORTFOLIO")
      (started-at . "2025-12-06T10:00:00Z")
      (messages-used . 45)
      (messages-remaining . 55)
      (token-limit-reached . #f))

    (focus
      (current-project . "Oblibeny")
      (current-phase . "Parser implementation")
      (deadline . "2025-12-13")
      (blocking-projects . ("My Language Solo" "ZeroStep" "Aletheia")))

    (projects
      ;; Core language project - critical path
      ((name . "Oblibeny")
       (status . "in-progress")
       (completion . 40)
       (category . "language")
       (phase . "parser-development")
       (dependencies . ())
       (blockers . ("LLVM backend not started"))
       (next . ("Finish Rust parser" "Type checker validation" "Begin LLVM codegen"))
       (chat-reference . "2025-11-28-oblibeny-grammar")
       (notes . "Core language - blocks 5+ other projects"))

      ;; Depends on Oblibeny
      ((name . "My Language Solo")
       (status . "blocked")
       (completion . 15)
       (category . "education")
       (phase . "curriculum-design")
       (dependencies . ("Oblibeny"))
       (blockers . ("Waiting for Oblibeny parser"))
       (next . ("Draft module 1" "Create exercises"))
       (chat-reference . #f)
       (notes . "Solo course teaching language implementation"))

      ;; Depends on Oblibeny
      ((name . "ZeroStep")
       (status . "blocked")
       (completion . 10)
       (category . "formal-verification")
       (phase . "specification")
       (dependencies . ("Oblibeny"))
       (blockers . ("Needs Oblibeny type system"))
       (next . ("Write formal spec" "Implement proof checker"))
       (chat-reference . #f)
       (notes . "Zero-knowledge proof system"))

      ;; Hardware project - ready for testing
      ((name . "NeuroPhone")
       (status . "in-progress")
       (completion . 80)
       (category . "ai")
       (phase . "hardware-testing")
       (dependencies . ())
       (blockers . ())
       (next . ("Install on Oppo Reno 13" "Verify sensor calibration" "Test neural interface"))
       (chat-reference . "2025-12-05-neurophone")
       (notes . "Ready for physical device testing"))

      ;; Conversation preservation system
      ((name . "Echomesh")
       (status . "in-progress")
       (completion . 35)
       (category . "ai")
       (phase . "core-implementation")
       (dependencies . ("UPM"))
       (blockers . ("UPM integration pending" "Conversation persistence design"))
       (next . ("Complete MVP" "Test with real conversations" "Integrate STATE"))
       (chat-reference . "2025-12-01-echomesh")
       (notes . "Will eventually replace manual STATE download/upload"))

      ;; Universal Project Manager
      ((name . "UPM")
       (status . "in-progress")
       (completion . 25)
       (category . "infrastructure")
       (phase . "architecture")
       (dependencies . ())
       (blockers . ("Schema design incomplete"))
       (next . ("Finalize project schema" "Implement Rust core" "Create Elixir API"))
       (chat-reference . #f)
       (notes . "Core infrastructure - many projects depend on this"))

      ;; Cryptographic identity
      ((name . "JanusKey")
       (status . "paused")
       (completion . 60)
       (category . "infrastructure")
       (phase . "cryptography")
       (dependencies . ())
       (blockers . ("Waiting for security audit"))
       (next . ("Complete audit" "Implement key rotation"))
       (chat-reference . #f)
       (notes . "Paused pending external audit"))

      ;; Documentation format
      ((name . "Palimpsest")
       (status . "in-progress")
       (completion . 55)
       (category . "standards")
       (phase . "v0.6-specification")
       (dependencies . ())
       (blockers . ())
       (next . ("Review v0.6 spec" "Implement reference parser" "Write migration guide"))
       (chat-reference . "2025-11-20-palimpsest")
       (notes . "Living document format specification"))

      ;; Formal verification system - depends on Oblibeny
      ((name . "Aletheia")
       (status . "blocked")
       (completion . 5)
       (category . "formal-verification")
       (phase . "design")
       (dependencies . ("Oblibeny" "ZeroStep"))
       (blockers . ("Blocked on Oblibeny and ZeroStep"))
       (next . ("Define verification primitives"))
       (chat-reference . #f)
       (notes . "Truth verification system"))

      ;; Completed project for reference
      ((name . "Seven Tentacles")
       (status . "complete")
       (completion . 100)
       (category . "education")
       (phase . "published")
       (dependencies . ())
       (blockers . ())
       (next . ())
       (chat-reference . "2025-11-15-seven-tentacles")
       (notes . "Curriculum published and in use")))

    (critical-next
      ("Finish Oblibeny parser (blocks 5+ projects)"
       "Install NeuroPhone on hardware this week"
       "Review Palimpsest v0.6 specification"
       "Complete Echomesh MVP"))

    (history
      ;; Completion snapshots for velocity tracking
      (snapshots
        ((timestamp . "2025-12-01T10:00:00Z")
         (projects
           ((name . "Oblibeny") (completion . 25))
           ((name . "My Language Solo") (completion . 10))
           ((name . "ZeroStep") (completion . 5))
           ((name . "NeuroPhone") (completion . 70))
           ((name . "Echomesh") (completion . 20))
           ((name . "UPM") (completion . 15))
           ((name . "JanusKey") (completion . 60))
           ((name . "Palimpsest") (completion . 45))
           ((name . "Aletheia") (completion . 0))
           ((name . "Seven Tentacles") (completion . 95))))

        ((timestamp . "2025-12-03T10:00:00Z")
         (projects
           ((name . "Oblibeny") (completion . 32))
           ((name . "My Language Solo") (completion . 12))
           ((name . "ZeroStep") (completion . 8))
           ((name . "NeuroPhone") (completion . 75))
           ((name . "Echomesh") (completion . 28))
           ((name . "UPM") (completion . 20))
           ((name . "JanusKey") (completion . 60))
           ((name . "Palimpsest") (completion . 50))
           ((name . "Aletheia") (completion . 2))
           ((name . "Seven Tentacles") (completion . 100))))

        ((timestamp . "2025-12-06T10:00:00Z")
         (projects
           ((name . "Oblibeny") (completion . 40))
           ((name . "My Language Solo") (completion . 15))
           ((name . "ZeroStep") (completion . 10))
           ((name . "NeuroPhone") (completion . 80))
           ((name . "Echomesh") (completion . 35))
           ((name . "UPM") (completion . 25))
           ((name . "JanusKey") (completion . 60))
           ((name . "Palimpsest") (completion . 55))
           ((name . "Aletheia") (completion . 5))
           ((name . "Seven Tentacles") (completion . 100))))))

    (files-created-this-session
      ("VEDS_HANDOVER.md"
       "Oblibeny_Grammar_v0.6.ebnf"
       "Seven_Tentacles_Curriculum.adoc"))

    (files-modified-this-session
      ("src/parser/grammar.rs"
       "docs/architecture.djot"))

    (context-notes . "Critical: Oblibeny parser is on critical path. NeuroPhone ready for hardware. Download STATE.scm at end of session!")))

;;; ==================================================
;;; END example-state.scm
;;; ==================================================
