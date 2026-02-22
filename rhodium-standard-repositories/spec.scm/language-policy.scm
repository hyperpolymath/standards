;; SPDX-License-Identifier: MPL-2.0-or-later
;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath
;;
;; RSR Language Policy - Machine Readable
;; Derived from CCCP (Campaign for Cooler Coding and Programming)
;;
;; VERSION: 1.0.0 (FROZEN 2025-12-27)
;; Language allowed/banned lists are immutable in v1.x.

(define-module (rsr language-policy)
  #:use-module (rsr version)
  #:export (allowed-languages
            banned-languages
            language-allowed?
            get-replacement
            file-extension-language
            spec-version))

;; Allowed languages with use cases
(define allowed-languages
  '((rescript    . ((extensions . (".res" ".resi"))
                    (use-case . "Primary application code")
                    (compiles-to . "javascript")))
    (rust        . ((extensions . (".rs"))
                    (use-case . "Systems, performance, WASM")
                    (preferred-for . ("cli" "wasm"))))
    (deno        . ((extensions . (".ts" ".js"))  ; only via Deno runtime
                    (use-case . "Runtime & package management")
                    (replaces . ("node" "npm" "bun"))))
    (gleam       . ((extensions . (".gleam"))
                    (use-case . "Backend services")
                    (targets . ("beam" "javascript"))))
    (ocaml       . ((extensions . (".ml" ".mli"))
                    (use-case . "Compilers, formal methods")))
    (ada         . ((extensions . (".adb" ".ads"))
                    (use-case . "Safety-critical systems")
                    (verification . "spark")))
    (julia       . ((extensions . (".jl"))
                    (use-case . "Data processing, batch scripts")))
    (guile       . ((extensions . (".scm"))
                    (use-case . "State/meta files")
                    (required-for . ("STATE.scm" "META.scm" "ECOSYSTEM.scm"))))
    (nickel      . ((extensions . (".ncl"))
                    (use-case . "Configuration language")))
    (bash        . ((extensions . (".sh" ".bash"))
                    (use-case . "Scripts, automation")
                    (note . "Keep minimal")))))

;; Banned languages with replacements
(define banned-languages
  '((typescript  . ((extensions . (".ts" ".tsx"))
                    (replacement . rescript)
                    (rationale . "Unsound gradual typing")))
    (nodejs      . ((replacement . deno)
                    (rationale . "Insecure by default")))
    (npm         . ((replacement . deno)
                    (rationale . "Supply chain risks")))
    (bun         . ((replacement . deno)
                    (rationale . "Supply chain risks")))
    (go          . ((extensions . (".go"))
                    (replacement . rust)
                    (rationale . "Error handling, generics")))
    (python      . ((extensions . (".py"))
                    (replacement . (rescript rust))
                    (rationale . "No static types")
                    (exception . "SaltStack only")))
    (java        . ((extensions . (".java"))
                    (replacement . rust)
                    (rationale . "JVM overhead")))
    (kotlin      . ((extensions . (".kt" ".kts"))
                    (replacement . (rust tauri dioxus))
                    (rationale . "Platform lock-in")))
    (swift       . ((extensions . (".swift"))
                    (replacement . (tauri dioxus))
                    (rationale . "Platform lock-in")))))

;; Check if a language is allowed
(define (language-allowed? lang)
  (and (assoc lang allowed-languages) #t))

;; Get replacement for banned language
(define (get-replacement lang)
  (let ((banned (assoc lang banned-languages)))
    (if banned
        (assoc-ref (cdr banned) 'replacement)
        #f)))

;; Map file extension to language
(define (file-extension-language ext)
  (let loop ((langs (append allowed-languages banned-languages)))
    (if (null? langs)
        #f
        (let* ((lang-entry (car langs))
               (lang-name (car lang-entry))
               (lang-data (cdr lang-entry))
               (extensions (assoc-ref lang-data 'extensions)))
          (if (and extensions (member ext extensions))
              lang-name
              (loop (cdr langs)))))))

;; Return spec version for this module
(define (spec-version)
  (version-string))
