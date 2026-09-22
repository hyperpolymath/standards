;; SPDX-License-Identifier: MPL-2.0
;; SPDX-FileCopyrightText: 2024-2026 hyperpolymath
;;
;; RSR Language Policy - Machine Readable
;; Derived from CCCP (Campaign for Cooler Coding and Programming)
;;
;; MODULE-VERSION: 2.0.0 (2026-09-22) - UNFROZEN from 1.0.0
;;
;; THREE different version numbers meet in this file. Conflating them is what
;; let it drift, so they are named here explicitly:
;;   1. MODULE-VERSION (this line, 2.0.0) - the version of THIS module's
;;      allowed/banned lists. A ban is a MAJOR bump.
;;   2. spec/LANGUAGE-POLICY.adoc :revnumber: (1.6.0, 2026-09-22) - the
;;      canonical human-readable policy document this file mirrors.
;;   3. (spec-version) below returns the RSR SPECIFICATION version from
;;      (rsr version) - 1.0.0, status `frozen`. That is a DIFFERENT artefact
;;      with its own freeze and is deliberately NOT touched here.
;;
;; v1.0.0 of this module was frozen 2025-12-27 with the note "Language
;; allowed/banned lists are immutable in v1.x". That freeze outlived the policy
;; it protected: four bans were ruled after it and none could be recorded here,
;; so this file drifted into asserting the OPPOSITE of ratified policy --
;; `(bun . ((replacement . deno)))` while spec/LANGUAGE-POLICY.adoc SS1 ranks
;; Bun tier 1 and bans Deno outright. A machine-readable twin that contradicts
;; its own spec is worse than no twin, because tooling trusts it silently.
;;
;; Concretely, the freeze also hid a live defect: `.ts` was claimed by BOTH
;; the allowed `deno` entry and the banned `typescript` entry, and because
;; file-extension-language searches allowed FIRST, it resolved `.ts` to `deno`
;; -- silently blessing a banned extension. See the warning on that function.
;;
;; The lists are therefore versioned, not frozen.
;;
;; Ratified changes since 1.0.0, all now reflected below:
;;   2026-01-03  Python's "SaltStack only" exception REMOVED (fully banned).
;;   2026-04-10  V-lang banned -> Zig. ATS2 banned -> Idris2 / Rust-SPARK.
;;               "Rust" redefined estate-wide as "Rust/SPARK".
;;   2026-04-30  ReScript banned -> AffineScript (directly; not via ReScript).
;;   2026-05-28  Zig made the estate default for APIs/FFIs/gateways/client SDKs.
;;   2026-08-27  TypeScript banned -> AffineScript ("no typescript ... that
;;               should not exist at all"). It is not a fallback tier.
;;   2026-09-22  Deno banned -> Bun. Bun is tier 1 ("deno is over, we're
;;               prioritising bun, and using bunx").
;;
;; CANONICAL SOURCE: spec/LANGUAGE-POLICY.adoc. This file MUST mirror it.
;; Where they disagree, the .adoc wins and this file is the defect.

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
  '((affinescript . ((extensions . (".affine"))
                     (use-case . "Primary application code")
                     (compiles-to . "typed-wasm")
                     (note . "RS/TS/JS -> AffineScript -> typed-wasm")))
    (bun          . ((extensions . ())
                     (use-case . "JS runtime & package management (tier 1)")
                     (replaces . ("node" "npm" "deno"))
                     (manifest . ("package.json" "bun.lock"))
                     (note . "bunx <tool> for one-off tooling")))
    (npm          . ((extensions . ())
                     (use-case . "JS package management of last resort (tier 4)")
                     (tier . 4)
                     (preferred-instead . bun)
                     (rationale . "Supply chain risks")
                     (note . "Tier 4: permitted, never preferred - NOT banned. Order is bun (1) -> pnpm (3) -> npm (4). It lives here, not in banned-languages, because `language-allowed?` answers from THIS list: a tier-4 fallback listed as banned reads as prohibited to every consumer. package-lock.json must still not be tracked (standards#67).")))
    (rust         . ((extensions . (".rs"))
                     (use-case . "Systems, performance, WASM, CLI, safety-critical")
                     (preferred-for . ("cli" "wasm"))
                     (verification . "spark")
                     (note . "\"Rust\" always means Rust/SPARK (2026-04-10)")))
    (zig          . ((extensions . (".zig"))
                     (use-case . "APIs, FFIs, gateways, client SDKs (estate default)")
                     (note . "Default since 2026-05-28; Idris2 owns ABIs")))
    (idris2       . ((extensions . (".idr"))
                     (use-case . "Formal verification (primary, ABI-style proofs)")))
    (agda         . ((extensions . (".agda"))
                     (use-case . "Formal verification (foundational)")
                     (note . "Constructive only - no postulates in load-bearing tracks")))
    (gleam        . ((extensions . (".gleam"))
                     (use-case . "Backend services")
                     (targets . ("beam" "javascript"))))
    (elixir       . ((extensions . (".ex" ".exs"))
                     (use-case . "Backend services, distributed systems")))
    (haskell      . ((extensions . (".hs"))
                     (use-case . "Type-heavy tools, registry validation")))
    (ocaml        . ((extensions . (".ml" ".mli"))
                     (use-case . "AffineScript compiler, formal methods")))
    (ada          . ((extensions . (".adb" ".ads"))
                     (use-case . "Safety-critical systems (legacy)")
                     (verification . "spark")
                     (note . "Rust/SPARK absorbs new work; no new pure-Ada projects")))
    (julia        . ((extensions . (".jl"))
                     (use-case . "Data processing, batch scripts")))
    (guile        . ((extensions . (".scm"))
                     (use-case . "State/meta files, package manifests")
                     (required-for . ("STATE.scm" "META.scm" "ECOSYSTEM.scm"))
                     (note . "Guix is primary package management (guix.scm)")))
    (nickel       . ((extensions . (".ncl"))
                     (use-case . "Configuration language")))
    (javascript   . ((extensions . (".js"))
                     (use-case . "Only where AffineScript cannot reach")
                     (note . "Transitional. MCP/LSP glue, VSCode host, npm front door. Prefer .affine.")))
    (bash         . ((extensions . (".sh" ".bash"))
                     (use-case . "Scripts, automation")
                     (note . "Keep minimal")))))

;; Banned languages with replacements
(define banned-languages
  '((typescript  . ((extensions . (".ts" ".tsx"))
                    (replacement . affinescript)
                    (rationale . "Unsound gradual typing; AffineScript governs")
                    (banned-on . "2026-08-27")
                    (carve-outs . ("**/*.d.ts" "**/bindings/ts/**" "**/vscode/**"))))
    (rescript    . ((extensions . (".res" ".resi"))
                    (replacement . affinescript)
                    (rationale . "Superseded; migrate .res directly to .affine")
                    (banned-on . "2026-04-30")))
    (deno        . ((extensions . ())
                    (replacement . bun)
                    (rationale . "Owner ruling: \"deno is over, we're prioritising bun, and using bunx\"")
                    (banned-on . "2026-09-22")
                    (note . "Shrink-only ledger: .machine_readable/deno-allow.txt in standards")))
    (nodejs      . ((replacement . bun)
                    (rationale . "Bun is Node-compatible; run the code, drop the runtime")))
    (yarn        . ((replacement . bun)
                    (rationale . "Not in the tier list at all")))
    (vlang       . ((extensions . (".v"))
                    (replacement . zig)
                    (rationale . "Banned 2026-04-10; migration completed 2026-05-28")
                    (carve-outs . ("v-cartridge/" "v-adapter/" "v-bindings/" "v-client/"))
                    (note . "WARNING .v is shared with Coq proof scripts and Verilog - check before flagging")))
    (ats2        . ((extensions . (".dats" ".sats"))
                    (replacement . (idris2 rust))
                    (rationale . "Rejected in favour of Idris2 and Rust/SPARK")))
    (go          . ((extensions . (".go"))
                    (replacement . rust)
                    (rationale . "Error handling, generics")))
    (python      . ((extensions . (".py"))
                    (replacement . (affinescript rust julia))
                    (rationale . "No static types")
                    (note . "FULLY banned - the \"SaltStack only\" exception was removed 2026-01-03")))
    (java        . ((extensions . (".java"))
                    (replacement . rust)
                    (rationale . "JVM overhead")))
    (kotlin      . ((extensions . (".kt" ".kts"))
                    (replacement . (rust tauri dioxus))
                    (rationale . "Platform lock-in")))
    (swift       . ((extensions . (".swift"))
                    (replacement . (tauri dioxus))
                    (rationale . "Platform lock-in")))
    (react-native . ((replacement . (tauri dioxus))
                     (rationale . "Google/Meta platform lock-in")))
    (dart        . ((extensions . (".dart"))
                    (replacement . (tauri dioxus))
                    (rationale . "Flutter/Dart - Google lock-in")))
    (make        . ((extensions . ("Makefile" "makefile" ".mk"))
                    (replacement . (mustfile justfile))
                    (rationale . "Replaced by Mustfile/justfile estate-wide")))))

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
;;
;; WARNING: allowed-languages is searched FIRST, so an extension claimed by both
;; lists resolves to the allowed entry. No extension is currently duplicated
;; across the two lists; keep it that way, or this silently blesses a banned one.
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

;; Return the RSR SPECIFICATION version from (rsr version) -- NOT this module's
;; own MODULE-VERSION (see header). The two are independent artefacts; the
;; spec version is 1.0.0 / frozen, this module is 2.0.0 / unfrozen.
(define (spec-version)
  (version-string))
