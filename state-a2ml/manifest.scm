;;; SPDX-License-Identifier: MIT AND LicenseRef-Palimpsest-0.8
;;; manifest.scm — Guix package manifest for STATE development
;;;
;;; This manifest defines all packages needed for STATE development.
;;;
;;; Usage:
;;;   guix shell -m manifest.scm
;;;
;;; Or with channels:
;;;   guix time-machine -C channels.scm -- shell -m manifest.scm
;;;
;;; To create a profile:
;;;   guix package -m manifest.scm -p ./guix-profile
;;;   source ./guix-profile/etc/profile

(specifications->manifest
  '(;; Core runtime
    "guile"                    ; Guile Scheme 3.x
    "guile-minikanren"         ; minikanren for relational queries

    ;; Development tools
    "git"                      ; Version control
    "just"                     ; Task runner (justfile)

    ;; Documentation
    "asciidoctor"              ; AsciiDoc processor

    ;; Visualization
    "graphviz"                 ; DOT graph rendering

    ;; Container tools (optional, for local container builds)
    ;; "podman"                 ; Container runtime
    ;; "buildah"                ; Container building
    ))
