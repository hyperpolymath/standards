;;; SPDX-License-Identifier: MPL-2.0
;;; guix.scm — Guix environment for K9 SVC development
;;;
;;; Usage:
;;;   guix shell -D -f guix.scm
;;;

(use-modules (guix packages)
             (guix build-system gnu)
             (guix licenses)
             (gnu packages base)
             (gnu packages crypto)
             (gnu packages shells)
             (gnu packages rust-apps) ; for just
             (gnu packages node))     ; for nickel if available or used

(package
  (name "k9-svc")
  (version "1.0.0")
  (source #f) ; Development environment only
  (build-system gnu-build-system)
  (native-inputs
   (list just
         openssl
         nickel
         ripgrep))
  (synopsis "Self-Validating Components - Data-first configuration")
  (description
   "Development environment for K9 SVC, including Nickel for schema
validation and OpenSSL for cryptographic signing.")
  (home-page "https://github.com/hyperpolymath/standards/tree/main/k9-svc")
  (license agpl3+))
