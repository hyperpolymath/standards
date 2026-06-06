;; SPDX-License-Identifier: MPL-2.0
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
;;
;; Guix development environment for standards.
;; Usage: guix shell -D -f guix.scm

(use-modules (guix packages)
             (guix build-system gnu)
             (gnu packages node))

(package
  (name "standards")
  (version "0.1.0")
  (source #f)
  (build-system gnu-build-system)
  (native-inputs
   (list deno
         nickel))
  (synopsis "Hyperpolymath standards monorepo")
  (description
   "Collection of hyperpolymath standards including A2ML, K9, Axel
protocol, Groove protocol, eNSAID configuration, and component
readiness grades, with Deno tooling and Nickel configuration.")
  (license #f))
