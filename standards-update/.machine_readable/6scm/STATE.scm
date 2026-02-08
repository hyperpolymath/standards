;; STATE.scm - RSR State File
;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (C) 2025 Jonathan D.A. Jewell
;;
;; This file tracks the current state of the project using S-expressions.
;; It is machine-readable and used by RSR tooling for validation.

(state
  (version . "0.1.0")
  (phase . "alpha")
  (updated . "2025-12-27T00:00:00Z")

  (project
    (name . "standards")
    (tier . "infrastructure")
    (license . "AGPL-3.0-or-later")
    (language . "ada"))

  (compliance
    (rsr . #t)
    (security-hardened . #t)
    (ci-cd . #t)
    (guix-primary . #f)
    (nix-fallback . #f))

  (artifacts
    (binary . "bin/standards")
    (container . "ghcr.io/hyperpolymath/standards:latest"))

  (dependencies
    (build
      ("gnat" . ">=12")
      ("gprbuild" . ">=22"))
    (runtime))

  (milestones
    (v0.1.0
      (status . "released")
      (date . "2025-12-27")
      (features
        "Task runner"
        "Template engine"
        "Requirements enforcer"
        "Deploy command"))
    (v0.2.0
      (status . "planned")
      (features
        "Mustache partials"
        "Content requirement checks"
        "TOML variable loading"))))
