;; SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — standards

(ecosystem
  (version "1.0.0")
  (name "standards")
  (type "project")
  (purpose "Project in the hyperpolymath ecosystem")

  (position-in-ecosystem
    "Part of hyperpolymath ecosystem. Follows RSR guidelines.")

  (related-projects
    (project (name "rhodium-standard-repositories")
             (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
             (relationship "standard")))

  (what-this-is "Project in the hyperpolymath ecosystem")
  (what-this-is-not "- NOT exempt from RSR compliance"))
