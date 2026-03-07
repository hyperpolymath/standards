;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm — Overlay Protocol specification meta-level information
;; Media-Type: application/meta+scheme

(meta
  ((metadata
     ((version . "1.0.0-draft")
      (media-type . "application/meta+scheme")))

   (architecture-decisions
     ((adr-001
        ((title . "Two peer types under one protocol")
         (status . "accepted")
         (date . "2026-03-07")
         (context . "o-extension and aggregate-library share core invariants (non-modification, switchable, declared relationship) but differ in activation mechanism and intent.")
         (decision . "Unify both under the Overlay Protocol with peer-type as the differentiator, rather than defining two separate protocols.")
         (rationale . "Shared invariants mean shared validation tooling, shared ECOSYSTEM.scm format, and a single concept for developers to learn. The peer-type field captures the meaningful difference.")
         (consequences . ("One spec to maintain instead of two"
                          "Conformance tooling works for both types"
                          "Future peer types can be added without protocol changes"))))

      (adr-002
        ((title . "ECOSYSTEM.scm as declaration mechanism")
         (status . "accepted")
         (date . "2026-03-07")
         (context . "Overlays need a machine-readable way to declare their relationship to a base project. Options: separate manifest file, ECOSYSTEM.scm section, or package.json/Cargo.toml field.")
         (decision . "Use the overlay-protocol section in ECOSYSTEM.scm, which is already required by RSR for all hyperpolymath projects.")
         (rationale . "ECOSYSTEM.scm already describes project relationships. Adding overlay-protocol as a section keeps all relationship data in one place and requires no new files.")
         (consequences . ("No new files needed"
                          "Existing ECOSYSTEM.scm tooling can parse overlay declarations"
                          "Non-hyperpolymath projects would need to adopt ECOSYSTEM.scm or define an equivalent"))))))

   (development-practices
     ((documentation "Markdown (spec), Guile Scheme (SCM files)")
      (versioning "SemVer")
      (branching "main for stable")))))
