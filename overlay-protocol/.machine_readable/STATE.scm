;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm — Overlay Protocol specification state

(state
  ((metadata
     ((version . "1.0.0-draft")
      (last-updated . "2026-03-07")
      (completion . "90%")))

   (current-position
     ((phase . "draft")
      (milestone . "v1.0.0-draft")
      (description . "Initial specification written with both peer types defined, core invariants formalised, ECOSYSTEM.scm declaration format specified, and conformance checklist provided.")))

   (components
     ((spec-document
        ((status . "complete")
         (file . "OVERLAY-PROTOCOL-SPEC.md")
         (description . "Full specification document")))
      (ecosystem-scm
        ((status . "complete")
         (description . "Machine-readable ecosystem position")))
      (conformance-tooling
        ((status . "complete")
         (file . "scripts/check-conformance.sh")
         (description . "Automated conformance validation script — 7 check categories, tested against both reference implementations")))
      (idris2-abi
        ((status . "complete")
         (file . "src/abi/OverlayProtocol.idr")
         (description . "Formal type-level proofs of all five overlay invariants, composition, and chaining")))
      (examples
        ((status . "exists-externally")
         (description . "HOL-o-extension and aggregate-library serve as reference implementations")))))

   (blockers-and-issues ())

   (critical-next-actions
     ((1 . "Review spec against both reference implementations for completeness")
      (2 . "Create conformance validation script (echidnabot integration)")
      (3 . "Write Idris2 ABI formalising overlay invariants as dependent types")
      (4 . "Promote from draft to accepted after ecosystem review")))))
