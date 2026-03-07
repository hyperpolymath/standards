;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm — Overlay Protocol specification state

(state
  ((metadata
     ((version . "1.0.0-draft")
      (last-updated . "2026-03-07")
      (completion . "95%")))

   (current-position
     ((phase . "draft")
      (milestone . "v1.0.0-draft")
      (description . "Full specification with Idris2 ABI proofs, Zig FFI implementation, conformance checker, and two reference implementations adopted.")))

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
      (zig-ffi
        ((status . "complete")
         (file . "ffi/zig/src/main.zig")
         (description . "C-compatible FFI implementing all five invariant checks, composition, full conformance, 14 unit tests + 8 integration tests, builds with Zig 0.15")))
      (examples
        ((status . "exists-externally")
         (description . "HOL-o-extension (FULLY CONFORMANT) and aggregate-library (CONFORMANT) serve as reference implementations")))))

   (blockers-and-issues ())

   (critical-next-actions
     ((1 . "Promote from draft to accepted after ecosystem review")
      (2 . "Generate C headers from Zig FFI for non-Zig consumers")
      (3 . "Integrate conformance checker into echidnabot workflow")))))
