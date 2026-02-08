;; SPDX-License-Identifier: PMPL-1.0-or-later
;; PLAYBOOK.scm - Operational runbook for ecosystem.scm
;; License: Palimpsest-MPL License 1.0

(define playbook
  `((version . "1.0.0")
    (procedures
      ((deploy . (("build" . "just build")
                  ("test" . "just test")
                  ("release" . "just release")))
       (rollback . ())
       (debug . ())))
    (alerts . ())
    (contacts . ())))