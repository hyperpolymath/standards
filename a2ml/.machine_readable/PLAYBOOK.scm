;; SPDX-License-Identifier: PMPL-1.0-or-later
;; PLAYBOOK.scm - Operational runbook for a2ml

(define playbook
  `((version . "1.0.0")
    (procedures
      ((docs . (("build" . "just docs")))
       (release . ())
       (rollback . ())
       (debug . ())))
    (alerts . ())
    (contacts . ())))
