// SPDX-License-Identifier: PMPL-1.0-or-later

// Access control guard module
module AccessGuard = {
  type t = {
    sessionManager: Session.SessionManager.t,
    manifest: Types.aiManifest,
  }

  let make = (sessionManager: Session.SessionManager.t, manifest: Types.aiManifest): t => {
    {
      sessionManager,
      manifest,
    }
  }

  // Check if session has access to perform operations
  let checkAccess = (guard: t, sessionId: string): Types.accessResult => {
    switch Session.SessionManager.getSession(guard.sessionManager, sessionId) {
    | None => {
        allowed: false,
        reason: Some("Invalid session ID. Session may have expired."),
      }
    | Some(session) =>
      if !session.acknowledgedManifest {
        let hashPreview = String.substring(guard.manifest.hash, ~start=0, ~end=16)
        {
          allowed: false,
          reason: Some(
            "⚠️ ACCESS DENIED\n\n" ++
            "You must read and acknowledge the AI manifest (AI.a2ml) before " ++
            "accessing any files in this repository.\n\n" ++
            "Call the acknowledge_manifest tool with the manifest hash to proceed.\n\n" ++
            `Expected hash: ${hashPreview}...`,
          ),
        }
      } else {
        {allowed: true, reason: None}
      }
    }
  }

  // Validate that a file path doesn't violate manifest invariants
  let validatePath = (guard: t, path: string): Types.accessResult => {
    // Check for SCM file duplication invariant
    if Array.includes(guard.manifest.invariants, "no_scm_duplication") {
      let scmFiles = [
        "STATE.scm",
        "META.scm",
        "ECOSYSTEM.scm",
        "AGENTIC.scm",
        "NEUROSYM.scm",
        "PLAYBOOK.scm",
        "LANGUAGES.scm",
      ]

      let isViolation = Array.some(scmFiles, scmFile => {
        String.endsWith(path, scmFile) && !String.includes(path, ".machine_readable/")
      })

      if isViolation {
        let violatedFile =
          Array.find(scmFiles, scmFile =>
            String.endsWith(path, scmFile)
          )->Belt.Option.getWithDefault("unknown")

        {
          allowed: false,
          reason: Some(
            "⚠️ INVARIANT VIOLATION\n\n" ++
            `Attempted to access ${violatedFile} outside of .machine_readable/ directory.\n\n` ++
            "Per AI.a2ml manifest: SCM files MUST be in .machine_readable/ only.\n" ++ "This prevents duplicate file errors.",
          ),
        }
      } else {
        {allowed: true, reason: None}
      }
    } else {
      {allowed: true, reason: None}
    }
  }

  // Require acknowledgment before any operation
  let requireAcknowledgment = (guard: t, sessionId: string, operation: string): unit => {
    let access = checkAccess(guard, sessionId)
    if !access.allowed {
      let reason = Belt.Option.getWithDefault(access.reason, "Unknown reason")
      JsError.throwWithMessage(`Cannot perform ${operation}: ${reason}`)
    }
  }
}
