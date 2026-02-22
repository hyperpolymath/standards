// SPDX-License-Identifier: PMPL-1.0-or-later

// External Deno/Node crypto API
@module("node:crypto") external randomUUID: unit => string = "randomUUID"
@val external setTimeout: (unit => unit, int) => unit = "setTimeout"

// Session manager class
module SessionManager = {
  type t = {
    mutable sessions: dict<Types.sessionState>,
    config: Types.guardianConfig,
  }

  let make = (config: Types.guardianConfig): t => {
    {
      sessions: Dict.make(),
      config,
    }
  }

  // Create a new session for an AI agent
  let createSession = (manager: t, repoPath: string): Types.sessionState => {
    let session: Types.sessionState = {
      sessionId: randomUUID(),
      acknowledgedManifest: false,
      attestationHash: None,
      acknowledgedAt: None,
      repoPath,
    }

    Dict.set(manager.sessions, session.sessionId, session)

    // Set timeout to clean up session
    setTimeout(() => {
      Dict.set(manager.sessions, session.sessionId, session)
      ()
    }, manager.config.sessionTimeout)->ignore

    session
  }

  // Get session by ID
  let getSession = (manager: t, sessionId: string): option<Types.sessionState> => {
    Dict.get(manager.sessions, sessionId)
  }

  // Acknowledge manifest for a session
  let acknowledgeManifest = (
    manager: t,
    sessionId: string,
    manifest: Types.aiManifest,
    attestationHash: string,
  ): bool => {
    switch Dict.get(manager.sessions, sessionId) {
    | None => false
    | Some(session) =>
      if manifest.hash !== attestationHash {
        false
      } else {
        let updatedSession: Types.sessionState = {
          ...session,
          acknowledgedManifest: true,
          attestationHash: Some(attestationHash),
          acknowledgedAt: Some(Date.make()),
        }

        Dict.set(manager.sessions, sessionId, updatedSession)
        true
      }
    }
  }

  // Check if session has acknowledged manifest
  let isAcknowledged = (manager: t, sessionId: string): bool => {
    switch Dict.get(manager.sessions, sessionId) {
    | None => false
    | Some(session) => session.acknowledgedManifest
    }
  }

  // Destroy a session
  let destroySession = (manager: t, sessionId: string): unit => {
    Dict.set(manager.sessions, sessionId, Dict.get(manager.sessions, sessionId)->Belt.Option.getExn)
    ()
  }

  // Get all active sessions
  let getActiveSessions = (manager: t): array<Types.sessionState> => {
    Dict.valuesToArray(manager.sessions)
  }
}
