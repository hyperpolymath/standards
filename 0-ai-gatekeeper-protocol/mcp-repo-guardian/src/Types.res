// SPDX-License-Identifier: PMPL-1.0-or-later

// AI.a2ml manifest structure
type canonicalLocations = {
  scmFiles: string,
  botDirectives: string,
  agentInstructions: array<string>,
}

type aiManifest = {
  hash: string,
  canonicalLocations: canonicalLocations,
  invariants: array<string>,
  parsedAt: Date.t,
}

// Session state for an AI agent
type sessionState = {
  sessionId: string,
  acknowledgedManifest: bool,
  attestationHash: option<string>,
  acknowledgedAt: option<Date.t>,
  repoPath: string,
}

// Access control result
type accessResult = {
  allowed: bool,
  reason: option<string>,
}

// Guardian configuration
type guardianConfig = {
  basePath: string,
  strictMode: bool,
  sessionTimeout: int,
}
