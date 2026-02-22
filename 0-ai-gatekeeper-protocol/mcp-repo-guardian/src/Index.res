// SPDX-License-Identifier: PMPL-1.0-or-later

// MCP SDK External Bindings
module MCP = {
  // Server type
  type server

  // Transport type
  type transport

  // Request/Response types
  type toolRequest = {
    name: string,
    arguments: dict<JSON.t>,
  }

  type toolResponse = {content: array<JSON.t>}

  // External functions
  @module("@modelcontextprotocol/sdk/server/index.js") @new
  external createServer: {"name": string, "version": string} => server = "Server"

  @module("@modelcontextprotocol/sdk/server/stdio.js") @new
  external createStdioTransport: unit => transport = "StdioServerTransport"

  @send external connect: (server, transport) => promise<unit> = "connect"

  @send
  external setRequestHandler: (server, string, toolRequest => promise<toolResponse>) => unit =
    "setRequestHandler"
}

// File system external bindings
@module("node:fs/promises") external readFile: (string, string) => promise<string> = "readFile"
@module("node:fs/promises") external readdir: string => promise<array<string>> = "readdir"
@module("node:path") external resolve: (string, string) => string = "resolve"
@module("node:path") external join: (string, string) => string = "join"

// Get environment variable with default
let getEnv = (name: string, default: string): string => {
  let env: dict<string> = %raw("process.env")
  switch Dict.get(env, name) {
  | Some(v) => v
  | None => default
  }
}

// Main server class
module GuardianServer = {
  type t = {
    server: MCP.server,
    config: Types.guardianConfig,
    sessionManager: Session.SessionManager.t,
    mutable manifests: dict<Types.aiManifest>,
  }

  let make = (): t => {
    let cwd: unit => string = %raw("() => process.cwd()")
    let config: Types.guardianConfig = {
      basePath: getEnv("REPOS_PATH", cwd()),
      strictMode: getEnv("STRICT_MODE", "false") == "true",
      sessionTimeout: Belt.Int.fromString(
        getEnv("SESSION_TIMEOUT", "3600000"),
      )->Belt.Option.getWithDefault(3600000),
    }

    let server = MCP.createServer({"name": "repo-guardian", "version": "0.1.0"})
    let sessionManager = Session.SessionManager.make(config)

    {
      server,
      config,
      sessionManager,
      manifests: Dict.make(),
    }
  }

  // Handle get_manifest tool
  let handleGetManifest = async (guardian: t, repoPath: string): MCP.toolResponse => {
    let fullPath = resolve(guardian.config.basePath, repoPath)
    let manifest = await Manifest.parseManifest(fullPath)

    // Store manifest
    Dict.set(guardian.manifests, repoPath, manifest)

    // Return manifest info
    let content = [
      JSON.Encode.object(
        Dict.fromArray([
          ("type", JSON.Encode.string("text")),
          (
            "text",
            JSON.Encode.string(
              `Manifest hash: ${manifest.hash}\n\nYou must acknowledge this manifest with the hash to proceed.`,
            ),
          ),
        ]),
      ),
    ]

    {content: content}
  }

  // Handle acknowledge_manifest tool
  let handleAcknowledgeManifest = async (
    guardian: t,
    repoPath: string,
    attestationHash: string,
  ): MCP.toolResponse => {
    switch Dict.get(guardian.manifests, repoPath) {
    | None => {
        let content = [
          JSON.Encode.object(
            Dict.fromArray([
              ("type", JSON.Encode.string("text")),
              ("text", JSON.Encode.string("ERROR: You must call get_manifest first")),
            ]),
          ),
        ]
        {content: content}
      }
    | Some(manifest) =>
      if !Manifest.validateAttestation(manifest, attestationHash) {
        let content = [
          JSON.Encode.object(
            Dict.fromArray([
              ("type", JSON.Encode.string("text")),
              ("text", JSON.Encode.string("ERROR: Invalid attestation hash")),
            ]),
          ),
        ]
        {content: content}
      } else {
        // Create session
        let session = Session.SessionManager.createSession(guardian.sessionManager, repoPath)

        // Acknowledge manifest
        let _ = Session.SessionManager.acknowledgeManifest(
          guardian.sessionManager,
          session.sessionId,
          manifest,
          attestationHash,
        )

        let content = [
          JSON.Encode.object(
            Dict.fromArray([
              ("type", JSON.Encode.string("text")),
              (
                "text",
                JSON.Encode.string(`✅ Manifest acknowledged! Session ID: ${session.sessionId}`),
              ),
            ]),
          ),
        ]
        {content: content}
      }
    }
  }

  // Handle read_file tool
  let handleReadFile = async (guardian: t, sessionId: string, path: string): MCP.toolResponse => {
    // Get manifest for session
    switch Session.SessionManager.getSession(guardian.sessionManager, sessionId) {
    | None => {
        let content = [
          JSON.Encode.object(
            Dict.fromArray([
              ("type", JSON.Encode.string("text")),
              ("text", JSON.Encode.string("ERROR: Invalid session ID")),
            ]),
          ),
        ]
        {content: content}
      }
    | Some(session) =>
      switch Dict.get(guardian.manifests, session.repoPath) {
      | None => {
          let content = [
            JSON.Encode.object(
              Dict.fromArray([
                ("type", JSON.Encode.string("text")),
                ("text", JSON.Encode.string("ERROR: Manifest not found")),
              ]),
            ),
          ]
          {content: content}
        }
      | Some(manifest) =>
        let accessGuard = Guards.AccessGuard.make(guardian.sessionManager, manifest)

        // Check access
        let accessResult = Guards.AccessGuard.checkAccess(accessGuard, sessionId)
        if !accessResult.allowed {
          let reason = Belt.Option.getWithDefault(accessResult.reason, "Unknown")
          let content = [
            JSON.Encode.object(
              Dict.fromArray([
                ("type", JSON.Encode.string("text")),
                ("text", JSON.Encode.string(`ERROR: ${reason}`)),
              ]),
            ),
          ]
          {content: content}
        } else {
          // Validate path
          let pathResult = Guards.AccessGuard.validatePath(accessGuard, path)
          if !pathResult.allowed {
            let reason = Belt.Option.getWithDefault(pathResult.reason, "Unknown")
            let content = [
              JSON.Encode.object(
                Dict.fromArray([
                  ("type", JSON.Encode.string("text")),
                  ("text", JSON.Encode.string(`ERROR: ${reason}`)),
                ]),
              ),
            ]
            {content: content}
          } else {
            // Read file
            let fullPath = join(guardian.config.basePath, join(session.repoPath, path))
            try {
              let fileContent = await readFile(fullPath, "utf-8")
              let content = [
                JSON.Encode.object(
                  Dict.fromArray([
                    ("type", JSON.Encode.string("text")),
                    ("text", JSON.Encode.string(fileContent)),
                  ]),
                ),
              ]
              {content: content}
            } catch {
            | _ => {
                let content = [
                  JSON.Encode.object(
                    Dict.fromArray([
                      ("type", JSON.Encode.string("text")),
                      ("text", JSON.Encode.string(`ERROR: Failed to read file ${path}`)),
                    ]),
                  ),
                ]
                {content: content}
              }
            }
          }
        }
      }
    }
  }

  // Start server
  let start = async (guardian: t): unit => {
    // Set up request handlers
    MCP.setRequestHandler(guardian.server, "tools/list", async _request => {
      let content = []
      ({content: content}: MCP.toolResponse)
    })

    MCP.setRequestHandler(guardian.server, "tools/call", async request => {
      let name = request.name
      let args = request.arguments

      switch name {
      | "get_manifest" => {
          let repoPath =
            Dict.get(args, "repoPath")
            ->Belt.Option.flatMap(JSON.Decode.string)
            ->Belt.Option.getWithDefault("")
          await handleGetManifest(guardian, repoPath)
        }
      | "acknowledge_manifest" => {
          let repoPath =
            Dict.get(args, "repoPath")
            ->Belt.Option.flatMap(JSON.Decode.string)
            ->Belt.Option.getWithDefault("")
          let attestationHash =
            Dict.get(args, "attestationHash")
            ->Belt.Option.flatMap(JSON.Decode.string)
            ->Belt.Option.getWithDefault("")
          await handleAcknowledgeManifest(guardian, repoPath, attestationHash)
        }
      | "read_file" => {
          let sessionId =
            Dict.get(args, "sessionId")
            ->Belt.Option.flatMap(JSON.Decode.string)
            ->Belt.Option.getWithDefault("")
          let path =
            Dict.get(args, "path")
            ->Belt.Option.flatMap(JSON.Decode.string)
            ->Belt.Option.getWithDefault("")
          await handleReadFile(guardian, sessionId, path)
        }
      | _ => {
          let content = [
            JSON.Encode.object(
              Dict.fromArray([
                ("type", JSON.Encode.string("text")),
                ("text", JSON.Encode.string(`Unknown tool: ${name}`)),
              ]),
            ),
          ]
          {content: content}
        }
      }
    })

    // Connect to stdio transport
    let transport = MCP.createStdioTransport()
    await MCP.connect(guardian.server, transport)

    Console.log("MCP Repository Guardian started")
  }
}

// Main entry point
let main = async () => {
  let guardian = GuardianServer.make()
  await GuardianServer.start(guardian)
}

// Run main
main()->ignore
