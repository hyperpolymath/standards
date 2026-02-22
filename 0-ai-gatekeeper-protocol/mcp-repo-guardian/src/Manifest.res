// SPDX-License-Identifier: PMPL-1.0-or-later

// External Deno APIs
@module("node:fs/promises") external readFile: (string, string) => promise<string> = "readFile"
@module("node:path") external join: (string, string) => string = "join"
@module("node:crypto") external createHash: string => 'a = "createHash"

type rec hashObj = {
  update: string => hashObj,
  digest: string => string,
}

@send external update: (hashObj, string) => hashObj = "update"
@send external digest: (hashObj, string) => string = "digest"

// Extract canonical locations from manifest content
let extractCanonicalLocations = (content: string): Types.canonicalLocations => {
  // Use regex to find canonical locations
  let scmMatch = RegExp.exec(/SCM files.*?`([^`]+)`/i, content)
  let botMatch = RegExp.exec(/Bot [Dd]irectives.*?`([^`]+)`/i, content)

  let scmFiles = switch scmMatch {
  | Some(result) => {
      let matches = RegExp.Result.matches(result)
      switch matches[1] {
      | Some(v) => Belt.Option.getWithDefault(v, ".machine_readable/")
      | None => ".machine_readable/"
      }
    }
  | None => ".machine_readable/"
  }

  let botDirectives = switch botMatch {
  | Some(result) => {
      let matches = RegExp.Result.matches(result)
      switch matches[1] {
      | Some(v) => Belt.Option.getWithDefault(v, ".bot_directives/")
      | None => ".bot_directives/"
      }
    }
  | None => ".bot_directives/"
  }

  let agentInstructions = [".claude/CLAUDE.md", "AI.a2ml", "0-AI-MANIFEST.a2ml"]

  {
    scmFiles,
    botDirectives,
    agentInstructions,
  }
}

// Extract invariants from manifest content
let extractInvariants = (content: string): array<string> => {
  let invariants = []

  if String.match(content, /No SCM file duplication/i)->Belt.Option.isSome {
    Array.push(invariants, "no_scm_duplication")
  }

  if String.match(content, /Single source of truth/i)->Belt.Option.isSome {
    Array.push(invariants, "single_source_of_truth")
  }

  if String.match(content, /No stale metadata/i)->Belt.Option.isSome {
    Array.push(invariants, "no_stale_metadata")
  }

  invariants
}

// Parse and validate an AI.a2ml manifest file
let parseManifest = async (repoPath: string): Types.aiManifest => {
  let manifestNames = ["0-AI-MANIFEST.a2ml", "AI.a2ml", "!AI.a2ml"]

  let rec tryReadManifest = async (names: array<string>, index: int): option<(string, string)> => {
    if index >= Array.length(names) {
      None
    } else {
      switch names[index] {
      | None => await tryReadManifest(names, index + 1)
      | Some(name) => {
          let path = join(repoPath, name)
          try {
            let content = await readFile(path, "utf-8")
            Some((content, path))
          } catch {
          | _ => await tryReadManifest(names, index + 1)
          }
        }
      }
    }
  }

  let result = await tryReadManifest(manifestNames, 0)

  switch result {
  | None => {
      let msg = `No AI manifest found in ${repoPath}. Expected one of: ${Array.join(
          manifestNames,
          ", ",
        )}`
      JsError.throwWithMessage(msg)
    }
  | Some((manifestContent, _manifestPath)) => {
      // Compute SHA-256 hash
      let hashObj = createHash("sha256")
      let hash = hashObj->update(manifestContent)->digest("hex")

      // Parse manifest structure
      let canonicalLocations = extractCanonicalLocations(manifestContent)
      let invariants = extractInvariants(manifestContent)

      {
        hash,
        canonicalLocations,
        invariants,
        parsedAt: Date.make(),
      }
    }
  }
}

// Validate manifest attestation hash
let validateAttestation = (manifest: Types.aiManifest, providedHash: string): bool => {
  manifest.hash === providedHash
}
