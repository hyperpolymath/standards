// SPDX-License-Identifier: PMPL-1.0-or-later
// VeriSimDB query client — executes VCL queries, returns octad rows.
//
// Thin wrapper over fetch(). Consumers (panels) call one of the
// typed helpers below rather than constructing raw VCL strings.

type octadRow = {
  "id": string,
  "entity": string,
  "semantic": Js.Dict.t<Js.Json.t>,
  "temporal": Js.Dict.t<Js.Json.t>,
}

type queryResult = {
  rows: array<octadRow>,
  queryMs: int,
}

@val external fetch: (string, 'a) => promise<'b> = "fetch"

let baseUrl = "http://localhost:8097"

let runVcl = async (vcl: string): result<queryResult, string> => {
  let body = Js.Json.stringifyAny({"vcl": vcl})->Belt.Option.getWithDefault("{}")
  let init = {
    "method": "POST",
    "headers": Js.Dict.fromArray([("Content-Type", "application/json")]),
    "body": body,
  }
  try {
    let resp = await fetch(baseUrl ++ "/vcl/query", init)
    let json: queryResult = %raw("resp.json()")
    Ok(json)
  } catch {
  | _ => Error("VeriSimDB query failed — is it running on port 8097?")
  }
}

// ─────────────────────────────────────────────────────────────
// Typed queries — one per panel use-case
// ─────────────────────────────────────────────────────────────

let gradeDistribution = () =>
  runVcl(`octads(entity="crg-grade") |> latest_per("component") |> group_by("grade") |> count()`)

let openFindings = () =>
  runVcl(
    `octads(entity="compliance-scan") |> where("temporal.resolved_at == null") |> group_by("repo, severity") |> count()`,
  )

let currentProofs = () =>
  runVcl(
    `octads(entity="proof-status") |> latest_per("file, theorem_name") |> project("file, prover, theorem_name, trust_level, verified_at")`,
  )

let staleProofs = (days: int) =>
  runVcl(
    `octads(entity="proof-status") |> where("temporal.proof_age_days > ${days->Belt.Int.toString}") |> order_by("proof_age_days desc")`,
  )
