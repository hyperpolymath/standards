// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
// Generate proof data using proven library bindings

// Use the ReScript SafeUrl bindings directly
let urls = [
  "https://example.com/unsubscribe?token=abc123",
  "http://example.com/unsubscribe?token=abc123",
  "not-a-url",
]

type urlProof = {
  input: string,
  parse_ok: bool,
  https: bool,
  error: option<string>,
}

type consentProof = {
  id: string,
  initial_request: string,
  confirmation: string,
  token: string,
  valid: bool,
  reason: string,
}

let urlProofs = urls->Array.map(input => {
  let parsed = ProvenSafeUrl.parse(input)
  switch parsed {
  | Ok(_) => {input, parse_ok: true, https: ProvenSafeUrl.isHttps(input), error: None}
  | Error(e) => {input, parse_ok: false, https: false, error: Some(e)}
  }
})

let consentProofs: array<consentProof> = [
  {
    id: "consent-ok",
    initial_request: "2026-02-01T12:00:00Z",
    confirmation: "2026-02-01T12:00:30Z",
    token: "user_123_consent_token_abc",
    valid: true,
    reason: "confirmation after request, token length >= 10",
  },
  {
    id: "consent-invalid",
    initial_request: "2026-02-01T12:00:30Z",
    confirmation: "2026-02-01T12:00:10Z",
    token: "short",
    valid: false,
    reason: "confirmation before request or token too short",
  },
]

let main = async () => {
  let data = {
    "generated_at": Date.make()->Date.toISOString,
    "urls": urlProofs,
    "consent": consentProofs,
  }

  await Deno_Api.writeTextFile(
    "public/proof-data.json",
    JSON.stringifyAnyWithIndent(data, 2)->Option.getOr("{}") ++ "\n",
  )

  Console.log("Wrote public/proof-data.json")
}

let _ = main()
