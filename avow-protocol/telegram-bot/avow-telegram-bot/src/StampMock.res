// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

/**
 * Mock STAMP verification library (ReScript)
 *
 * Temporary implementation for MVP. Will be replaced with real libstamp FFI.
 */

// ============================================================================
// Types
// ============================================================================

type unsubscribeParams = {
  url: string,
  tested_at: float,
  response_code: int,
  response_time: int,
  token: string,
  signature: string,
}

type consentParams = {
  initial_request: float,
  confirmation: float,
  ip_address: string,
  token: string,
}

type rateLimitParams = {
  sender_id: string,
  account_created: float,
  messages_today: int,
  daily_limit: int,
}

type proof = {
  type_: string,
  data: Js.Json.t,
  timestamp: float,
  signature: string,
}

type verificationResult =
  | Success
  | ErrorInvalidUrl
  | ErrorTimeout
  | ErrorInvalidResponse
  | ErrorInvalidSignature
  | ErrorRateLimitExceeded
  | ErrorConsentInvalid
  | ErrorNullPointer
  | ErrorInternal

// ============================================================================
// Verification Functions
// ============================================================================

let verifyUnsubscribe = (params: unsubscribeParams): verificationResult => {
  // Check URL format
  if !Js.String2.startsWith(params.url, "https://") {
    ErrorInvalidUrl
  } else {
    // Check test was recent (within last 60 seconds)
    let now = Js.Date.now()
    let age_ms = now -. params.tested_at

    if age_ms > 60000.0 || age_ms < 0.0 {
      ErrorTimeout
    } else if params.response_code !== 200 {
      ErrorInvalidResponse
    } else if params.response_time >= 200 {
      ErrorTimeout
    } else if params.signature == "" {
      ErrorInvalidSignature
    } else {
      Success
    }
  }
}

let verifyConsent = (params: consentParams): verificationResult => {
  // Check confirmation happened AFTER initial request
  if params.confirmation <= params.initial_request {
    ErrorConsentInvalid
  } else {
    // Check confirmation was timely (within 24 hours)
    let time_diff = params.confirmation -. params.initial_request

    if time_diff > 86400000.0 {
      ErrorConsentInvalid
    } else if params.token == "" {
      ErrorInvalidSignature
    } else {
      Success
    }
  }
}

let verifyRateLimit = (params: rateLimitParams): verificationResult => {
  // Check messages don't exceed limit
  if params.messages_today >= params.daily_limit {
    ErrorRateLimitExceeded
  } else {
    // Check daily limit is appropriate for account age
    let now = Js.Date.now()
    let age_ms = now -. params.account_created
    let age_days = age_ms /. (24.0 *. 60.0 *. 60.0 *. 1000.0)

    let max_limit = if age_days < 30.0 {
      1000
    } else if age_days < 90.0 {
      10000
    } else {
      100000
    }

    if params.daily_limit > max_limit {
      ErrorRateLimitExceeded
    } else {
      Success
    }
  }
}

// ============================================================================
// Proof Generation
// ============================================================================

let generateProof = (
  type_: [#unsubscribe | #consent | #rateLimit],
  data: Js.Json.t,
): proof => {
  let timestamp = Js.Date.now()
  let random = Js.Math.random_int(0, 999999)->Belt.Int.toString
  let signature = `mock_sig_${timestamp->Belt.Float.toString}_${random}`

  let type_str = switch type_ {
  | #unsubscribe => "unsubscribe_verification"
  | #consent => "consent_verification"
  | #rateLimit => "rate_limit_verification"
  }

  {
    type_: type_str,
    data: data,
    timestamp: timestamp,
    signature: signature,
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

let resultToString = (result: verificationResult): string => {
  switch result {
  | Success => "✓ SUCCESS"
  | ErrorInvalidUrl => "✗ INVALID_URL"
  | ErrorTimeout => "✗ TIMEOUT"
  | ErrorInvalidResponse => "✗ INVALID_RESPONSE"
  | ErrorInvalidSignature => "✗ INVALID_SIGNATURE"
  | ErrorRateLimitExceeded => "✗ RATE_LIMIT_EXCEEDED"
  | ErrorConsentInvalid => "✗ CONSENT_INVALID"
  | ErrorNullPointer => "✗ NULL_POINTER"
  | ErrorInternal => "✗ INTERNAL_ERROR"
  }
}

let formatProof = (proof: proof): string => {
  Js.Json.stringifyWithSpace(
    Js.Json.object_(
      Js.Dict.fromArray([
        ("type", Js.Json.string(proof.type_)),
        ("data", proof.data),
        ("timestamp", Js.Json.number(proof.timestamp)),
        ("signature", Js.Json.string(proof.signature)),
      ])
    ),
    2
  )
}

let generateUnsubscribeUrl = (userId: int, token: string): string => {
  `https://stamp-bot.example.com/unsubscribe?user=${userId->Belt.Int.toString}&token=${token}`
}

let testUnsubscribeUrl = async (url: string): promise<(int, int)> => {
  let start = Js.Date.now()

  // Simulate network delay
  let delay = 50.0 +. Js.Math.random() *. 100.0
  await Js.Promise2.make((~resolve, ~reject as _) => {
    let _ = Js.Global.setTimeout(() => resolve(. ()), delay->Belt.Float.toInt)
  })

  let response_time = (Js.Date.now() -. start)->Belt.Float.toInt
  let response_code = if Js.String2.startsWith(url, "https://") { 200 } else { 404 }

  (response_code, response_time)
}

let generateToken = (userId: int): string => {
  let random = Js.Math.random()
    ->Belt.Float.toString
    ->Js.String2.slice(~from=2, ~to_=15)
  let timestamp = Js.Date.now()
    ->Belt.Float.toInt
    ->Belt.Int.toString
  `${userId->Belt.Int.toString}_${timestamp}_${random}`
}

let generateSignature = (data: string): string => {
  let timestamp = Js.Date.now()->Belt.Float.toInt->Belt.Int.toString
  let length = data->Js.String2.length->Belt.Int.toString
  let random = Js.Math.random()
    ->Belt.Float.toString
    ->Js.String2.slice(~from=2, ~to_=9)
  `sig_${timestamp}_${length}_${random}`
}

// ============================================================================
// JSON Encoding Helpers (for proof generation)
// ============================================================================

let unsubscribeParamsToJson = (params: unsubscribeParams): Js.Json.t => {
  Js.Json.object_(
    Js.Dict.fromArray([
      ("url", Js.Json.string(params.url)),
      ("tested_at", Js.Json.number(params.tested_at)),
      ("response_code", Js.Json.number(params.response_code->Belt.Int.toFloat)),
      ("response_time", Js.Json.number(params.response_time->Belt.Int.toFloat)),
      ("token", Js.Json.string(params.token)),
      ("signature", Js.Json.string(params.signature)),
    ])
  )
}

let consentParamsToJson = (params: consentParams): Js.Json.t => {
  Js.Json.object_(
    Js.Dict.fromArray([
      ("initial_request", Js.Json.number(params.initial_request)),
      ("confirmation", Js.Json.number(params.confirmation)),
      ("ip_address", Js.Json.string(params.ip_address)),
      ("token", Js.Json.string(params.token)),
    ])
  )
}

let rateLimitParamsToJson = (params: rateLimitParams): Js.Json.t => {
  Js.Json.object_(
    Js.Dict.fromArray([
      ("sender_id", Js.Json.string(params.sender_id)),
      ("account_created", Js.Json.number(params.account_created)),
      ("messages_today", Js.Json.number(params.messages_today->Belt.Int.toFloat)),
      ("daily_limit", Js.Json.number(params.daily_limit->Belt.Int.toFloat)),
    ])
  )
}
